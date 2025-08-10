#include "mpc.h"
#include <errno.h>
#include <readline/history.h>
#include <readline/readline.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct lenv lenv;
typedef struct lval lval;
typedef enum ltype ltype;
typedef lval *(*lbuiltin)(lenv *, lval *);

static lval *lval_eval(lenv *, lval *);
static lval *lval_add(lval *, lval *);
static lval *lval_pop(lval *, size_t);
static lval *lval_take(lval *, size_t);
static lval *lval_read(mpc_ast_t *);
static void lval_delete(lval *);
static void lval_print(lval *);
static lval *lval_copy(lval *);
static bool streq(char *, char *);
static lenv *lenv_new();
static lenv *lenv_copy(lenv *);
static void lenv_delete(lenv *);
static lval *lenv_get(lenv *, lval *);
static void lenv_put(lenv *, lval *, lval *);
static void lenv_def(lenv *, lval *, lval *);

enum ltype {
  LTYPE_NUM,
  LTYPE_ERR,
  LTYPE_SYM,
  LTYPE_SEXP,
  LTYPE_QEXP,
  LTYPE_FUN,
};

struct lenv {
  lenv *par;
  size_t count;
  char **syms;
  lval **vals;
};

struct lval {
  ltype type;
  union {
    long num;
    char *sym;
    char *err;
    struct {
      lenv *env;
      lbuiltin builtin;
      lval *formals;
      lval *body;
    };
    struct {
      size_t count;
      lval **cell;
    };
  };
};

static char *ltype_name(ltype t) {
  switch (t) {
  case LTYPE_NUM:
    return "Number";
  case LTYPE_SYM:
    return "Symbol";
  case LTYPE_FUN:
    return "Function";
  case LTYPE_ERR:
    return "Error";
  case LTYPE_SEXP:
    return "S-Expression";
  case LTYPE_QEXP:
    return "Q-Expression";
  default:
    return "Unknown";
  }
}

static bool has_tag(mpc_ast_t *a, char *tag) {
  /* printf("has_tag(%s, %s)\n", a->tag, tag); */
  char *pos = strstr(a->tag, tag);
  if (pos == 0) {
    return false;
  }
  size_t taglen = strlen(tag);
  bool delim_left = pos == a->tag || *(pos - 1) == '|';
  bool delim_right = *(pos + taglen) == 0 || *(pos + taglen) == '|';
  /* printf("left: %d, right: %d, %s %s\n", delim_left, delim_right, a->tag,
   * tag); */
  return delim_left && delim_right;
}

static bool streq(char *left, char *right) { return strcmp(left, right) == 0; }

static lval *lval_num(long val) {
  lval *ret = malloc(sizeof(lval));
  *ret = (lval){.type = LTYPE_NUM, .num = val};
  return ret;
}

static lval *lval_err(char *fmt, ...) {
  lval *ret = malloc(sizeof(lval));
  char err[512] = {0};
  va_list va = {0};

  va_start(va, fmt);
  vsnprintf(err, sizeof(err) - 1, fmt, va);
  va_end(va);

  *ret = (lval){.type = LTYPE_ERR, .err = strdup(err)};

  return ret;
}

static lval *lval_sym(char *sym) {
  lval *ret = malloc(sizeof(lval));
  *ret = (lval){.type = LTYPE_SYM, .sym = strdup(sym)};
  return ret;
}

static lval *lval_sexp() {
  lval *ret = malloc(sizeof(lval));
  *ret = (lval){.type = LTYPE_SEXP};
  return ret;
}

static lval *lval_qexp() {
  lval *ret = malloc(sizeof(lval));
  *ret = (lval){.type = LTYPE_QEXP};
  return ret;
}

static lval *lval_fun(lbuiltin fun) {
  lval *ret = malloc(sizeof(lval));
  *ret = (lval){.type = LTYPE_FUN, .builtin = fun};
  return ret;
}

static lval *lval_lambda(lval *formals, lval *body) {
  lval *ret = malloc(sizeof(lval));
  *ret = (lval){
      .type = LTYPE_FUN,
      .env = lenv_new(),
      .formals = formals,
      .body = body,
  };
  return ret;
}

static void lval_delete(lval *val) {
  if (val) {
    switch (val->type) {
    case LTYPE_ERR:
      free(val->err);
      break;
    case LTYPE_SYM:
      free(val->sym);
      break;
    case LTYPE_SEXP:
    case LTYPE_QEXP:
      for (size_t i = 0; i < val->count; i++) {
        lval_delete(val->cell[i]);
      }
      free(val->cell);
      break;
    case LTYPE_FUN:
      if (val->env) {
        lenv_delete(val->env);
      }
      if (val->formals) {
        lval_delete(val->formals);
      }
      if (val->body) {
        lval_delete(val->body);
      }
      break;
    default:
      break;
    }
    free(val);
  }
}

#define LASSERT(args, cond, fmt, ...)                                          \
  if (!(cond)) {                                                               \
    lval *err = lval_err(fmt, ##__VA_ARGS__);                                  \
    lval_delete(args);                                                         \
    return err;                                                                \
  }

#define LASSERT_NUM(fn, args, exp)                                             \
  LASSERT(args, args->count == exp, "'%s' expected %zu args, got %zu", fn,     \
          exp, args->count)

#define LASSERT_NEMPTY(fn, args, i)                                            \
  LASSERT(args, args->cell[i]->count > 0, "%s got empty list at %zu", fn, i)

#define LASSERT_TYPE(fn, args, i, expected_type)                               \
  LASSERT(args, args->cell[i]->type == expected_type,                          \
          "'%s' expected %s, got %s at index %zu", fn,                         \
          ltype_name(expected_type), ltype_name(args->cell[i]->type), i)

static lval *builtin_op(lenv *e, lval *v, char *op) {
  if (v->count == 0) {
    lval_delete(v);
    return lval_err("No args for %s", op);
  }

  for (size_t i = 0; i < v->count; i++) {
    LASSERT_TYPE(op, v, i, LTYPE_NUM);
  }

  lval *x = lval_pop(v, 0);

  if (streq(op, "-") && v->count == 0) {
    x->num = -x->num;
  }

  while (v->count > 0) {
    lval *y = lval_pop(v, 0);

    if (streq(op, "+")) {
      x->num += y->num;
    } else if (streq(op, "-")) {
      x->num -= y->num;
    } else if (streq(op, "*")) {
      x->num *= y->num;
    } else if (streq(op, "/")) {
      if (y->num == 0) {
        lval_delete(y);
        lval_delete(x);
        lval_delete(v);
        return lval_err("Division by zero: %ld/%ld", x->num, y->num);
      }
      x->num /= y->num;
    }
  }

  lval_delete(v);
  return x;
}

static lval *builtin_add(lenv *e, lval *v) { return builtin_op(e, v, "+"); }
static lval *builtin_sub(lenv *e, lval *v) { return builtin_op(e, v, "-"); }
static lval *builtin_mul(lenv *e, lval *v) { return builtin_op(e, v, "*"); }
static lval *builtin_div(lenv *e, lval *v) { return builtin_op(e, v, "/"); }

static lval *builtin_head(lenv *e, lval *v) {
  LASSERT_NUM("head", v, 1);
  LASSERT_TYPE("head", v, 0, LTYPE_QEXP);
  LASSERT_NEMPTY("head", v, 0);
  lval *x = lval_take(v, 0);
  while (x->count > 1) {
    lval_delete(lval_pop(x, 1));
  }
  return x;
}

static lval *builtin_tail(lenv *e, lval *v) {
  LASSERT_NUM("tail", v, 1);
  LASSERT_TYPE("tail", v, 0, LTYPE_QEXP);
  LASSERT_NEMPTY("tail", v, 0);
  lval *x = lval_take(v, 0);
  lval_delete(lval_pop(x, 0));
  return x;
}

static lval *builtin_list(lenv *e, lval *v) {
  v->type = LTYPE_QEXP;
  return v;
}

static lval *builtin_eval(lenv *e, lval *v) {
  LASSERT_NUM("eval", v, 1);
  LASSERT_TYPE("eval", v, 0, LTYPE_QEXP);
  lval *x = lval_take(v, 0);
  x->type = LTYPE_SEXP;
  return lval_eval(e, x);
}

static lval *lval_join(lval *x, lval *y) {
  while (y->count > 0) {
    x = lval_add(x, lval_pop(y, 0));
  }
  lval_delete(y);
  return x;
}

static lval *lval_call(lenv *e, lval *f, lval *a) {
  if (f->builtin) {
    return f->builtin(e, a);
  }
  for (size_t i = 0; i < f->formals->count; i++) {
    lenv_put(f->env, f->formals->cell[i], a->cell[i]);
  }
  lval_delete(a);
  f->env->par = e;
  return builtin_eval(f->env, lval_add(lval_sexp(), lval_copy(f->body)));
}

static lval *builtin_join(lenv *e, lval *v) {
  for (size_t i = 0; i < v->count; i++) {
    LASSERT_TYPE("join", v, i, LTYPE_QEXP);
  }
  lval *x = lval_pop(v, 0);
  while (v->count > 0) {
    x = lval_join(x, lval_pop(v, 0));
  }
  lval_delete(v);
  return x;
}

static lval *builtin_var(lenv *e, lval *v, char *fn) {
  LASSERT(v, v->count > 0, "no args for '%s'", fn);
  LASSERT_TYPE(fn, v, 0, LTYPE_QEXP);
  lval *symlist = v->cell[0];
  for (size_t i = 0; i < symlist->count; i++) {
    LASSERT(v, symlist->cell[i]->type == LTYPE_SYM,
            "first '%s' arg must be a list of symbols, got %s at %zu", fn,
            ltype_name(symlist->cell[i]->type), i);
  }
  LASSERT(v, symlist->count == v->count - 1,
          "'%s' expected exactly %zu values, got %zu", symlist->count, fn,
          v->count - 1);
  for (size_t i = 0; i < symlist->count; i++) {
    if (streq(fn, "def")) {
      lenv_def(e, symlist->cell[i], v->cell[i + 1]);
    } else if (streq(fn, "=")) {
      lenv_put(e, symlist->cell[i], v->cell[i + 1]);
    } else {
      exit(1);
    }
  }
  lval_delete(v);
  return lval_sexp();
}

static lval *builtin_def(lenv *e, lval *v) { return builtin_var(e, v, "def"); }

static lval *builtin_put(lenv *e, lval *v) { return builtin_var(e, v, "="); }

static lval *builtin_lambda(lenv *e, lval *v) {
  LASSERT_NUM("\\", v, 2);
  LASSERT_TYPE("\\", v, 0, LTYPE_QEXP);
  LASSERT_TYPE("\\", v, 1, LTYPE_QEXP);

  lval *first = v->cell[0];
  for (size_t i = 0; i < first->count; i++) {
    LASSERT(v, first->cell[i]->type == LTYPE_SYM,
            "'lambda' formals must be a list of symbols, got %s at %zu",
            ltype_name(first->cell[i]->type), i);
  }

  lval *formals = lval_pop(v, 0);
  lval *body = lval_pop(v, 0);
  lval_delete(v);
  return lval_lambda(formals, body);
}

static lval *lval_pop(lval *v, size_t i) {
  lval *c = v->cell[i];
  memmove(&v->cell[i], &v->cell[i + 1], sizeof(*v->cell) * (v->count - i - 1));
  v->count--;
  if (v->count > 0) {
    v->cell = realloc(v->cell, sizeof(*c->cell) * v->count);
  } else {
    free(v->cell);
    v->cell = 0;
  }
  return c;
}

static lval *lval_take(lval *v, size_t i) {
  lval *c = lval_pop(v, i);
  lval_delete(v);
  return c;
}

static lval *lval_eval_sexp(lenv *e, lval *v) {
  for (size_t i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(e, v->cell[i]);
  }
  for (size_t i = 0; i < v->count; i++) {
    if (v->cell[i]->type == LTYPE_ERR) {
      return lval_take(v, i);
    }
  }
  if (v->count == 0) {
    return v;
  }
  if (v->count == 1) {
    return lval_take(v, 0);
  }
  lval *f = lval_pop(v, 0);
  if (f->type != LTYPE_FUN) {
    lval_delete(v);
    lval_delete(f);
    return lval_err("Not a function: %s", ltype_name(f->type));
  }
  lval *ret = lval_call(e, f, v);
  lval_delete(f);
  return ret;
}

static lval *lval_eval_sym(lenv *e, lval *v) {
  lval *x = lenv_get(e, v);
  lval_delete(v);
  return x;
}

static lval *lval_eval(lenv *e, lval *v) {
  switch (v->type) {
  case LTYPE_SEXP:
    return lval_eval_sexp(e, v);
  case LTYPE_SYM:
    return lval_eval_sym(e, v);
  default:
    return v;
  }
}

static void lval_print_exp(lval *v, char open, char end) {
  putchar(open);
  for (size_t i = 0; i < v->count; i++) {
    lval_print(v->cell[i]);
    if (i < v->count - 1) {
      printf(" ");
    }
  }
  putchar(end);
}

static void lval_print(lval *v) {
  switch (v->type) {
  case LTYPE_NUM:
    printf("%ld", v->num);
    break;
  case LTYPE_ERR:
    printf("error: %s", v->err);
    break;
  case LTYPE_SYM:
    printf("%s", v->sym);
    break;
  case LTYPE_SEXP:
    lval_print_exp(v, '(', ')');
    break;
  case LTYPE_QEXP:
    lval_print_exp(v, '{', '}');
    break;
  case LTYPE_FUN:
    if (v->builtin) {
      printf("<builtin>");
    } else {
      printf("(\\ ");
      lval_print(v->formals);
      putchar(' ');
      lval_print(v->body);
      putchar(')');
    }
    break;
  }
}

static lval *lval_copy(lval *v) {
  lval *ret = malloc(sizeof(lval));
  *ret = (lval){.type = v->type};

  switch (v->type) {
  case LTYPE_NUM:
    ret->num = v->num;
    break;
  case LTYPE_FUN:
    if (v->builtin) {
      ret->builtin = v->builtin;
    } else {
      ret->formals = lval_copy(v->formals);
      ret->body = lval_copy(v->body);
      ret->env = lenv_copy(v->env);
    }
    break;
  case LTYPE_SYM:
    ret->sym = strdup(v->sym);
    break;
  case LTYPE_ERR:
    ret->err = v->err;
    break;
  case LTYPE_SEXP:
  case LTYPE_QEXP:
    ret->count = v->count;
    ret->cell = malloc(sizeof(*ret->cell) * ret->count);
    for (size_t i = 0; i < v->count; i++) {
      ret->cell[i] = lval_copy(v->cell[i]);
    }
    break;
  }

  return ret;
}

static lval *lval_read_num(mpc_ast_t *a) {
  errno = 0;
  long num = strtol(a->contents, 0, 10);
  if (errno == EINVAL || errno == ERANGE) {
    return lval_err("Not a number: %s", a->contents);
  }
  return lval_num(num);
}

static lval *lval_add(lval *v, lval *c) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(*v->cell) * v->count);
  v->cell[v->count - 1] = c;
  return v;
}

static lval *lval_read_children(lval *v, mpc_ast_t *a) {
  for (size_t i = 0; i < a->children_num; i++) {
    lval *c = lval_read(a->children[i]);
    if (c) {
      v = lval_add(v, c);
    }
  }
  return v;
}

static lval *lval_read(mpc_ast_t *a) {
  if (has_tag(a, "number")) {
    return lval_read_num(a);
  }
  if (has_tag(a, "symbol")) {
    return lval_sym(a->contents);
  }
  if (has_tag(a, "qexp")) {
    return lval_read_children(lval_qexp(), a);
  }
  if (has_tag(a, ">") || has_tag(a, "sexp")) {
    return lval_read_children(lval_sexp(), a);
  }
  return 0;
}

static lenv *lenv_new() {
  lenv *e = malloc(sizeof(*e));
  *e = (lenv){0};
  return e;
}

static lenv *lenv_copy(lenv *e) {
  lenv *c = lenv_new();
  *c = (lenv){.par = e->par};
  for (size_t i = 0; i < e->count; i++) {
    lenv_put(c, lval_sym(e->syms[i]), e->vals[i]);
  }
  return c;
}

static void lenv_delete(lenv *e) {
  if (e) {
    for (size_t i = 0; i < e->count; i++) {
      free(e->syms[i]);
      lval_delete(e->vals[i]);
    }
    if (e->syms) {
      free(e->syms);
    }
    if (e->vals) {
      free(e->vals);
    }
    free(e);
  }
}

static lval *lenv_get(lenv *e, lval *k) {
  if (k->type != LTYPE_SYM) {
    return lval_err("Not a symbol: %s", ltype_name(k->type));
  }
  for (size_t i = 0; i < e->count; i++) {
    if (streq(e->syms[i], k->sym)) {
      return lval_copy(e->vals[i]);
    }
  }
  if (e->par) {
    return lenv_get(e->par, k);
  }
  return lval_err("Unbound symbol %s", k->sym);
}

static void lenv_put(lenv *e, lval *k, lval *v) {
  if (k->type != LTYPE_SYM) {
    return;
  }
  for (size_t i = 0; i < e->count; i++) {
    if (streq(e->syms[i], k->sym)) {
      e->vals[i] = lval_copy(v);
      return;
    }
  }
  e->count++;
  e->syms = realloc(e->syms, sizeof(*e->syms) * e->count);
  e->vals = realloc(e->vals, sizeof(*e->vals) * e->count);
  e->syms[e->count - 1] = strdup(k->sym);
  e->vals[e->count - 1] = lval_copy(v);
}

static void lenv_def(lenv *e, lval *k, lval *v) {
  while (e->par) {
    e = e->par;
  }
  lenv_put(e, k, v);
}

static void lenv_add_builtin(lenv *e, char *name, lbuiltin fun) {
  lval *k = lval_sym(name);
  lval *v = lval_fun(fun);
  lenv_put(e, k, v);
  lval_delete(k);
  lval_delete(v);
}

static void lenv_add_builtins(lenv *e) {
  lenv_add_builtin(e, "list", builtin_list);
  lenv_add_builtin(e, "head", builtin_head);
  lenv_add_builtin(e, "tail", builtin_tail);
  lenv_add_builtin(e, "eval", builtin_eval);
  lenv_add_builtin(e, "join", builtin_join);
  lenv_add_builtin(e, "def", builtin_def);
  lenv_add_builtin(e, "\\", builtin_lambda);
  lenv_add_builtin(e, "=", builtin_put);
  lenv_add_builtin(e, "+", builtin_add);
  lenv_add_builtin(e, "-", builtin_sub);
  lenv_add_builtin(e, "*", builtin_mul);
  lenv_add_builtin(e, "/", builtin_div);
}

int main(int argc, char **argv) {
  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Symbol = mpc_new("symbol");
  mpc_parser_t *Sexp = mpc_new("sexp");
  mpc_parser_t *Qexp = mpc_new("qexp");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
            "number: /-?[0-9]+/ ;\n"
            "symbol: /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;\n"
            "sexp: '(' <expr>* ')' ;\n"
            "qexp: '{' <expr>* '}' ;\n"
            "expr: <number> | <symbol> | <sexp> | <qexp> ;\n"
            "lispy: /^/ <expr>* /$/ ;\n",
            Number, Symbol, Sexp, Qexp, Expr, Lispy);

  mpc_result_t r = {0};
  lenv *e = lenv_new();

  lenv_add_builtins(e);

  while (1) {
    char *input = readline("lispy> ");
    add_history(input);

    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      mpc_ast_t *a = r.output;
      lval *v = lval_read(a);

      lval_print(v);
      printf("\n");

      lval *x = lval_eval(e, v);
      lval_print(x);
      printf("\n");

      lval_delete(x);
      mpc_ast_delete(a);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
  }

  lenv_delete(e);
  mpc_cleanup(6, Number, Symbol, Sexp, Qexp, Expr, Lispy);

  return 0;
}
