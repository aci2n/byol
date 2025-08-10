#include "mpc.h"
#include <errno.h>
#include <readline/readline.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct lval lval;
typedef enum ltype ltype;
static lval *lval_eval(lval *);
static lval *lval_add(lval *, lval *);
static lval *lval_pop(lval *, size_t);
static lval *lval_take(lval *, size_t);
static lval *lval_read(mpc_ast_t *);
static void lval_print(lval *);

enum ltype {
  LTYPE_NUM,
  LTYPE_ERR,
  LTYPE_SYM,
  LTYPE_SEXP,
  LTYPE_QEXP,
};

struct lval {
  ltype type;
  union {
    long num;
    char *sym;
    char *err;
    struct {
      size_t count;
      lval **cell;
    };
  };
};

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

static lval *lval_err(char *err) {
  lval *ret = malloc(sizeof(lval));
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
    default:
      break;
    }
    free(val);
  }
}

#define LASSERT(args, cond, err)                                               \
  if (!(cond)) {                                                               \
    lval_delete(args);                                                         \
    return lval_err(err);                                                      \
  }

static lval *builtin_head(lval *v) {
  LASSERT(v, v->count == 1, "headargs");
  LASSERT(v, v->cell[0]->type == LTYPE_QEXP, "headtype");
  LASSERT(v, v->cell[0]->count != 0, "headempty");
  lval *x = lval_take(v, 0);
  while (x->count > 1) {
    lval_delete(lval_pop(x, 1));
  }
  return x;
}

static lval *builtin_tail(lval *v) {
  LASSERT(v, v->count == 1, "tailargs");
  LASSERT(v, v->cell[0]->type == LTYPE_QEXP, "tailtype");
  LASSERT(v, v->cell[0]->count != 0, "tailempty");
  lval *x = lval_take(v, 0);
  lval_delete(lval_pop(x, 0));
  return x;
}

static lval *builtin_list(lval *v) {
  v->type = LTYPE_QEXP;
  return v;
}

static lval *builtin_eval(lval *v) {
  LASSERT(v, v->count == 1, "evalargs");
  LASSERT(v, v->cell[0]->type == LTYPE_QEXP, "evaltype");
  lval *x = lval_take(v, 0);
  x->type = LTYPE_SEXP;
  return lval_eval(x);
}

static lval *lval_join(lval *x, lval *y) {
  while (y->count > 0) {
    x = lval_add(x, lval_pop(y, 0));
  }
  lval_delete(y);
  return x;
}

static lval *builtin_join(lval *v) {
  for (size_t i = 0; i < v->count; i++) {
    LASSERT(v, v->cell[i]->type == LTYPE_QEXP, "jointype");
  }
  lval *x = lval_pop(v, 0);
  while (v->count > 0) {
    x = lval_join(x, lval_pop(v, 0));
  }
  lval_delete(v);
  return x;
}

static lval *builtin_op(lval *v, char *op) {
  for (size_t i = 0; i < v->count; i++) {
    if (v->cell[i]->type != LTYPE_NUM) {
      lval_delete(v);
      return lval_err("nan");
    }
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
        return lval_err("zerodiv");
      }
      x->num /= y->num;
    }
  }

  lval_delete(v);
  return x;
}

static lval *lval_pop(lval *v, size_t i) {
  lval *c = v->cell[i];
  memmove(&v->cell[i], &v->cell[i + 1], sizeof(*v->cell) * (v->count - i - 1));
  v->count--;
  v->cell = realloc(v->cell, sizeof(*c->cell) * v->count);
  return c;
}

static lval *lval_take(lval *v, size_t i) {
  lval *c = lval_pop(v, i);
  lval_delete(v);
  return c;
}

static lval *builtin(lval *v, char *op) {
  if (streq(op, "list")) {
    return builtin_list(v);
  }
  if (streq(op, "head")) {
    return builtin_head(v);
  }
  if (streq(op, "tail")) {
    return builtin_tail(v);
  }
  if (streq(op, "join")) {
    return builtin_join(v);
  }
  if (streq(op, "eval")) {
    return builtin_eval(v);
  }
  if (strstr("+-/*", op) != 0) {
    return builtin_op(v, op);
  }
  lval_delete(v);
  return lval_err("unknownfun");
}

static lval *lval_eval_sexp(lval *v) {
  for (size_t i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(v->cell[i]);
  }
  for (size_t i = 0; i < v->count; i++) {
    if (v->cell[i]->type == LTYPE_ERR) {
      return lval_take(v, i);
    }
  }
  lval *s = lval_pop(v, 0);
  if (s->type != LTYPE_SYM) {
    lval_delete(v);
    lval_delete(s);
    return lval_err("nosym");
  }
  lval *ret = builtin(v, s->sym);
  lval_delete(s);
  return ret;
}

static lval *lval_eval(lval *v) {
  switch (v->type) {
  case LTYPE_SEXP:
    return lval_eval_sexp(v);
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
  }
}

static lval *lval_read_num(mpc_ast_t *a) {
  errno = 0;
  long num = strtol(a->contents, 0, 10);
  if (errno == EINVAL || errno == ERANGE) {
    return lval_err("nan");
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

int main(int argc, char **argv) {
  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Symbol = mpc_new("symbol");
  mpc_parser_t *Sexp = mpc_new("sexp");
  mpc_parser_t *Qexp = mpc_new("qexp");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
            "number: /-?[0-9]+/ ;\n"
            "symbol: \"list\" | \"head\" | \"tail\" | \"join\" | \"eval\" | "
            "'+' | '-' | '*' | '/' ;\n"
            "sexp: '(' <expr>* ')' ;\n"
            "qexp: '{' <expr>* '}' ;\n"
            "expr: <number> | <symbol> | <sexp> | <qexp> ;\n"
            "lispy: /^/ <expr>* /$/ ;\n",
            Number, Symbol, Sexp, Qexp, Expr, Lispy);

  mpc_result_t r = {0};

  while (1) {
    char *input = readline("lispy> ");
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      mpc_ast_t *a = r.output;

      lval *v = lval_read(a);
      lval_print(v);
      printf("\n");

      lval *e = lval_eval(v);
      lval_print(e);
      printf("\n");
      lval_delete(e);

      mpc_ast_delete(a);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
  }

  mpc_cleanup(6, Number, Symbol, Sexp, Qexp, Expr, Lispy);

  return 0;
}
