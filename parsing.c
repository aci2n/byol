#include "mpc.h"
#include <errno.h>
#include <readline/readline.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum ltype ltype;
enum ltype {
  LTYPE_NUM,
  LTYPE_ERR,
};

typedef enum lerr lerr;
enum lerr {
  LERR_ZERODIV,
  LERR_NAN,
};

typedef struct lval lval;
struct lval {
  ltype type;
  union {
    long numval;
    lerr err;
  };
};

static void dump_ast(mpc_ast_t *a, size_t d) {
  /* mpc_ast_print(a); */
  char indent[10] = {0};
  for (size_t i = 0; i < d && i < sizeof(indent) - 1; i++) {
    indent[i] = ' ';
  }
  printf("%sTag: %s\n", indent, a->tag);
  printf("%sContents: %s\n", indent, a->contents);
  printf("%s# children: %d\n", indent, a->children_num);
  for (size_t i = 0; i < a->children_num; i++) {
    mpc_ast_t *c = a->children[i];
    dump_ast(c, d + 1);
  }
}

static bool has_tag(mpc_ast_t *a, char *tag) {
  /* printf("has_tag(%s, %s)\n", a->tag, tag); */
  char *pos = strstr(a->tag, tag);
  if (pos == NULL) {
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

static lval num_lval(long val) {
  return (lval){.type = LTYPE_NUM, .numval = val};
}

static lval apply_op(char *op, lval left, lval right) {
  if (left.type == LTYPE_ERR) {
    return left;
  }
  if (right.type == LTYPE_ERR) {
    return right;
  }
  if (streq(op, "+")) {
    return num_lval(left.numval + right.numval);
  }
  if (streq(op, "-")) {
    return num_lval(left.numval - right.numval);
  }
  if (streq(op, "*")) {
    return num_lval(left.numval * right.numval);
  }
  if (streq(op, "/")) {
    if (right.numval == 0) {
      return (lval){.type = LTYPE_ERR, .err = LERR_ZERODIV};
    }
    return num_lval(left.numval / right.numval);
  }
  return num_lval(0);
}

static lval eval(mpc_ast_t *a) {
  /* printf("eval(%s, %s)\n", a->tag, a->contents); */
  if (has_tag(a, "number")) {
		errno = 0;
    long numval = strtol(a->contents, NULL, 10);
    if (errno == EINVAL || errno == ERANGE) {
      return (lval){.type = LTYPE_ERR, .err = LERR_NAN};
    }
    return num_lval(numval);
  }
  if (has_tag(a, "expr")) {
    char *op = a->children[1]->contents;
    lval ret = eval(a->children[2]);
    for (size_t i = 3; i < a->children_num - 1; i++) {
      ret = apply_op(op, ret, eval(a->children[i]));
    }
    return ret;
  }
  if (has_tag(a, ">")) {
    return eval(a->children[1]);
  }
  return num_lval(0);
}

static char *lerr_str(lerr err) {
  switch (err) {
  case LERR_ZERODIV:
    return "zerodiv";
  case LERR_NAN:
		return "nan";
  }
}

static void lval_print(lval v) {
  switch (v.type) {
  case LTYPE_NUM:
    printf("num %ld\n", v.numval);
    break;
  case LTYPE_ERR:
    printf("err %s\n", lerr_str(v.err));
    break;
  }
}

int main(int argc, char **argv) {
  mpc_parser_t *Number = mpc_new("number");
  mpc_parser_t *Operator = mpc_new("operator");
  mpc_parser_t *Expr = mpc_new("expr");
  mpc_parser_t *Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
            "number: /-?[0-9]+/ ;\n"
            "operator: '+' | '-' | '*' | '/' ;\n"
            "expr: <number> | '(' <operator> <expr>+ ')' ;\n"
            "lispy: /^/ <expr> /$/ ;\n",
            Number, Operator, Expr, Lispy);

  mpc_result_t r = {0};

  while (1) {
    char *input = readline("lispy> ");
    if (mpc_parse("<stdin>", input, Lispy, &r)) {
      mpc_ast_t *a = r.output;
      /* dump_ast(a, 0); */
      lval_print(eval(a));
      mpc_ast_delete(a);
    } else {
      mpc_err_print(r.error);
      mpc_err_delete(r.error);
    }
  }

  mpc_cleanup(4, Number, Operator, Expr, Lispy);

  return 0;
}
