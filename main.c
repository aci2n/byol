#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include "mpc.h"

int main(int argc, char **argv) {
  puts("Lispy Version 0.0.1");
  puts("Press Ctrl+c to Exit\n");
  mpc_parser_t *Adjective = mpc_new("adjective");

	free(Adjective);
	
  while (1) {
    char *input = readline("lispy> ");
    add_history(input);
    printf("input: %s\n", input);
		free(input);
  }

	return EXIT_SUCCESS;
}
