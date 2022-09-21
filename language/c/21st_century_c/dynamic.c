/*
 * dynamic.c
 */

// clang-format off
#include <assert.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
// clang-format on

void get_a_function() {
	FILE *f = fopen("./fn.c", "w+");
	assert(f != NULL);
	fprintf(f,
			"#include <math.h>\n"
			"double fn(double in) {\n");

	char *a_line = NULL;
	char *prompt = ">>double fn(double in){\n>> ";
	do {
		free(a_line);
		a_line = readline(prompt);
		fprintf(f, "%s\n", a_line);
		prompt = ">> ";
	} while (strcmp(a_line, "}"));

	fclose(f);
}

void compile_and_run() {
	if (system("c99 -fPIC -shared ./fn.c -o ./fn.so") != 0) {
		fprintf(stderr, "Failed to compiled the fn.c source code.\n");
		return;
	}
	void *handle = NULL;
	handle = dlopen("./fn.so", RTLD_LAZY);
	if (handle == NULL) {
		fprintf(stderr, "Failed to load the dynamic library fn.so\n");
		dlerror();
	}
	fprintf(stdout, "Successfully loaded the fn.so library!\n");

	// Define the type of the function pointer
	typedef double (*fn_type)(double);
	fn_type f = dlsym(handle, "fn");
	assert(f != NULL);
	printf("f(1)=%g\n", f(1));
	printf("f(2)=%g\n", f(2));
	printf("f(3)=%g\n", f(10));

	dlclose(handle);
}

int main() {
	printf(
		"I'm going to run a function, but you need to firstly input the "
		"function body.\n"
		"Please input the function body and conclude it with a '}' alone on a "
		"new line.\n");
	get_a_function();
	compile_and_run();
	return EXIT_SUCCESS;
}
