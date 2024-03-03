#include "dbg.h"

#include <stdio.h>
#include <stdlib.h>

void test_debug() {
    debug("I have Brown Hair.");
    debug("I am %d years old.", 37);
}

void test_log_err() { log_err("I believe everthing is broken."); }

void test_log_info() { log_info("Well I did something mundane."); }

int test_check(char *file_name) {
    FILE *input = NULL;
    char *block = NULL;

    block = malloc(100);
    check_mem(block);

    input = fopen(file_name, "r");
    check(input, "Failed to open %s.", file_name);

    free(block);
    fclose(input);

    log_info("Func test_check fininshed successfully.");
    return 0;

error:
    if (block) {
        free(block);
    }
    if (input) {
        fclose(input);
    }
    return -1;
}

int main(int argc, char *argv[]) {
    check(argc == 1, "Need no arguments");

    test_debug();
    test_log_err();
    test_log_info();

    check(test_check("./ext20.c") == 0, "failed with ext20.c");
    check(test_check("./dbg.h") == 0, "failed with dbg.h");

    return EXIT_SUCCESS;

error:
    return EXIT_FAILURE;
}
