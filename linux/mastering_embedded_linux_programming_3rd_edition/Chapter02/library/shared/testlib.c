#include "testlib.h"
#include <stdio.h>

int add_ints(int n1, int n2) {
    printf("%s\n", __func__);
    return n1 + n2;
}

int multiply_ints(int n1, int n2) {
    printf("%s\n", __func__);
    return n1 * n2;
}
