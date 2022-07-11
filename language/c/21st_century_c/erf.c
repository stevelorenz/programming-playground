#include <math.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf(
        "The integral of a Normal(0, 1) distribution "
        "between -1.96 and 1.96 is: %g\n",
        erf(1.96 * sqrt(1 / 2.0)));
    return 0;
}
