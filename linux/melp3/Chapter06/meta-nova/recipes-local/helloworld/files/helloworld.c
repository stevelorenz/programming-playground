#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
    while (1) {
        printf("Hello, world!\n");
        sleep(10);
    }
    return 0;
}
