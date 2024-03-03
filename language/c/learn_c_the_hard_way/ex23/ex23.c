#include <stdlib.h>

#include "dbg.h"

int normal_copy(char *from, char *to, int count) {
    int i = 0;

    for (i = 0; i < count; i++) {
        to[i] = from[i];
    }

    return i;
}

int duffs_device(char *from, char *to, int count) {
    {
        int n = (count + 7) / 8;

        switch (count % 8) {
        case 0:
            do {
                *to++ = *from++;
            case 7:
                *to++ = *from++;
            case 6:
                *to++ = *from++;
            case 5:
                *to++ = *from++;
            case 4:
                *to++ = *from++;
            case 3:
                *to++ = *from++;
            case 2:
                *to++ = *from++;
            case 1:
                *to++ = *from++;
            } while (--n > 0);
        }
    }

    return count;
}

int valid_copy(char *data, int count, char expects) {
    int i = 0;

    for (i = 0; i < count; i++) {
        if (data[i] != expects) {
            log_err("[%d] %c != %c", i, data[i], expects);
            return 0;
        }
    }

    return 1;
}

int main(int argc, char *argv[]) {

    char from[1003] = {'a'};
    char to[1003] = {'c'};
    int rc = 0;

    memset(from, 'x', 1003);
    memset(to, 'y', 1003);
    check(valid_copy(from, 1003, 'x'), "from: Not initialized right.");
    check(valid_copy(to, 1003, 'y'), "to: Not initialized right.");

    rc = normal_copy(from, to, 1003);
    check(rc == 1003, "Normaly copy failed: %d", rc);
    check(valid_copy(to, 1003, 'x'), "Normal copy failed.");

    // Reset
    memset(to, 'y', 1003);

    // Duffs version
    rc = duffs_device(from, to, 1003);
    check(rc = 1003, "Duff's device failed %d", rc);
    check(valid_copy(to, 1003, 'x'), "Duff's device failed copy.");

    log_info("Waku! Waku!");
    return EXIT_SUCCESS;

error:
    return EXIT_FAILURE;
}
