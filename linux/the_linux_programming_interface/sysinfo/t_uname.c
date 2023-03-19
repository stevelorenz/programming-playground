#include <sys/utsname.h>

#include "tlpi_hdr.h"

int main() {
	struct utsname uts;

	if (uname(&uts) == -1) {
		errExit("uname");
	}

    printf("Node name: %s\n", uts.nodename);
    printf("System name: %s\n", uts.sysname);
    printf("Release: %s\n", uts.release);
    printf("Version: %s\n", uts.version);
    printf("Machine: %s\n", uts.machine);

	exit(EXIT_SUCCESS);
}
