#include "become_daemon.h"
#include "tlpi_hdr.h"

int main() {
	becomeDaemon(0);
	sleep(20);
	exit(EXIT_SUCCESS);
}
