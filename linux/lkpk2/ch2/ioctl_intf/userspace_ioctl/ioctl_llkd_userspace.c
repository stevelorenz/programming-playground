#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "../ioctl_llkd.h"

int main(int argc, char *argv[]) {
	int fd = 0;
	int power = 0;

	if (argc < 2) {
		fprintf(stderr,
				"Usage: %s device_file\n\
  If device_file does not exist, create it using mknod(1) (as root)\n",
				argv[0]);
		exit(EXIT_FAILURE);
	}

	if ((fd = open(argv[1], O_RDWR, 0)) == -1) {
		perror("open");
		exit(EXIT_FAILURE);
	}
	printf("device opened: fd=%d\n", fd);

	printf(
		"(FYI, IOCTL_LLKD_IOCRESET = 0x%x IOCTL_LLKD_IOCQPOWER= 0x%x "
		"IOCTL_LLKD_IOCSPOWER=0x%x)\n",
		IOCTL_LLKD_IOCRESET, (unsigned int)IOCTL_LLKD_IOCQPOWER,
		(unsigned int)IOCTL_LLKD_IOCSPOWER);

	// 1. Reset the device
	if (ioctl(fd, IOCTL_LLKD_IOCRESET, 0) == -1) {
		perror("ioctl IOCTL_LLKD_IOCRESET failed");
		close(fd);
		exit(EXIT_FAILURE);
	}
	printf("%s device reset.\n", argv[0]);

	// 2. Query its power value
	if (ioctl(fd, IOCTL_LLKD_IOCQPOWER, &power)) {
		perror("ioctl IOCTL_LLKD_IOCQPOWER failed");
		close(fd);
		exit(EXIT_FAILURE);
	}
	printf("%s: power=%d\n", argv[0], power);

	// 3. Toggle its power status
	if (power == 0) {
		printf("%s: Device OFF, powering it on now...\n", argv[0]);
		if (ioctl(fd, IOCTL_LLKD_IOCSPOWER, 1) == -1) {
			perror("ioctl IOCTL_LLKD_IOCSPOWER failed");
			close(fd);
			exit(EXIT_FAILURE);
		}
		printf("%s: power is ON now.\n", argv[0]);
	} else if (power == 1) {
		printf("%s: Device ON, powering it OFF in 3s ...\n", argv[0]);
		sleep(3); /* yes, careful here of sleep & signals! */
		if (ioctl(fd, IOCTL_LLKD_IOCSPOWER, 0) == -1) {
			perror("ioctl IOCTL_LLKD_IOCSPOWER failed");
			close(fd);
			exit(EXIT_FAILURE);
		}
		printf("%s: power OFF ok, exiting..\n", argv[0]);
	}

	close(fd);
	exit(EXIT_SUCCESS);
}
