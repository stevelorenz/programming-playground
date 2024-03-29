#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "../sed_common.h"

static void decrypt_msg(int fd, char *msg, char *prg) {
	struct sed_ds *kd;

	// Send a 'packet' to the underlying driver
	kd = calloc(sizeof(struct sed_ds), 1);
	if (!kd) {
		fprintf(stderr, "%s: calloc() kd failed!\n", prg);
		exit(EXIT_FAILURE);
	}

	kd->data_xform = XF_DECRYPT;
	kd->len = strlen(msg);
	memcpy(kd->data, msg, strlen(msg));

	/* The 'returned' data payload will be in the last parameter kd;
	 * iow, it's an "in-out" / "value-result" style parameter
	 */
	printf("msg before decrypt: %s\n", msg);
	if (ioctl(fd, IOCTL_LLKD_SED_IOC_DECRYPT_MSG, kd) == -1) {
		perror("ioctl IOCTL_LLKD_SED_IOC_DECRYPT_MSG failed");
		free(kd);
		close(fd);
		exit(EXIT_FAILURE);
	}
	if (kd->timed_out == 1) {
		fprintf(stderr, "*** Operation Timed Out ***\n");
		memset(kd, 0, sizeof(struct sed_ds));
		exit(EXIT_FAILURE);
	}

	printf("ioctl IOCTL_LLKD_SED_IOC_DECRYPT_MSG done; len=%d\n", kd->len);

	memcpy(msg, kd->data, kd->len);
	free(kd);
}

static void encrypt_msg(int fd, char *msg, char *prg) {
	struct sed_ds *kd;

	kd = calloc(sizeof(struct sed_ds), 1);
	if (!kd) {
		fprintf(stderr, "%s: calloc() kd failed\n", prg);
		exit(EXIT_FAILURE);
	}

	kd->data_xform = XF_ENCRYPT;
	kd->len = strlen(msg);
	memcpy(kd->data, msg, strlen(msg));

	printf("msg before encrypt: %s\n", msg);

	if (ioctl(fd, IOCTL_LLKD_SED_IOC_ENCRYPT_MSG, kd) == -1) {
		perror("ioctl IOCTL_LLKD_SED_IOC_ENCRYPT_MSG failed");
		free(kd);
		close(fd);
		exit(EXIT_FAILURE);
	}

	if (kd->timed_out == 1) {
		fprintf(stderr, "*** Operation Timed Out ***\n");
		memset(kd, 0, sizeof(struct sed_ds));
		exit(EXIT_FAILURE);
	}
	printf("ioctl IOCTL_LLKD_SED_IOC_ENCRYPT_MSG done; len=%d\n", kd->len);

	memcpy(msg, kd->data, kd->len);
	free(kd);
}

int main(int argc, char *argv[]) {
	int fd;
	char buf[MAX_DATA];

	if (argc < 3) {
		fprintf(stderr, "Usage: %s device_file message\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	if (strlen(argv[2]) <= 0 || strlen(argv[2]) > MAX_DATA) {
		fprintf(stderr, "%s: invalid message\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	memcpy(buf, argv[2], MAX_DATA);

	fd = open(argv[1], O_RDWR, 0);
	if (fd == -1) {
		perror("open");
		exit(EXIT_FAILURE);
	}
	printf("device opened: fd=%d\n", fd);

	encrypt_msg(fd, buf, argv[0]);
	printf("msg after encrypt: %s\n\n", buf);
	sleep(1);
	decrypt_msg(fd, buf, argv[0]);
	printf("msg after decrypt: %s\n", buf);

	close(fd);
	exit(EXIT_SUCCESS);
}
