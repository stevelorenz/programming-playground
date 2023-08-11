#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define MAXBYTES 128

static int stay_alive = 0;

static inline void usage(char *prg) {
	fprintf(stderr,
			"Usage: %s opt=read/write device_file [\"secret-msg\"]\n"
			" opt = 'r' => we shall issue the read(2), retrieving the 'secret' "
			"form the driver\n"
			" opt = 'w' => we shall issue the write(2), writing the secret "
			"message <secret-msg>\n"
			"  (max %d bytes)\n",
			prg, MAXBYTES);
}

int main(int argc, char *argv[]) {
	char *buf = NULL;
	char opt = 'r';
	int fd = 0;
	int flags = O_RDONLY;
	size_t num = 0;
	size_t n = 0;

	if (argc < 3) {
		usage(argv[0]);
		exit(EXIT_FAILURE);
	}

	opt = argv[1][0];
	if (opt != 'r' && opt != 'w') {
		usage(argv[0]);
		exit(EXIT_FAILURE);
	}
	if ((opt == 'w' && argc != 4) || (opt == 'r' && argc != 3)) {
		usage(argv[0]);
		exit(EXIT_FAILURE);
	}

	// String length is always a pain for C programming...
	if ('w' == opt && strnlen(argv[3], MAXBYTES) > MAXBYTES) {
		fprintf(stderr,
				"%s: too big a secret (%zu bytes); pl restrict"
				" to %d bytes max\n",
				argv[0], strnlen(argv[3], MAXBYTES), MAXBYTES);
		exit(EXIT_FAILURE);
	}

	if (opt == 'w') {
		flags = O_WRONLY;
	}
	fd = open(argv[2], flags, 0);
	if (fd == -1) {
		fprintf(stderr, "%s: open on %s failed!\n", argv[0], argv[2]);
		perror("open");
		exit(EXIT_FAILURE);
	}
	printf("Device file %s opened (in %s mode): fd=%d\n", argv[2],
		   (flags == O_RDONLY ? "read-only" : "write-only"), fd);

	if (opt == 'w') {
		num = strnlen(argv[3], MAXBYTES) + 1;
		if (num > MAXBYTES) {
			num = MAXBYTES;
		}
	} else {
		num = MAXBYTES;
	}

	buf = malloc(num);
	if (!buf) {
		fprintf(stderr, "%s: out of memory!\n", argv[0]);
		close(fd);
		exit(EXIT_FAILURE);
	}

	if (opt == 'r') {
		n = read(fd, buf, num);
		if (n < 0) {
			perror("read failed");
			fprintf(stderr, "Tip: see the kernel log\n");
			free(buf);
			close(fd);
			exit(EXIT_FAILURE);
		}
		printf("%s: read %zd bytes from %s\n", argv[0], n, argv[2]);
		printf("The 'secret' is:\n \"%.*s\"\n", (int)n, buf);
	} else {
		strncpy(buf, argv[3], num);
		n = write(fd, buf, num);
		if (n < 0) {
			perror("write failed");
			fprintf(stderr, "Tip: see the kernel log\n");
			free(buf);
			close(fd);
			exit(EXIT_FAILURE);
		}
		printf("%s: wrote %zd bytes to %s\n", argv[0], n, argv[2]);
	}

	if (stay_alive == 1) {
		printf("%s:%d: stayin alive via pause...\n", argv[0], getpid());
		pause();
	}

	free(buf);
	close(fd);
	return 0;
}
