#define _GNU_SOURCE

#include <limits.h>
#include <sys/inotify.h>

#include "tlpi_hdr.h"

static void displayInotifyEvent(struct inotify_event *e) {
	printf("  wd = %2d;", e->wd);
	if (e->cookie > 0) {
		printf("cookie = %4d;", e->cookie);
	}
	printf("mask=");
	if (e->mask & IN_ACCESS) printf("IN_ACCESS ");
	if (e->mask & IN_ATTRIB) printf("IN_ATTRIB ");
	if (e->mask & IN_CLOSE_NOWRITE) printf("IN_CLOSE_NOWRITE ");
	if (e->mask & IN_CLOSE_WRITE) printf("IN_CLOSE_WRITE ");
	if (e->mask & IN_CREATE) printf("IN_CREATE ");
	if (e->mask & IN_DELETE) printf("IN_DELETE ");
	if (e->mask & IN_DELETE_SELF) printf("IN_DELETE_SELF ");
	if (e->mask & IN_IGNORED) printf("IN_IGNORED ");
	if (e->mask & IN_ISDIR) printf("IN_ISDIR ");
	if (e->mask & IN_MODIFY) printf("IN_MODIFY ");
	if (e->mask & IN_MOVE_SELF) printf("IN_MOVE_SELF ");
	if (e->mask & IN_MOVED_FROM) printf("IN_MOVED_FROM ");
	if (e->mask & IN_MOVED_TO) printf("IN_MOVED_TO ");
	if (e->mask & IN_OPEN) printf("IN_OPEN ");
	if (e->mask & IN_Q_OVERFLOW) printf("IN_Q_OVERFLOW ");
	if (e->mask & IN_UNMOUNT) printf("IN_UNMOUNT ");

	printf("\n");
	if (e->len > 0) {
		printf("  name = %s\n", e->name);
	}
}

#define BUF_LEN (10 * (sizeof(struct inotify_event) + NAME_MAX + 1))

int main(int argc, char *argv[]) {
	int inotifyFd, wd, j;
	char buf[BUF_LEN] __attribute__((aligned(8)));
	char *p;
	ssize_t numRead;
	struct inotify_event *event;

	if (argc < 2 || strcmp(argv[1], "--help") == 0) {
		usageErr("%s pathname...\n", argv[0]);
	}

	inotifyFd = inotify_init();
	if (inotifyFd == -1) {
		errExit("inotify_init");
	}

	for (j = 1; j < argc; ++j) {
		wd = inotify_add_watch(inotifyFd, argv[j], IN_ALL_EVENTS);
		if (wd == -1) {
			errExit("inotify_add_watch");
		}
		printf("Watching %s using wd: %d\n", argv[j], wd);
	}

	for (;;) {
		numRead = read(inotifyFd, buf, BUF_LEN);
		if (numRead == 0) {
			fatal("read() from inotify fd returned 0!");
		}
		if (numRead == -1) {
			errExit("read");
		}
		printf("Read %ld bytes from inotify fd\n", (long)(numRead));

		for (p = buf; p < buf + numRead;) {
			event = (struct inotify_event *)p;
			displayInotifyEvent(event);
			p = p + sizeof(struct inotify_event) + event->len;
		}
	}

	exit(EXIT_SUCCESS);
}
