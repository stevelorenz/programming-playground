#include <dirent.h>
#include <limits.h>
#include <stddef.h>
#include <sys/types.h>

#include "tlpi_hdr.h"

static void listFiles(const char *dirpath) {
	DIR *dirp;
	Boolean isCurrent;
	struct dirent *result, *entryp;

	isCurrent = strcmp(dirpath, ".") == 0;

	dirp = opendir(dirpath);
	if (dirp == NULL) {
		errMsg("opendir failed on %s", dirpath);
		return;
	}

	for (;;) {
		entryp = readdir(dirp);
		if (entryp == NULL) {
			break;
		}

		if (strcmp(entryp->d_name, ".") == 0 ||
			strcmp(entryp->d_name, "..") == 0) {
			continue;
		}

		if (!isCurrent) printf("%s/", dirpath);
		printf("%s\n", entryp->d_name);
	}

	if (closedir(dirp) == -1) {
		errMsg("closedir");
	}
}

int main(int argc, char *argv[]) {
	if (argc == 1) {
		listFiles(".");
	} else {
		for (argv++; *argv; argv++) {
			listFiles(*argv);
		}
	}

	exit(EXIT_SUCCESS);
}
