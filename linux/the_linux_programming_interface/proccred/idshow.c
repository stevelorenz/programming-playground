#define _GNU_SOURCE

#include <limits.h>
#include <sys/fsuid.h>
#include <unistd.h>

#include "tlpi_hdr.h"
#include "ugid_functions.h"

#define SG_SIZE (NGROUPS_MAX + 1)

int main() {
	uid_t ruid, euid, suid, fsuid;
	gid_t rgid, egid, sgid, fsgid;
	gid_t suppGroups[SG_SIZE];
	int numGroups, j;
	char *p;

	if (getresuid(&ruid, &euid, &suid) == -1) {
		errExit("getresuid");
	}
	if (getresgid(&rgid, &egid, &sgid)) {
		errExit("getresgid");
	}

	// Attempts to change the file-system IDs are always ignored
	// for unprivileged processes, but the calls return the current file-system
	// ID.
	fsuid = setfsuid(0);
	fsgid = setfsgid(0);

	printf("UID:");
	p = userNameFromId(ruid);
	printf("real=%s(%ld),", (p == NULL) ? "???" : p, (long)(ruid));
	p = userNameFromId(euid);
	printf("eff=%s(%ld),", (p == NULL) ? "???" : p, (long)(euid));
	p = userNameFromId(suid);
	printf("saved=%s(%ld),", (p == NULL) ? "???" : p, (long)(suid));
	p = userNameFromId(fsuid);
	printf("fs=%s(%ld),", (p == NULL) ? "???" : p, (long)(fsuid));
	printf("\n");

	printf("GID:");
	p = userNameFromId(rgid);
	printf("real=%s(%ld),", (p == NULL) ? "???" : p, (long)(rgid));
	p = userNameFromId(egid);
	printf("eff=%s(%ld),", (p == NULL) ? "???" : p, (long)(egid));
	p = userNameFromId(sgid);
	printf("saved=%s(%ld),", (p == NULL) ? "???" : p, (long)(sgid));
	p = userNameFromId(fsgid);
	printf("fs=%s(%ld),", (p == NULL) ? "???" : p, (long)(fsgid));
	printf("\n");

	numGroups = getgroups(SG_SIZE, suppGroups);
	if (numGroups == -1) {
		errExit("getgroups");
	}

	printf("Supplementary groups (%d): ", numGroups);
	for (j = 0; j < numGroups; j++) {
		p = groupNameFromId(suppGroups[j]);
		printf("%s (%ld) ", (p == NULL) ? "???" : p, (long)suppGroups[j]);
	}
	printf("\n");

	exit(EXIT_SUCCESS);
}
