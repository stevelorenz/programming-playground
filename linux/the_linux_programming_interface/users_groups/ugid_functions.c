#include "ugid_functions.h"

#include <ctype.h>
#include <grp.h>
#include <pwd.h>

char *userNameFromId(uid_t uid) {
	struct passwd *pwd;

	pwd = getpwuid(uid);
	if (pwd == NULL) {
		return NULL;
	} else {
		return pwd->pw_name;
	}
}

uid_t userIDFromName(const char *name) {
	struct passwd *pwd;
	uid_t u;
	char *endptr;

	if (name == NULL || *name == '\0') {
		return -1;
	}

	u = strtol(name, &endptr, 10);
	if (*endptr == '\0') {
		return u;
	}

	pwd = getpwnam(name);
	if (pwd == NULL) {
		return -1;
	}
	return pwd->pw_uid;
}

char *groupNameFromId(gid_t gid) {
	struct group *grp;

	grp = getgrgid(gid);
	return (grp == NULL) ? NULL : grp->gr_name;
}

gid_t groupIdFromName(const char *name) {
	struct group *grp;
	gid_t g;
	char *endptr;

	if (name == NULL || *name == '\0') /* On NULL or empty string */
		return -1;					   /* return an error */

	g = strtol(name, &endptr, 10); /* As a convenience to caller */
	if (*endptr == '\0')		   /* allow a numeric string */
		return g;

	grp = getgrnam(name);
	if (grp == NULL) return -1;

	return grp->gr_gid;
}
