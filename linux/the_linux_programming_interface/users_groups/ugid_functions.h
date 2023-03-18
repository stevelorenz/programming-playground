#ifndef UGID_FUNCTIONS_H
#define UGID_FUNCTIONS_H

#include "tlpi_hdr.h"

char* userNameFromId(uid_t uid);

uid_t userIDFromName(const char* name);

char *groupNameFromId(gid_t gid);

gid_t groupIdFromName(const char *name);

#endif
