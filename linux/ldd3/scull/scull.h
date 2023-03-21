#ifndef _SCULL_H
#define _SCULL_H

#include <linux/cdev.h>
#include <linux/ioctl.h>

#ifndef SCULL_MAJOR
#define SCULL_MAJOR 0
#endif

#ifndef SCULL_NR_DEVS
#define SCULL_NR_DEVS 4
#endif

struct scull_qset {
    void** data;
    struct scull_qset* next;
};

struct scull_dev {
    int quantum;
    int qset;
    unsigned long size;
    unsigned int access_key;
    struct mutex lock;
    struct cdev cdev;
};

#endif
