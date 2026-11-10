obj-m += hello-1.o
obj-m += hello-2.o
obj-m += hello-3.o
obj-m += hello-4.o
obj-m += hello-5.o
obj-m += hello-6.o
obj-m += chardev.o
obj-m += procfs1.o
obj-m += hello-sysfs.o
obj-m += dma.o
obj-m += startstop.o
startstop-objs:= start.o stop.o
obj-m += example_tasklet.o
obj-m += sched.o
obj-m += intrpt.o
obj-m += bottomhalf.o
obj-m += bh_threaded.o
obj-m += blkram.o
obj-m += vnetloop.o
obj-m += devicetree.o


KDIR ?= /lib/modules/$(shell uname -r)/build
PWD := $(CURDIR)
CLANG_FORMAT ?= $(shell command -v clang-format-20 2>/dev/null || command -v clang-format 2>/dev/null)

ifeq ($(CONFIG_STATUS_CHECK_GCC),y)
CC=$(STATUS_CHECK_GCC)
ccflags-y += -fanalyzer
endif

all:
	$(MAKE) -C $(KDIR) CC=$(CC) M=$(PWD) modules

clean:
	$(MAKE) -C $(KDIR) CC=$(CC) M=$(PWD) clean
	$(RM) other/cat_nonblock *.plist

indent:
	@if [ -z "$(CLANG_FORMAT)" ]; then \
		echo "No clang-format found. Install clang-format-20 or clang-format." >&2; \
		exit 1; \
	fi
	"$(CLANG_FORMAT)" -i *.[ch]
	"$(CLANG_FORMAT)" -i other/*.[ch]
