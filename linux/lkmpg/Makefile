obj-m += hello-1.o

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
