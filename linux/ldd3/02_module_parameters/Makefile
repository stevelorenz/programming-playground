LDDINC=$(PWD)/../include
EXTRA_CFLAGS += -I$(LDDINC)

# If KERNELRELEASE is defined, we've been invoked from the kernel build system and can use its language.
# Otherwise we were called directly from the command line; invoke the kernel build system.

ifneq ($(KERNELRELEASE),)
	ccflags-y := -std=gnu99
	module_parameters-objs := main.o
	# Rename the module name to module_parameters.ko
	obj-m := module_parameters.o
else
	KERNELDIR ?= /lib/modules/$(shell uname -r)/build
	PWD  := $(shell pwd)
default:
	# Here actually the makefile invokes the make command again and could trigger the first obj-m clause with
	# KERNELDIR defined!
	$(MAKE) -C $(KERNELDIR) M=$(PWD) modules
endif

clean:
	rm -rf *.o *~ core .depend .*.cmd *.ko *.mod.c .tmp_versions *.mod modules.order *.symvers
