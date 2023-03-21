obj-m := std-timer.o waitqueue.o

KERNELDIR ?= /lib/modules/$(shell uname -r)/build

all defaults: modules
install: modules_install

modules modules_install help clean:
	$(MAKE) -C $(KERNELDIR) M=$(shell pwd) $@
