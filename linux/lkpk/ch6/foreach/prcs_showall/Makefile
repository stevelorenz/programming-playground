# No cross compile is configured
ARCH := $(shell arch)
KDIR := /lib/modules/$(shell uname -r)/build

# Set FNAME_C to the kernel module name source filename (without .c)
FNAME_C := prcs_showall

PWD            := $(shell pwd)
obj-m          += ${FNAME_C}.o
EXTRA_CFLAGS   += -DDEBUG

all:
	@echo
	@echo '--- Building : KDIR=${KDIR} ARCH=${ARCH} EXTRA_CFLAGS=${EXTRA_CFLAGS} ---'
	@echo
	make -C $(KDIR) M=$(PWD) modules
install:
	@echo
	@echo "--- installing ---"
	@echo " [First, invoke the 'make' ]"
	make
	@echo
	@echo " [Now for the 'sudo make install' ]"
	sudo make -C $(KDIR) M=$(PWD) modules_install
	sudo depmod
clean:
	@echo
	@echo "--- cleaning ---"
	@echo
	make -C $(KDIR) M=$(PWD) clean
	rm -f *~   # from 'indent'

#--------------- More (useful) targets! -------------------------------

# code-style : "wrapper" target over the following kernel code style targets
code-style:
	make checkpatch

# Detailed check on the source code styling / etc
checkpatch:
	make clean
	@echo
	@echo "--- kernel code style check with checkpatch.pl ---"
	@echo
	$(KDIR)/scripts/checkpatch.pl --no-tree -f --max-line-length=95 *.[ch]
	  # add source files as required

#--- Static Analysis
# sa : "wrapper" target over the following kernel static analyzer targets
sa:
	make sa_sparse
	make sa_gcc
	make sa_flawfinder
	make sa_cppcheck

# static analysis with sparse
sa_sparse:
	make clean
	@echo
	@echo "--- static analysis with sparse ---"
	@echo
# if you feel it's too much, use C=1 instead
	make C=2 CHECK="/usr/bin/sparse" -C $(KDIR) M=$(PWD) modules

# static analysis with gcc
sa_gcc:
	make clean
	@echo
	@echo "--- static analysis with gcc ---"
	@echo
	make W=1 -C $(KDIR) M=$(PWD) modules

# static analysis with flawfinder
sa_flawfinder:
	make clean
	@echo
	@echo "--- static analysis with flawfinder ---"
	@echo
	flawfinder *.[ch]

# static analysis with cppcheck
sa_cppcheck:
	make clean
	@echo
	@echo "--- static analysis with cppcheck ---"
	@echo
	cppcheck -v --force --enable=all -i .tmp_versions/ -i *.mod.c -i bkp/ --suppress=missingIncludeSystem .

# Packaging; just tar.xz as of now
PKG_NAME := ${FNAME_C}
tarxz-pkg:
	rm -f ../${PKG_NAME}.tar.xz 2>/dev/null
	make clean
	@echo
	@echo "--- packaging ---"
	@echo
	tar caf ../${PKG_NAME}.tar.xz *
	ls -l ../${PKG_NAME}.tar.xz
	@echo '=== package created: ../$(PKG_NAME).tar.xz ==='
	@echo 'Tip: when extracting, to extract into a dir of the same name as the tar file,'
	@echo ' do: tar -xvf ${PKG_NAME}.tar.xz --one-top-level'

help:
	@echo '=== Makefile Help : additional targets available ==='
	@echo
	@echo 'TIP: type make <tab><tab> to show all valid targets'
	@echo

	@echo '--- 'usual' kernel LKM targets ---'
	@echo 'typing "make" or "all" target : builds the kernel module object (the .ko)'
	@echo 'install     : installs the kernel module(s) to INSTALL_MOD_PATH (default here: /lib/modules/$(shell uname -r)/)'
	@echo 'clean       : cleanup - remove all kernel objects, temp files/dirs, etc'

	@echo
	@echo '--- kernel code style targets ---'
	@echo 'code-style : "wrapper" target over the following kernel code style targets'
	@echo ' checkpatch : run the kernel code style checker tool on source file(s)'

	@echo
	@echo '--- kernel static analyzer targets ---'
	@echo 'sa         : "wrapper" target over the following kernel static analyzer targets'
	@echo ' sa_sparse     : run the static analysis sparse tool on the source file(s)'
	@echo ' sa_gcc        : run gcc with option -W1 ("Generally useful warnings") on the source file(s)'
	@echo ' sa_flawfinder : run the static analysis flawfinder tool on the source file(s)'
	@echo ' sa_cppcheck   : run the static analysis cppcheck tool on the source file(s)'
	@echo 'TIP: use coccinelle as well (requires spatch): https://www.kernel.org/doc/html/v4.15/dev-tools/coccinelle.html'

	@echo
	@echo '--- kernel dynamic analysis targets ---'
	@echo 'da_kasan   : DUMMY target: this is to remind you to run your code with the dynamic analysis KASAN tool enabled; requires configuring the kernel with CONFIG_KASAN On, rebuild and boot it'
	@echo 'da_lockdep : DUMMY target: this is to remind you to run your code with the dynamic analysis LOCKDEP tool (for deep locking issues analysis) enabled; requires configuring the kernel with CONFIG_PROVE_LOCKING On, rebuild and boot it'
	@echo 'TIP: best to build a debug kernel with several kernel debug config options turned On, boot via it and run all your test cases'

	@echo
	@echo '--- misc targets ---'
	@echo 'tarxz-pkg  : tar and compress the LKM source files as a tar.xz into the dir above; allows one to transfer and build the module on another system'
	@echo ' Tip: when extracting, to extract into a dir of the same name as the tar file,'
	@echo '  do: tar -xvf ${PKG_NAME}.tar.xz --one-top-level'
	@echo 'help       : this help target'
