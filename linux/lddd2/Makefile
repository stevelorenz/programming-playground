SUBDIRS = Chapter03 Chapter04 Chapter07

all: subdirs

subdirs:
	for n in ${SUBDIRS}; do $(MAKE) -C $$n || exit 1; done

clean:
	for n in ${SUBDIRS}; do $(MAKE) -C $$n clean || exit 1; done
