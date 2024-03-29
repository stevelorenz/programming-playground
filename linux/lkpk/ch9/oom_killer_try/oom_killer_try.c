#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BLK (getpagesize() * 2)

static int force_page_fault = 0;

int main(int argc, char *argv[]) {
	char *p;
	int i = 0, j = 1, stepval = 5000, verbose = 0;

	if (argc < 3) {
		fprintf(stderr,
				"Usage: %s alloc-loop-count force-page-fault[0|1] "
				"[verbose_flag[0|1]]\n",
				argv[0]);
		exit(EXIT_FAILURE);
	}

	if (atoi(argv[2]) == 1) force_page_fault = 1;

	if (argc >= 4) {
		if (atoi(argv[3]) == 1) verbose = 1;
	}

	printf("%s: PID %d (verbose mode: %s)\n", argv[0], getpid(),
		   (verbose == 1 ? "on" : "off"));
	do {
		p = (char *)malloc(BLK);
		if (!p) {
			fprintf(stderr, "%s: loop #%d: malloc failure.\n", argv[0], i);
			break;
		}

		/* *IMPORTANT* Demand Paging :
		 * Force the MMU to raise the page fault exception by writing into the
		 * page; writing a single byte, any byte, will do the trick! This is as
		 * the virtual address referenced will have no PTE entry, causing the
		 * MMU to raise the page fault!
		 * The fault handler, being intelligent, figures out it's a "good fault"
		 * (a minor fault) and allocates a page frame via the page allocator!
		 * Only now do we have physical memory!
		 */
		if (force_page_fault) {
			p[1103] &= 0x0b;  // write something into a byte of the 1st page
			p[5227] |= 0xaa;  // write something into a byte of the 2nd page
		}
		if (!(i % stepval)) {  // every 'stepval' iterations..
			if (!verbose) {
				if (!(j % 5))
					printf(". ");
				else
					printf(".");
				fflush(stdout);
				j++;
			} else {
				printf("%06d\taddr p = %p   break = %p\n", i, (void *)p,
					   (void *)sbrk(0));
			}
		}
		i++;
	} while (p && (i < atoi(argv[1])));

	exit(EXIT_SUCCESS);

	return 0;
}
