/* Uninitialized and zero-value global variables are stored in BSS segment
 * These variables are part of the static memory layout, they become allocated
 * when process is loading and never get deallocated until the process is alive.
 *
 * The reason of using a separate BSS segment is to reduce the program size for
 * loading. Since the BSS segment does not need to store the value of each
 * variable. It can just store the size. When a program is loaded into the
 * memory. It could use following pseudo-code for loading process:
 *
 * for(i=0; i<all_explicitly_initialized_objects; i++)
 * {
 *   .data[i] = init_value[i];
 * }
 *
 * memset(.bss,
 *        0,
 *        all_implicitly_initialized_objects);
 *
 * - Only init_value need to be stored on the ROM.
 * - memset is very efficient to set initial values.
 * */
int global_int_var1;
int global_int_var2;
int global_int_var3 = 0;

/* Initialized global variables are stored in Data segment.
 * Data segment should increase 4 + 1 = 5 bytes.
 * */
int global_int_var4 = 3;
char global_int_var5 = 'A';
char global_int_var6 = 'B';

int global_int_var7 = 33;  // 0x00000021

void dummy() {
	/* Static local variables mean that the dummy function has access to
	 * these vairables in Data or BSS segment. So they have a static
	 * lifetime. And no matter how many times this function is called. The i
	 * and j will always keep their most recent values.
	 * */
	static int i;
	static int j = 1;
}

int main(int argc, char *argv[]) { return 0; }
