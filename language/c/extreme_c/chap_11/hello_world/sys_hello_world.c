#include <linux/kernel.h>
#include <linux/slab.h>	 // For kmalloc and kfree
#include <linux/string.h>
#include <linux/syscalls.h>
#include <linux/uaccess.h>

SYSCALL_DEFINE4(hello_world, const char __user *, str,	// Input name
				const unsigned int, str_len,			// Length of input name
				char __user *, buf,						// Output buffer
				unsigned int, buf_len) {
	// Keep the content of the input buffer.
	char name[64];
	// Keep the content of the output buffer.
	char message[96];
	printk("System call fired!\n");
	if (str_len > 64) {
		printk("Too long input string!\n");
		return -1;
	}
	if (copy_from_user(name, str, str_len)) {
		printk("Faild to copy data from the user space.\n");
		return -2;
	}

	strcpy(message, "Hello ");
	strcat(message, name);
	strcat(message, "!");

	if (strlen(message) > buf_len - 1) {
		printk("The output buffer is too small.\n");
		return -3;
	}

	if (copy_to_user(buf, message, strlen(message) + 1)) {
		printk("Faild to copy data to the user space.\n");
		return -4;
	}

	printk("Message: %s\n", message);

	return 0;
}
