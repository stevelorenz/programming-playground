#include <linux/init.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/proc_fs.h>

struct proc_dir_entry *proc_file;

// The read callback function.
ssize_t proc_file_read(struct file *file, char __user *ubuf, size_t count,
					   loff_t *ppos) {
	int copied = 0;

	if (*ppos > 0) {
		return 0;
	}

	copied = sprintf(ubuf, "Hello World From Kernel Module!\n");
	*ppos = copied;
	return copied;
}

static const struct file_operations proc_file_fops = {

	.owner = THIS_MODULE, .read = proc_file_read

};

static int __init hwkm_init(void) {
	proc_file = proc_create("hwkm", 0, NULL, &proc_file_fops);
	if (!proc_file) {
		return -ENOMEM;
	}
	printk("Hello World module is loaded!\n");
	return 0;
}

static void __exit hwkm_exit(void) {
	proc_remove(proc_file);
	printk("Goodbye World\n");
}

// Define module callbacks
module_init(hwkm_init);
module_exit(hwkm_exit);
