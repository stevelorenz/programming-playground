#define pr_fmt(fmt) "%s:%s(): " fmt, KBUILD_MODNAME, __func__

#include <linux/fs.h>  // the fops structure
#include <linux/init.h>
#include <linux/miscdevice.h>
#include <linux/mm.h>  // kvmalloc()
#include <linux/module.h>
#include <linux/slab.h>	 // k[m|z]alloc(), k[z]free(), ...

// copy_[to|from]_user()
#include <linux/version.h>
#if LINUX_VERSION_CODE > KERNEL_VERSION(4, 11, 0)
#include <linux/uaccess.h>
#else
#include <asm/uaccess.h>
#endif

#include <linux/mutex.h>  // mutex lock, unlock, etc

#include "../../convenient.h"

#define OURMODNAME "miscdrv_rdwr_mutexlock"

MODULE_AUTHOR("Zuo Xiang");
MODULE_DESCRIPTION("");
MODULE_LICENSE("GPL");
MODULE_VERSION("0.1");

static int ga, gb = 1;
DEFINE_MUTEX(
	lock1);	 // this mutex lock is meant to protect the integers ga and gb

// State info for device driver
struct drv_ctx {
	struct device *dev;
	int tx, rx, err, myword;
	u32 config1, config2;
	u64 config3;
#define MAXBYTES 128
	char oursecret[MAXBYTES];
	struct mutex lock;	// this mutex protects this data structure
};
static struct drv_ctx *ctx;

static int open_miscdrv_rdwr(struct inode *inode, struct file *filp) {
	struct device *dev = ctx->dev;
	PRINT_CTX();

	mutex_lock(&lock1);
	ga++;
	gb--;
	mutex_unlock(&lock1);

	dev_info(dev,
			 " filename: \"%s\"\n"
			 " wrt open file: f_flags = 0x%x\n"
			 " ga = %d, gb = %d\n",
			 filp->f_path.dentry->d_iname, filp->f_flags, ga, gb);

	return 0;
}

static ssize_t read_miscdrv_rdwr(struct file *filp, char __user *ubuf,
								 size_t count, loff_t *off) {
	int ret = count, secret_len;
	struct device *dev = ctx->dev;

	mutex_lock(&ctx->lock);
	secret_len = strlen(ctx->oursecret);
	mutex_unlock(&ctx->lock);

	PRINT_CTX();
	dev_info(dev, "%s wants to read (upto) %zu bytes\n", current->comm, count);

	ret = -EINVAL;
	if (count < MAXBYTES) {
		dev_warn(dev,
				 "request # of bytes (%zu) is < required size"
				 " (%d), aborting read\n",
				 count, MAXBYTES);
		goto out_notok;
	}
	if (secret_len <= 0) {
		dev_warn(dev,
				 "whoops, something's wrong, the 'secret' isn't"
				 " available..; aborting read\n");
		goto out_notok;
	}

	/* In a 'real' driver, we would now actually read the content of the
	 * device hardware (or whatever) into the user supplied buffer 'ubuf'
	 * for 'count' bytes, and then copy it to the userspace process (via
	 * the copy_to_user() routine).
	 * (FYI, the copy_to_user() routine is the *right* way to copy data from
	 * userspace to kernel-space; the parameters are:
	 *  'to-buffer' (dest), 'from-buffer' (src), count (# of bytes)
	 * Returns 0 on success, i.e., non-zero return implies an I/O fault).
	 * Here, we simply copy the content of our context structure's 'secret'
	 * member to userspace.
	 */
	ret = -EFAULT;
	mutex_lock(&ctx->lock);
	if (copy_to_user(ubuf, ctx->oursecret, secret_len)) {
		dev_warn(dev, "copy_to_user() failed\n");
		goto out_ctu;
	}
	ret = secret_len;

	// Update stats
	ctx->tx += secret_len;	// our 'transmit' is wrt this driver
	dev_info(dev, " %d bytes read, returning... (stats: tx=%d, rx=%d)\n",
			 secret_len, ctx->tx, ctx->rx);
out_ctu:
	mutex_unlock(&ctx->lock);
out_notok:
	return ret;
}

static ssize_t write_miscdrv_rdwr(struct file *filp, const char __user *ubuf,
								  size_t count, loff_t *off) {
	int ret;
	void *kbuf = NULL;
	struct device *dev = ctx->dev;

	PRINT_CTX();
	dev_info(dev, "%s wants to write %zu bytes\n", current->comm, count);

	ret = -ENOMEM;
	kbuf = kvmalloc(count, GFP_KERNEL);
	if (unlikely(!kbuf)) {
		dev_warn(dev, "kvmalloc() failed!\n");
		goto out_nomem;
	}
	memset(kbuf, 0, count);

	/* Copy in the user supplied buffer 'ubuf' - the data content to write -
	 * via the copy_from_user() macro.
	 * (FYI, the copy_from_user() macro is the *right* way to copy data from
	 * userspace to kernel-space; the parameters are:
	 *  'to-buffer', 'from-buffer', count
	 *  Returns 0 on success, i.e., non-zero return implies an I/O fault).
	 */
	ret = -EFAULT;
	if (copy_from_user(kbuf, ubuf, count)) {
		dev_warn(dev, "copy_from_user() failed\n");
		goto out_cfu;
	}

	/* In a 'real' driver, we would now actually write (for 'count' bytes)
	 * the content of the 'ubuf' buffer to the device hardware (or whatever),
	 * and then return.
	 * Here, we first acquire the mutex lock, then write the just-accepted
	 * new 'secret' into our driver 'context' structure, and unlock.
	 */
	mutex_lock(&ctx->lock);
	strscpy(ctx->oursecret, kbuf, (count > MAXBYTES ? MAXBYTES : count));
#if 0
	print_hex_dump_bytes("ctx ", DUMP_PREFIX_OFFSET, ctx, sizeof(struct drv_ctx));
#endif
	// Update stats
	ctx->rx += count;  // our 'receive' is wrt userspace

	ret = count;
	dev_info(dev, " %zu bytes written, returning... (stats: tx=%d, rx=%d)\n",
			 count, ctx->tx, ctx->rx);
	mutex_unlock(&ctx->lock);

out_cfu:
	kvfree(kbuf);
out_nomem:
	return ret;
}

static int close_miscdrv_rdwr(struct inode *inode, struct file *filp) {
	struct device *dev = ctx->dev;

	PRINT_CTX();  // displays process (or intr) context info

	mutex_lock(&lock1);
	ga--;
	gb++;
	mutex_unlock(&lock1);

	dev_info(dev, "filename: \"%s\"\n ga = %d, gb = %d\n",
			 filp->f_path.dentry->d_iname, ga, gb);

	return 0;
}

static const struct file_operations llkd_miscdev_fops = {
	.open = open_miscdrv_rdwr,
	.read = read_miscdrv_rdwr,
	.write = write_miscdrv_rdwr,
	.llseek = no_llseek,  // dummy, we don't support lseek(2)
	.release = close_miscdrv_rdwr,
};

static struct miscdevice llkd_miscdev = {
	.minor = MISC_DYNAMIC_MINOR,
	.name = "llkd_miscdev_rdwr_mutexlock",
	.mode = 0666,
	.fops = &llkd_miscdev_fops,
};

static int __init miscdrv_init_mutexlock(void) {
	pr_info("%s: inserted!\n", OURMODNAME);
	int ret = 0;

	ret = misc_register(&llkd_miscdev);
	if (ret < 0) {
		pr_notice("misc device registration failed!\n");
		return ret;
	}
	// No explicit free is needed.
	ctx = devm_kzalloc(llkd_miscdev.this_device, sizeof(struct drv_ctx),
					   GFP_KERNEL);
	if (unlikely(!ctx)) {
		return -ENOMEM;
	};

	mutex_init(&ctx->lock);
	ctx->dev = llkd_miscdev.this_device;

	/* Initialize the "secret" value :-) */
	strscpy(ctx->oursecret, "initmsg", 8);
	/* Why don't we protect the above strscpy() with the mutex lock?
	 * It's working on shared writable data, yes?
	 * Yes, BUT this is the init code; it's guaranteed to run in exactly
	 * one context (typically the insmod(8) process), thus there is
	 * no concurrency possible here. The same goes for the cleanup
	 * code path.
	 */

	dev_dbg(ctx->dev, "A sample print via the dev_dbg(): driver initialized\n");

	return 0;
}

static void __exit miscdrv_exit_mutexlock(void) {
	mutex_destroy(&lock1);
	mutex_destroy(&ctx->lock);
	misc_deregister(&llkd_miscdev);
	pr_info("LLKD misc driver %s deregistered, bye\n", llkd_miscdev.name);
}

module_init(miscdrv_init_mutexlock);
module_exit(miscdrv_exit_mutexlock);
