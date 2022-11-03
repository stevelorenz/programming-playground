#include <assert.h>
#include <fcntl.h>
#include <mqueue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
	mqd_t mq;
	struct mq_attr attr;

	attr.mq_flags = 0;
	attr.mq_maxmsg = 10;
	attr.mq_msgsize = 32;
	attr.mq_curmsgs = 0;

	pid_t child_pid;

	child_pid = fork();
	assert(child_pid != -1);

	if (child_pid == 0) {
		// Child waits for parent to create the queue and send the
		// message.
		sleep(1);
		mq = mq_open("/mq0", O_WRONLY);
		assert(mq != (mqd_t)-1);
		char msg[] = "Hello Daddy!";
		fprintf(stdout, "Child: Send data to the parent.\n");
		sleep(2);
		mq_send(mq, msg, strlen(msg) + 1, 0);
		mq_close(mq);
	} else {
		mq = mq_open("/mq0", O_RDONLY | O_CREAT, S_IRUSR | S_IWUSR, &attr);
		fprintf(stdout, "Parent: Read from child...\n");
		char buf[32];
		mq_receive(mq, buf, 32, NULL);
		fprintf(stdout, "Parent: Received from the child: %s\n", buf);
		mq_close(mq);
		mq_unlink("/mq0");
	}

	return 0;
}
