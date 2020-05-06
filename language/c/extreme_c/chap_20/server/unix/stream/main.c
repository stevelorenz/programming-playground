#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>

#include <sys/socket.h>
#include <sys/un.h>

#include <stream_server_core.h>

int main(int argc, char *argv[])
{
	// The path to create the Unix domain socket.
	char sock_file[] = "/tmp/calc_svc.sock";

	// 1. Create the socket object.
	int server_sd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (server_sd == -1) {
		fprintf(stderr, "Could not create socket: %s\n",
			strerror(errno));
		exit(1);
	}

	// 2. Bind the socket file
	unlink(sock_file);
	struct sockaddr_un addr;
	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strcpy(addr.sun_path, sock_file);

	int result = bind(server_sd, (struct sockaddr *)&addr, sizeof(addr));
	if (result == -1) {
		fprintf(stderr, "Could not bind the address: %s\n",
			strerror(errno));
		exit(1);
	}

	// 3. Prepare backlog
	result = listen(server_sd, 10);
	if (result == -1) {
		close(server_sd);
		fprintf(stderr, "Could not set the backlog: %s\n",
			strerror(errno));
		exit(1);
	}

	//4. Start accepting clients
	accept_forever(server_sd);

	return 0;
}
