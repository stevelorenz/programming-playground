#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include <sys/socket.h>
#include <sys/un.h>

#include <stream_client_core.h>

int main(int argc, char *argv[])
{
	char sock_file[] = "/tmp/calc_svc.sock";

	// 1. Create the socket
	int conn_sd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (conn_sd == -1) {
		fprintf(stderr, "Cound not create the socket: %s\n",
			strerror(errno));
		exit(1);
	}

	// 2. Connect to server
	struct sockaddr_un addr;
	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strcpy(addr.sun_path, sock_file);

	int result = connect(conn_sd, (struct sockaddr *)&addr, sizeof(addr));
	if (result == -1) {
		close(conn_sd);
		fprintf(stderr, "Could not connect: %s\n", strerror(errno));
		exit(1);
	}

	stream_client_loop(conn_sd);

	return 0;
}
