BIN_CLIENT = client.a
BIN_SERVER = server.a

all:
	gcc -I../ -static -o $(BIN_CLIENT) client.c
	gcc -I../ -static -o $(BIN_SERVER) server.c

.PHONY: clean
clean:
	rm -fr $(BIN_CLIENT) $(BIN_SERVER)
