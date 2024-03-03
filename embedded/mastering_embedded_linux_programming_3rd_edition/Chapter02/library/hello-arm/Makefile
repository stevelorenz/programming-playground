CFLAGS := -c -Wall -I../inc
PROGS := hello-arm-static.out hello-arm-shared.out

all: $(PROGS)

hello-arm.o: hello-arm.c
	$(CC) $(CFLAGS) -o $@ $^

hello-arm-static.out: hello-arm.o
	$(CC) -o $@ $^ -L../static -ltest

hello-arm-shared.out: hello-arm.o
	$(CC) -o $@ $^ -L../shared -ltest

clean:
	rm -f *.o
	rm -f $(PROGS)

install:
	cp $(PROGS) $(TARGET_DIR)/usr/local/bin
