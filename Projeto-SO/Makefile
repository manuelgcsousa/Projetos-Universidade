CC = gcc
CFLAGS0 = -O2 -Wall -g 

program:
	$(CC) $(CFLAGS0) -o controlador controlador.c 
	$(CC) $(CFLAGS0) -o node node.c
	$(CC) -o filter filter.c
	$(CC) -o window window.c
	$(CC) -o const const.c
	$(CC) -o spawn spawn.c
	$(CC) -o t t.c
clean:
	rm controlador
	rm node
	rm filter
	rm window
	rm t
	rm spawn
	rm const
	rm 1 2 3 4 5 6 7 8 9 10 11 12

