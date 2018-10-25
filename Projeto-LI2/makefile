
CFLAGS= -Wall -Wextra -pedantic -Wno-long-long -ansi -O2
OBJS=$(patsubst %.c, %.o, $(wildcard *.c))
EXEC=cartas

$(EXEC): $(OBJS)
		$(CC) $(CFLAGS) -o $(EXEC) $(OBJS)

limpar:
		rm $(EXEC) *.o

install: cartas
		sudo cp -r cards /var/www/html
		sudo cp -r imagens /var/www/html
		sudo cp cartas /usr/lib/cgi-bin
