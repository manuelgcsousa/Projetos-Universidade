CC = gcc
CFLAGS0 = -O2 -Wall -std=c11 -g 
CFLAGS1 = `pkg-config --cflags libxml-2.0`
CFLAGS2 = `pkg-config --cflags --libs glib-2.0`
LIBS1 = `pkg-config --libs libxml-2.0`
LIBS2 = `pkg-config --cflags --libs glib-2.0`

program:
	$(CC) $(CFLAGS0) $(CFLAGS1) $(CFLAGS2) -c getInfoFstAVL.c $(LIBS1) $(LIBS2)
	$(CC) $(CFLAGS0) $(CFLAGS1) $(CFLAGS2) -c getInfoSndAVL.c $(LIBS1) $(LIBS2)
	$(CC) $(CFLAGS0) $(CFLAGS1) $(CFLAGS2) -c querysAux.c $(LIBS1) $(LIBS2)
	$(CC) $(CFLAGS0) $(CFLAGS1) $(CFLAGS2) -c querys.c $(LIBS1) $(LIBS2)
	$(CC) $(CFLAGS0) $(CFLAGS1) $(CFLAGS2) -c program.c $(LIBS1) $(LIBS2)
	$(CC) -o program getInfoFstAVL.o getInfoSndAVL.o querysAux.o querys.o program.o $(LIBS1) $(LIBS2)

clean:
	rm *.o
	rm program
