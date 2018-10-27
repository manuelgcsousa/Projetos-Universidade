CFLAGS = `pkg-config --cflags glib-2.0`
LIBS = `pkg-config --libs glib-2.0`

all:
	flex PerguntaC.lex
	gcc -o perguntaC $(CFLAGS) lex.yy.c -ll $(LIBS)
	./perguntaC < ../exemplo-utf8.bib	

clean:
	rm -f perguntaC indice.txt lex.yy.c
