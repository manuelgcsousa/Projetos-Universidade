CFLAGS = `pkg-config --cflags glib-2.0`
LIBS = `pkg-config --libs glib-2.0`

pC:
	flex PerguntaD.lex
	gcc -o pD $(CFLAGS) lex.yy.c -ll $(LIBS)
	./pD "${author}" < ../exemplo-utf8.bib	

graph:
	dot -Tpng -Gcharset=latin1 graph.gv -o graph.png
	eog graph.png

clean:
	rm -f pD graph.gv lex.yy.c graph.png
