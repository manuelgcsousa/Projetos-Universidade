pA: PerguntaA.lex 
	flex PerguntaA.lex
	gcc -o pA lex.yy.c 
	./pA < ../exemplo-utf8.bib

pB: PerguntaB.lex
	flex PerguntaB.lex
	gcc -o pB lex.yy.c
	./pB < ../exemplo-utf8.bib

clean: 
	rm -f HTML/*.html pA pB lex.yy.c
