CFLAGS = `pkg-config --cflags glib-2.0`
LIBS = `pkg-config --libs glib-2.0`

all: data/emigrantes.csv data/operacoes.csv awk/processEmi.awk awk/processOps.awk emi.fl emi.y
	awk -f awk/processEmi.awk data/emigrantes.csv > data/emigrantes.txt
	awk -f awk/processOps.awk data/operacoes.csv > data/operacoes.txt
	cat data/emigrantes.txt data/operacoes.txt > data/dados.txt
	@echo "\n"
	flex emi.fl
	yacc emi.y
	gcc -o emi $(CFLAGS) structs.c y.tab.c -ll $(LIBS)

run:
	./emi < data/dados.txt
	
graph:
	dot -Tsvg dot_files/countries.dot -o svg_files/countries.svg

draw:
	chromium-browser svg_files/countries.svg	

clean:
	rm -f data/emigrantes.txt data/operacoes.txt data/dados.txt emi y.tab.c dot_files/* svg_files/*
