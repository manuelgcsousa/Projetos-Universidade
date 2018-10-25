#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

int da2e4Col(int* inicio, int* fim, char buffer[256]){
	int a = 0;
	int Sc = 0;
	int Qc = 0;
	int pontos = 0;
	char Scol[10];
	char Qcol[10];

	while(buffer[a]!='\0'){
		if (buffer[a]==':') pontos++;
		if(pontos == 1 && buffer[a]!=':'){
			Scol[Sc]=buffer[a];
			Sc++;
		}
		if(pontos == 3 && buffer[a]!=':'){
			Qcol[Qc]=buffer[a];
			Qc++;
		}
		a++;
	}
	Scol[Sc]='\0';
	Qcol[Qc]='\0';

	*inicio = atoi(Scol);
	*fim = atoi(Qcol);
}


void const2(char* value){
	int a;
	int i = 0;
	int inicio;
	int fim;
	int r;
	char buffer[256];
	int anterior;
	char ant[256];

	while((r = read(0,buffer,256)) >= 0){
		buffer[r-1] = ':';
		
		for(i=0;value[i]!='\0';i++){
			buffer[r] = value[i];
			r++;
		}
		
		buffer[r]='\n';
		write(1,buffer,r+1);
	}
}

int main(int argv, char** argc){
	const2(argc[1]);
	return 0;
}