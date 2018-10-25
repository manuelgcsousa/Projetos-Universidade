#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */
#include <fcntl.h> /* O_RDONLY, O_WRONLY, O_CREAT, O_* */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void da2e4Col(int* inicio, char buffer[256], int col1){
	int a = 0;
	int Sc = 0;
	int pontos = 0;
	char Scol[10];

	while(buffer[a]!='\n'){
		if(buffer[a]==':') pontos++;
		if(pontos == (col1-1) && buffer[a]!=':'){
			Scol[Sc] = buffer[a];
			Sc++;
		}
		a++;
	}
	Scol[Sc]='\0';
	*inicio = atoi(Scol);
}

int spawn(char* cmd, char** argv){
	int r;
	char c;
	char buffer[256];
	int i = 0;

	while((r = read(0,&c,1)) >= 0){

		buffer[i] = c;
		i++;

		if(c == '\n'){
			int x = fork();
			if(x == 0){
				execvp(cmd,argv);
				perror("Nao executou comando");
			}

			buffer[i - 1] = ':';
			buffer[i] = '0';
			buffer[i + 1] = '\n';

			write(1,buffer,i + 2);

			memset(buffer,0,256);
			i = 0;
		}
	}
}

int main(int argc, char** argv){
	spawn(argv[1], argv + 1);
	return 0;
}
