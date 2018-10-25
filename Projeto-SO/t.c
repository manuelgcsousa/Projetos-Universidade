#include <stdio.h>
#include <unistd.h>
#include <fcntl.h> 

int main(int argc, char** argv){
	char* teste1 = "1020:25:10\n";
	char* teste2 = "100:26:101\n";
	char* teste3 = "11:25:1\n";
	char* teste4 = "1:1000:1000\n";
	write(1,teste1,11);
	write(1,teste2,11);
	write(1,teste3,8);
	write(1,teste4,12);
	return 0;
}

