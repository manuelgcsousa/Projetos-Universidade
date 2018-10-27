%option noyywrap

%{
#include <string.h>

char* cat[20];
int count[20];

void existeCat(char* pal);
void printInfo();
%}


%%

\@.*/\{      { existeCat(yytext); }

.|\n         { }

<<EOF>>      { printInfo(); return 0; }

%%


void existeCat(char* pal) 
{
    int flag = 1;
    int i = 0;
 
    while (cat[i] != NULL && flag) {
        if (strcmp(pal, cat[i]) == 0) {
            count[i]++;
            flag = 0;
        }
        i++;
    }

    if (flag == 1) {   
        cat[i] = (char*)malloc(sizeof(char) * 15);
        count[i]++;
        strcpy(cat[i], pal);
    }
}

void printInfo() 
{
    int i = 0;
    FILE* f = fopen("HTML/tabelaCat.html", "w");

    fprintf(f, "<h1> Tabela de Categorias </h1>");
    fprintf(f, "<html><head><style>\n");
    fprintf(f, "table { border-collapse: collapse; width 100%%; }\n");
    fprintf(f, "th, td { text-align: left; padding: 8px; }\n");
    fprintf(f, "tr:nth-child(even){ background-color: #f2f2f2 }\n");
    fprintf(f, "th { background-color: #3A80F2; color: white; }\n");
    fprintf(f, "</style><meta charset='UTF-8'> </head><body>\n");
    fprintf(f, "<table><tr> <th>Categoria</th> <th>Repetições</th> </tr>");


    while (cat[i] != NULL) {
        char* aux = (char*)malloc(sizeof(char) * 50);
        strcpy(aux, "HTML/");
        strcat(aux, cat[i] + 1);
        strcat(aux, ".html");
            
        FILE* f1 = fopen(aux, "a");
          
        fprintf(f1, "<h1 style='color:blue;'> <i> Lista  </i> </h1> \n");
        fprintf(f1, "<html><head><title>Lista </title><meta charset='ISO-88591'> </head> \n");

        if (i == 0) {
            fprintf(f, "<tr><td> %s </td> <td> %d </td></tr>", cat[i], count[i]);
        } else {
            fprintf(f, "<tr><td><a href = %s> %s </a> </td> <td> %d </td>", aux + 5, cat[i], count[i]);
            fprintf(f1, "<html><head><style>\n");
            fprintf(f1, "table { border-collapse: collapse; width 100%%; }\n");
            fprintf(f1, "th, td { text-align: left; padding: 8px; }\n");
            fprintf(f1, "tr:nth-child(even){ background-color: #f2f2f2 }\n");
            fprintf(f1, "th { background-color: #3A80F2; color: white; }\n");
            fprintf(f1, "</style><meta charset='ISO-88591'> </head><body>\n");
            fprintf(f1, "<table><tr> <th>Categoria</th> <th>Código</th> <th>Autores</th> <th>Título</th> </tr>");
        }

        free(aux);
        i++;
    }

    fprintf(f, "</table></body></html>");
}

int main(int argc, char** argv) 
{
    int i = 0;

    for (i = 0; i < 20; i++) {
        cat[i] = NULL;
        count[i] = 0;
    }

    yylex();
}
