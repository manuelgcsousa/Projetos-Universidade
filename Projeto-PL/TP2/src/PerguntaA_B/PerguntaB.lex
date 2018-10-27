%option noyywrap

%{
int flag = 0;
int indice = 0;
char* cat[200];
char* chave[200];
char* autor[200];
char* titulo[200];
int x = 0;
void printInfo();
int contaN(char* palavra);
void separaString(char* str, char* vSep);
void printHTML();
%}


%%

\@[^ ]*/,                           { separaString(yytext, "{"); }

[Aa][Uu][Tt][Hh][Oo][Rr]\ *=.*/,    { 
                                      int sum = contaN(yytext);
                                      int tam = strlen(yytext);
                                      
                                      if (flag == 0) {
                                        strncpy(autor[indice], (yytext + sum + 1), (tam - sum - 2));
                                        flag = 1;
                                      }

                                      if (flag == 2) {
                                        strncpy(autor[indice], (yytext + sum + 1), (tam - sum - 2));
                                        indice++;
                                        flag = 0;
                                      }
                                    } 

\ [Tt][Ii][Tt][Ll][Ee]\ *=.*        {
                                      int sum = contaN(yytext);
                                      int tam = strlen(yytext);                        
                                      
                                      if (flag == 0) {
                                        strncpy(titulo[indice], (yytext + sum + 1), (tam - sum - 2));
                                        flag = 2;
                                      }

                                      if (flag == 1) { 
                                        strncpy(titulo[indice], (yytext + sum + 1), (tam - sum - 2));
                                        indice++;
                                        flag = 0;
                                      }
                                    } 

.|\n                                { }

<<EOF>>                             { printHTML(); return 0; }

%%


void printHTML() 
{    
    int flag = 1;
    int i = 0;
    
    FILE* f;

    for (i = 0; i < indice; i++) {
        char* aux = (char*)malloc(sizeof(char) * 50);
        strcpy(aux, "./HTML/");
        strcat(aux, cat[i] + 1);
        strcat(aux, ".html");
                   
        f = fopen(aux, "a");
        
        fprintf(f, "<tr><td> %s </td> <td> %s </td> <td> %s </td> <td> %s </td> </tr>\n", cat[i] , chave[i], autor[i], titulo[i]);

        /*
        fprintf(f, "<p><b> Categoria:</b> %s </p>", cat[i]);
        fprintf(f, "<p><b> Código:</b> %s </p>", chave[i]);
        fprintf(f, "<p><b> Título:</b> %s </p>", titulo[i]);
        fprintf(f, "<p><b> Autores:</b> %s </p>", autor[i]);
        fprintf(f, "<hr>");
        */

        fclose(f);
    }
    fprintf(f,"</table></body></html>");
}


void printInfo() 
{
    int i = 0;

    while (i < indice)  {
        printf("Cat: %s \n || Codigo: %s \n || Autor: %s \n || Titulo: %s \n", cat[i], chave[i], autor[i], titulo[i]);
        i++;
    }
}

void separaString(char* str, char* vSep) 
{
    char* token;
    token = strstr(str, vSep);
    
    int x = token - str;
    
    str[x] = '\0';
    
    strcpy(cat[indice], str);
    strcpy(chave[indice], token + 1);
}

int contaN(char* palavra) 
{
    int i = 0;

    while (palavra[i] != '\0') {
        if (palavra[i] != '{' && palavra[i] != '"') {
            i++;
        } else {
            break;
        }
    }

    return i;
}


int main(int argc, char** argv) 
{
    int i = 0;

    for (i = 0; i < 200; i++) {
        cat[i] = (char*) malloc(sizeof(char) * 15);
        chave[i] = (char*) malloc(sizeof(char) * 10);
        autor[i] = (char*) malloc(sizeof(char) * 200);
        titulo[i] = (char*) malloc(sizeof(char) * 150);
    }

    yylex();
}