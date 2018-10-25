#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "big2.h"

/**
Esta função tem como objetivo imprimir as cartas no tabuleiro.
Se a carta a apresentar se encontre na mão do jogador (mao toma o valor de 0 e o actual_jogador no estado for igual a 0),
e caso a carta seja selecionada é então adicionada ao highlight. Se já se encontrar no highlight, então é removida de lá.
Caso contrário, se a carta se encontrar na mão de um bot, é imprimido o verso de uma carta, caso contrário a carta já foi jogada por um bot logo é mostrada a carta correspondente.
@param path O path.
@param x Coordenada HTML.
@param y Coordenada HTML.
@param e O estado atual.
@param mao Número de um jogador.
@param naipe O naipe da carta (inteiro entre 0 e 3).
@param valor O valor da carta (inteiro entre 0 e 12).
*/
void imprime_carta(char *path, int x, int y, ESTADO e, int mao, int naipe, int valor) {
    char *suit = NAIPES;
    char *rank = VALORES;
    char script[10240];
    ESTADO novo = e;
    novo.card = 1;

    if (mao == 0 && e.actual_jogador == 0) {
        if (carta_existe (novo.highlight, naipe, valor)) {
            novo.highlight = rem_carta(novo.highlight, naipe, valor);
        }

        else {
            novo.highlight = add_carta(novo.highlight, naipe, valor);
        }

        sprintf(script, "%s?%s", SCRIPT, estado2str(novo));
        printf("<a xlink:href = \"%s\"><image x = \"%d\" y = \"%d\" height = \"110\" width = \"80\" xlink:href = \"%s/%c%c.svg\" /></a>\n", script, x, y, path, rank[valor], suit[naipe]);
    }

    else {
        if (carta_existe(e.mao[1],naipe,valor) || carta_existe(e.mao[2],naipe,valor) || carta_existe(e.mao[3],naipe,valor))
            printf("<image x = \"%d\" y = \"%d\" height = \"110\" width = \"80\" xlink:href = \"http://localhost/imagens/11C.png\" />\n", x, y);
        else printf("<image x = \"%d\" y = \"%d\" height = \"110\" width = \"80\" xlink:href = \"%s/%c%c.svg\" />\n", x, y, path, rank[valor], suit[naipe]);
    }
}

/*
Função auxiliar que recebe parametros da função imprime. Aqui é vista a forma de como as cartas são apresentadas, neste caso por valor.
*/
void imprime_aux_valor (ESTADO e, int m, int bx1, int by1, int bx2, int by2, int bx3, int by3, int x, int y, char *path) {

    int n,v;

    for (v = 0; v < 13; v++) {
        for (n = 0; n < 4; n++){
            if (m == 1 && carta_existe(e.cartas_bots[1],n,v)) {
                imprime_carta(path, bx1, by1, e, m, n, v);
                by1+=20;
            }
            if (m == 2 && carta_existe(e.cartas_bots[2],n,v)) {
                imprime_carta(path, bx2, by2, e, m, n, v);
                bx2+=20;
            }
            if (m == 3 && carta_existe(e.cartas_bots[3],n,v)) {
                imprime_carta(path,bx3, by3, e, m, n, v);
                by3 += 20;
            }
            if (carta_existe(e.mao[m], n, v)) {
                if (m % 2 == 0) {
                    x += 30;
                }
                else {
                    y += 30;
                }
                if (m == 0 && carta_existe(e.highlight, n, v)) {
                    imprime_carta(path, x, (y - 20), e, m, n, v);
                }
                else {
                    imprime_carta(path, x, y, e, m, n, v);
                }
            }
        }
    }
}

/*
Função auxiliar que recebe parametros da função imprime. Aqui é vista a forma de como as cartas são apresentadas, neste caso por naipe.
*/
void imprime_aux_naipe (ESTADO e, int m, int bx1, int by1, int bx2, int by2, int bx3, int by3, int x, int y, char *path) {

    int n,v;

    for (n = 0; n < 4; n++){
        for (v = 0; v < 13; v++) {
            if (m == 1 && carta_existe(e.cartas_bots[1],n,v)) {
                imprime_carta(path, bx1, by1, e, m, n, v);
                by1+=20;
            }
            if (m == 2 && carta_existe(e.cartas_bots[2],n,v)) {
                imprime_carta(path, bx2, by2, e, m, n, v);
                bx2+=20;
            }
            if (m == 3 && carta_existe(e.cartas_bots[3],n,v)) {
                imprime_carta(path,bx3, by3, e, m, n, v);
                by3 += 20;
            }
            if (carta_existe(e.mao[m], n, v)) {
                if (m % 2 == 0) {
                    x += 30;
                }
                else {
                    y += 30;
                }
                if (m == 0 && carta_existe(e.highlight, n, v)) {
                    imprime_carta(path, x, (y - 20), e, m, n, v);
                }
                else {
                    imprime_carta(path, x, y, e, m, n, v);
                }
            }
        }
    }
}

/**
Apresenta as cartas no tabuleiro nas respetivas coordenadas.
@param path O path.
@param e O estado do jogo actual.
*/
void imprime (char *path, ESTADO e) {

    int m, bx1= 395  , by1 = 250 , bx2=570 , by2 = 150 , bx3= 820 , by3 = 250;
    int X[4] = {405, 10 , 405, 1190};
    int Y[4] = {595, 100, 15, 100};

    for(m = 0; m < 4; m++) {
        int x = X[m], y = Y[m];
        if (e.layout == 1) {
            imprime_aux_valor(e, m, bx1, by1, bx2, by2, bx3, by3, x, y, path);
        }
        else{
            imprime_aux_naipe(e, m, bx1, by1, bx2, by2, bx3, by3, x, y, path);
        }
    }
}

/**
Esta função trata de imprimir o botão "SUBMIT" na página HTML. Aqui, usamos a função posso_jogar.
Se for possível jogar, o botão é clicável, sendo primitida a jogada. Caso contrário, o botão fica mais escuro, não sendo clicável.
@param e O estado actual.
*/
void imprime_botao_jogar(ESTADO e) {

    char script[10240];
    ESTADO novo = e;

    if (posso_jogar(e) && e.highlight != 0) {
        novo.ultima_jogada = e.highlight;
        novo.cartas[0] = e.cartas[0] - (numero_de_cartas(novo.ultima_jogada));
        novo.actual_jogador = incrementa_jogador(e);
        novo.play = 1;
        novo.ultimo_jogador = 0;
        sprintf(script, "%s?%s", SCRIPT, estado2str(novo));
        printf("<a xlink:href = \"%s\"><image x = \"510\" y = \"710\" height = \"80\" width = \"80\" xlink:href = \"http://localhost/imagens/SubmitLI2.png\" /></a>\n", script);
    }

    else {
        printf("<image x = \"510\" y = \"710\" height = \"80\" width = \"80\" xlink:href = \"http://localhost/imagens/SubmitLI2out.png\" />\n");
    }
}

/**
Esta função trata de imprimir o botão "PASS" na página HTML. Quando o utilizador não tem nenhuma combinação válida possível
em relação a última jogada, é possível usar o botão "PASS". O utilizador pode usar sempre o botão "PASS",
excepto quando é o primeiro a jogar (3 de ouros) ou quando todos passam a jogada e este foi o ultimo a jogar uma carta/combinaçao
@param e O estado actual.
*/
void imprime_botao_passar(ESTADO e) {

    char script[10240];
    ESTADO novo = e;

    if (e.actual_jogador == 0 && e.ultima_jogada != -1 && e.ultimo_jogador != 0) {
        novo.ultima_jogada = e.ultima_jogada;
        novo.highlight = 0;
        novo.actual_jogador = incrementa_jogador(e);
        novo.pass = 1;
        sprintf(script, "%s?%s", SCRIPT, estado2str(novo));
        printf("<a xlink:href = \"%s\"><image x = \"610\" y = \"710\" height = \"80\" width = \"80\" xlink:href = \"http://localhost/imagens/PassLI2.png\" /></a>\n", script);
    }

    else {
        printf("<image x = \"610\" y = \"710\" height = \"80\" width = \"80\" xlink:href = \"http://localhost/imagens/PassLI2out.png\" />\n");
    }
}



/**
A função imprime_botao_clear imprime o botão clear para o browser, o qual vai limpar as cartas do highlight, e retorná-las à nossa mão.
@param e O estado actual.
*/
void imprime_botao_clear(ESTADO e) {

    char script[10240];

    e = clear(e);
    sprintf(script, "%s?%s", SCRIPT, estado2str(e));
    printf("<a xlink:href = \"%s\"><image x = \"710\" y = \"710\" height = \"80\" width = \"80\" xlink:href = \"http://localhost/imagens/ClearLI2.png\" /></a>\n", script);
}

/**
A função imprime_botao_sugestao imprime o botão de sugestão para o browser.
@param e O estado actual.
*/
void imprime_botao_sugestao(ESTADO e) {

    char script[10240];
    e = clear(e);

    e = sugestao(e);

    if (e.highlight == 0){
        e = passar(e);

}
    sprintf(script, "%s?%s", SCRIPT, estado2str(e));
    printf("<a xlink:href = \"%s\"><image x = \"40\" y = \"710\" height = \"80\" width = \"80\" xlink:href = \"http://localhost/imagens/sugestaostallman.gif\" /></a>\n", script);
}


/**
A função imprime_botao_start imprime o botão de start do menu inicial para o browser.
@param e O estado actual.
*/
void imprime_botao_start(ESTADO e) {

    char script[10240];

    if (e.start == 1){
        e = bots1(baralhar());

        while(e.actual_jogador != 0){
            e = bots2(e);
        }

        e.start=0;
        sprintf(script, "%s?%s", SCRIPT, estado2str(e));
        printf("<rect x = \"0\" y = \"0\" height = \"800\" width = \"1280\" style = \"fill:#000000\"/>\n");
        printf("<image x = \"0\" y = \"0\" height = \"800\" width = \"1280\" xlink:href = \"http://localhost/imagens/bg.png\" /></a>\n");
        printf("<a xlink:href = \"%s\"><image x = \"1080\" y = \"650\" height = \"110\" width = \"150\" xlink:href = \"http://localhost/imagens/StartLI2.png\" /></a>\n", script);
    }
}

/**
A função imprime_botao_exit imprime o botão de exit para o browser, de forma a ser possível recomeçar o jogo, voltando ao menu inicial com o botão start.
@param e O estado actual.
*/
void imprime_botao_exit(ESTADO e) {

    char script[10240];

    if (e.start == 0){
        e = bots1(baralhar());

        while(e.actual_jogador != 0){
            e = bots2(e);
        }

        e.start=1;
        sprintf(script, "%s?%s", SCRIPT, estado2str(e));
        printf("<a xlink:href = \"%s\"><image x = \"1140\" y = \"603\" height = \"80\" width = \"100\" xlink:href = \"http://localhost/imagens/ExitLI2.png\" /></a>\n", script);
    }
}


/**
A função imprime_botao_reset imprime um botão que quando algum dos jogadores ganha a ronda, ou seja, acabam as cartas da sua mão.
@param e O estado actual.
*/
void imprime_botao_reset(ESTADO e) {

    char script[10240];

    if (e.cartas[0] != 0 && e.cartas[1] != 0 && e.cartas[2] != 0 && e.cartas[3] != 0) {

    }
    else {
        e = bots1(baralhar());

        while(e.actual_jogador != 0) {
            e = bots2(e);
        }
        sprintf(script, "%s?%s", SCRIPT, estado2str(e));
        printf("<a xlink:href = \"%s\"><image x = \"850\" y = \"530\" height = \"220\" width = \"200\" xlink:href = \"http://localhost/imagens/playagain.png\" /></a>\n", script);
    }
}


/*
A função imprime_botao_layout imprime um botão que muda a organização inicial das cartas (de naipe para valor).
@param e O estado actual.
*/
void imprime_botao_layout(ESTADO e) {

    char script[10240];

    if (e.layout == 0) {
        e.layout = 1;
    }
    else {
        e.layout = 0;
    }

    sprintf(script, "%s?%s", SCRIPT, estado2str(e));
    printf("<a xlink:href = \"%s\"><image x = \"1140\" y = \"710\" height = \"80\" width = \"80\" xlink:href = \"http://localhost/imagens/Reset.png\" /></a>\n", script);
}

void imprime_botao_teste(ESTADO e){

    char script[10240];
    ESTADO novo = {{0},0,{0},0,0,0,0,-1,0,{0},0,1};

    novo.mao[0] = (long long int)428947008165248;
    novo.mao[1] = (long long int)71554295677052;
    novo.mao[2] = (long long int)1733423634915331;
    novo.mao[3] = (long long int)2269674688612864;
    novo.start = 0;
    novo.actual_jogador = primeiro_jogar(novo);
    novo.cartas_bots[novo.actual_jogador] = 1;
    novo.ultimo_jogador = novo.actual_jogador;
    novo.layout = 0;
    novo.cartas[0] = numero_de_cartas(novo.mao[0]);
    novo.cartas[1] = numero_de_cartas(novo.mao[1]);
    novo.cartas[2] = numero_de_cartas(novo.mao[2]);
    novo.cartas[3] = numero_de_cartas(novo.mao[3]);


        e = bots1(novo);

        while(e.actual_jogador != 0){
            e = bots2(e);
        }

    sprintf(script, "%s?%s", SCRIPT, estado2str(e));
    printf("<a xlink:href = \"%s\"><image x = \"1050\" y = \"710\" height = \"80\" width = \"80\" xlink:href = \"http://localhost/imagens/test.png\" /></a>\n", script);

}

/*
A função imprime_botao_trofeu imprime uma imagem com um trofeu correspondente ao vencedor do jogo no final do mesmo.
@param e O estado actual.
*/
void imprime_botao_trofeu(ESTADO e) {

    char script[10240];

    if (e.cartas[0] == 0) {
        sprintf(script, "%s?%s", SCRIPT, estado2str(e));
        printf("<a xlink:href = \"%s\"><image x = \"1200\" y = \"40\" height = \"750\" width = \"750\" xlink:href = \"http://localhost/imagens/trofeu.png\" /></a>\n", script);
    }

    if (e.cartas[1] == 0) {
        sprintf(script, "%s?%s", SCRIPT, estado2str(e));
        printf("<a xlink:href = \"%s\"><image x = \"1200\" y = \"40\" height = \"750\" width = \"750\" xlink:href = \"http://localhost/imagens/trofeu2.png\" /></a>\n", script);
    }

    if (e.cartas[2] == 0) {
        sprintf(script, "%s?%s", SCRIPT, estado2str(e));
        printf("<a xlink:href = \"%s\"><image x = \"1200\" y = \"40\" height = \"750\" width = \"750\" xlink:href = \"http://localhost/imagens/trofeu3.png\" /></a>\n", script);
    }

    if (e.cartas[3] == 0){
        sprintf(script, "%s?%s", SCRIPT, estado2str(e));
        printf("<a xlink:href = \"%s\"><image x = \"1200\" y = \"40\" height = \"750\" width = \"750\" xlink:href = \"http://localhost/imagens/trofeu4.png\" /></a>\n", script);
    }
}

/** brief Trata os argumentos da CGI
Esta função recebe a query que é passada à cgi-bin e trata-a.
Neste momento, a query contém o estado que é um inteiro que representa um conjunto de cartas.
Cada carta corresponde a um bit que está a 1 se essa carta está no conjunto e a 0 caso contrário.
Caso não seja passado nada à cgi-bin, ela assume que todas as cartas estão presentes.
@param query A query que é passada à cgi-bin
*/

void parse (char *query) {

    ESTADO e;

    int a;

    if (query != NULL && strlen(query) != 0) {

        e = str2estado(query);

        if (e.card) e.card = 0;
        if (e.play) e = jogar(e);
        if (e.pass) e = passar(e);
    }

    else {

        e = bots1(baralhar());

        while(e.actual_jogador != 0){
            e = bots2(e);
        }

        a=e.mao[0];
        printf("%dn", a);
    }

    if (e.start == 1) {
        imprime_botao_start(e);
    }

    else{

        if (e.cartas[0] == 0 || e.cartas[1] == 0 || e.cartas[2] == 0 || e.cartas[3] == 0) {
            imprime_botao_trofeu(e);
            imprime(BARALHO,e);
            imprime_botao_reset(e);
        }

        else {
            imprime(BARALHO, e);
            imprime_botao_jogar(e);
            imprime_botao_passar(e);
            imprime_botao_layout(e);
            imprime_botao_clear(e);
            imprime_botao_sugestao(e);
            imprime_botao_exit(e);
            imprime_botao_teste(e);
        }
    }
}

/** brief Função principal
Função principal do programa que imprime os cabeçalhos necessários e depois disso invoca
a função que vai imprimir o código html para desenhar as cartas
*/


int main() {
/*
 * Cabeçalhos necessários numa CGI
 */

    srand(time(NULL));
    printf("Content-Type: text/html; charset=utf-8\n\n");
    printf("<header><title>Big2wo</title></header>\n");
    printf("<body>\n");
/*  printf("<h1>Big2wo</h1>\n");*/
    printf("<svg height = \"1500\" width = \"2000\">\n");
/*  printf("<rect x = \"0\" y = \"0\" height = \"800\" width = \"800\" style = \"fill:#007700\"/>\n");*/
    printf("<image x = \"0\" y = \"0\" height = \"720\" width = \"1280\" xlink:href = \"http://localhost/imagens/tabfinal.png\" /></a>\n");
    printf("<image x = \"0\" y = \"719\" height = \"80\" width = \"1280\" xlink:href = \"http://localhost/imagens/barra.png\" /></a>\n");
    printf("<image x = \"1250\" y = \"15\" height = \"10\" width = \"20\" xlink:href = \"http://localhost/imagens/easteregg.gif\" /></a>\n");
/*
 * Ler os valores passados à cgi que estão na variável ambiente e passá-los ao programa
 */

    parse(getenv("QUERY_STRING"));
    printf("</svg>\n");
    printf("</body>\n");
    return 0;
}
