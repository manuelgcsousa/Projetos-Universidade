#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "big2.h"

/** 
Transforma um estado numa string do url, e retorna essa string. 
@param e O estado actual.
@returns Uma string com o estado.
*/
char* estado2str(ESTADO e){
    static char str[10240];
    sprintf(str, FORMATO, e.mao[0], e.mao[1], e.mao[2], e.mao[3], e.highlight, e.cartas[0], e.cartas[1], e.cartas[2], e.cartas[3], e.play, e.pass, e.card,e.ultimo_jogador, e.ultima_jogada, e.actual_jogador, e.cartas_bots[0], e.cartas_bots[1], e.cartas_bots[2], e.cartas_bots[3], e.layout,e.start);
    
    return str;
}

/** 
Transforma a string do url num estado novo, e retorna esse estado. 
@param str Uma string com o estado.
@returns O novo estado.
*/
ESTADO str2estado(char* str){
    ESTADO e;
    sscanf(str, FORMATO, &e.mao[0], &e.mao[1], &e.mao[2], &e.mao[3], &e.highlight, &e.cartas[0], &e.cartas[1], &e.cartas[2], &e.cartas[3], &e.play, &e.pass, &e.card,&e.ultimo_jogador, &e.ultima_jogada, &e.actual_jogador, &e.cartas_bots[0], &e.cartas_bots[1], &e.cartas_bots[2], &e.cartas_bots[3], &e.layout,&e.start);
    
    return e;
}

/** 
Conforme as regras do jogo, a primeiro jogador a jogar é aquele que tem o 3 de ouros na sua mão.
Esta função procura nas mãos de cada jogador o valor e o naipe correspondentes ao 3 de ouros. 
O jogador que tiver essa carta, passará a ser o actual_jogador, dando então prosseguimento. 
@param e O estado actual.
@returns Um inteiro correspondente ao jogador que vai executar primeiro a jogada.
*/

int primeiro_jogar(ESTADO e){
    int n = 0;
    
    if (carta_existe(e.mao[0], 0, 0)) {
        n = 0;
    }
    
    if (carta_existe(e.mao[1], 0, 0)) {
        n = 1;   
    }
    
    if (carta_existe(e.mao[2], 0, 0)) {
        n = 2;
    }
    
    if (carta_existe(e.mao[3], 0, 0)) {
        n = 3;  
    }
    
    return n;
}


/** 
Função que distribui as cartas por todas as mãos, de igual modo (13 para cada).
Através da função "rand", as cartas são distribuidas aleatoriamente. Aqui, são feitas outras tarefas:
O actual_jogador vai tomar o valor da função primeiro_jogador, previamente definida, que indica o jogador que começa o jogo,
da-mos a e.cartas_bots[e.actual_jogador] o valor 1, de maneira que se um bot começar o jogo, este vai apresentar o 3 de ouros no tabuleiro,
e a ultimo_jogador toma o valor do actual_jogador, guardando assim o ultimo jogador que jogou corretamente.
@returns O novo estado.
*/

ESTADO baralhar () {
    int a=0, b=0, i=0, j=0, k=0, l=0, n=0, v=0, jogador;
    ESTADO e = {{0},0,{0},0,0,0,0,-1,0,{0},0,1};

    for (a = 0; a < 4; a++) {
        e.cartas[a] = 13;
    }

    for (b = 0; b < 4; b++) {
        e.cartas_bots[b] = 0;
    }

    for (n = 0; n <= 3; n++)
        for (v = 0; v <= 12; v++) {
            jogador = (rand() % 4);
            switch (jogador) {
                case 0: if (i == 13) v--; else { e.mao[jogador] +=  (add_carta (0, n, v)); i++; } break;
                case 1: if (j == 13) v--; else { e.mao[jogador] +=  (add_carta (0, n, v)); j++; } break;
                case 2: if (k == 13) v--; else { e.mao[jogador] +=  (add_carta (0, n, v)); k++; } break;
                case 3: if (l == 13) v--; else { e.mao[jogador] +=  (add_carta (0, n, v)); l++; } break;
            }
        }
        
    e.actual_jogador = primeiro_jogar(e);
    e.cartas_bots[e.actual_jogador] = 1;
    e.ultimo_jogador = e.actual_jogador;
    e.layout = 0;
    return e;   
}


/**
A valida_bots_jogadas_normais é a função que vai validar as jogadas dos bots (uma, duas e três cartas).
É retornado 0 se os bots não puderem efetuar a jogada, e 1 caso contrário.
@param e O estado actual.
@param m A mão de um jogador.
@returns Um inteiro 1 se for possível jogar, 0 caso contrário.
*/
int valida_bots_jogadas_normais (ESTADO e, MAO m) {
    
    if (!combinacao_valida (m)) {
        return 0;
    }
    else {
        if (!compara_tamanho (e.ultima_jogada, m)) {
            return 0;
        }
        else {
            if (!combinacao_maior (e.ultima_jogada, m)) {
                return 0;
            }
        }
    }
    return 1;
}


/**
O estado joga_straight_bot3ouros é aquele que faz com que o bot jogue um straight contendo o 3 de ouros. 
Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_straight_bot3ouros (ESTADO e) {
    long long int m=0, n=0;

    int v1,v2,v3,v4,v5,n1,n2,n3,n4,n5,p1,p2,p3,p4,p5;

    p1 = seleciona_maior_carta_straight_bots(e.mao[e.actual_jogador]);
    p2 = codifica((seleciona_maior_carta_straight_bots(e.mao[e.actual_jogador]))-1);
    p3 = codifica((seleciona_maior_carta_straight_bots(e.mao[e.actual_jogador]))-2);
    p4 = codifica((seleciona_maior_carta_straight_bots(e.mao[e.actual_jogador]))-3);
    p5 = codifica((seleciona_maior_carta_straight_bots(e.mao[e.actual_jogador]))-4);

    v1 = p1;
    v2 = descodifica_straight(p2);
    v3 = descodifica_straight(p3);
    v4 = descodifica_straight(p4);
    v5 = descodifica_straight(p5);

    n1 = seleciona_maior_naipeCarta_straight_bots((e.mao[e.actual_jogador]),v1);
    n2 = seleciona_maior_naipeCarta_straight_bots((e.mao[e.actual_jogador]),v2);
    n3 = seleciona_maior_naipeCarta_straight_bots((e.mao[e.actual_jogador]),v3);
    n4 = seleciona_maior_naipeCarta_straight_bots((e.mao[e.actual_jogador]),v4);
    n5 = seleciona_maior_naipeCarta_straight_bots((e.mao[e.actual_jogador]),v5);

    m = add_carta(0,n1,v1);
    m = add_carta(m,n2,v2);
    m = add_carta(m,n3,v3);
    m = add_carta(m,n4,v4);
    m = add_carta(m,n5,v5);

    n = rem_carta((e.mao[e.actual_jogador]),n1,v1);
    n = rem_carta(n,n2,v2);
    n = rem_carta(n,n3,v3);
    n = rem_carta(n,n4,v4);
    n = rem_carta(n,n5,v5);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
O estado joga_flush_bot3ouros é aquele que faz com que o bot jogue um flush contendo o 3 de ouros. 
Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_flush_bot3ouros (ESTADO e) {
    long long int m=0, n=0;
    int flag1,flag2,flag3,flag4,v1=0,v2=0,v3=0,v4=0,v5=0,p1=0,p2=0,p3=0,p4=0,p5=0,n1=0;

    n1 = 0; /* Naipe do três de ouros */

    flag1 = 0;
    p1 = v1;
    for(v2 = (v1 + 1); v2 < 13 && flag1 != 1; v2++) {
        if (carta_existe(e.mao[e.actual_jogador],n1,v2)) {
            p2 = v2;
            flag1 = 1;
        }
    }

    flag2 = 0;
    for (v3 = v2; v3 < 13 && flag2 != 1; v3++) {
        if (carta_existe(e.mao[e.actual_jogador],n1,v3)) {
            p3 = v3;
            flag2 = 1;
        }
    }

    flag3 = 0;
    for (v4 = v3; v4 < 13 && flag3 != 1; v4++) {
        if (carta_existe(e.mao[e.actual_jogador],n1,v4)) {
            p4 = v4;
            flag3 = 1;
        }
    }

    flag4 = 0;
    for (v5 = v4; v5 < 13 && flag4 != 1; v5++) {
        if (carta_existe(e.mao[e.actual_jogador],n1,v5)) {
            p5 = v5;
            flag4 = 1;
        }
    }

    m = add_carta(0,n1,p1);
    m = add_carta(m,n1,p2);
    m = add_carta(m,n1,p3);
    m = add_carta(m,n1,p4);
    m = add_carta(m,n1,p5);

    n = rem_carta((e.mao[e.actual_jogador]),n1,p1);
    n = rem_carta(n,n1,p2);
    n = rem_carta(n,n1,p3);
    n = rem_carta(n,n1,p4);
    n = rem_carta(n,n1,p5);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
O estado joga_fullhouse_bot3ourosPar é aquele que faz com que o bot jogue um fullhouse contendo o 3 de ouros.
Neste caso, o 3 de ouros faz parte do par correspondente ao fullhouse. 
Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_fullhouse_bot3ourosPar (ESTADO e) {
    long long int m=0, f=0;
    int vPar=0,n=0,n3=0,n2=0,n1=0,p1=0,p3=0,np1=0,np2=0,flag,flag1,flag3,flag4;    
    
    vPar = seleciona_par_fullhouse(e.mao[e.actual_jogador]);

    flag = 0;
    for (n = 3; n >= 1 && flag != 1; --n) {
        if(carta_existe(e.mao[e.actual_jogador], n, 0)) {
            n1 = n;
            flag = 1;
        }
    }

    flag1 = 0;
    p1 = n1-1;
    for (n = p1; n >= 1 && flag1 != 1; --n) {
        if(carta_existe(e.mao[e.actual_jogador], n, 0)) {
            n2 = n;
            flag1 = 1;
        }
    }

    n3 = 0; /* Naipe do três de ouros */

    f = rem_carta((e.mao[e.actual_jogador]),n1,0);
    f = rem_carta(f,n2,0);
    f = rem_carta(f,n3,0);

    flag3 = 0;
    for (n = 3; n >= 0 && flag3 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, vPar)) {
            np1 = n;
            flag3 = 1;
        }
    }

    flag4 = 0;
    p3 = n;
    for (n = p3; n >= 0 && flag4 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, vPar)) {
            np2 = n;
            flag4 = 1;
        }
    }

    m = add_carta(0,n1,0);
    m = add_carta(m,n2,0);
    m = add_carta(m,n3,0);
    m = add_carta(m,np1,vPar);
    m = add_carta(m,np2,vPar);


    f = rem_carta(f,np1,vPar);
    f = rem_carta(f,np2,vPar);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = f;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
O estado joga_fullhouse_bot3ourosTrio é aquele que faz com que o bot jogue um fullhouse contendo o 3 de ouros.
Neste caso, o 3 de ouros faz parte do trio correspondente ao fullhouse. 
Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_fullhouse_bot3ourosTrio (ESTADO e) {
    long long int m=0, f=0;
    int vTrio=0,n=0,n3=0,n2=0,n1=0,p1=0,p2=0,np1=0,np2=0,flag,flag1,flag2,flag3;
    
    vTrio = seleciona_trio_fullhouse(e.mao[e.actual_jogador]);

    flag = 0;
    for (n = 3; n >= 0 && flag != 1; --n) {
        if(carta_existe(e.mao[e.actual_jogador], n, vTrio)) {
            n1 = n;
            flag = 1;
        }
    }

    flag1 = 0;
    p1 = n1-1;
    for (n = p1; n >= 0 && flag1 != 1; --n) {
        if(carta_existe(e.mao[e.actual_jogador], n, vTrio)) {
            n2 = n;
            flag1 = 1;
        }
    }

    flag2 = 0;
    p2 = n2-1;
    for (n = p2; n >= 0 && flag2 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, vTrio)) {
            n3 = n;
            flag2 = 1;
        }
    }


    f = rem_carta((e.mao[e.actual_jogador]),n1,vTrio);
    f = rem_carta(f,n2,vTrio);
    f = rem_carta(f,n3,vTrio);

    flag3 = 0;
    for (n = 3; n >= 0 && flag3 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, 0)) {
            np1 = n;
            flag3 = 1;
        }
    }

    np2 = 0;

    m = add_carta(0,n1,vTrio);
    m = add_carta(m,n2,vTrio);
    m = add_carta(m,n3,vTrio);
    m = add_carta(m,np1,0);
    m = add_carta(m,np2,0);


    f = rem_carta(f,np1,0);
    f = rem_carta(f,np2,0);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = f;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}


/**
O estado joga_fourkind_bot3ouros é aquele que faz com que o bot jogue um four of a kind contendo o 3 de ouros. 
Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_fourkind_bot3ouros (ESTADO e) {
    int flag=0,p=0,f=0, a=0, b=0;
    long long int m=0,x=0;
    
    m = add_carta(0,0,0);
    m = add_carta(m,1,0);
    m = add_carta(m,2,0);
    m = add_carta(m,3,0);
    for (p = 1; p <= 12 && flag != 1; p++)
        for (f = 0; f <= 3 && flag != 1; f++) {
            if (carta_existe(e.mao[e.actual_jogador],f,p)){
                a = f;
                b = p;
                flag = 1;
            }
        }
    m = add_carta(m,a,b);

    x = rem_carta(e.mao[e.actual_jogador],0,0);
    x = rem_carta(x,1,0);
    x = rem_carta(x,2,0);
    x = rem_carta(x,3,0);
    x = rem_carta(x,a,b);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = x;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}


/**
O estado joga_straightflush_bot3ouros é aquele que faz com que o bot jogue um straight flush contendo o 3 de ouros. 
Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_straightflush_bot3ouros(ESTADO e) {
    
    long long int m=0, n=0;
    
    int v1,v2,v3,v4,v5,p2,p3,p4,p5;
    
    v1 = seleciona_maior_carta_straightflush_bots(e.mao[e.actual_jogador]);
    p2 = codifica ((seleciona_maior_carta_straightflush_bots(e.mao[e.actual_jogador]))-1);
    p3 = codifica ((seleciona_maior_carta_straightflush_bots(e.mao[e.actual_jogador]))-2);
    p4 = codifica ((seleciona_maior_carta_straightflush_bots(e.mao[e.actual_jogador]))-3);
    p5 = codifica ((seleciona_maior_carta_straightflush_bots(e.mao[e.actual_jogador]))-4);
    
    v2 = descodifica_straight(p2);
    v3 = descodifica_straight(p3);
    v4 = descodifica_straight(p4);
    v5 = descodifica_straight(p5);
    /* os ns sao todos iguais */
    
    m = add_carta(0,0,v1);
    m = add_carta(m,0,v2);
    m = add_carta(m,0,v3);
    m = add_carta(m,0,v4);
    m = add_carta(m,0,v5);
    
    n = rem_carta((e.mao[e.actual_jogador]),0,v1);
    n = rem_carta(n,0,v2);
    n = rem_carta(n,0,v3);
    n = rem_carta(n,0,v4);
    n = rem_carta(n,0,v5);
    
    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
O estado joga_trio_bot3ouros é aquele que faz com que o bot jogue um trio contendo o 3 de ouros. 
Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_trio_bot3ouros (ESTADO e) {
    int flag,flag1,flag2,n=0,n1=0,n2=0,n3=0,p1=0,p2=0;
    long long int x=0,m=0;

    flag = 0;
    for (n = 0; n < 4 && flag != 1; n++) {
        if (carta_existe(e.mao[e.actual_jogador], n, 0)) {
            n1 = n;
            flag = 1;
        }
    }

    flag1 = 0;
    p1 = n1+1;
    for (n = p1; n < 4 && flag1 != 1; n++) {
        if (carta_existe(e.mao[e.actual_jogador], n, 0)) {
            n2 = n;
            flag1 = 1;
        }
    }

    flag2 = 0;
    p2 = n2+1;
    for (n = p2; n < 4 && flag2 != 1; n++) {
        if (carta_existe(e.mao[e.actual_jogador], n, 0)) {
            n3 = n;
            flag2 = 1;
        }
    }

    m = add_carta(0,n1,0);
    m = add_carta(m,n2,0);
    m = add_carta(m,n3,0);

    x = rem_carta(e.mao[e.actual_jogador],n1,0);
    x = rem_carta(x,n2,0);
    x = rem_carta(x,n3,0);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 3;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = x;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
O estado joga_par_bot3ouros é aquele que faz com que o bot jogue um par contendo o 3 de ouros. 
Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_par_bot3ouros (ESTADO e) {
    int flag,flag1,n=0,n1=0,n2=0,p1=0;
    long long int x=0,m=0;

    flag = 0;
    for (n = 0; n < 4 && flag != 1; n++) {
        if (carta_existe(e.mao[e.actual_jogador], n, 0)) {
            n1 = n;
            flag = 1;
        }
    }

    flag1 = 0;
    p1 = n1+1;
    for (n = p1; n < 4 && flag1 != 1; n++) {
        if (carta_existe(e.mao[e.actual_jogador], n, 0)) {
            n2 = n;
            flag1 = 1;
        }
    }

    m = add_carta(0,n1,0);
    m = add_carta(m,n2,0);

    x = rem_carta(e.mao[e.actual_jogador],n1,0);
    x = rem_carta(x,n2,0);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 2;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = x;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
A função bots1 vai ser executada quando um bot começa a jogar(sendo que este tem o 3 de ouros na sua mão).
E executada na modificação do baralhar na parse. É selecionada para ser jogada a maior combinação que o bot tem na mão contendo o 3 de ouros.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO bots1(ESTADO e){

    int n,v;

    if (e.ultima_jogada == -1 && e.actual_jogador != 0 ) {

        int i;
        int contaValores[13];
        int contaNaipes[4];

        for (i = 0; i < 13; i++) {
            contaValores[i] = 0;
        }


        for (i = 0; i < 4; i++) {
            contaNaipes[i] = 0;
        }

        i = 0;
        for (v = 0; v < 13; v++) {
            for (n = 0; n < 4; n++) {
                if (carta_existe(e.mao[e.actual_jogador], n, v)) { contaValores[i]++; }
            }
            i++;
        }

        i = 0;
        for (n = 0; n < 4; n++) {
            for (v = 0; v < 13; v++) {  
                if (carta_existe(e.mao[e.actual_jogador], n, v)) { contaNaipes[i]++; }
            }
            i++;
        }

        if (seleciona_maior_carta_straightflush_bots(e.mao[e.actual_jogador]) != -1) {
            e = joga_straightflush_bot3ouros(e);
            return e;
        }

        /* FOUR KIND COM 3 DE OUROS */
        if (contaValores[0] == 4) {
            e = joga_fourkind_bot3ouros(e);
            return e;
        }

        if (contaValores[0] >= 2 && (seleciona_trio_fullhouse(e.mao[e.actual_jogador]) != -1)) {
            e = joga_fullhouse_bot3ourosTrio(e);
            return e;
        }

        if (contaValores[0] >= 3 && (seleciona_par_fullhouse(e.mao[e.actual_jogador]) != -1)) {
            e = joga_fullhouse_bot3ourosPar(e);
            return e;
        }

        /*FLUSH COM 3 DE OUROS */
        if (contaNaipes[0] == 5) {
            e = joga_flush_bot3ouros(e);
            return e;
        }

        if (seleciona_maior_carta_straight_bots(e.mao[e.actual_jogador]) != -1) {
            e = joga_straight_bot3ouros(e);
            return e;
        }

        if (contaValores[0] == 3) {
            e = joga_trio_bot3ouros(e);
            return e;
        }

        if (contaValores[0] == 2) {
            e = joga_par_bot3ouros(e);
            return e;
        }

    e.cartas[e.actual_jogador] = (e.cartas[(e.actual_jogador)]) - 1;
    e.mao[e.actual_jogador] = rem_carta(e.mao[(e.actual_jogador)],0,0);
    e.ultima_jogada = 1;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

    return e;
}




/**
Esta função, usada para os bots, verifica nas suas mãos se existe uma combinação de 3 cartas para ser jogada. Se sim, é permitida a jogada.
@param m A mão de um jogador.
@returns Um inteiro correspondente ao valor do triplo. Caso contrário, retorna -1, e não é válido.
*/
int valida_3cartas(MAO m){
    int contaValores[13];
    int a=0,i=0,n=0,v=0,b=0;
    

    for (i = 0; i < 13; i++) {
        contaValores[i] = 0;
    }

    
    for(v=0; v<13; v++){
        for(n=0;n<4; n++){
            if (carta_existe(m,n,v)) {
                contaValores[a]++;
            }
        } 
        a++;
    }
    

        while(b<13){   
            if (contaValores[b] >= 3) {
                return b;
            }
            b++;
        }
    

    return -1;
}

/**
Esta função, usada para os bots, verifica nas suas mãos se existe uma combinação de 2 cartas para ser jogada. Se sim, é permitida a jogada.
@param m A mão de um jogador.
@returns Um inteiro correspondente ao valor do par. Caso contrário, retorna -1, e não é válido.
*/
int valida_2cartas(MAO m) {
    int contaValores[13];
    int a=0,i=0,n=0,v=0,b=0;
    

    for (i = 0; i < 13; i++) {
        contaValores[i] = 0;
    }

    
    for(v=0; v<13; v++){
        for(n=0;n<4; n++){
            if (carta_existe(m,n,v)) {
                contaValores[a]++;
            }
        } 
        a++;
    }
    
    while(b<13){   
        if (contaValores[b] >= 2) {
            return b;
        }
        b++;
    }
    

    return -1;
}


/**
Seleciona três cartas com o mesmo valor da mão do bot para serem jogadas.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO bot_comeca_3cartas (ESTADO e) {
    long long int m=0, n=0;
    int v1=0,n3=0,n2=0,n1=0,p1=0,p2=0,flag,flag1,flag2;

    v1 = valida_3cartas(e.mao[e.actual_jogador]); 

    flag = 0;
    for (n = 3; n >= 0 && flag != 1; --n) {
        if(carta_existe(e.mao[e.actual_jogador], n, v1)) {
            n1 = n;
            flag = 1;
        }
    }

    flag1 = 0;
    p1 = n1-1;
    for (n = p1; n >= 0 && flag1 != 1; --n) {
        if(carta_existe(e.mao[e.actual_jogador], n, v1)) {
            n2 = n;
            flag1 = 1;
        }
    }

    flag2 = 0;
    p2 = n2-1;
    for (n = p2; n >= 0 && flag2 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, v1)) {
            n3 = n;
            flag2 = 1;
        }
    }

    m = add_carta(0,n1,v1);
    m = add_carta(m,n2,v1);
    m = add_carta(m,n3,v1);

    n = rem_carta((e.mao[e.actual_jogador]),n1,v1);
    n = rem_carta(n,n2,v1);
    n = rem_carta(n,n3,v1);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 3;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
Seleciona duas cartas com o mesmo valor da mão do bot para serem jogadas.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO bot_comeca_2cartas (ESTADO e) {
    long long int m = 0, n = 0;
    int np1=0,np2=0,p=0,p1=0,flag,flag1;

    p = valida_2cartas(e.mao[e.actual_jogador]);

    flag = 0;
    for (n = 3; n >= 0 && flag != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, p)) {
            np1 = n;
            flag = 1;
        }
    }

    flag1 = 0;
    p1 = n;
    for (n = p1; n >= 0 && flag1 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, p)) {
            np2 = n;
            flag1 = 1;
        }
    }

    m = add_carta(0,np1,p);
    m = add_carta(m,np2,p);

    n = rem_carta(e.mao[e.actual_jogador],np1,p);
    n = rem_carta(n,np2,p);

    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 2;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
Seleciona a uma carta da mão do bot para ser jogada.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO bot_comeca_1carta (ESTADO e) {
    long long int m = 0;
    int n,v;

    for (v = 0; v <= 12; v++){
        for (n = 0; n <= 3; n++){
            m = add_carta(0,n,v);
            if (carta_existe(e.mao[e.actual_jogador],n,v)){
                m = add_carta(0,n,v);
                e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) -1 ;
                e.ultima_jogada = m;
                e.cartas_bots[e.actual_jogador] = m;
                e.mao[e.actual_jogador] = rem_carta(e.mao[e.actual_jogador],n,v) ;
                e.ultimo_jogador = e.actual_jogador;
                e.actual_jogador = incrementa_jogador(e);
                e.card = 0;
                return e;
            }   
        }       
    }
    return e;
}


/**
Neste estado, como o bot começa uma jogada (quando toda a gente passa), ele vai ver qual a maior combinação que pode jogar. 
@param e O estado actual.
@returns O novo estado.
*/
ESTADO bot_comeca_jogada (ESTADO e) {

    if (maior_carta_straight_bots(e.mao[e.actual_jogador]) != -1) {
        e = joga_straight(e);
      
        return e;
    }
    else {
        if (valida_flush(e.mao[e.actual_jogador]) != -1) {
            e = joga_flush(e);
            
            return e;
        }   
        else {
            if (valida_fullhouse(e.mao[e.actual_jogador]) != -1) {
                e = joga_fullhouse(e);
                return e;
            }       
            else {
                if (maior_carta_fourkind(e.mao[e.actual_jogador]) != -1) {
                    e = joga_fourkind(e);
                    return e;
                }
                else {
                    if (maior_carta_straightflush_bots(e.mao[e.actual_jogador]) != -1) {
                        e = joga_straightflush(e);
                       
                        return e;
                    }
                    else {
                        if (valida_3cartas(e.mao[e.actual_jogador]) != -1) {
                            e = bot_comeca_3cartas(e);
                            return e;
                        }

                        else {
                            if (valida_2cartas(e.mao[e.actual_jogador]) != -1){
                                e = bot_comeca_2cartas(e);
                                return e;
                            }

                            else {
                                e = bot_comeca_1carta(e);
                                return e;
                            }
                        }
                    }
                }
            }
        }
    }

    return e;
}

/**
Neste estado, o bot vai jogar uma carta consoante a ultima carta jogada. Aqui, é vista a menor carta possível de o bot jogar, relativamente à ultima jogada.
Se o bot não tiver uma carta maior do que a ultima jogada, passará a jogada.
@param e O estado actual.
@returns O novo estado. 
*/
ESTADO joga_bot_1carta (ESTADO e) {

    long long int m = 0;
    int n,v;

    for (v = 0; v <= 12; v++){
        for (n = 0; n <= 3; n++){
            m = add_carta(0,n,v);
            if (carta_existe(e.mao[e.actual_jogador],n,v) && valida_bots_jogadas_normais(e,m)){
                m = add_carta(0,n,v);
                e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) -1 ;
                e.ultima_jogada = m;
                e.cartas_bots[e.actual_jogador] = m;
                e.mao[e.actual_jogador] = rem_carta(e.mao[e.actual_jogador],n,v) ;
                e.ultimo_jogador = e.actual_jogador;
                e.actual_jogador = incrementa_jogador(e);
                e.card = 0;
                return e;
            }
        }
    }
    e.cartas_bots[e.actual_jogador] = 0;
    e.actual_jogador = incrementa_jogador(e);
    return e;
}

/**
Neste estado, o bot vai jogar duas cartas consoante as ultimas cartas jogadas. Aqui, é visto o menor par possível de o bot jogar, relativamente à ultima jogada.
Se o bot não tiver um par maior do que a ultima jogada, passará a jogada.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_bot_2cartas (ESTADO e) {
    
    long long int m = 0, z = 0, p = 0;
    int n,v,k;
    
    for(v = 0; v <= 12; v++) {
        for(n = 0; n <= 3; n++) {
            if (carta_existe(e.mao[e.actual_jogador], n, v)) {
                m = add_carta(0,n,v);
                for(k = 0; k <= 3; k++) {
                    z = add_carta(0, k, v);
                    m = add_carta(0,n,v);
                    p = add_carta(m,k,v);
                    if (carta_existe(e.mao[e.actual_jogador],k,v) && z != m && valida_bots_jogadas_normais(e,p) ) {
                        m = add_carta(0,n,v);
                        p = add_carta(m, k, v);
                        e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 2;
                        e.ultima_jogada = p;
                        e.cartas_bots[e.actual_jogador] = p;
                        e.mao[e.actual_jogador] = rem_carta(e.mao[e.actual_jogador], n, v);
                        e.mao[e.actual_jogador] = rem_carta(e.mao[e.actual_jogador], k, v);
                        e.ultimo_jogador = e.actual_jogador;
                        e.actual_jogador = incrementa_jogador(e);
                        e.card = 0;
                        return e;
                    }
                }
            }
        }
    }
    e.cartas_bots[e.actual_jogador] = 0;
    e.actual_jogador = incrementa_jogador(e);
    return e;
}

/**
Neste estado, o bot vai jogar três cartas consoante as ultimas cartas jogadas. Aqui, é visto o menor triplo possível de o bot jogar, relativamente à ultima jogada.
Se o bot não tiver um triplo maior do que a ultima jogada, passará a jogada.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_bot_3cartas (ESTADO e) {
    
    long long int m = 0, z = 0, p = 0, g = 0;
    int n,v,k,l;
    
    for(v = 0; v <= 12; v++) {
        for(n = 0; n <= 3; n++) {
            if (carta_existe(e.mao[e.actual_jogador], n, v)) {
                m = add_carta(0,n,v);
                for(l = 0; l <= 3; l++) {
                    if (carta_existe(e.mao[e.actual_jogador], l, v)) {
                        g = add_carta(0,l,v);
                        for(k = 0; k <= 3; k++) {
                            g = add_carta(0,l,v);
                            z = add_carta(0, k, v);
                            m = add_carta(0,n,v);
                            p = add_carta(m,k,v);
                            p = add_carta(p,l,v);
                            if (carta_existe(e.mao[e.actual_jogador],k,v) && z != m && g != m && g != z && valida_bots_jogadas_normais(e,p) ) {
                                m = add_carta(0,n,v);
                                p = add_carta(m, k, v);
                                p = add_carta(p,l,v);
                                e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 3;
                                e.ultima_jogada = p;
                                e.cartas_bots[e.actual_jogador] = p;
                                e.mao[e.actual_jogador] = rem_carta(e.mao[e.actual_jogador], n, v);
                                e.mao[e.actual_jogador] = rem_carta(e.mao[e.actual_jogador], k, v);
                                e.mao[e.actual_jogador] = rem_carta(e.mao[e.actual_jogador], l, v);
                                e.ultimo_jogador = e.actual_jogador;
                                e.actual_jogador = incrementa_jogador(e);
                                e.card = 0;
                                return e;
                            }
                        }
                    }
                }
            }
        }
    }
    e.cartas_bots[e.actual_jogador] = 0;
    e.actual_jogador = incrementa_jogador(e);
    return e;
}

/**
O estado pbot executa a função validaco_5cartas, de forma a verificar qual a combinação da ultima jogada, de forma a avaliar com a combinação que vai ser jogada.
Posto isto, é chamado o esatdo fazjogada, para verificar se, e comparando com a jogada anterior, o bot tem uma combinação válida para ser jogada.
É retornado o estado do jogo actual.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO pbot(ESTADO e){
    int v;
    v = 0;
    v = validacao_5cartas(e.ultima_jogada);
    e = (fazjogada (e, v));
  
    return e;
}


/**
O estado joga_straight é aquele que faz com que o bot jogue um straight. Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_straight(ESTADO e) {
    
    long long int m=0, n=0;
    
    int v1,v2,v3,v4,v5,n1,n2,n3,n4,n5,p1,p2,p3,p4,p5;
    
    p1 = maior_carta_straight_bots (e.mao[e.actual_jogador]);
    p2 = codifica((maior_carta_straight_bots(e.mao[e.actual_jogador]))-1);
    p3 = codifica((maior_carta_straight_bots(e.mao[e.actual_jogador]))-2);
    p4 = codifica((maior_carta_straight_bots(e.mao[e.actual_jogador]))-3);
    p5 = codifica((maior_carta_straight_bots(e.mao[e.actual_jogador]))-4);
    
    v1 = p1;
    v2 = descodifica_straight(p2);
    v3 = descodifica_straight(p3);
    v4 = descodifica_straight(p4);
    v5 = descodifica_straight(p5);
    
    n1 = maior_naipe_straight_bots((e.mao[e.actual_jogador]),v1);
    n2 = maior_naipe_straight_bots((e.mao[e.actual_jogador]),v2);
    n3 = maior_naipe_straight_bots((e.mao[e.actual_jogador]),v3);
    n4 = maior_naipe_straight_bots((e.mao[e.actual_jogador]),v4);
    n5 = maior_naipe_straight_bots((e.mao[e.actual_jogador]),v5);
    
    m = add_carta(0,n1,v1);
    m = add_carta(m,n2,v2);
    m = add_carta(m,n3,v3);
    m = add_carta(m,n4,v4);
    m = add_carta(m,n5,v5);
    
    n = rem_carta((e.mao[e.actual_jogador]),n1,v1);
    n = rem_carta(n,n2,v2);
    n = rem_carta(n,n3,v3);
    n = rem_carta(n,n4,v4);
    n = rem_carta(n,n5,v5);
    
    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}


/**
O estado joga_flush é aquele que faz com que o bot jogue um flush. Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_flush(ESTADO e) {
    
    long long int m=0, n=0;
    
    int flag1,flag2,flag3,flag4,v1,v2,v3,v4,v5,p1=0,p2=0,p3=0,p4=0,p5=0,n1;
    
    n1 = valida_flush(e.mao[e.actual_jogador]);
    v1 = maior_carta_flush_bots(e.mao[e.actual_jogador], n1);
    
    flag1 = 0;
    p1 = v1;
    for(v2 = (v1 - 1); v2 >= 0 && flag1 != 1; --v2) {
        if (carta_existe(e.mao[e.actual_jogador],n1,v2)) {
            p2 = v2;
            flag1 = 1;
        }
    }
    
    flag2 = 0;
    for (v3 = v2; v3 >= 0 && flag2 != 1; --v3) {
        if (carta_existe(e.mao[e.actual_jogador],n1,v3)) {
            p3 = v3;
            flag2 = 1;
        }
    }
    
    flag3 = 0;
    for (v4 = v3; v4 >= 0 && flag3 != 1; --v4) {
        if (carta_existe(e.mao[e.actual_jogador],n1,v4)) {
            p4 = v4;
            flag3 = 1;
        }
    }
    
    flag4 = 0;
    for (v5 = v4; v5 >= 0 && flag4 != 1; --v5) {
        if (carta_existe(e.mao[e.actual_jogador],n1,v5)) {
            p5 = v5;
            flag4 = 1;
        }
    }
    
    m = add_carta(0,n1,p1);
    m = add_carta(m,n1,p2);
    m = add_carta(m,n1,p3);
    m = add_carta(m,n1,p4);
    m = add_carta(m,n1,p5);
    
    n = rem_carta((e.mao[e.actual_jogador]),n1,p1);
    n = rem_carta(n,n1,p2);
    n = rem_carta(n,n1,p3);
    n = rem_carta(n,n1,p4);
    n = rem_carta(n,n1,p5);
    
    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    
    e.actual_jogador = incrementa_jogador(e);
       
    e.card = 0;
    return e;
}

/**
O estado joga_fullhouse é aquele que faz com que o bot jogue um full house. Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_fullhouse(ESTADO e) {
    
    long long int m=0, f=0;
    
    int v1=0,n=0,n3=0,n2=0,n1=0,p=0,p1=0,p2=0,p3=0,np1=0,np2=0,flag,flag1,flag2,flag3,flag4;
    
    v1 = valida_fullhouse(e.mao[e.actual_jogador]);
    
    flag = 0;
    for (n = 3; n >= 0 && flag != 1; --n) {
        if(carta_existe(e.mao[e.actual_jogador], n, v1)) {
            n1 = n;
            flag = 1;
        }
    }
    
    flag1 = 0;
    p1 = n1-1;
    for (n = p1; n >= 0 && flag1 != 1; --n) {
        if(carta_existe(e.mao[e.actual_jogador], n, v1)) {
            n2 = n;
            flag1 = 1;
        }
    }
    
    flag2 = 0;
    p2 = n2-1;
    for (n = p2; n >= 0 && flag2 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, v1)) {
            n3 = n;
            flag2 = 1;
        }
    }


    f = rem_carta((e.mao[e.actual_jogador]),n1,v1);
    f = rem_carta(f,n2,v1);
    f = rem_carta(f,n3,v1);
    p = maior_carta_par_fullhouse(f); /* valor do par */


    flag3 = 0;
    for (n = 3; n >= 0 && flag3 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, p)) {
            np1 = n;
            flag3 = 1;
        }
    }
    
    flag4 = 0;
    p3 = n;
    for (n = p3; n >= 0 && flag4 != 1; --n) {
        if (carta_existe(e.mao[e.actual_jogador], n, p)) {
            np2 = n;
            flag4 = 1;
        }
    }
    
    m = add_carta(0,n1,v1);
    m = add_carta(m,n2,v1);
    m = add_carta(m,n3,v1);
    m = add_carta(m,np1,p);
    m = add_carta(m,np2,p);
    
    
    f = rem_carta(f,np1,p);
    f = rem_carta(f,np2,p);
    
    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = f;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
O estado joga_fourkind é aquele que faz com que o bot jogue um four of a kind. Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_fourkind(ESTADO e) {
    
    long long int m=0, n=0;
    
    int v1=0,n2=0,n1=0,p=0;
    
    v1 = maior_carta_fourkind(e.mao[e.actual_jogador]);
    
    p = da_carta_fourkind(e.mao[e.actual_jogador]);
    
    for (n1 = 3; n1 >= 0; --n1) {
        if (carta_existe(e.mao[e.actual_jogador],n1,p)) {
            n2 = n1;
        }
    }
    
    /* os v's são todos iguais, exceto v5 */
    
    m = add_carta(0,0,v1);
    m = add_carta(m,1,v1);
    m = add_carta(m,2,v1);
    m = add_carta(m,3,v1);
    m = add_carta(m,n2,p);
    
    n = rem_carta((e.mao[e.actual_jogador]),0,v1);
    n = rem_carta(n,1,v1);
    n = rem_carta(n,2,v1);
    n = rem_carta(n,3,v1);
    n = rem_carta(n,n2,p);
    
    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
O estado joga_straightflush é aquele que faz com que o bot jogue um straight flush. Após todas as validações da sua mão, ele adiciona cartas para jogar, removendo-as da sua mão.
Posto isto, mostra as cartas no tabuleiro, e é a vez de outro jogador.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO joga_straightflush(ESTADO e) {
    
    long long int m=0, n=0;
    
    int v1,v2,v3,v4,v5,n1,n2,n3,n4,n5,p2,p3,p4,p5;
    
    v1 = maior_carta_straightflush_bots (e.mao[e.actual_jogador]);
    p2 = codifica ((maior_carta_straightflush_bots(e.mao[e.actual_jogador]))-1);
    p3 = codifica ((maior_carta_straightflush_bots(e.mao[e.actual_jogador]))-2);
    p4 = codifica ((maior_carta_straightflush_bots(e.mao[e.actual_jogador]))-3);
    p5 = codifica ((maior_carta_straightflush_bots(e.mao[e.actual_jogador]))-4);
    
    v2 = descodifica_straight(p2);
    v3 = descodifica_straight(p3);
    v4 = descodifica_straight(p4);
    v5 = descodifica_straight(p5);
    /* os ns sao todos iguais */
    n1 = maior_naipeCarta_straightflush_bots((e.mao[e.actual_jogador]));
    n2 = maior_naipeCarta_straightflush_bots((e.mao[e.actual_jogador]));
    n3 = maior_naipeCarta_straightflush_bots((e.mao[e.actual_jogador]));
    n4 = maior_naipeCarta_straightflush_bots((e.mao[e.actual_jogador]));
    n5 = maior_naipeCarta_straightflush_bots((e.mao[e.actual_jogador]));
    
    m = add_carta(0,n1,v1);
    m = add_carta(m,n2,v2);
    m = add_carta(m,n3,v3);
    m = add_carta(m,n4,v4);
    m = add_carta(m,n5,v5);
    
    n = rem_carta((e.mao[e.actual_jogador]),n1,v1);
    n = rem_carta(n,n2,v2);
    n = rem_carta(n,n3,v3);
    n = rem_carta(n,n4,v4);
    n = rem_carta(n,n5,v5);
    
    e.cartas[e.actual_jogador] = (e.cartas[e.actual_jogador]) - 5;
    e.ultima_jogada = m;
    e.cartas_bots[e.actual_jogador] = m;
    e.mao[e.actual_jogador] = n;
    e.ultimo_jogador = e.actual_jogador;
    e.actual_jogador = incrementa_jogador(e);
    e.card = 0;
    return e;
}

/**
O estado passabot permite que o bot passe uma jogada, se não tiver qualquer tipo de combinação válida para ser jogada.
Retorna o estado do jogo actual, após passagem de ronda.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO passabot(ESTADO e) {
    e.cartas_bots[e.actual_jogador] = 0;
    e.actual_jogador = incrementa_jogador(e);
    return e;
}

/**
O estado bots2 é executado quando um bot executa uma jogada que não seja jogar um 3 de ouros, isto inclui jogadas de 1, 2, 3 e 5 cartas.
Se não existir nenhuma combinação válida a ser jogada (comparando com a ultima jogada), o bot passa a jogada.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO bots2(ESTADO e){
    
    int ncartas = numero_de_cartas(e.ultima_jogada);

    if (e.ultimo_jogador == e.actual_jogador) {
        e = bot_comeca_jogada(e);
        return e;
    }

    else {
        if (ncartas == 1) {
            e = joga_bot_1carta(e);
            return e;
        }
    
        if (ncartas == 2) {
            e = joga_bot_2cartas(e);
            return e;
        }
    
        if (ncartas == 3) {
            e = joga_bot_3cartas(e);
            return e;
        }

        if (ncartas == 5) {
            e = pbot(e);
          
            return e;
        }
    }

    e.cartas_bots[e.actual_jogador] = 0;
    e.actual_jogador = incrementa_jogador(e);
    return e;
}


/** 
Em cada jogada, o jogador é incrementado, tomando assim o controlo do jogador que está em jogo.
Como estes variam entre 0 e 3 (4 jogadores), enquanto ele for diferente de 3, vai incrementar para um próximo jogador, e uma próxima jogada. 
@param e O estado actual.
@returns Um inteiro correspondente ao número de um novo jogador.
*/
int incrementa_jogador (ESTADO e){
    int i=0;
    if (e.actual_jogador != 3) return (e.actual_jogador += 1);
    else return i;
}


/**
A função jogar vai ser invocada pela função parse, e vai retirar as cartas da mão do jogador caso esta exista no highlight,
colocando-a na posição do tabuleiro correspondete a jogada do utilizador, ou seja, vai ser a função executada quando carregamos no botao jogar para um dado highlight.
Esta função também convoca as funções dos bots que executam as suas jogadas enquanto não for a vez do utilizador jogar.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO jogar (ESTADO e) {
    
    int n, v, x, y;
    
    x = 551;
    y = 450;
    
    e.play = 0;
    if(e.layout == 1){
        for (v = 0; v < 13; v++) {
            for (n = 0; n < 4; n++) {
                if (carta_existe((e.highlight), n, v)) {
                    e.mao[0] = rem_carta(e.mao[0], n, v);
                    x += 20;
                    imprime_carta(BARALHO, x, y, e, 4, n , v);
                }
            }
        }
    }
    else{
        for (n = 0; n < 4; n++) {
            for (v = 0; v < 13; v++) {
                if (carta_existe((e.highlight), n, v)) {
                    e.mao[0] = rem_carta(e.mao[0], n, v);
                    x += 20;
                    imprime_carta(BARALHO, x, y, e, 4, n , v);
                }
            }
        }
        
    }
    
    e = bots2(e);
    while(e.actual_jogador != 0 && e.cartas[0] != 0 && e.cartas[1] != 0 && e.cartas[2] != 0 && e.cartas[3] != 0){
        e = bots2(e);
    }

    e.actual_jogador = 0;
    e.highlight = 0;
    return e;
    
}

/**
Função invocada na parse, que vai ser executada quando carregamos no botao passar, e que só incrementa o jogador e mantém o resto do estado.
Esta função também convoca as funções dos bots que executam as suas jogadas enquanto não for a vez do utilizador jogar.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO passar (ESTADO e) {
    
   e.pass = 0;
               
    e = bots2(e);
 
    while(e.actual_jogador != 0 && e.cartas[0] != 0 && e.cartas[1] != 0 && e.cartas[2] != 0 && e.cartas[3] != 0){     
        e = bots2(e);
    }
 
    e.actual_jogador = 0;
    e.highlight = 0;
    return e;
}


/**
O estado clear permite-nos após termos diferentes cartas no highlight, removê-las do mesmo, e colocando-as de novo na nossa mão, melhorando assim a jogabilidade.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO clear(ESTADO e){
    int n,v;
    n=0;
    v=0;
    
    for (n = 0; n <= 3; n++)
    for (v = 0; v <= 12; v++) {
        if(carta_existe(e.highlight,n,v)){
            e.highlight = rem_carta(e.highlight,n,v);
        }
    }
    return e;
}

/**
O estado sugestão permite ao utilizador clicar num botão, onde este lhe seleciona cartas válidas a ser jogadas, passando-as para o highlight.
Se o utilizador quiser jogar as cartas, simplesmente carrega no botão "SUBMIT". O estado sugestão passará a jogada sempre que não houver cartas válidas para serem jogadas.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO sugestao(ESTADO e){
    ESTADO novo =  e;

    novo.actual_jogador = 1;
    novo.mao[1] = e.mao[0]; 

    if (e.ultimo_jogador  == 0) novo.ultimo_jogador = 1;
    if (e.ultimo_jogador == 1) novo.ultimo_jogador = 2; 

    if (e.ultima_jogada == -1){
        novo = bots1(novo);
        e.highlight = novo.ultima_jogada;
    }
    else {
        novo = bots2(novo);
        e.highlight = novo.cartas_bots[1];
    }
    return e;
}
