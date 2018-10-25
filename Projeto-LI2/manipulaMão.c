#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "big2.h"

/** brief Devolve o índice da carta
@param naipe O naipe da carta (inteiro entre 0 e 3)
@param valor O valor da carta (inteiro entre 0 e 12)
@return O índice correspondente à carta
*/
int indice(int naipe, int valor) {
    return naipe * 13 + valor;
}

/** brief Adiciona uma carta ao estado
@param ESTADO O estado atual
@param naipe O naipe da carta (inteiro entre 0 e 3)
@param valor O valor da carta (inteiro entre 0 e 12)
@return O novo estado
*/
long long int add_carta(long long int ESTADO, int naipe, int valor) {
    int idx = indice(naipe, valor);
    return ESTADO | ((long long int) 1 << idx);
}

/** brief Remove uma carta do estado
@param ESTADO O estado atual
@param naipe O naipe da carta (inteiro entre 0 e 3)
@param valor O valor da carta (inteiro entre 0 e 12)
@return O novo estado
*/
long long int rem_carta(long long int ESTADO, int naipe, int valor) {
    int idx = indice(naipe, valor);
    return ESTADO & ~((long long int) 1 << idx);
}

/** brief Verifica se uma carta pertence ao estado
@param ESTADO O estado atual
@param naipe O naipe da carta (inteiro entre 0 e 3)
@param valor O valor da carta (inteiro entre 0 e 12)
@return 1 se a carta existe e 0 caso contrário
*/
int carta_existe(long long int ESTADO, int naipe, int valor) {
    int idx = indice(naipe, valor);
    return (ESTADO >> idx) & 1;
}

/**
Conta o número de cartas que se encontra numa mao numa dada altura.
@param m A mão de um jogador.
@returns O número de cartas.
*/
int numero_de_cartas(MAO m) {
    
    int n, v, contaCartas=0;
    
    for (n = 0; n < 4; n++) {
        for (v = 0; v < 13; v++)
        if (carta_existe(m, n, v)) contaCartas++;
    }
    
    return contaCartas;
}


/** 
Confirma se a mão que é recebida (o highlight), tem uma, duas ou três cartas.
@param m A mão de um jogador.
@returns Um inteiro 0 se tiver mais de três cartas, caso contrário retorna 1. 
*/
int combinacao_valida(MAO m) {
    
    if ((numero_de_cartas (m)) > 3) {
        return 0;
    }
    
    else return 1;
}

/** 
Compara o tamanha entre duas mãos, ou seja, ve o tamanho da ultima jogada, e compara o tamanho com a jogada actual,
e vê se a combinação tem o mesmo tamanho.
@param m1 A mão de um jogador.
@param m2 A mão de outro jogador.
@returns Um inteiro 1 se as mãos tiverem o mesmo tamanha, 0 caso contrário.
*/
int compara_tamanho(MAO m1, MAO m2){
    if (numero_de_cartas(m1) == numero_de_cartas(m2)) {
        return 1;
    }
    
    return 0;
}

/* *
Numa jogada de 1,2 e 3 cartas esta função é utilizada para comparar o valor entre a ultima_jogada e a efetuda pelo atual_jogador. 
@param m A mão de um jogador.
@returns O valor de uma carta.
*/
int da_valor (MAO m){
    
    int n, v, primeiraCarta, flag = 0;
    primeiraCarta = 0;
    
    for (n = 0; n < 4 && flag != 1; n++) {
        
        for (v = 0; v < 13 && flag != 1; v++)
        
        if (carta_existe(m, n, v)){
            primeiraCarta = v ;
            flag = 1;
        }
    }
    
    for (n = 0; n < 4 ; n++) {
        
        for (v = 0; v < 13 ; v++)
        
        if (carta_existe(m, n, v)){
            if (v != primeiraCarta){
                return -1 ;
            }
        }
    }
    return primeiraCarta;
}

/** 
Esta função obtém o maior naipe dentro de uma mão, de forma a ser usado para avaliar a maior jogada. 
@param m A mão de um jogador.
@returns O naipe de uma carta.
*/

int da_maior_naipe (MAO m){
    int n, v, maior=0;
    
    for (n = 0; n < 4; n++) {
        
        for (v = 0; v < 13; v++)
        
        if (carta_existe(m, n, v)) { if (n > maior) maior = n;}
    }
    
    return maior;
}


/** 
Aqui, é verificado se a jogada actual é maior que a jogada anterior. Conforme o retorno da função dá valor, a jogada
actual é possível ser realizada ou não. Ao comparar, de os retornos das duas funções forem iguais, ou seja, se os maiores
valores de cada mão forem iguais, comparamos o naipe, assim identificando qual a maior jogada.
@param m1 A mão de um jogador.
@param m2 A mão de outro jogador.
@returns Um inteiro 1 caso seja possível jogar, 0 caso contrário.
*/

int combinacao_maior (MAO m1, MAO m2) {
    int n =0;
    if ((da_valor(m2)) != -1){
        if (da_valor(m1) < da_valor(m2)) n =1;
        if (da_valor(m1) > da_valor(m2)) n= 0 ;
        if (da_valor(m1) == da_valor(m2)) {
            if (da_maior_naipe(m1) < da_maior_naipe(m2)) n=1;
            
        }
    }
    
    else n= 0;
    
    return n;
}

/**
Ao receber o highlight do utilizador (neste caso um straight), confirma se este é válido para ser efetuada uma jogada.
@param m A mão de um jogador.
@returns Um inteiro 1, se for válido o straight, e -1 caso contrário.
*/
int valida_straight (MAO m) {
    
    int v,i,n,j;
    
    int contaValores[14];
    
    for (i = 0; i < 14; i++) {
        contaValores[i] = 0;
    }
    
    i = 2;
    for (v = 0; v <= 13; v++) {
        for(n = 0; n < 4; n++) {
            switch (v) {
                case 11: if (carta_existe(m,n,v)) { contaValores[0]++; contaValores[13]++; } break;
                case 12: if (carta_existe(m,n,v)) { contaValores[1]++; } break;
                default: if (carta_existe(m,n,v)) { contaValores[i]++; } break;
            }
        }
        i++;
    }
    
    j = 0;
    while ((j + 4) < 14) {
        if ((contaValores[j] != 0) && (contaValores[j+1] != 0) && (contaValores[j+2] != 0) && (contaValores[j+3] != 0) && (contaValores[j+4] != 0)) {
            return 1;
        }
        j++;
    }
    
    return -1;
}

/**
Nas cartas selecionadas pelo utilizador (neste caso um straight), determina o valor da maior carta desse, para mais tarde comparar, se necessário, com as ultimas jogadas, de forma a avaliar a jogada.
@param m A mão de um jogador.
@returns O valor da maior carta de um straight.
*/
int maior_carta_straight (MAO m) {
    
    int v,i,n,j;
    
    int contaValores[14];
    
    for (i = 0; i < 14; i++) {
        contaValores[i] = 0;
    }
    
    i = 2;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            switch (v) {
                case 11: if (carta_existe(m,n,v)) { contaValores[0]++; contaValores[13]++; } break;
                case 12: if (carta_existe(m,n,v)) { contaValores[1]++; } break;
                default: if (carta_existe(m,n,v)) { contaValores[i]++; } break;
            }
        }
        i++;
    }
    
    if ((contaValores[1] != 0)) {
        for (j = 12; j >= 0; j--) {
            if (contaValores[j] != 0) {
                return (contaValores[j]);
            }
        }
    }
    
    else {
        for (j = 13; j > 0; j--) {
            if (contaValores[j] != 0) {
                return contaValores[j];
            }
        }
    }
    return 0; /* TER CUIDADO COM ISTO, FOI ADICIONADO PARA RESOLVER PROBLEMAS DE COMPILAÇÃO */
}


/**
Nas cartas selecionadas pelo utilizador (neste caso um straight), determina o naipe da maior carta desse, para mais tarde comparar, se necessário, com as ultimas jogadas, de forma a avaliar a jogada.
@param m A mão de um jogador.
@param maiorCarta Maior carta de um straight.
@returns O naipe da maior carta do straight.
*/
int maior_naipe_straight (MAO m, int maiorCarta) {
    
    int i, n, v;
    i = 0;
    v = maiorCarta;
    
    switch (v) {
        case 0: { v = 11; } break;
        case 1: { v = 12; } break;
        default: { v -= 2; } break;
    }
    
    for (n = 0; n < 4; n++) {
        if (carta_existe(m,n,v)) i = n;
    }
    
    return i;
}

/**
Ao receber o highlight do utilizador (neste caso um flush), confirma se este é válido para ser efetuada uma jogada.
Nesta função, é retornado 0,1,2 ou 3 conforme o naipe do flush.
Exemplo:
0 -> Ouros;
1 -> Copas;
etc...
Com isto, conseguimos mais tarde avaliar dois flushes.
@param m A mão de um jogador.
@returns Um inteiro (0,1,2,3), que corresponde a diferentes naipes.
*/
int valida_flush (MAO m) {
    
    int v, n, i;
    
    int contaNaipes[4];
    
    for (i = 0; i < 4; i++) {
        contaNaipes[i] = 0;
    }
    
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            switch(n) {
                case 0: if (carta_existe(m,n,v)) { contaNaipes[0]++; } break;
                case 1: if (carta_existe(m,n,v)) { contaNaipes[1]++; } break;
                case 2: if (carta_existe(m,n,v)) { contaNaipes[2]++; } break;
                case 3: if (carta_existe(m,n,v)) { contaNaipes[3]++; } break;
            }
        }
    }
    
    if (contaNaipes[3] >= 5) return 3;
    
    else {
        if (contaNaipes[2] >= 5) return 2;
        
        else {
            if (contaNaipes[1] >= 5) return 1;
            
            else {
                if (contaNaipes[0] >= 5) return 0;
            }
        }
    }

    return -1;
}


/**
Nas cartas selecionadas pelo utilizador (neste caso um flush), determina o valor da maior carta desse, para mais tarde comparar, se necessário, com as ultimas jogadas, de forma a avaliar a jogada.
@param m A mão de um jogador.
@returns O valor da maior carta do flush.
*/
int maior_carta_flush (MAO m) {
    
    int i, n, v;
    i = 0;
    for (v = 0; v < 13; v++) {
        for (n = 0; n < 4; n++) {
            if (carta_existe(m,n,v)) i = v;
        }
    }
    
    return i;
}


/**
Ao receber o highlight do utilizador (neste caso um fullhouse), confirma se este é válido para ser efetuada uma jogada.
@param m A mão de um jogador.
@returns O valor.
*/
int valida_fullhouse (MAO m) {
    
    int v,i,n,j,p,flag;
    int contaValores[13];
    
    p=-1;
    flag=0;
    
    for (i = 0; i < 13; i++) {
        contaValores[i] = 0;
    }
    
    i = 0;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            if (carta_existe(m,n,v)) {
                contaValores[i]++;
            }
        }
        i++;
    }
    
    for (i = 0; i < 13 && flag != 1; i++) {;
        if (contaValores[i] >= 3 && flag != 1) {
            for (j = 0; j < 13 && flag != 1; j++) {
                if ((contaValores[j] >= 2) && j != i && flag != 1) {
                    p = i;
                    flag = 1;
                    return p;
                }
            }
        }
    }
    return p;
}


/**
Nas cartas selecionadas pelo utilizador (neste caso um fullhouse), determina o valor da maior carta do trio desse, para mais tarde comparar, se necessário, com as ultimas jogadas, de forma a avaliar a jogada.
@param m A mão de um jogador.
@returns O valor da maior carta do full house.
*/
int maior_carta_trio_fullhouse (MAO m) {
    
    int v,i,n,j,var;
    int contaValores[14];
    var = 0;
    
    for (i = 0; i < 14; i++) {
        contaValores[i] = 0;
    }
    
    i = 0;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            if (carta_existe(m,n,v)) {
                contaValores[i]++;
            }
        }
        i++;
    }
    
    for (j = 0; j < 13; j++) {
        if (contaValores[j] == 3) {
            var = j;
        }
    }
    
    return var;
}


/**
Ao receber o highlight do utilizador (neste caso um four of a kind), confirma se este é válido para ser efetuada uma jogada.
@param m A mão de um jogador.
@returns Um inteiro 1, se for válido o four of a kind, e -1 caso contrário.
*/
int valida_fourkind (MAO m) {
    
    int v,i,n,j;
    
    int contaValores[13];
    
    for (i = 0; i < 13; i++) {
        contaValores[i] = 0;
    }
    
    i = 0;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            if ((carta_existe(m,n,v))) {
                contaValores[i]++;
            }
        }
        i++;
    }
    
    for (j = 0; j < 13; j++) {
        if (contaValores[j] == 4) {
            
            return 1;
        }
    }
    
    return 0;
}


/**
Nas cartas selecionadas pelo utilizador (neste caso um four of a kind), determina o valor da maior carta de entre as quatro cartas com o mesmo valor,
para mais tarde comparar, se necessário, com as ultimas jogadas, de forma a avaliar a jogada.
@param m A mão de um jogador.
@returns O valor das quatro cartas iguais.
*/
int maior_carta_fourkind (MAO m) {
    
    int v,i,x,n,j,var,flag;
    int contaValores[13];
    
    var = -1;
    
    for (i = 0; i < 13; i++) {
        contaValores[i] = 0;
    }
    
    x = 0;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            if (carta_existe(m,n,v)) {
                contaValores[x]++;
            }
        }
        x++;
    }
    
    flag = 0;
    for (j = 12; j >= 0 && flag != 1; --j) {
        if ((contaValores[j]) == 4) {
            var = j;
            flag = 1;
        }
    }
    return var;
}


/**
Ao receber o highlight do utilizador (neste caso um straight flush), confirma se este é válido para ser efetuada uma jogada.
@param m A mão de um jogador.
@returns Um inteiro 1, se for válido o straight, e -1 caso contrário.
*/
int valida_straightflush (MAO m) {
    
    int v,i,n,j,var,flag;
    
    int contaValores[14];
    int contaNaipes[4];
    
    for (i = 0; i < 14; i++) {
        contaValores[i] = 0;
    }
    
    for (i = 0; i < 4; i++) {
        contaNaipes[i] = 0;
    }
    
    i = 2;
    for (v = 0; v < 14; v++) {
        for(n = 0; n < 4; n++) {
            switch (v) {
                case 11: if (carta_existe(m,n,v)) { contaValores[0]++; contaValores[13]++; } break;
                case 12: if (carta_existe(m,n,v)) { contaValores[1]++; } break;
                default: if (carta_existe(m,n,v)) { contaValores[i]++; } break;
            }
        }
        i++;
    }
    
    for (v = 0; v < 14; v++) {
        for(n = 0; n < 4; n++) {
            switch(n) {
                case 0: if (carta_existe(m,n,v)) { contaNaipes[0]++; } break;
                case 1: if (carta_existe(m,n,v)) { contaNaipes[1]++; } break;
                case 2: if (carta_existe(m,n,v)) { contaNaipes[2]++; } break;
                case 3: if (carta_existe(m,n,v)) { contaNaipes[3]++; } break;
            }
        }
    }
    
    j = 0;
    flag = 1;
    var = 0;
    while ((j + 4) < 14) {
        if ((contaValores[j] != 0) && (contaValores[j+1] != 0) && (contaValores[j+2] != 0) && (contaValores[j+3] != 0) && (contaValores[j+4] != 0)) {
            while(flag != 0) {
                for (v = 0; v < 14; v++) {
                    for (n = 0; n < 4; n++) {
                        if (carta_existe(m,n,v)) {
                            var = n;
                            flag = 0;
                        }
                    }
                }
            }
            for (v = 0; v < 14; v++) {
                for (n = 0; n < 4; n++) {
                    if ((carta_existe(m,n,v)) && (var != n)) {
                        return -1;
                    }
                }
            }
            return 1;
        }
        j++;
    }
    
    return -1;
}


/**
Função que valida o highlight do jogador, consoante a ultima jogada. 
Neste caso, como a ultima jogada foi um straight, a função valida se as 5 cartas (straight) escolhidas pelo utilizador são válidas de ser jogadas.
@param e O estado actual.
@returns Um inteiro 1 se for possível jogar, caso contrário retorna 0.
*/
int utilizador_joga_straight (ESTADO e) {
    /*VAI BUSCAR A MAIOR CARTA DAS DUAS*/
    if ((maior_carta_straight_bots(e.highlight)) > (maior_carta_straight_bots(e.ultima_jogada))) {
        return 1;
    }
    else {
        if ((maior_carta_straight_bots(e.highlight)) == (maior_carta_straight_bots(e.ultima_jogada))) {
            if ((maior_naipe_straight_bots(e.highlight, (maior_carta_straight_bots(e.highlight)))) > (maior_naipe_straight_bots(e.ultima_jogada, (maior_carta_straight_bots(e.ultima_jogada))))) {
                return 1;
            }
            else {
                return 0;
            }
        }
    }
    return 0;
}


/**
Função que valida o highlight do jogador, consoante a ultima jogada. 
Neste caso, como a ultima jogada foi um flush, a função valida se as 5 cartas (flush) escolhidas pelo utilizador são válidas de ser jogadas.
@param e O estado actual.
@returns Um inteiro 1 se for possível jogar, caso contrário retorna 0.
*/
int utilizador_joga_flush (ESTADO e) {
    /*VAI BUSCAR O NAIPE DAS DUAS, E VE SE O DE O HIGHLIGHT É MAIOR, E SE O NAIPE FOR IGUAL, VAI BUSCAR A MAIOR CARTA*/
    if ((valida_flush(e.highlight)) < (valida_flush(e.ultima_jogada))) {
        return 0;
    }
    else {
        if ((valida_flush(e.highlight)) > (valida_flush(e.ultima_jogada))) {
            return 1;
        }
        else { /*e.highlight == e.ultima_jogada*/
            if ((maior_carta_flush(e.highlight)) < (maior_carta_flush(e.ultima_jogada))) {
                return 0;
            }
            else {
                return 1;
            }
        }
    }
    return 0;
}



/**
Função que valida o highlight do jogador, consoante a ultima jogada. 
Neste caso, como a ultima jogada foi um full house, a função valida se as 5 cartas (full house) escolhidas pelo utilizador são válidas de ser jogadas.
@param e O estado actual.
@returns Um inteiro 1 se for possível jogar, caso contrário retorna 0.
*/
int utilizador_joga_fullhouse (ESTADO e) {
    if (valida_fullhouse(e.highlight) == -1 || (valida_fullhouse(e.highlight) < valida_fullhouse(e.ultima_jogada))) {
        return 0;
    }
    return 1;
}


/**
Função que valida o highlight do jogador, consoante a ultima jogada. 
Neste caso, como a ultima jogada foi um four of a kind, a função valida se as 5 cartas (four of a kind) escolhidas pelo utilizador são válidas de ser jogadas.
@param e O estado actual.
@returns Um inteiro 1 se for possível jogar, caso contrário retorna 0.
*/
int utilizador_joga_fourkind (ESTADO e) {
    if (maior_carta_fourkind(e.highlight) > maior_carta_fourkind(e.ultima_jogada) && maior_carta_fourkind(e.highlight) != -1 ) {
        return 1;
    }
    return 0;
}

/**
Função que valida o highlight do jogador, consoante a ultima jogada. 
Neste caso, como a ultima jogada foi um straight flush, a função valida se as 5 cartas (straight flush) escolhidas pelo utilizador são válidas de ser jogadas.
@param e O estado actual.
@returns Um inteiro 1 se for possível jogar, caso contrário retorna 0.
*/
int utilizador_joga_straightflush (ESTADO e) {
    if ((maior_carta_straightflush_bots(e.highlight)) == -1) {
        return 0;
    }
    else {
        if (maior_carta_straight(e.highlight) > maior_carta_straight(e.ultima_jogada)) {
            return 1;
        }
        else {
            if (maior_carta_straight(e.highlight) == maior_carta_straight(e.ultima_jogada) && (valida_flush(e.highlight)) > (valida_flush(e.ultima_jogada))) {
                return 1;
            }
            else {
                return 0;
            }
        }
    }
    return 0;
}

/**
Função que valida o highlight do jogador, para 1, 2 ou 3 cartas. Aqui, é permitido ao utilizador jogar uma das 3 combinações.
@param e O estado actual.
@returns Um inteiro 1 se for possível jogar, caso contrário retorna 0.
*/
int utilizador_joga_singles_pares_triplos (ESTADO e) {
    if (e.ultima_jogada == -1) {
        if (!combinacao_valida (e.highlight) ) {
            return 0;
        }
        else {
            if (e.actual_jogador != 0) {
                return 0;
            }
            else {
                if ((da_valor (e.highlight) != -1) && (carta_existe(e.highlight, 0, 0))) {
                    return 1;
                }
                else return 0;
            }
        }
    }
    
    if (e.ultimo_jogador == 0) {
        if (!combinacao_valida (e.highlight) ) {
            return 0;
        }
        else {
            if (e.actual_jogador != 0) {
                return 0;
            }
            else {
                if ((da_valor (e.highlight) != -1) ) {
                    return 1;
                }
                else return 0;
            }
        }
    }
    
    else {
        if (!combinacao_valida (e.highlight)) {
            return 0;
        }
        else {
            if (e.actual_jogador != 0) {
                return 0;
            }
            else {
                if (!compara_tamanho (e.ultima_jogada, e.highlight)) {
                    return 0;
                }
                else {
                    if (!combinacao_maior (e.ultima_jogada, e.highlight)) {
                        return 0;
                    }
                }
            }
        }
        return 1;
    }
}

/** 
No caso dos straight e straight flush, os ases podiam tomar dois valores diferentes, e o 2 era uma carta baixa, e não a mais alta do jogo. Para isso, criamos um array com 14 posições.
Conforme a validação do que é selecionado pelo utilizador, vemos que "valor" é que toma os ases e o 2 (quando são selecionados), estando estes codificados.
Para usarmos esses valores em comparações ou em qualquer outro processo, temos de os descodificar, para tudo funcionar corretamente. A função descodifica_straight faz exatamente isso, retorna o "verdadeiro" valor da carta (não codificado).
@param maiorCarta O valor da maior carta codificado.
@returns O valor da maior carta descodificado.
*/
int descodifica_straight (int maiorCarta) {
    int v;
    v = maiorCarta;
    switch (v) {
        case 0: { v = 11; } break;
        case 1: { v = 12; } break;
        default: { v -= 2; } break;
    }
    return v;
}

/**
Esta função faz o contrário da função descodifica_straight. Pega nos valores normais do jogo, e codifica-os de forma a serem trabalhados em validações de straight e straight flush.
@param v O valor da maior carta descodificado.
@returns O valor da maior carta codificado.
*/
int codifica(int v){
    switch (v) {
        case 11: return 13; break;
        case 12: return 1; break;
        default:{v=v+2; return v;} break;
    }
}


/**
A função validacao_5cartas identifica qual tipo de combinação é que vai ser analisada. Conforme os returns de 1,2,3,4,5, o programa sabe qual combinação avaliar.
@param m A mão de um jogador.
@returns Um inteiro correspondente a uma sequência.
*/
int validacao_5cartas (MAO m) {
    
    if ((maior_carta_straightflush_bots(m)) != -1) {
        return 5;
    }
    else {
        if ((valida_straight(m)) == 1) {
            return 1;
        }
        else {
            if ((valida_flush(m)) != -1) {
                return 2;
            }
            else {
                if ((valida_fullhouse(m)) != -1) {
                    return 3;
                }
                else {
                    if ((maior_carta_fourkind(m)) != -1) {
                        return 4;
                    }
                }
            }
        }
    }
    return -1;
}


/**
É nesta função que a jogada do utilizador é verificada. Se esta for possível, é permitido ao utilizador colocar as cartas
no meio do tabuleiro, registando assim a sua jogada. Caso contrário, ou o utilizador passa, ou tenta arranjar outro tipo de combinação.
@param e O estado actual.
@returns Um inteiro 1 se for possível jogar, caso contrário retorna 0. 
*/
int posso_jogar (ESTADO e) {
    
    if ((numero_de_cartas(e.highlight)) == 5) {
        if (e.ultima_jogada == -1) {
            if (((validacao_5cartas(e.highlight)) != -1) && (carta_existe(e.highlight, 0, 0)) && (e.actual_jogador == 0)) {
                return 1;
            }
            else {
                return 0;
            }
        }
        if (e.ultimo_jogador == 0 && e.actual_jogador == 0) {
            if ((validacao_5cartas(e.highlight)) != -1) {
                return 1;
            }
            else {
                return 0;
            }
        }
        else {
            if (!compara_tamanho(e.ultima_jogada, e.highlight)) { return 0; }
            else{
                if (e.ultimo_jogador == 0) {
                    if (((validacao_5cartas(e.highlight)) != -1) && (e.actual_jogador == 0)) {
                        return 1;
                    }
                    else {
                        return 0;
                    }
                }
                else {   
                    if ((validacao_5cartas(e.highlight)) > (validacao_5cartas(e.ultima_jogada))) {
                        return 1;
                    }
                    else {
                        if ((validacao_5cartas(e.highlight)) < (validacao_5cartas(e.ultima_jogada))) {
                            return 0;
                        }   
                        else {
                            if ((validacao_5cartas(e.highlight)) == 1) {
                                int a = utilizador_joga_straight(e);
                                return a;
                            }   
                            else {
                                if ((validacao_5cartas(e.highlight)) == 2) {
                                    int b = utilizador_joga_flush(e);
                                    return b;
                                }
                                if ((validacao_5cartas(e.highlight)) == 3) {
                                    int c = utilizador_joga_fullhouse(e);
                                    return c;
                                }
                                if ((validacao_5cartas(e.highlight)) == 4) {
                                    int d = utilizador_joga_fourkind(e);
                                    return d;
                                }
                                if ((validacao_5cartas(e.highlight)) == 5) {
                                    int f = utilizador_joga_straightflush(e);
                                    return f;
                                }
                            }
                        }
                    }
                }
                return 0;
            }
        }
    }
    
    else {
        int g = utilizador_joga_singles_pares_triplos(e);
        return g;
    }
}

/**
A função seleciona_maior_carta_straightflush_bots retorna o maior valor da carta de um straight flush
Aqui temos em consideração só todos os straight flushes possíveis com a carta 3 de ouros.
@param m A mão de um jogador.
@returns Um inteiro correspondente à maior carta de um straight flush, -1 caso não exista.
*/
int seleciona_maior_carta_straightflush_bots (MAO m) {
    
    int x,v,i,n,j,p0,p1,p2,p3,p4;
    
    int contaValores1[14];
    int contaNaipes1[4];
    int maioresCartasStraightsPossiveis1[3];

    x = p0 = p1 = p2 = p3 = p4 = 0;
    
    for (i = 0; i < 14; i++) {
        contaValores1[i] = 0;
    }
    
    for (i = 0; i < 4; i++) {
        contaNaipes1[i] = 0;
    }
    
    for (i = 0; i < 3; i++) {
        maioresCartasStraightsPossiveis1[i] = -1;
    }

    i = 2;
    for (v = 0; v < 14; v++) {
        for(n = 0; n < 4; n++) {
            switch (v) {
                case 11: if (carta_existe(m,n,v)) { contaValores1[0]++; contaValores1[13]++; } break;
                case 12: if (carta_existe(m,n,v)) { contaValores1[1]++; } break;
                default: if (carta_existe(m,n,v)) { contaValores1[i]++; } break;
            }
        }
        i++;
    }
    
    for (v = 0; v < 14; v++) {
        for(n = 0; n < 4; n++) {
            switch(n) {
                case 0: if (carta_existe(m,n,v)) { contaNaipes1[0]++; } break;
                case 1: if (carta_existe(m,n,v)) { contaNaipes1[1]++; } break;
                case 2: if (carta_existe(m,n,v)) { contaNaipes1[2]++; } break;
                case 3: if (carta_existe(m,n,v)) { contaNaipes1[3]++; } break;
            }
        }
    }
    
    for (i = 6, j = 0; (i - 4) >= 0; --i) {
        if ((contaValores1[i] != 0) && (contaValores1[i-1] != 0) && (contaValores1[i-2] != 0) && (contaValores1[i-3] != 0) && (contaValores1[i-4] != 0)) {
            maioresCartasStraightsPossiveis1[j] = i;
            j++;
        }
    }
    
    for (i = 0; i < 3; i++) {
        if (maioresCartasStraightsPossiveis1[i] != -1) {
            x = maioresCartasStraightsPossiveis1[i];
            p0 = descodifica_straight(x);
            p1 = descodifica_straight(x-1);
            p2 = descodifica_straight(x-2);
            p3 = descodifica_straight(x-3);
            p4 = descodifica_straight(x-4);
            
            n = 0;
            if ((carta_existe(m,n,p0)) && (carta_existe(m,n,p1)) && (carta_existe(m,n,p2)) && (carta_existe(m,n,p3)) && (carta_existe(m,n,p4))) {
                return p0;
            }
        }
    }
    return -1;
}


/**
A função seleciona_par_fullhouse retorna o valor de um par para o full house, lembrando que o par é feito por cartas com valor 3 (incluíndo o 3 de ouros).
@param m A mão de um jogador.
@returns Um inteiro correspondente ao valor do par, -1 caso não exista.
*/
int seleciona_par_fullhouse (MAO m) {
    int n,v,i;
    int contaValoresFullHouse[13];

    for (i = 0; i < 13; i++) {
        contaValoresFullHouse[i] = 0;
    }

    i = 0;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            if ((carta_existe(m,n,v))) {
                contaValoresFullHouse[i]++;
            }
        }
        i++;
    }

    for (i = 1; i < 13; i++) {
        if (contaValoresFullHouse[i] >= 2) return i;
    }
    return -1;
}

/**
A função seleciona_par_fullhouse retorna o valor de um trio para o full house, lembrando que o trio é feito por cartas com valor 3 (incluíndo o 3 de ouros).
@param m A mão de um jogador.
@returns Um inteiro correspondente ao valor do par, -1 caso não exista.
*/
int seleciona_trio_fullhouse (MAO m) {

    int n,v,i;
    int contaValoresFullHouse[13];

    for (i = 0; i < 13; i++) {
        contaValoresFullHouse[i] = 0;
    }

    i = 0;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            if ((carta_existe(m,n,v))) {
                contaValoresFullHouse[i]++;
            }
        }
        i++;
    }

    for (i = 1; i < 13; i++) {
        if (contaValoresFullHouse[i] >= 3) return i;
    }
    return -1;
}

/**
A função seleciona_maior_carta_straight_bots retorna o maior valor da carta de um straight.
Aqui temos em consideração todos os straights possíveis com a carta 3 de ouros.
@param m A mão de um jogador.
@returns Um inteiro correspondente ao valor da maior carta de um straight, -1 caso não exista.
*/
int seleciona_maior_carta_straight_bots (MAO m) {
    
    int v,i,n,j;
    
    int contaValores[14];
    
    for (i = 0; i < 14; i++) {
        contaValores[i] = 0;
    }
    
    i = 2;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            switch (v) {
                case 11: if (carta_existe(m,n,v)) { contaValores[0]++; contaValores[13]++; } break;
                case 12: if (carta_existe(m,n,v)) { contaValores[1]++; } break;
                default: if (carta_existe(m,n,v)) { contaValores[i]++; } break;
            }
        }
        i++;
    }

    j = 6;
    while ((j-4) >= 0) {
        if ((contaValores[j] != 0) && (contaValores[j-1] != 0) && (contaValores[j-2] != 0) && (contaValores[j-3] != 0) && (contaValores[j-4] != 0)) {
            switch (j) {
                case 0: { j = 11; } break;
                case 1: { j = 12; } break;
                default: { j -= 2; } break;
            }
            return j;
        }
        j--;
    }

    return -1;
}

/**
A função seleciona_maior_naipeCarta_straight_bots retorna o naipe da maior carta de um straight.
@param m A mão de um jogador.
@param maiorCarta Maior carta de um straight.
@returns Um inteiro correspondente ao naipe da maior carta de um straight.
*/
int seleciona_maior_naipeCarta_straight_bots(MAO m, int maiorCarta) {
    int i,n;
    i = 0;
    if (maiorCarta == 0) return 0;
    for (n = 3; n >= 0; --n) {
        if (carta_existe(m,n,maiorCarta)) i = n;
    }
    return i;
} 


/**
Nas cartas selecionadas pelo utilizador (neste caso um straight flush), determina o valor da maior carta desse, para mais tarde comparar, se necessário, com as ultimas jogadas, de forma a avaliar a jogada.
Esta função foi usada para avaliar a mão do utilizador, pois seguia o mesmo precedimento.
A função retorna então o valor da maior carta de um straight se existir um straight, e -1 se não existir.
@param m A mão de um jogador.
@returns O valor de uma carta.
*/
int maior_carta_straight_bots(MAO m){
    /* USAMOS MAIOR_CARTA_STRAIGHT_BOTS PARA AVALIAR A MAIOR CARTA DE UM STRAIGHT FLUSH, POIS ERA O MESMO PROCEDIMENTO */
    int v,i,n,j;
    
    int contaValores[14];
    
    for (i = 0; i < 14; i++) {
        contaValores[i] = 0;
    }
    
    i = 2;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            switch (v) {
                case 11: if (carta_existe(m,n,v)) { contaValores[0]++; contaValores[13]++; } break;
                case 12: if (carta_existe(m,n,v)) { contaValores[1]++; } break;
                default: if (carta_existe(m,n,v)) { contaValores[i]++; } break;
            }
        }
        i++;
    }
    
    j = 13;
    while ((j-4) >= 0) {
        if ((contaValores[j] != 0) && (contaValores[j-1] != 0) && (contaValores[j-2] != 0) && (contaValores[j-3] != 0) && (contaValores[j-4] != 0)) {
            switch (j) {
                case 0: { j = 11; } break;
                case 1: { j = 12; } break;
                default: { j -= 2; } break;
            }
            return j;
        }
        j--;
    }

    return -1;
}


/**
Depois de ser validada um straight na mão de um dos bots, esta função determina o naipe da maior carta nesse straight, para se for necessário comparar com valores iguais.
A função retorna o valor correspondente ao naipe da maior carta do straight existente.
@param m A mão de um jogador.
@param maiorCarta A maior carta de um straight.
@returns O naipe da maior carta de um straight.
*/
int maior_naipe_straight_bots (MAO m, int maiorCarta) {
    int i,n;
    i = 0;
    for (n = 3; n >= 0; --n) {
        if (carta_existe(m,n,maiorCarta)) i = n;
    }
    return i;
}


/**
Depois de ser validada um flush na mão de um dos bots, esta função determina o valor da maior carta nesse flush, para se for necessário comparar com valores iguais.
A função retorna o valor da maior carta do flush.
@param m A mão de um jogador.
@param n1 Naipe correspondente ao flush.
@returns O valor da maior carta do flush.
*/
int maior_carta_flush_bots (MAO m, int n1) {
    int i,v,flag;
    i = 0;
    flag = 0;
    for (v = 12; (v >= 0) && (flag != 1); --v) {
        if (carta_existe(m,n1,v)) {
            flag = 1;
            i = v;
        }
    }
    return i;
}


/**
Para a formação de um four of a kind, é necessário quatro cartas com o mesmo valor mais uma carta qualquer existente na mão.
Esta função escolhe essa carta, verificando primeiro se existe na mão. A primeira que for encontrada, é adicionada ao four of a kind.
A função retorna o valor da primeira carta que encontra para juntar ao four of a kind.
@param m A mão de um jogador.
@returns O valor de uma carta.
*/
int da_carta_fourkind (MAO m) {
    
    int v,i,n,j,var=0,flag;
    int contaValores[13];
    
    for (i = 0; i < 13; i++) {
        contaValores[i] = 0;
    }
    
    i = 0;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            if ((carta_existe(m,n,v))) {
                contaValores[i]++;
            }
        }
        i++;
    }
    
    flag = 0;
    for (j = 0; (j < 13) && (flag != 1); j++) {
        if (contaValores[j] == 1) {
            var = j;
            flag = 1;
        }
    }
    
    return var;
}



/**
Esta função procura na mão um par válido para ser usado num full house. Se existir um par, é retornado o valor desse.
@param m A mão de um jogador.
@returns Valor de um par para full house.
*/
int maior_carta_par_fullhouse (MAO m) {
    
    int v,i,n,j,var,flag;
    int contaValores[13];
    var = 0;

    for (i = 0; i < 13; i++) {
        contaValores[i] = 0;
    }
    
    i = 0;
    for (v = 0; v < 13; v++) {
        for(n = 0; n < 4; n++) {
            if (carta_existe(m,n,v)) {
                contaValores[i]++;
            }
        }
        i++;
    }
    
    flag = 0;
    for (j = 0; (j < 13) && (flag != 1); j++) {
        if (contaValores[j] >= 2) {
            var = j;
            flag = 1;
        }
    }
    
    return var;
}


/**
Função para determinar a maior carta de um straight flush na mão de um bot, de forma a validar para ser jogado. Esta função retorna a maior carta desse straight flush para usarmos na validação e comparação com as outras jogadas.
Esta função foi também usada para o highlight do utilizado, visto que funciona para os dois casos.
@param m A mão de um jogador.
@returns O valor da maior carta.
*/
int maior_carta_straightflush_bots (MAO m) {
    
    int x,v,i,n,j,p0,p1,p2,p3,p4;
    
    int contaValores1[14];
    int contaNaipes1[4];
    int maioresCartasStraightsPossiveis1[8];
    
    x = p0 = p1 = p2 = p3 = p4 = 0;
    
    for (i = 0; i < 14; i++) {
        contaValores1[i] = 0;
    }
    
    for (i = 0; i < 4; i++) {
        contaNaipes1[i] = 0;
    }
    
    for (i = 0; i < 8; i++) {
        maioresCartasStraightsPossiveis1[i] = -1;
    }
    
    i = 2;
    for (v = 0; v < 14; v++) {
        for(n = 0; n < 4; n++) {
            switch (v) {
                case 11: if (carta_existe(m,n,v)) { contaValores1[0]++; contaValores1[13]++; } break;
                case 12: if (carta_existe(m,n,v)) { contaValores1[1]++; } break;
                default: if (carta_existe(m,n,v)) { contaValores1[i]++; } break;
            }
        }
        i++;
    }
    
    for (v = 0; v < 14; v++) {
        for(n = 0; n < 4; n++) {
            switch(n) {
                case 0: if (carta_existe(m,n,v)) { contaNaipes1[0]++; } break;
                case 1: if (carta_existe(m,n,v)) { contaNaipes1[1]++; } break;
                case 2: if (carta_existe(m,n,v)) { contaNaipes1[2]++; } break;
                case 3: if (carta_existe(m,n,v)) { contaNaipes1[3]++; } break;
            }
        }
    }
    
    for (i = 13, j = 0; (i - 4) >= 0; --i) {
        if ((contaValores1[i] != 0) && (contaValores1[i-1] != 0) && (contaValores1[i-2] != 0) && (contaValores1[i-3] != 0) && (contaValores1[i-4] != 0)) {
            maioresCartasStraightsPossiveis1[j] = i;
            j++;
        }
    }
    
    for (i = 0; i < 8; i++) {
        if (maioresCartasStraightsPossiveis1[i] != -1) {
            x = maioresCartasStraightsPossiveis1[i];
            p0 = descodifica_straight(x);
            p1 = descodifica_straight(x-1);
            p2 = descodifica_straight(x-2);
            p3 = descodifica_straight(x-3);
            p4 = descodifica_straight(x-4);
            
            for (n = 3; n >= 0; --n) {
                if ((carta_existe(m,n,p0)) && (carta_existe(m,n,p1)) && (carta_existe(m,n,p2)) && (carta_existe(m,n,p3)) && (carta_existe(m,n,p4))) {
                    return p0;
                }
            }
        }
    }
    return -1;
}


/**
Função para determinar o naipe da maior carta de um straight flush na mão de um bot. Esta função tem o intuito de se na ultima jogada for jogado um straight flush com o mesmo valor
do que o que vai ser jogado agora, temos de analisar o naipe para ver se a jogada é válida.
Aqui é retornado o valor correspondente ao naipe da maior carta de um straight flush. Senão, é retornado -1.
@param m A mão de um jogador.
@returns O naipe correspondente à maior carta.
*/
int maior_naipeCarta_straightflush_bots (MAO m) {
    
    int v,i,n,j,x,p0,p1,p2,p3,p4;
    int contaValores[14];
    int contaNaipes[4];
    int maioresCartasStraightsPossiveis[8];
    
    x = p0 = p1 = p2 = p3 = p4 = 0;
    
    for (i = 0; i < 14; i++) {
        contaValores[i] = 0;
    }
    
    for (i = 0; i < 4; i++) {
        contaNaipes[i] = 0;
    }
    
    for (i = 0; i < 8; i++) {
        maioresCartasStraightsPossiveis[i] = -1;
    }
    
    i = 2;
    for (v = 0; v < 14; v++) {
        for(n = 0; n < 4; n++) {
            switch (v) {
                case 11: if (carta_existe(m,n,v)) { contaValores[0]++; contaValores[13]++; } break;
                case 12: if (carta_existe(m,n,v)) { contaValores[1]++; } break;
                default: if (carta_existe(m,n,v)) { contaValores[i]++; } break;
            }
        }
        i++;
    }
    
    for (v = 0; v < 14; v++) {
        for(n = 0; n < 4; n++) {
            switch(n) {
                case 0: if (carta_existe(m,n,v)) { contaNaipes[0]++; } break;
                case 1: if (carta_existe(m,n,v)) { contaNaipes[1]++; } break;
                case 2: if (carta_existe(m,n,v)) { contaNaipes[2]++; } break;
                case 3: if (carta_existe(m,n,v)) { contaNaipes[3]++; } break;
            }
        }
    }
    
    for (i = 13, j = 0; (i - 4) >= 0; --i) {
        if ((contaValores[i] != 0) && (contaValores[i-1] != 0) && (contaValores[i-2] != 0) && (contaValores[i-3] != 0) && (contaValores[i-4] != 0)) {
            maioresCartasStraightsPossiveis[j] = i;
            j++;
        }
    }
    
    for (i = 0; i < 8; i++) {
        if (maioresCartasStraightsPossiveis[i] != -1) {
            x = maioresCartasStraightsPossiveis[i];
            p0 = descodifica_straight(x);
            p1 = descodifica_straight(x-1);
            p2 = descodifica_straight(x-2);
            p3 = descodifica_straight(x-3);
            p4 = descodifica_straight(x-4);
            
            for (n = 3; n >= 0; --n) {
                if ((carta_existe(m,n,p0)) && (carta_existe(m,n,p1)) && (carta_existe(m,n,p2)) && (carta_existe(m,n,p3)) && (carta_existe(m,n,p4))) {
                    return n;
                }
            }
        }
    }
    return -1;
}

/**
Neste estado é avaliada a próxima jogada de 5 cartas do bot. Neste caso, a ultima jogada foi um straight.
Consoante essa informação, se o bot tiver um straight maior que o anterior, joga-o. Senão, procura na sua mão a menor combinação possível a ser jogada.
Se não tiver o que jogar, o bot passará a jogada.
@param e O estado do jogo.
@returns O novo estado.
*/
ESTADO fazjogada_straight (ESTADO e) {
    if (maior_carta_straight_bots(e.mao[e.actual_jogador]) > maior_carta_straight_bots(e.ultima_jogada)) {
        e = joga_straight(e);
        return e;
    }
    else {
        if ((maior_carta_straight_bots(e.mao[e.actual_jogador])) == (maior_carta_straight_bots(e.ultima_jogada)) && ((maior_naipe_straight_bots(e.mao[e.actual_jogador], (maior_carta_straight_bots(e.mao[e.actual_jogador])))) > (maior_naipe_straight_bots(e.ultima_jogada, (maior_carta_straight_bots(e.ultima_jogada)))))) {
            e = joga_straight(e);
            return e;
        }
        else{
            if (valida_flush(e.mao[e.actual_jogador]) != -1){
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
                        if (maior_carta_straightflush_bots(e.mao[e.actual_jogador]) != -1){
                            e = joga_straightflush(e);
                            return e;
                        }
                        else {
                            e.cartas_bots[e.actual_jogador] = 0;
                            e.actual_jogador = incrementa_jogador(e);
                            return e;
                        }
                    }
                }
            }
        }
    }
    return e;
}

/**
Neste estado é avaliada a próxima jogada de 5 cartas do bot. Neste caso, a ultima jogada foi um flush.
Consoante essa informação, se o bot tiver um flush maior que o anterior, joga-o. Senão, procura na sua mão a menor combinação possível a ser jogada.
Se não tiver o que jogar, o bot passará a jogada.
A função retorna sempre o estado do jogo atual.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO fazjogada_flush (ESTADO e) {

    if ((valida_flush(e.mao[e.actual_jogador])) > (valida_flush(e.ultima_jogada))) {
        e = joga_flush(e);
        return e;
    }
    else {
        if ((valida_flush(e.mao[e.actual_jogador])) == (valida_flush(e.ultima_jogada)) && ((maior_carta_flush_bots(e.mao[e.actual_jogador], (valida_flush(e.mao[e.actual_jogador])))) > (maior_carta_flush_bots(e.ultima_jogada, (valida_flush(e.ultima_jogada)))))) {
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
                    if (maior_carta_straightflush_bots(e.mao[e.actual_jogador]) != -1){
                        e = joga_straightflush(e);
                        return e;
                    }
                    else {
                        e.cartas_bots[e.actual_jogador] = 0;
                        e.actual_jogador = incrementa_jogador(e);
                        return e;
                    }
                }
            }
        }
    }
    return e;
}

/**
Neste estado é avaliada a próxima jogada de 5 cartas do bot. Neste caso, a ultima jogada foi um full house.
Consoante essa informação, se o bot tiver um full house maior que o anterior, joga-o. Senão, procura na sua mão a menor combinação possível a ser jogada.
Se não tiver o que jogar, o bot passará a jogada.
A função retorna sempre o estado do jogo atual.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO fazjogada_fullhouse (ESTADO e) {
    if (valida_fullhouse(e.mao[e.actual_jogador]) > valida_fullhouse(e.ultima_jogada) && (valida_fullhouse(e.mao[e.actual_jogador] != -1))) {
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
                e.cartas_bots[e.actual_jogador] = 0;
                e.actual_jogador = incrementa_jogador(e);
                return e;
            }
        }
    }
    return e;
}

/**
Neste estado é avaliada a próxima jogada de 5 cartas do bot. Neste caso, a ultima jogada foi um four of a kind.
Consoante essa informação, se o bot tiver um four of a kind maior que o anterior, joga-o. Senão, procura na sua mão a menor combinação possível a ser jogada.
Se não tiver o que jogar, o bot passará a jogada.
A função retorna sempre o estado do jogo atual.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO fazjogada_fourkind (ESTADO e) {
    if ((maior_carta_fourkind(e.mao[e.actual_jogador])) > (maior_carta_fourkind(e.ultima_jogada)) && (maior_carta_fourkind(e.mao[e.actual_jogador]) != -1)) {
        e = joga_fourkind(e);
        return e;
    }
    else {
        if (maior_carta_straightflush_bots(e.mao[e.actual_jogador]) != -1){
            e = joga_straightflush(e);
            return e;
        }
        else {
            e.cartas_bots[e.actual_jogador] = 0;
            e.actual_jogador = incrementa_jogador(e);
            return e;
        }
    }
    return e;
}

/**
Neste estado é avaliada a próxima jogada de 5 cartas do bot. Neste caso, a ultima jogada foi um straight flush.
Consoante essa informação, se o bot tiver um straight flush maior que o anterior, joga-o. 
Caso contrário, e como o straight flush é a maior combinação, o bot passará a jogada.
A função retorna sempre o estado do jogo atual.
@param e O estado actual.
@returns O novo estado.
*/
ESTADO fazjogada_straightflush (ESTADO e) {
    if((maior_carta_straightflush_bots(e.mao[e.actual_jogador]) == -1)) {
        e.cartas_bots[e.actual_jogador] = 0;
        e.actual_jogador = incrementa_jogador(e);
        return e;
    }
    else {
        if (codifica(maior_carta_straightflush_bots(e.mao[e.actual_jogador])) > codifica(maior_carta_straightflush_bots(e.ultima_jogada))) {
            e = joga_straightflush(e);
            return e;
        }
        else {
            if ((codifica(maior_carta_straightflush_bots(e.mao[e.actual_jogador])) == (codifica(maior_carta_straightflush_bots(e.ultima_jogada)))) && (maior_naipeCarta_straightflush_bots(e.mao[e.actual_jogador])) > (maior_naipeCarta_straightflush_bots(e.ultima_jogada))) {
                e = joga_straightflush(e);
                return e;
            }
        }
    }
    return e;
}


/**
O estado fazjogada é aquele que verifica se o bot pode jogar uma combinação de 5 cartas.
Através de todas as validações das diferentes combinações, e comparando sempre com a última jogada, o bot vê qual a próxima combinação que vai jogar (a menor possível).
Se não tiver nada válido para jogar, o bot passa a jogada.
Este estado recebe o pŕoprio estado e o int 'v', o qual representa qual a combinação a ser analisada, de forma a ver um conjunto mais amplo de jogadas possíveis.
Exemplo:
-> v = 1: Straight;
-> v = 2: Flush;
-> etc....
É retornado um novo estado, estado esse que contém todas as altereções feitas na ronda (cartas jogadas, passagem de ronda).
@param e O estado actual.
@param v O valor correspondente a uma sequência.
@returns O novo estado.
*/
ESTADO fazjogada (ESTADO e, int v) {
    if (v == 1) {
        e = fazjogada_straight(e);
        return e;
    }
    
    if (v == 2) {
        e = fazjogada_flush(e);
        return e;
    }
    
    if (v == 3) {
        e = fazjogada_fullhouse(e);
        return e;
    }
    
    if (v == 4) {
        e = fazjogada_fourkind(e);
        return e;
    }
    
    if (v == 5) {
        e = fazjogada_straightflush(e);
        return e;
    }
    
    e = passabot(e);
    return e;
}
