typedef long long int MAO;

typedef struct estado {
    MAO mao[4];
    MAO highlight;
    int cartas[4];
    int play, pass, card, ultimo_jogador;
    MAO ultima_jogada;
    int actual_jogador;
    MAO cartas_bots[4];
    int layout;
    int start;
} ESTADO;

#define SCRIPT		"http://127.0.0.1/cgi-bin/cartas"
#define BARALHO		"http://127.0.0.1/cards"
#define NAIPES		"DCHS"
#define VALORES		"3456789TJQKA2"
#define FORMATO 	"%lld_%lld_%lld_%lld_%lld_%d_%d_%d_%d_%d_%d_%d_%d_%lld_%d_%lld_%lld_%lld_%lld_%d_%d"





char* estado2str(ESTADO e);
ESTADO str2estado(char* str);
int carta_existe(long long int ESTADO, int naipe, int valor) ;
int primeiro_jogar(ESTADO e);
ESTADO baralhar ();
int indice(int naipe, int valor);
long long int add_carta(long long int ESTADO, int naipe, int valor); 
long long int rem_carta(long long int ESTADO, int naipe, int valor);
int carta_existe(long long int ESTADO, int naipe, int valor) ;
void imprime_carta(char *path, int x, int y, ESTADO e, int mao, int naipe, int valor) ;
void imprime (char *path, ESTADO e);
int numero_de_cartas(MAO m) ;
int combinacao_valida(MAO m) ; 
int compara_tamanho(MAO m1, MAO m2);
int da_valor (MAO m);
int da_maior_naipe (MAO m);
int combinacao_maior (MAO m1, MAO m2) ;
int valida_straight (MAO m) ;
int maior_carta_straight (MAO m) ;
int maior_naipe_straight (MAO m, int maiorCarta) ;
int valida_flush (MAO m) ;
int maior_carta_flush (MAO m) ;
int valida_fullhouse (MAO m) ;
int maior_carta_trio_fullhouse (MAO m) ;
int valida_fourkind (MAO m) ;
int maior_carta_fourkind (MAO m) ;
int valida_straightflush (MAO m) ;
int maior_carta_straight_bots(MAO m);
int descodifica_straight (int maiorCarta) ;
int maior_carta_straightflush_bots (MAO m) ;
int validacao_5cartas (MAO m) ;
int posso_jogar (ESTADO e) ;
int incrementa_jogador (ESTADO e);
ESTADO bots2(ESTADO e);
void imprime_botao_jogar(ESTADO e) ;
void imprime_botao_passar(ESTADO e) ;
ESTADO jogar (ESTADO e) ;
ESTADO passar (ESTADO e) ;
int valida_bots_jogadas_normais (ESTADO e, MAO m) ;
int validacao_2maos_bots (ESTADO e, MAO p) ;
ESTADO bots1(ESTADO e);
int maior_naipe_straight_bots (MAO m, int maiorCarta) ;
ESTADO joga_straight(ESTADO e) ;
ESTADO passabot(ESTADO e) ;
int maior_carta_flush_bots (MAO m, int n1) ;
ESTADO joga_flush(ESTADO e) ;
int maior_naipeCarta_straightflush_bots (MAO m) ;
int codifica(int v);
int da_carta_fourkind (MAO m) ;
int maior_carta_par_fullhouse (MAO m) ;
int valida_3cartas(MAO m);
int valida_2cartas(MAO m);
int seleciona_par_fullhouse (MAO m);
int seleciona_trio_fullhouse (MAO m);
int seleciona_maior_carta_straight_bots (MAO m);
int seleciona_maior_naipeCarta_straight_bots(MAO m, int maiorCarta);
int seleciona_maior_carta_straightflush_bots (MAO m);
ESTADO joga_straight_bot3ouros (ESTADO e);
ESTADO joga_flush_bot3ouros (ESTADO e);
ESTADO joga_fullhouse_bot3ourosPar (ESTADO e);
ESTADO joga_fullhouse_bot3ourosTrio (ESTADO e);
ESTADO joga_fourkind_bot3ouros (ESTADO e);
ESTADO joga_trio_bot3ouros (ESTADO e);
ESTADO joga_par_bot3ouros (ESTADO e);
ESTADO bot_comeca_3cartas (ESTADO e);
ESTADO bot_comeca_2cartas (ESTADO e);
ESTADO bot_comeca_1carta (ESTADO e);
ESTADO bot_comeca_jogada (ESTADO e);
ESTADO joga_bot_1carta (ESTADO e);
ESTADO joga_bot_2cartas (ESTADO e);
ESTADO joga_bot_3cartas (ESTADO e);
ESTADO joga_fullhouse(ESTADO e) ;
ESTADO joga_fourkind(ESTADO e) ;
ESTADO joga_straightflush(ESTADO e) ;
ESTADO fazjogada (ESTADO e, int v) ;
ESTADO pbot(ESTADO e);
void imprime_botao_reset(ESTADO e) ;
void imprime_botao_layout(ESTADO e) ;
void imprime_botao_trofeu(ESTADO e) ;
void imprime_botao_start(ESTADO e);
void imprime_botao_exit(ESTADO e);
ESTADO sugestao(ESTADO e);
ESTADO clear(ESTADO e);
void imprime_botao_clear(ESTADO e) ;
void imprime_botao_sugestao(ESTADO e) ;
void parse (char *query) ;