%-----------------------------------------------------------------------------
% Sistemas de Representação de Conhecimento e Raciocínio - MiEI/3ºano
% => Carlos Pedrosa - (a77320)
% => David Sousa - (a78938)
% => Manuel Sousa - (a78869)


%-----------------------------------------------------------------------------
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%-----------------------------------------------------------------------------
% SICStus PROLOG: definicoes iniciais

:- op( 900, xfy,'::' ).
:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic utente/5.
:- dynamic prestador/4.
:- dynamic cuidado/5.
:- dynamic medicamento/3.
:- dynamic receita/4.
:- dynamic imprecisoCusto/3.
:- dynamic incCusto/1.
:- dynamic nuloInterditoMor/1.
:- dynamic nuloInterditoIda/1.
:- dynamic incIdade/1.
:- dynamic (::)/2.
:- dynamic incMorada/1.
:- dynamic incEspecialidade/1.
% ----------------------------- CONHECIMENTO -------------------------------- %

% utente: #IdUt, Nome, Sexo, Idade, Morada ↝ { 𝕍,𝔽 }
utente(1, ana, 'F', 15, 'Aveiro').
utente(2, bruno, 'M', 16, 'Maia').
utente(3, catarina, 'F', 18, 'Maia').
utente(4, diogo, 'M', 18, 'Porto').
utente(5, eduardo, 'M', 19, 'Coimbra').
utente(6, filipe, 'M', 20, 'Cascais').
utente(7, hugo, 'M', 25, 'Alpendorada').
utente(8, isabel, 'F', 30, 'Magrelos').
utente(9, joao, 'M', 38, 'Magrelos').
utente(10, luis, 'M', 42, 'Santo Tirso').

% prestador: #IdPrest, Nome, Especialidade, Instituicao ↝ { 𝕍,𝔽 }
prestador(1, manuel, 'Dermatologia', 'Santa Maria').
prestador(2, nuno, 'Medicina Geral', 'Santa Maria').
prestador(3, octavio, 'Dermatologia', 'Santo Antonio').
prestador(4, pedro, 'Neurologia', 'Santa Maria').
prestador(5, rodrigo, 'Dermatologia', 'Sao Joao').
prestador(6, sara, 'Cardiologia', 'Santa Luzia').
prestador(7, tiago, 'Neurologia', 'Trofa').
prestador(8, ulisses, 'Podologia', 'Trofa').
prestador(9, vitor, 'Oftalmologista', 'Guimaraes').
prestador(10, xavier, 'Psicologia', 'Braga').

% cuidado: Data, #IdUt, #IdPrest, Descrição, Custo ↝ { 𝕍,𝔽 }
cuidado((2018,3,8), 1, 1, 'Remocao de um pequeno nodo', 2500).
cuidado((2018,3,8), 1, 2, 'Consulta de Rotina', 30).
cuidado((2018,3,10), 2, 3, 'Incisao nas costas sobre um furunculo', 150).
cuidado((2018,3,10), 3, 3, 'Remocao de pontos negros', 20).
cuidado((2018,3,10), 4, 3, 'Tratamento de alergia no joelho direito', 30).


% ------------------------- CONHECIMENTO NEGATIVO ---------------------------- %

% Não pode ser um utente se for um prestador
% utente : N -> {V,F,D}
-utente(_, Nome, Sexo, Idade, Morada) :- prestador(ID, Nome, Especialidade, Instituicao).

% prestador : N -> {V,F,D}
-prestador(_, Nome, Especialidade, Instituicao) :- utente(ID, Nome, Sexo, Idade, Morada).

% É falsa a informação de que certos utentes/prestador tenham estado/trabalhem no centro de Saúde
-utente(11, miguel, 'M', 22, 'Mafra').
-utente(12, marta, 'F', 33, 'Oeiras').
-utente(13, fatima, 'F', 32, 'Faro').

-prestador(11, veronica, 'Imunoalergologia', 'Braganca').
-prestador(12, nicole, 'Ortopedia', 'Barcelos').
-prestador(13, judite, 'Pneumologia', 'Alentejo').

% O centro de saúde não funcionou no dia 27/03/2018 porque esteve fechado por causa da greve:
%     - É falsa a existência de cuidados para esse dia
-cuidado((2018,3,27), _, _, _, _).

% ------------------------ CONHECIMENTO IMPERFEITO --------------------------- %

% --------- CONHECIMENTO INCERTO

% Adoção do pressuposto do domínio fechado -> Utente
-utente(ID, Nome, Sexo, Idade, Morada) :- nao(utente(ID, Nome, Sexo, Idade, Morada)),
                                          nao(excecao(utente(ID, Nome, Sexo, Idade, Morada))).

% Deu entrada no centro de saúde um utente com idade desconhecida
utente(14, margarida, 'F', incIdade1, 'Famalicao').
excecao(utente(ID, Nome, Sexo, Idade, Morada)) :- utente(ID, Nome, Sexo, incIdade1, Morada).
incIdade(incIdade1).

% Deu entrada no centro de saúde um utende com morada desconhecida
utente(15, patricia, 'F', 18, incMorada1).
excecao(utente(ID, Nome, Sexo, Idade, Morada)) :- utente(ID, Nome, Sexo, Idade, incMorada1).
incMorada(incMorada1).

% Adoção do pressuposto do domínio fechado -> Prestador
-prestador(ID, Nome, Especialidade, Instituicao) :- nao(prestador(ID, Nome, Especialidade, Instituicao)),
                                                    nao(excecao(prestador(ID, Nome, Especialidade, Instituicao))).

% Foi contratado um prestador em formação pelo que este ainda não possui especialidade
prestador(14, alberto, incEspecialidade1, 'Tomar').
excecao(prestador(ID, Nome, Especialidade, Instituicao)) :- prestador(ID, Nome, incEspecialidade1, Instituicao).
incEspecialidade(incEspecialidade1).
% Adoção do pressuposto do dominio fechado -> Cuidado
-cuidado(Data, IdUt, IdPrest, Descricao, Custo) :- nao(cuidado(Data, IdUt, IdPrest, Descricao, Custo)),
                                                   nao(excecao(cuidado(Data, IdUt, IdPrest, Descricao, Custo))).


% O cuidado tem um valor desconhecido
cuidado((2018,3,28), 5, 3, 'Tratamento de alergia no joelho esquerdo', incValor1).
excecao(cuidado(Data, IdUt, IdPrest, Descricao, Custo)) :- cuidado(Data, IdUt, IdPrest, Descricao, incValor1).
incCusto(incValor1).


% --------- CONHECIMENTO IMPRECISO

% O valor do cuidado X encontra-se entre 50 e 60 euros
excecao(cuidado((2018,3,29), 7, 6, 'Recolha de um eletrocardiograma', X)) :- X > 50, X < 60.
imprecisoCusto((2018,3,29),7,6).

% --------- CONHECIMENTO INTERDITO

% Por questões de confidencialidade não se pode saber:
%         - idade
%         - morada
nuloInterditoIda(nuloIdade1).
excecao(utente(ID, Nome, Sexo, Idade, Morada)) :- utente(ID, Nome, Sexo, nuloIdade1, Morada). % Não esquecer de adicionar invariante!
utente(16, guilherme, 'M', nuloIdade1, 'Viana de Castelo').
+utente(Id,No,Se,Ida,Mo) :: (findall( (Id,No,Se,I,Mo), (utente(16,guilherme,'M',I,'Viana de Castelo'),nao(nuloInterditoIda(I))), S),
                            comprimento(S,N),
                            N==0).

nuloInterditoMor(nuloMorada1).
excecao(utente(ID, Nome, Sexo, Idade, Morada)) :- utente(ID, Nome, Sexo, Idade, nuloMorada1). % Não esquecer de adicionar invariante!
utente(17, monica, 'F', 19, nuloMorada1).
+utente(Id,No,Se,Ida,Mo) :: (findall( (Id,No,Se,I,Mo), (utente(17,monica,'F',19,M),nao(nuloInterditoMor(M))), S),
                            comprimento(S,N),
                            N==0).



% ---------------------------------- Invariantes ------------------------------- %
% (A utilização deste invariante já impede a adição de outro tipo de conhecimentos à base ) Funciona apenas na evolucao propriamente dita.
% Não podemos adicionar conhecimento perfeito com ID repetido quando já existe conhecimento perfeito
+utente( ID, _, _, _, _ ) :: ( integer(ID),
                               findall( ID, (utente(ID, Nome, Sexo, Idade, Morada), nao(incIdade(Idade)),nao(incMorada(Morada)), nao(nuloInterditoIda(Idade)) , nao(nuloInterditoMor(Morada))  ), S),
                                comprimento( S, N ), N == 1 ).

% Adicionar idade certa quando já existe incerta
+utente( ID, _, _, Idade, _ ) :: ( nao(incIdade(Idade)), integer(ID),
                               findall( ID, (utente(ID, Nome, Sexo, IdadeX, Morada), incIdade(IdadeX),nao(incMorada(Morada)), nao(nuloInterditoIda(IdadeX)) , nao(nuloInterditoMor(Morada))  ), S),
                               comprimento( S, N ), N == 0 ).

% Adicionar morada certa quando já existe incerta
+utente( ID, _, _, _, Morada ) :: (  nao(incMorada(Morada)), integer(ID),
                                findall( ID, (utente(ID, Nome, Sexo, Idade, MoradaX), nao(incIdade(Idade)),(incMorada(MoradaX)), nao(nuloInterditoIda(Idade)) , nao(nuloInterditoMor(MoradaX))  ), S),
                                comprimento( S, N ), N == 0 ).

% Invariante Estrutural: Não podem existir Prestadores Repetidos (com o mesmo ID).
% Não podemos adicionar conhecimento perfeito com ID repetido quando já existe conhecimento perfeito
+prestador( ID, _, _, _ ) :: ( integer(ID),
                               findall(ID, (prestador(ID, Nome, Especialidade, Instituicao), nao(incEspecialidade(Especialidade))), S),
                                comprimento( S, N ), N == 1 ).

+prestador( ID, _, Especialidade, _ ) :: ( nao(incEspecialidade(Especialidade)), integer(ID),
                               findall(ID, (prestador(ID, Nome, EspecialidadeX, Instituicao), (incEspecialidade(EspecialidadeX))), S),
                               comprimento( S, N ), N == 0 ).

% Invariante Estrutural: Não podem existir Cuidados Repetidos (com a mesma informação).
% Não podemos adicionar conhecimento perfeito com data,IdUt,IdPrest repetido quando já existe conhecimento perfeito
+cuidado( Data, IdUt, IdPrest, _ , _ ) :: ( findall((Data,IdUt,IdPrest), ( cuidado(Data, IdUt, IdPrest, Descricao, Custo), nao(incCusto(Custo)), nao(imprecisoCusto(Data,IdUt,IdPrest))) , S),
                                            comprimento( S, N ), N == 1 ).




+cuidado( Data, IdUt, IdPrest, _ , Custo ) :: (nao(incCusto(Custo)),
                                              findall((Data,IdUt,IdPrest), ( cuidado(Data, IdUt, IdPrest, Descricao, CustoX), (incCusto(CustoX)), nao(imprecisoCusto(Data,IdUt,IdPrest))) , S),
                                              comprimento( S, N ), N == 0 ).

+cuidado( Data, IdUt, IdPrest, _ , Custo ) :: (nao(imprecisoCusto(Data,IdUt,IdPrest)),
                                              findall((Data,IdUt,IdPrest), ( cuidado(Data, IdUt, IdPrest, Descricao, CustoX), nao(incCusto(CustoX)), (imprecisoCusto(Data,IdUt,IdPrest))) , S),
                                              comprimento( S, N ), N == 0 ).

+(-cuidado(Data,IdUt,IdPrest,_,_)) :: nao(imprecisoCusto(Data,IdUt,IdPrest)).


% Invariante que impede a inserção de conhecimento contraditório
+(-utente(ID,Nome,Sexo,Idade,Morada)) :: (findall( (ID), utente(ID,NomeX,SexoX,IdadeX,MoradaX),S),
                                           comprimento(S,N), N == 0).

+(-prestador(ID, Nome, Especialidade, Instituicao)) :: (findall( (ID), prestador(ID, NomeX, EspecialidadeX, InstituicaoX),S),
                                           comprimento(S,N), N == 0).

+(-cuidado( Data, IdUt, IdPrest, Descricao, Custo )) :: (findall( (Data,IdUt,IdPrest,Descricao,Custo), cuidado( Data, IdUt, IdPrest, DescricaoX, CustoX ),S),
                                           comprimento(S,N), N == 0).

% Não se pode adicionar um excecao se for para conhecimento perfeito.
+excecao(T) :: nao(T).

% -------------------------- Predicados Auxiliares -------------------------- %

% Extensão do Predicado comprimento: ListaElem, Comp -> {V,F}
comprimento([],0).
comprimento([X|L], C) :- comprimento(L, N), C is 1+N.

% Invariante que resulta da adição de um custo incerto quando já existe imprecisos
% Extensão do Predicado existeCustoImpreciso: Data IdUt,IdPrest,Descricao,Custo -> {V,F}
existeCustoImpreciso(Data, IdUt, IdPrest, Descricao, Custo) :-  (findall((D,IdU,IdP), (imprecisoCusto(Data,IdUt,IdPrest)), L ),
                                                                comprimento(L,N),
                                                                N==0).

% Só pode existir um incerto para um determinado idade porque não vamos permitir a inserção de conhecimento repetido
existeIdadeIncerto(utente(ID,Nome, Sexo, Idade, Morada)) :- (findall( (Idade), (utente(ID,NomeX, SexoX, IdadeX, MoradaX), incIdade(IdadeX)),L),
                                                              comprimento(L,N),
                                                              N==0).

% Só pode existir um incerto para um determinada morada, porque não vamos permitir a inserção de conhecimento repetido
existeMoradaIncerto(utente(ID,Nome, Sexo, Idade, Morada)) :- (findall( (MoradaX), (utente(ID,NomeX, SexoX, IdadeX, MoradaX), incMorada(MoradaX)),L),
                                                              comprimento(L,N),
                                                              N==0).

% Só pode existir um incerto para um determinado valor, porque não vamos permitir a inserção de conhecimento repetido
existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo)) :- (findall(Custo,(cuidado(Data, IdUt,IdPrest, Descricao, CustoX), incCusto(CustoX)),L  ),
                                                                     comprimento(L,N),
                                                                     N==0).

% Só pode existir um incerto para um determinado valor, porque não vamos permitir a inserção de conhecimento repetido
existeEspecIncerto(prestador(ID, Nome, Especialidade, Instituicao)) :- (findall(Especialidade, (prestador(ID, NomeX, EspecialidadeX, InstituicaoX), incEspecialidade(EspecialidadeX)),L),
                                                                        comprimento(L,N),
                                                                        N==0).

% Extensão do Predicado exosteIdadeIncerto2: PredicadoUtente, Lista -> {V,F}
existeIdadeIncerto2(utente(ID,Nome, Sexo, Idade, Morada),L) :- (findall( (IdadeX ), (utente(ID,NomeX, SexoX, IdadeX, MoradaX), incIdade(IdadeX)),[L|Ls])).
existeMoradaIncerto2(utente(ID,Nome, Sexo, Idade, Morada),L) :- (findall( (MoradaX ), (utente(ID,NomeX, SexoX, IdadeX, MoradaX), incMorada(MoradaX)),[L|Ls])).
existeEspecIncerto2(prestador(ID, Nome, Especialidade, Instituicao),L) :- (findall( (EspecialidadeX), (prestador(ID,NomeX,EspecialidadeX,InstituicaoX), incEspecialidade(EspecialidadeX)),[L|Ls])).
existeCustoIncerto2(cuidado(Data, IdUt,IdPrest, Descricao, Custo), L) :- (findall( (CustoX), (cuidado(Data, IdUt,IdPrest, DescricaoX, CustoX), incCusto(CustoX)),[L|Ls])).

% --------------------------  EVOLUÇÃO E INVOLUÇÃO -----------------------------

% Evolução do Conhecimento Perfeito
% Extensão do Predicado evolucaoPerfeitoIdade: PredicadoUtente -> {V,F}
evolucaoPerfeitoIdade(utente(ID,Nome,Sexo,Idade,Morada)) :-
                                nao(existeIdadeIncerto(utente(ID,Nome,Sexo,Idade,Morada))),
                                existeIdadeIncerto2(utente(ID,Nome,Sexo,Idade,Morada),L),
                                involucaoIdInc(utente(ID,Nome,Sexo,L,Morada)),
                                evolucao(utente(ID,Nome,Sexo,Idade,Morada)).

evolucaoPerfeitoIdade(utente(ID,Nome,Sexo,Idade,Morada)) :-
                            existeIdadeIncerto(utente(ID,Nome,Sexo,Idade,Morada)),
                            evolucao(utente(ID,Nome,Sexo,Idade,Morada)).


evolucaoPerfeitoMorada(utente(ID,Nome,Sexo,Idade,Morada)) :-
                              nao(existeMoradaIncerto(utente(ID,Nome,Sexo,Idade,Morada))),
                              existeMoradaIncerto2(utente(ID,Nome,Sexo,Idade,Morada),L),
                              involucaoMoradaInc(utente(ID,Nome,Sexo,Idade,L)),
                              evolucao(utente(ID,Nome,Sexo,Idade,Morada)).

evolucaoPerfeitoMorada(utente(ID,Nome,Sexo,Idade,Morada)) :-
                              existeMoradaIncerto(utente(ID,Nome,Sexo,Idade,Morada)),
                              evolucao(utente(ID,Nome,Sexo,Idade,Morada)).

evolucaoPerfeitoEspec(prestador(ID, Nome, Especialidade, Instituicao)) :-
                              nao(existeEspecIncerto(prestador(ID, Nome, Especialidade, Instituicao))),
                              existeEspecIncerto2(prestador(ID, Nome, Especialidade, Instituicao),L),
                              involucaoEspecInc(prestador(ID, Nome, L, Instituicao)),
                              evolucao(prestador(ID, Nome, Especialidade, Instituicao)).

evolucaoPerfeitoEspec(prestador(ID,Nome,Especialidade,Instituicao)) :-
                              existeEspecIncerto(prestador(ID, Nome, Especialidade, Instituicao)),
                              evolucao(prestador(ID, Nome, Especialidade, Instituicao)).

evolucaoPerfeitoCusto(cuidado(Data, IdUt,IdPrest, Descricao, Custo)) :-
                              nao(existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo))),
                              existeCustoIncerto2(cuidado(Data, IdUt,IdPrest, Descricao, Custo),L),
                              involucaoCustInc(cuidado(Data, IdUt,IdPrest, Descricao, L)),
                              evolucao(cuidado(Data, IdUt,IdPrest, Descricao, Custo)).

evolucaoPerfeitoCusto(cuidado(Data, IdUt,IdPrest, Descricao, Custo)) :-
                              existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo)),
                              evolucao(cuidado(Data, IdUt,IdPrest, Descricao, Custo)).

% Evolução do CONHECIMENTO Incerto
% -------------------------- UTENTE
% Idade
% Extensão do Predicado evolucaoIdInc: PredicadoUtente -> {V,F}
evolucaoIdInc(utente(ID,Nome,Sexo,Idade,Morada)) :-
        evolucao(utente(ID,Nome,Sexo,Idade,Morada)),
        assert( ( (excecao(utente(Id,N,S,I,M))) :- utente(Id,N,S,Idade,M) ) ),
        assert( incIdade(Idade)).

involucaoIdInc(utente(ID,Nome,Sexo,Idade,Morada)) :-
          involucao(utente(ID,Nome,Sexo,Idade,Morada)),
          retract( ( (excecao(utente(Id,N,S,I,M))) :- utente(Id,N,S,Idade,M) ) ),
          retract( incIdade(Idade)).

% Morada
evolucaoMoradaInc(utente(ID,Nome,Sexo,Idade,Morada)) :-
        evolucao(utente(ID,Nome,Sexo,Idade,Morada)),
        assert( ( (excecao(utente(Id,N,S,I,M))) :- utente(Id,N,S,I,Morada) ) ),
        assert( incMorada(Morada)).

involucaoMoradaInc(utente(ID,Nome,Sexo,Idade,Morada)) :-
          involucao(utente(ID,Nome,Sexo,Idade,Morada)),
          retract( ( (excecao(utente(Id,N,S,I,M))) :- utente(Id,N,S,I,Morada) ) ),
          retract( incMorada(Morada)).

% -------------------------- Prestador
% Especialidade
% Extensão do Predicado evolucaoEspecInc: PredicadoPrest -> {V,F}
evolucaoEspecInc(prestador(ID,Nome,Especialidade,Instituicao)) :-
          evolucao(prestador(ID,Nome,Especialidade,Instituicao)),
          assert( ( (excecao(prestador(Id,N,E,I))) :- prestador(Id,N,Especialidade,I) ) ),
          assert( incEspecialidade(Especialidade)).

involucaoEspecInc(prestador(ID,Nome,Especialidade,Instituicao)) :-
            involucao(prestador(ID,Nome,Especialidade,Instituicao)),
            retract( ( (excecao(prestador(Id,N,E,I))) :- prestador(Id,N,Especialidade,I) ) ),
            retract( incEspecialidade(Especialidade)).

% -------------------------- Cuidado
% Preço
% Extensão do Predicado evolucaoCustInc: PredicadoCuidado  -> {V,F}
evolucaoCustInc(cuidado(Data, IdUt, IdPrest, Descricao, Custo)) :-
            existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
            evolucao(cuidado(Data,IdUt,IdPrest,Descricao,Custo)),
            assert( ( ( excecao(cuidado(D, IdU, IdP, Des, C))) :- cuidado(D, IdU, IdP, Des, Custo))),
            assert(incCusto(Custo)).

involucaoCustInc(cuidado(Data, IdUt, IdPrest, Descricao, Custo)) :-
            involucao(cuidado(Data,IdUt,IdPrest,Descricao,Custo)),
            retract( ( ( excecao(cuidado(D, IdU, IdP, Des, C))) :- cuidado(D, IdU, IdP, Des, Custo))),
            retract(incCusto(Custo)).




% Evolução do CONHECIMENTO IMPRECISO
% [Auxiliares]
% Extensão do Predicado auxImpEvolucao: PredicadoCuidado, L -> {V,F}
auxImpEvolucao(cuidado(D,IDu,IDp,Des,Cust),[Q|[]]) :- evolucao(excecao(cuidado(D,IDu,IDp,Des,Q))).
auxImpEvolucao(cuidado(D,IDu,IDp,Des,Cust),[Q|Qs]) :- evolucao(excecao(cuidado(D,IDu,IDp,Des,Q))),
                                                      auxImpEvolucao(cuidado(D,IDu,IDp,Des,Cust),Qs).

auxImpInvolucao(cuidado(D,IDu,IDp,Des,Cust),[Q|[]]) :- involucao(excecao(cuidado(D,IDu,IDp,Des,Q))).
auxImpInvolucao(cuidado(D,IDu,IDp,Des,Cust),[Q|Qs]) :- involucao(excecao(cuidado(D,IDu,IDp,Des,Q))),
                                                       auxImpInvolucao(cuidado(D,IDu,IDp,Des,Cust),Qs).



% Custo [Lista]
% Extensão do Predicado evolucaoCustImp: PredicadoCuidado, L,lista -> {V,F}
evolucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), L,lista) :-
              existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
              nao(existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo))),
              existeCustoIncerto2(cuidado(Data, IdUt,IdPrest, Descricao, Custo),Aux),
              involucaoCustInc(cuidado(Data,IdUt, IdPrest, Descricao, Aux)),
              auxImpEvolucao(cuidado(Data,IdUt, IdPrest, Descricao, Custo), L),
              assert(imprecisoCusto(Data,IdUt,IdPrest)).

evolucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), L,lista) :-
                existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
                existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo)),
                auxImpEvolucao(cuidado(Data,IdUt, IdPrest, Descricao, Custo), L),
                assert(imprecisoCusto(Data,IdUt,IdPrest)).


involucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), L,lista) :-
                auxImpInvolucao(cuidado(Data,IdUt, IdPrest, Descricao, Custo), L),
                retract(imprecisoCusto(Data,IdUt,IdPrest)).

% Custo [Menor,Maior]
evolucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Menor,Maior,entre) :-
              existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
              findall(I,+(excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)))::I,Li),
              teste(Li),
              nao(existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo))),
              existeCustoIncerto2(cuidado(Data, IdUt,IdPrest, Descricao, Custo),Aux),
              involucaoCustInc(cuidado(Data,IdUt, IdPrest, Descricao, Aux)),
              assert( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo>Menor, Custo<Maior )),
              assert(imprecisoCusto(Data,IdUt,IdPrest)).

evolucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Menor,Maior,entre) :-
                existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
                findall(I,+(excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)))::I,Li),
                teste(Li),
                existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo)),
                assert( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo>Menor, Custo<Maior )),
                assert(imprecisoCusto(Data,IdUt,IdPrest)).



  involucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Menor,Maior,entre) :-
                findall(I,-(excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)))::I,Li),
                teste(Li),
                retract( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo>Menor, Custo<Maior )),
                retract(imprecisoCusto(Data,IdUt,IdPrest)).

% Custo [A partir de]
evolucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Menor,aPartir) :-
              existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
              findall(I,+(excecao(cuidado(Data,IdUt,IdPrest,Descricao,Custo)))::I,Li),
              teste(Li),
              nao(existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo))),
              existeCustoIncerto2(cuidado(Data, IdUt,IdPrest, Descricao, Custo),Aux),
              involucaoCustInc(cuidado(Data,IdUt, IdPrest, Descricao, Aux)),
              assert( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo>Menor )),
              assert(imprecisoCusto(Data,IdUt,IdPrest)).


evolucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Menor,aPartir) :-
                existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
                findall(I,+(excecao(cuidado(Data,IdUt,IdPrest,Descricao,Custo)))::I,Li),
                teste(Li),
                existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo)),
                assert( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo>Menor )),
                assert(imprecisoCusto(Data,IdUt,IdPrest)).



involucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Menor,aPartir) :-
              findall(I,-(excecao(cuidado(Data,IdUt,IdPrest,Descricao,Custo)))::I,Li),
              teste(Li),
              retract( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo>Menor )),
              retract(imprecisoCusto(Data,IdUt,IdPrest)).

% Custo [Até]
evolucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Maior,ate) :-
              existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
              findall(I,+(excecao(cuidado(Data,IdUt,IdPrest,Descricao,Custo)))::I,Li),
              teste(Li),
              nao(existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo))),
              existeCustoIncerto2(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Aux),
              involucaoCustInc(cuidado(Data,IdUt, IdPrest, Descricao, Aux)),
              assert( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo<Maior )),
              assert(imprecisoCusto(Data,IdUt,IdPrest)).


evolucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Maior,ate) :-
                existeCustoImpreciso(Data,IdUt,IdPrest,Descricao,Custo),
                findall(I,+(excecao(cuidado(Data,IdUt,IdPrest,Descricao,Custo)))::I,Li),
                teste(Li),
                existeCustoIncerto(cuidado(Data, IdUt,IdPrest, Descricao, Custo)),
                assert( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo<Maior )),
                assert(imprecisoCusto(Data,IdUt,IdPrest)).

involucaoCustImp(cuidado(Data, IdUt,IdPrest, Descricao, Custo), Maior,ate) :-
              findall(I,-(excecao(cuidado(Data,IdUt,IdPrest,Descricao,Custo)))::I,Li),
              teste(Li),
              retract( (excecao(cuidado(Data,IdUt, IdPrest, Descricao, Custo)) :- Custo<Maior )),
              retract(imprecisoCusto(Data,IdUt,IdPrest)).



% Evolução do CONHECIMENTO INTERDITO
% Idade
% Extensão do Predicado evolucaoIdadeInt : PredicadoUtente -> {V,F}
evolucaoIdadeInt(utente(ID,Nome, Sexo, Idade, Morada)) :-
                    existeIdadeIncerto(utente(ID,Nome, Sexo, Idade, Morada)),
                    evolucao( (utente(ID,Nome,Sexo,Idade,Morada)) ),
                    assert( (excecao(utente(I,N,S,I,M)) :- utente(I,N,S,Idade,M)) ),
                    assert(nuloInterditoIda(Idade)),
                    assert( (+utente(Id,No,Se,Ida,Mo) :: (findall( (Id,No,Se,I,Mo), (utente(ID,Nome,Sexo,I,Morada),nao(nuloInterditoIda(I))), S),
                            comprimento(S,N),
                            N==0))
                            ).


% Morada
evolucaoMoradaInt(utente(ID,Nome, Sexo, Idade, Morada)) :-
                    existeMoradaIncerto(utente(ID,Nome, Sexo, Idade, Morada)),
                    evolucao( (utente(ID,Nome,Sexo,Idade,Morada)) ),
                    assert( (excecao(utente(Id,N,S,I,M)) :- utente(Id,N,S,I,Morada)) ),
                    assert( (nuloInterditoMor(Morada))),
                    assert( (+utente(Id,No,Se,Ida,Mo) :: (findall( (Id,No,Se,Ida,M), (utente(ID,Nome,Sexo,Idade,M),nao(nuloInterditoIda(M))), S),
                            comprimento(S,N),
                            N==0))
                            ).

% Evolução do CONHECIMENTO PERFEITO POSITIVO
% insercao: T -> {V,F}
insercao(T) :- assert(T).
insercao(T) :- retract(T), !, fail.

% remocao: T -> {V,F}
remocao(T) :- retract(T).
remocao(T) :- assert(T), !, fail.

% teste: L -> {V,F}
teste( [] ).
teste( [I|Is] ) :- I, teste(Is).

% evolucao: T -> {V,F}
evolucao(T) :- findall(I,+T::I,Li),
		           insercao(T),
		           teste(Li).

% involucao: T -> {V,F}
involucao(T) :- T,
               findall(I,-T::I,Li),
               remocao(T),
               teste(Li).


% Evolução do CONHECIMENTO PERFEITO NEGATIVO
evolucaoNeg(T) :- findall(I,+(-T)::I,Li),
                  teste(Li),
                  assert(-T).

involucaoNeg(T) :- findall(I,+(-T)::I,Li),
                   teste(Li),
                   retract(-T).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
demo( Questao,verdadeiro ) :-
          Questao.
demo( Questao,falso ) :-
          -Questao.
demo( Questao,desconhecido ) :-
          nao( Questao ),
          nao( -Questao ).

% Extensao do predicado conjuncao: Q1, L2, R -> {V,F}
conjuncaoDesc(desconhecido, verdadeiro, desconhecido).
conjuncaoDesc(desconhecido, desconhecido, desconhecido).
conjuncaoDesc(desconhecido, falso, falso).
conjuncaoDesc(verdadeiro,verdadeiro,verdadeiro).
conjuncaoDesc(verdadeiro,desconhecido,desconhecido).
conjuncaoDesc(verdadeiro,falso,falso).
conjuncaoDesc(falso,verdadeiro,falso).
conjuncaoDesc(falso,desconhecido,falso).
conjuncaoDesc(falso,falso,falso).

% Extensao do meta-predicado demoLista : Questoes, Resposta -> {V,F}
demoListaC([Q|[]], R) :- demo(Q,R).
demoListaC([Q|Qs],R) :-  demo(Q,R1),
                         demoListaC(Qs,R2),
                         conjuncaoDesc(R1,R2,R).

demoListaD([Q|[]], R) :- demo(Q,R).
demoListaD([Q|Qs],R)  :- demo(Q,R1),
                         demoListaD(Qs,R2),
                         disjuDesc(R1,R2,R).

demoFinal([Q],R) :- demo(Q,R).
demoFinal([Q, e | Ls], R) :- demo(Q,R1),
                             demoFinal(Ls,R2),
                             conjuncaoDesc(R1,R2,R).

demoFinal([Q],R) :- demo(Q,R).
demoFinal([Q, ou | Ls], R) :- demo(Q,R1),
                              demoFinal(Ls,R2),
                              disjuDesc(R1,R2,R).

disjuDesc(verdadeiro, verdadeiro, verdadeiro).
disjuDesc(verdadeiro,desconhecido,verdadeiro).
disjuDesc(verdadeiro,falso,verdadeiro).
disjuDesc(desconhecido,verdadeiro,verdadeiro).
disjuDesc(desconhecido, desconhecido, desconhecido).
disjuDesc(desconhecido, falso, desconhecido).
disjuDesc(falso,verdadeiro,falso).
disjuDesc(falso,desconhecido,desconhecido).
disjuDesc(falso,falso,falso).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :-
      Questao, !, fail.
nao( Questao ).
