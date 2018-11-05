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
:- dynamic utente/5.
:- dynamic prestador/4.
:- dynamic cuidado/5.
:- dynamic medicamento/3.
:- dynamic receita/4.

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


% ------------------------- Construção do Caso Prático ---------------------- %
%
% ------------------------------------ 1 ------------------------------------ %
%
% Registar utentes, prestadores e cuidados de saúde;

% Invariante Estrutural: Não podem existir Utentes Repetidos (com o mesmo ID).
+utente( ID, _, _, _, _ ) :: ( integer(ID),
                               findall(ID, utente(ID, Nome, Sexo, Idade, Morada), S),
                               comprimento( S, N ), N == 1 ).

% Invariante Referencial: Limite para a idade entre 0 e 130 anos.
+utente( _, _, _, Idade, _) :: ( integer(Idade),
                                 Idade >= 0, Idade =< 130 ).

% Invariante Referencial: Só é possível adicionar utentes cujo sexo seja válido.
% Uso do predicado auxiliar sexoValido: S -> {V, F}
+utente( _, _, Sexo, _, _ ) :: sexoValido(Sexo).

sexoValido(S) :- S == 'M'.
sexoValido(S) :- S == 'F'.


% Invariante Estrutural: Não podem existir Prestadores Repetidos (com o mesmo ID).
+prestador( ID, _, _, _ ) :: ( integer(ID),
                               findall(ID, prestador(ID, Nome, Especialidade, Instituicao), S),
                               comprimento( S, N ), N == 1 ).

% Invariante Estrutural: Não podem existir Cuidados Repetidos (com a mesma informação).
+cuidado( Data, IdUt, IdPrest, Descricao, Custo ) :: ( findall((Data, IdUt, IdPrest, Descricao, Custo), cuidado(Data, IdUt, IdPrest, Descricao, Custo), S),
                                                       comprimento( S, N ), N == 1 ).

% Invariante Referencial: Só se pode adicionar um cuidado se o utente e o cuidador nesse cuidado existir.
+cuidado( _, IdUt, IdPrest, _, _ ) :: ( findall( IdUt, utente(IdUt, Nome, Sexo, Idade, Morada), L), comprimento(L,S), S == 1,
                                        findall( IdPrest, prestador(IdPrest, Nome, Especialidade, Instituicao), X), comprimento(X,C), C == 1 ).

%
% ---------------------------------- 2 -------------------------------------- %
%
% Remover utentes, prestadores e cuidados de saúde;

% Invariante Estrutural: Só se pode eliminar se existir o ID existir na Base de Conhecimento.
-utente( IdUt, _, _, _, _ ) :: ( findall( IdUt, utente(IdUt, Nome, Sexo, Idade, Morada), S),
                                 comprimento( S, N ),
                                 N == 0 ).

% Invariante Referencial: Só se pode eliminar utente se não existirem cuidados por ele sofridos, i.e, se n existirem prestadores que lhe realizaram cuidados.
-utente( IdUt, _, _, _, _ ) :: ( findall( IdPrest, cuidado(Data, IdUt, IdPrest, Descricao, Custo), L),
                                 comprimento( L, N),
                                 N == 0 ).

% Invariante Estrutural: Só se pode eliminar se o ID existir na Base de Conhecimento.
-prestador( IdPrest, _, _, _ ) :: ( findall(IdPrest, prestador(IdPrest, Nome, Especialidade, Instituicao), S),
                                    comprimento( S, N ),
                                    N == 0 ).

% Invariante Referencial: Só se pode eliminar prestador se não existirem cuidados por ele prestados, i.e, se n existirem utentes que tivessem sido atendidos por ele.
-prestador( IdPrest, _, _, _ ) :: ( findall( IdUt, cuidado(Data, IdUt, IdPrest, Descricao, Custo), L),
                                    comprimento( L, N),
                                    N == 0 ).

%
% ----------------------------------- 3 ------------------------------------- %
%
% Identificar utentes por critérios de seleção;
% ID
% Nome
% Sexo
% Idade
% Morada

% Extensão do Predicado idUtenteID: ID, L -> {V,F}
idUtenteID(ID, L) :- findall((Nome, Sexo, Idade, Morada), utente(ID, Nome, Sexo, Idade, Morada), L).

% Extensão do Predicado idUtenteNome: Nome, L -> {V,F}
idUtenteNome(Nome, L) :- findall((ID, Sexo, Idade, Morada), utente(ID, Nome, Sexo, Idade, Morada), L).

% Extensão do Predicado idUtenteSexo: Sexo, L -> {V,F}
idUtenteSexo(Sexo, L) :- findall((ID, Nome, Idade, Morada), utente(ID, Nome, Sexo, Idade, Morada), L).

% Extensão do Predicado idUtenteIdade: Idade, L -> {V,F}
idUtenteIdade(Idade, L) :- findall((ID, Nome, Sexo, Morada), utente(ID, Nome, Sexo, Idade, Morada), L).

% Extensão do Predicado idUtenteMorada: Morada, L -> {V,F}
idUtenteMorada(Morada, L) :- findall((ID, Nome, Sexo, Idade), utente(ID, Nome, Sexo, Idade, Morada), L).

%
% ----------------------------------- 4 ------------------------------------- %
%
% Identificar Instituições Prestadoras de Cuidados de Saúde

% Extensão do Predicado daInstCui: ListaInst -> {V,F}
daInstCui(L) :- findall(Instituicao, prestador(ID, Nome, Especialidade, Instituicao), X),
                apagaReps(X,L).
%
% ----------------------------------- 5 ------------------------------------ %
%
% Identificar cuidados de saúde prestados por Instituicao/cidade/datas;

      % --------------  Instituicao -------------- %

% Extensão do Predicado daCuiPorInst: Instituicao, ListaCuidados -> {V,F}
daCuiPorInst(Instituicao, L) :- daPrestadorPorInst(Instituicao, X),
                                daCuiPorPres(X, C),
                                concListList(C, L).

% Extensão do Predicado daPrestadorPorInst: Instituicao, ListaPrestador -> {V,F}
% Dá a lista de Prestadores de Uma Instituicao.
daPrestadorPorInst(Instituicao, L) :- findall( IdPrest, prestador(IdPrest, Nome, Especialidade, Instituicao), L).

% Extensão do Predicado daCuiPorPres: ListaPrestador, ListaCuidados -> {V,F}
% Dá a lista de listas com todos os cuidados de todos os prestadores na lista inicial.
daCuiPorPres([],[]).
daCuiPorPres([IdPrest|T], [H|C]) :- findall( (Data, IdUt, IdPrest, Descricao, Custo), cuidado(Data, IdUt, IdPrest, Descricao, Custo), H),
                                    daCuiPorPres(T,C).

      % --------------  Cidade -------------- %
% Extensão do Predicado daCuiPorCidade: Cidade, ListaCuidados -> {V,F}
daCuiPorCidade(Cidade, L) :- daUtentesPorCidade(Cidade, X),
                             daCuidadosPorUtentes(X,C),
                             concListList(C,L).

% Extensão do Predicado daUtentesPorCidade: Cidade, ListaUtentes -> {V,F}
% Dá os utentes que moram na Cidade
daUtentesPorCidade(Cidade, L) :- findall(IdUt, utente(IdUt, Nome, Sexo, Idade, Cidade), L).

% Extensão do Predicado daCuidadosPorUtente: ListaUtentes, ListaListaCuidados -> {V,F}
% Dá a lista de listas com todos os cuidados de todos os utentes na lista inicial (cidade).
daCuidadosPorUtentes([],[]).
daCuidadosPorUtentes([IdUt|T], [H|C]) :- findall( (Data, IdUt, IdPrest, Descricao, Custo), cuidado(Data, IdUt, IdPrest, Descricao, Custo), H),
                                         daCuidadosPorUtentes(T,C).

      % --------------  Data -------------- %
% Só no dia
% Extensão do Predicado daCuidadosPorData: Data, ListaCuidados -> {V,F}
daCuidadosPorData(Data, L) :- findall( (Data, IdUt, IdPrest, Descricao, Custo), cuidado(Data, IdUt, IdPrest, Descricao, Custo), L).

% Entre 2 datas
% Extensão do Predicado daCuidadosPorDatas: DataInicio, DataFim, ListaCuidados -> {V,F}
daCuidadosPorDatas(DataInicio, DataFim, L) :- daTodosCuidados(X),
                                              daCuidadosPorDatasAux(DataInicio, DataFim, X, L).

daTodosCuidados(L) :- findall( (Data, IdUt, IdPrest, Descricao, Custo), cuidado(Data, IdUt, IdPrest, Descricao, Custo), L).

% Extensão do Predicado daCuidadosPorDatasAux: ListaCuidados, ListaFinalCuidados -> {V,F}
daCuidadosPorDatasAux(DataInicio, DataFim, [], []).
daCuidadosPorDatasAux(DataInicio, DataFim, [(Data, IdUt, IdPrest, Descricao, Custo)|T],[(Data, IdUt, IdPrest, Descricao, Custo)|Ca]) :-
                      comparaDate(Data,DataInicio),
                      comparaDate(DataFim,Data),
                      daCuidadosPorDatasAux(DataInicio, DataFim, T, Ca).
daCuidadosPorDatasAux(DataInicio, DataFim, [(Data, IdUt, IdPrest, Descricao, Custo)|T], L) :-
                      daCuidadosPorDatasAux(DataInicio, DataFim, T, L).

%
% ----------------------------------- 6 ------------------------------------- %
% Identificar os utentes de um prestador/especialidade/instituição;
%

    % --------------------------- Prestador --------------------------- %

% Extensão do Predicado daUtentesPorPrestador: IDPrestador, ListaUtentes -> {V,F}
daUtentesPorPrestador(IdPrest, L) :- daUtentes(IdPrest, X),
                                     daInfoUtentesPorPrestador(X, C),
                                     apagaReps(C,D),
                                     concListList(D, L).

daUtentes(IdPrest, L) :- findall( IdUt, cuidado(Data, IdUt, IdPrest, Especialidade, Instituicao), L).

daInfoUtentesPorPrestador([], []).
daInfoUtentesPorPrestador([IdUt|T], [L|Ls]) :- idUtenteID(IdUt, L),
                                               daInfoUtentesPorPrestador(T, Ls).

      % ------------------------ Especialidade ------------------------ %

% Extensão do Predicado daUtentesPorEspecialidade: Especialidade, ListaUtentes -> {V,F}
daUtentesPorEspecialidade(Especialidade, L) :- daPrestadoresPorEspecialidade(Especialidade, X),
                                               daUtentesPorPrestadores(X, C),
                                               apagaReps(C,D),
                                               concListList(D, L).

% Extensão do Predicado daPrestadoresPorEspecialidade: Especialidade, ListaPrestador -> {V,F}
% Dá uma lista de prestadores de acordo com a Especialidade
daPrestadoresPorEspecialidade(Especialidade, L) :- findall( IdPrest, prestador(IdPrest, Nome, Especialidade, Instituicao), L).

% Extensão do Predicado daUtentesPorPrestadores: ListaPrestadores, ListaUtentes -> {V,F}
% Dá uma lista de utentes com base na lista de prestadores inicial
daUtentesPorPrestadores([],[]).
daUtentesPorPrestadores([IdPrest|C],[H|T]) :- findall( IdUt, cuidado(Data, IdUt, IdPrest, Especialidade, Instituicao), H),
                                              daUtentesPorPrestadores(C,T).

      % ------------------------- Instituicao ---------------------------- %

% Extensão do Predicado daUtentesPorInstituicao: Instituicao, ListaUtentes -> {V,F}
daUtentesPorInstituicao(Instituicao, L) :- daPrestadorPorInst(Instituicao, X),
                                           daUtentesPorPrestadores(X,C),
                                           apagaReps(C,D),
                                           concListList(D,L).

%
% ----------------------------------- 7 ------------------------------------ %
%
% Identificar cuidados de saúde realizados por utente/instituição/prestador;

% Extensão do Predicado daCuidadosPorUtente: IdUtente, ListaCuidados -> {V,F}
daCuidadosPorUtente(IdUt, L) :- findall( (Data, IdUt, IdPrest, Descricao, Custo), cuidado(Data, IdUt, IdPrest, Descricao, Custo), L).

daCuidadosPorPrestador(IdPrest,L) :- findall( (Data, IdUt, IdPrest, Descricao, Custo), cuidado(Data, IdUt, IdPrest, Descricao, Custo), L).

%
% ---------------------------------- 8 ------------------------------------- %
%
% Determinar todas as instituições/prestadores a que um utente já recorreu;

% Extensão do Predicado daPrestadoresPorUtente: IdUt, ListaPrestadores -> {V,F}
daPrestadoresPorUtente(IdUt, L) :- findall(IdPrest, cuidado(Data, IdUt, IdPrest, Descricao, Custo), X),
                                   apagaReps(X,L).

  % ------------------------------------------------------ %

% Extensão do Predicado daInstituicoesPorUtente: IdUt, ListaInstituicoes -> {V,F}
daInstituicoesPorUtente(IdUt, L) :- daPrestadoresPorUtente(IdUt, X),
                                    daInstituicoesPorPrestadores(X, C),
                                    concListList(C,D),
                                    apagaReps(D,L).

% Extensão do Predicado daInstituicoesPorPrestadores: ListaPrestadores, ListaInstituicoes -> {V,F}
% Da a lista de instituições a que os prestadores correspondem
daInstituicoesPorPrestadores([],[]).
daInstituicoesPorPrestadores([IdPrest|C],[H|T]) :- findall(Instituicao, prestador(IdPrest, Nome, Especialidade, Instituicao), H),
                                                   daInstituicoesPorPrestadores(C,T).


%
% ----------------------------------- 9 ------------------------------------- %
%
% Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas.

    % ------------------ Utente ------------------ %

% Extensão do Predicado daCustosPorUtente: IdUt, Custo -> {V,F}
daCustosPorUtente(IdUt, C) :- daCporUtente(IdUt, X),
                              sum(X,C).

daCporUtente(IdUt, L) :- findall(Custo, cuidado(Data, IdUt, IdPrest, Descricao, Custo), L).

    % ------------------ Especialidade ------------------ %

% Extensão do Predicado daCustosPorEspecialidade: Especialidade, Custo -> {V,F}
daCustosPorEspecialidade(Especialidade, C) :- daPrestadoresPorEspecialidade(Especialidade,L),
                                              daCustosPorPrestadores(L,C).

% Extensão do Predicado daCustosPorPrestadores: ListaPrestadores, Custo -> {V,F}
daCustosPorPrestadores([X], C) :- daCustosPorPrestador(X, C).
daCustosPorPrestadores([H|T], C) :- daCustosPorPrestador(H,C1),
                                    daCustosPorPrestadores(T,C2),
                                    C is C2 + C1.

    % ------------------ Prestador ------------------ %

% Extensão do Predicado daCustosPorPrestador: Prestador, Custo -> {V,F}
daCustosPorPrestador(IdPrest,C) :- daCporPrestador(IdPrest,X),
                                   sum(X,C).

daCporPrestador(IdPrest, L) :- findall(Custo, cuidado(Data, IdUt, IdPrest, Descricao, Custo), L).

% ------------------ Data ------------------ %
% Apenas uma data.
daCustosPorData(Data,C) :- daCporData(Data,L),
                           sum(L,C).

daCporData(Data, L) :- findall(Custo, cuidado(Data, IdUt, IdPrest, Descricao, Custo), L).

% Entre datas.
daCustosPorDatas(DataInicio,DataFim,C) :- daTodosCuidados(X),
                                          daCuidadosPorDatasAux(DataInicio, DataFim, X, L),
                                          daSomaCuidados(L,C).

daSomaCuidados([],0).
daSomaCuidados([(_,_,_,_,Custo)|T], C) :- daSomaCuidados(T,C1),
                                          C is C1 + Custo.




% ------------------------- Conhecimento adicional -------------------------- %

% medicamento: #IdMed, Nome, Custo -> {V, F}
medicamento(1, 'Creme Facial', 20).
medicamento(2, 'Centrum', 35).
medicamento(3, 'Brufen', 25).
medicamento(4, 'Xyzal', 10).

% receita: #IdMed, #IdUt, DataValidade, Quantidade -> {V, F}
receita(3, 1, (2018,4,8), 1).
receita(2, 1, (2018,4,8), 2).
receita(3, 2, (2018,4,10), 1).
receita(1, 3, (2018,4,10), 1).
receita(4, 4, (2018,4,10), 1).


% Invariante Estrutural: Não podem existir Medicamentos repetidos (com o mesmo ID).
+medicamento( IdMed, _, _ ) :: ( integer(IdMed),
                                 findall(IdMed, medicamento(IdMed, Nome, Custo), S),
                                 comprimento( S, N ), N == 1 ).

% Invariante Referencial: Não podem existir receitas cujas quantidades de medicamentos sejam maiores que 5.
+receita( _, _, _, Quantidade ) :: ( Quantidade =< 5 ).

% Invariante Estrutural: Ao adicionar uma receita, o Medicamento e o Utente têm de existir na Base do Conhecimento.
+receita( IdMed, IdUt, _, _ ) :: ( findall(IdMed, medicamento(IdMed, Nome, Quantidade), X),
                                   comprimento( X, N ), N == 1,
                                   findall(IdUt, utente(IdUt, Nome, Sexo, Idade, Morada), Y),
                                   comprimento( Y, Ns ), Ns == 1 ).

% Extensão do predicado historicoReceitas: IdUt, L -> {V, F}
% É devolvida uma lista com todas as receitas de um certo utente.
historicoReceitas(IdUt, L) :-
    findall( (IdMed, DataValidade, Quantidade), receita(IdMed, IdUt, DataValidade, Quantidade), L ).


% Extensão do Predicado historicoMedicamentos: IdUt, L -> {V, F}
% É devolvida uma lista com o nome de todos os medicamentos receitados a um certo utente.
historicoMedicamentos(IdUt, L) :- findall(IdMed, receita(IdMed, IdUt, DataValidade, Quantidade), X),
                                  daNomeMedicamentos(X, Y),
                                  concListList(Y, L).

% Extensão do predicado daNomeMedicamentos: L, L -> {V, F}
% Fornecendo os Ids de certos medicamentos, é devolvido o nome dos mesmos.
daNomeMedicamentos([],[]).
daNomeMedicamentos([IdMed|Ids], [H|L]) :- findall(Nome, medicamento(IdMed, Nome, Quantidade), H),
                                          daNomeMedicamentos(Ids, L).

% Extensão do predicado daCustoTotalMedicamentos: IdUt, C -> {V, F}
% Devolve o custo total dos medicamentos receitados a um utente (tendo em conta as respetivas quantidades).
daCustoTotalMedicamentos(IdUt, C) :- findall( (IdMed, Quantidade), receita(IdMed, IdUt, DataValidade, Quantidade), X),
                                     daPrecoReceita(X,C).

% Extensão do predicado daPrecoReceita: ListaMedQuantidade, CustoTotalDessesPares -> {V,F}
daPrecoReceita( [], 0 ).
daPrecoReceita( [(IdMed, Quantidade)|Ids], C ) :- daPrecoReceita(Ids, C1),
                                                  medicamento(IdMed, Nome, X),
                                                  C is X * Quantidade + C1.




% -------------------------- Predicados Auxiliares -------------------------- %

% Extensão do Predicado comprimento: ListaElem, Comp -> {V,F}
comprimento([],0).
comprimento([X|L], C) :- comprimento(L, N), C is 1+N.

% Extensão do predicado sum: X, R -> {V, F}, faz o somatório de uma lista.
sum([], 0).
sum([X|L], R) :- sum(L, R1), R is X + R1.

% Extensão do predicado apagaReps: L, R -> {V, F}
% Apaga diversos elementos repetidos numa lista.
apagaReps([], []).
apagaReps([H|T], [H|L]) :- apagaT(H, T, X),
                           apagaReps(X, L).

% Extensão do predicado apagaT: X, L, R -> {V, F}
% Apaga todas as ocorrências repetidas de um elemento numa lista.
apagaT(X, [], []).
apagaT(X,[X|L1],L2) :- apagaT(X,L1,L2).
apagaT(X,[Y|L1],[Y|L2]) :- apagaT(X,L1,L2).

% Extensão do predicado concatenar : L1,L2,R -> {V,F}
concatenar([],L,L).
concatenar([X|L1],L2,[X|L3]) :- concatenar(L1,L2,L3).

% Extensão do predicado concListList: LLs, L -> {V, F}
% Utilizando o predicado auxiliar concatenar, concatena listas dentro de uma lista.
concListList([], []).
concListList([H|T], L) :- concListList(T, L1),
                          concatenar(H, L1, L).

% Extensão do predicado comparaDate: (Y1,M1,D1), (Y2,M2,D2) -> {V,F}
% Compara 2 datas assumindo que estas sao representadas por 1 triplo.
comparaDate((Y1,M1,D1),(Y2,M2,D2)) :- Y1 > Y2.
comparaDate((Y,M1,D1),(Y,M2,D2)) :- M1 > M2.
comparaDate((Y,M,D1),(Y,M,D2)) :- D1 >= D2.

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
