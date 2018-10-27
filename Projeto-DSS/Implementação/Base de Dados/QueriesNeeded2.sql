select * from turno 
where capacidadeMax > capacidadeAtual;

update turno 
	set capacidadeAtual = 0
    where idTurno between 1 and 175;
    
select * from unidadecurricular_has_aluno ;   

select * from aluno_has_turno ;

select * from trocas;

delete  from trocas;

update turno 
	set capacidadeAtual = 30
    where idTurno = 2;

select * from turno;