select count(*) from aluno ;
select * from unidadecurricular;
select count(*) from UnidadeCurricular;
select count(*) from Turno;

delete from Aluno
	where idAluno = 77320;


select count(*) from Aluno;


SELECT *
	FROM UnidadeCurricular_has_Aluno;

DELETE FROM UnidadeCurricular_Has_Aluno
	WHERE 
		UnidadeCurricular_idUC = 10 and Aluno_idAluno = 39;

insert into UnidadeCurricular_has_Aluno 
	(UnidadeCurricular_idUC, Aluno_idAluno)
    Values 
		('Log', 39);