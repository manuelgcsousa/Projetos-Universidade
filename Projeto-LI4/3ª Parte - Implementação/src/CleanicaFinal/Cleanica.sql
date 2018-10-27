select username from Utilizador
where email = 'cr7@hotmail.com';

select * from Funcionario;

select * from Cliente;


select * from Servico;

select * from ServicosRealizados;

select * from Formulario_de_Satisfacao;

delete from Cliente
where Utilizador_email='carlospedrosa8b@gmail.com';

delete from Servico
where id_servico=5;

insert into Servico ( nome, descricao, preco, precoSProduto)
Values ('Jardinagem', 'Cuida do Seu Jardim', 17.00, 10.00),
	   ('Exteriores', 'Cuida da Sua Piscina', 20.00, 15.00),
	   ('Carros', 'Cuida do Seu Carro', 15.00, 10.00),
	   ('Interiores', 'Cuida da Sua Casa', 17.50, 12.50);

insert into ServicosRealizados
Values ('carlospedrosa8b@gmail.com', 'rua', '2018-5-15 16:00:00', 100.50, 'G', 'S', NULL, 'S', 3);

insert into ServicosRealizados
Values ('carlospedrosa8b@gmail.com', 'Rua da Cachorrela', '2018-5-16 16:00:00', 222.22, 'G', 'S', NULL, 'S');

update ServicosRealizados
set morada = 'Rua da Cachorrela nº 39'
where id_ServicosRealizados = 1;

insert into Utilizador
Values ('manuelgcsousa@gmail.com','gcsousa','gcsousa','F');

insert into Utilizador
Values ('boas@gmail.com','boas','boas','A');

insert into Utilizador
Values ('teste1@gmail.com','teste1','teste1','F');

insert into Funcionario
Values ('manuelgcsousa@gmail.com',8,11111111,91111111,'PT5011111', '2018-05-21', 'Aves');

insert into Funcionario
Values ('teste1@gmail.com',9,111112111,91111111,'PT5011111', '2018-05-21', 'Aves');

insert into Formulario_de_Satisfacao
Values ('carlospedrosa8b@gmail.com','teste1@gmail.com',NULL,NULL, 'P');

insert into Formulario_de_Satisfacao
Values ('carlospedrosa8b@gmail.com','manuelgcsousa@gmail.com',NULL,NULL, 'P');

insert into Formulario_de_Satisfacao
Values (3, 'carlospedrosa8b@gmail.com','manuelgcsousa@gmail.com',NULL,NULL, 'P');

insert into Utilizador
Values ('cc@gmail.com','cc','cc','F');

delete from Cliente
where Utilizador_email = 'lol2@gmail.com'

delete from Utilizador
where email = 'xxx4@gmail.com';

insert into Utilizador
Values ('xxx4@gmail.com', 'xxx4', 'xxx2', 'F');

insert into Funcionario
Values ('xxx3@gmail.com', 11, 1468698812, 918227350, 'PT50000000000', '2018-05-04', 'Alpendorada');

update Formulario_de_Satisfacao
set sugestoes = NULL, pontuacao = Null, pendente = 'P'
where id_FormularioS = 1