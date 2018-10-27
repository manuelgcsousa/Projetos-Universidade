
/*
DROP TABLE Animal
*/

INSERT INTO Espaco
	(idEspaco, Tipo, Tamanho, Quantidade)
	VALUES 
		(1, 'Jaula', 'G', 9),
        (2, 'Aquario', 'P', 20),
        (3, 'Jaula', 'M', 4);
        

INSERT INTO Animal
	(idAnimal, Nome, Especie, Genero, DataUltimaRefeicao, Espaco_idEspaco)
	VALUES 
		(1, 'Joao', 'Panthera leo', 'M', NULL, 1),
        (2, 'Oscar', 'Orcinus orca', 'M', '2017-11-24 22:36:12', 2),
        (3, 'Ricardo', 'Panthera pardus', 'M', '2017-11-25 11:21:53', 1),
        (4, 'Napoleão', 'Balaenoptera physalus', 'M', NULL, 2),
        (5, 'Paula', 'Equus asinus', 'F', NULL, 1),
        (6, 'Kika', 'Macropus rufus', 'F', '2017-11-23 16:45:21', 3),
        (7, 'Laila', 'Pan troglodytes', 'F', NULL, 1),
        (8, 'Maria', 'Loxodonta africana', 'F', '2017-11-25 07:43:54', 1),
        (9, 'Eddy', 'Ailuropoda melanoleuca', 'M', '2017-11-23 12:48:32', 2),
        (10, 'Lara', 'Carcharodon carcharias', 'F', NULL, 2),
        (11, 'Laila', 'Apis mellifera', 'F', NULL, 1);
        

INSERT INTO Alimento
	(idAlimento, Nome, Quantidade, Numero_Serie)
	VALUES 
		(1, 'Frango', 10, '4312'),
        (2, 'Peixinhos', 230, '2042'),
		(3, 'Frutos', 589, '324'),
        (4, 'Bamboo', 20, '1324'),
		(5, 'Atum', 1000, '642');


INSERT INTO Funcionario
	(idfuncionario, Nome, Salario, Cidade, Detalhes)
	VALUES 
		(1, 'André Marques', 250, 'Braga', 'Tenões'),
        (2, 'Mariana Fonte', 2000, 'Porto', 'Estádio do Dragao'),
        (3, 'Filipe Sousa', 9500, 'Braga', 'Partes desconhecidas'),
		(4, 'Nuno Gonçalves', 700, 'Lisboa', 'Praça do Rato'),
        (5, 'Ricardo Martins', 1200, 'Vila Franca de Xira', NULL),
        (6, 'Inês Ferreira', 9500, 'Guimarães', 'Partes desconhecidas');
        

INSERT INTO Animal_has_Alimento
	(Animal_idAnimal, Alimento_idAlimento, Quantidade)
	VALUES 
		(2, 1, 4),
        (3, 5, 26),
        (6, 3, 21),
        (8, 4, 13),
        (9, 5, 13);
        
        
INSERT INTO Funcionario_has_Alimento
	(Funcionario_idfuncionario, Alimento_idAlimento)
	VALUES 
		(1, 1),
        (2, 1),
        (3, 2),
        (3, 3),
        (4, 4),
        (5, 5),
        (5, 1),
        (6, 5);


INSERT INTO Contacto
	(Telemovel, Funcionario_idfuncionario)
	VALUES 
		(911121121, 1),
        (962345678, 2),
		(913323321, 3),
        (934442454, 4),
		(916231524, 1),
        (932136555, 5),
		(917646664, 6);