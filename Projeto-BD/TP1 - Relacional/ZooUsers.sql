USE ZooDB;

/* Criação do utilizador 'admin', o qual tem acesso a toda a base de dados. */
CREATE USER 'admin'@'localhost' 
	IDENTIFIED BY 'admin';

GRANT ALL PRIVILEGES ON ZooDB.* 
	TO 'admin'@'localhost';

/* Criação do utilizador 'func', bem como todas as suas views. */
CREATE USER 'func'@'localhost'
	IDENTIFIED BY 'func';

/* VIEW E GRANT do Funcionário em relação à tabela Animal. */
CREATE VIEW viewFunc_Animal AS
	SELECT idAnimal, Nome, DataUltimaRefeicao
		FROM Animal;

GRANT SELECT, UPDATE ON ZooDB.viewFunc_Animal TO
	'func'@'localhost';


/* VIEW E GRANT do Funcionário em relação à tabela Alimento. */
CREATE VIEW viewFunc_Alimento AS
	SELECT idAlimento, Nome, Quantidade
		FROM Alimento;

GRANT SELECT, UPDATE ON ZooDB.viewFunc_Alimento TO
	'func'@'localhost';


/* VIEW E GRANT do Funcionário em relação à tabela Espaco. */
CREATE VIEW viewFunc_Espaco AS
	SELECT Tipo, Tamanho, Quantidade
		FROM Espaco;

GRANT SELECT ON ZooDB.viewFunc_Espaco TO
	'func'@'localhost';
        
        
/* VIEW E GRANT do Funcionário em relação à tabela Animal_has_Alimento. */
CREATE VIEW viewFunc_Animal_has_Alimento AS
	SELECT *
		FROM Animal_has_Alimento;

GRANT SELECT, UPDATE ON ZooDB.viewFunc_Animal_has_Alimento TO
	'func'@'localhost';
