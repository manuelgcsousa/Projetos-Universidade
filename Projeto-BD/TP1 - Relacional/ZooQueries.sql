USE ZooDB;

-- Query 1
SELECT * 
	FROM Alimento 
		WHERE quantidade <= 100;

  
-- Query 2
SELECT idAnimal, Nome 
	FROM Animal
		INNER JOIN Espaco
			ON Animal.Espaco_idEspaco = Espaco.idEspaco
				WHERE Espaco.Tipo = 'Jaula';


-- Query 3
SELECT idFuncionario, Nome, Cidade, Salario
	FROM Funcionario
		WHERE Cidade = 'Braga'
			ORDER BY Salario;


-- Query 4
SELECT idEspaco, Tipo
	FROM Espaco
		WHERE Tipo = 'Jaula'
			AND Quantidade >= 3;


-- Query 5
SELECT idAnimal, Nome
	FROM Animal
		INNER JOIN Animal_has_Alimento
			ON Animal.idAnimal = Animal_idAnimal
				WHERE Animal_has_Alimento.Quantidade > 5;


-- Query 6
SELECT idFuncionario, Funcionario.Nome
	FROM Alimento
		INNER JOIN Funcionario_has_Alimento
			ON Alimento.idAlimento = Alimento_idAlimento
				INNER JOIN Funcionario
					ON Funcionario.idFuncionario = Funcionario_idFuncionario
						WHERE Alimento.Nome = 'Frango';


-- Trigger que atualiza a quantidade total
DELIMITER $$
CREATE TRIGGER atualiza_Quantidade
	AFTER INSERT ON Animal_has_Alimento 
	FOR EACH ROW
	BEGIN
		DECLARE Num INT;
        DECLARE Id_Al INT;
        
        SET Num = NEW.Quantidade;
        SET Id_Al = NEW.Alimento_idAlimento;
        
		UPDATE Alimento 
			SET Quantidade = Quantidade - Num
				WHERE idAlimento = Id_Al;
	END	
$$

DELIMITER $$
CREATE TRIGGER atualiza_Quantidade_Update
	AFTER UPDATE ON Animal_has_Alimento 
	FOR EACH ROW
	BEGIN
		DECLARE Num INT;
        DECLARE Id_Al INT;
        
        SET Num = NEW.Quantidade;
        SET Id_Al = NEW.Alimento_idAlimento;
        
		UPDATE Alimento 
			SET Quantidade = Quantidade - Num
				WHERE idAlimento = Id_Al;
	END	
$$

-- Teste aos triggers atualiza_Quantidade e atualza_Quantidade_Update
INSERT INTO Animal_has_Alimento
	(Animal_idAnimal, Alimento_idAlimento, Quantidade)
	VALUES 
		(2, 5, 2)
	ON DUPLICATE KEY UPDATE Quantidade = Quantidade + 2;

SELECT *
	FROM Animal_has_Alimento;

SELECT * FROM Alimento;


/* PROCEDURES */

DELIMITER $$
CREATE PROCEDURE infoAnimal (IN idAni Int)
BEGIN
	SELECT * 
		FROM Animal
			WHERE idAnimal = idAni;
END
$$

-- Teste procedure infoAnimal
CALL infoAnimal(2);


DELIMITER $$
CREATE PROCEDURE atualizarSalario (IN idFunc Int, IN salarioNovo DOUBLE(6,2))
BEGIN
	DECLARE antigoSalario DOUBLE(6,2);
    
    SELECT Salario INTO antigoSalario
		FROM Funcionario 
			WHERE idFuncionario = idFunc;
	
	IF (salarioNovo > antigoSalario) THEN	
		UPDATE Funcionario
			SET Salario = salarioNovo
				WHERE idFuncionario = idFunc;
	ELSE 
		SIGNAL SQLSTATE '45000' 
			SET MESSAGE_TEXT = 'Salario invalido';
	END IF;
END
$$

-- Teste procedure atualizarSalario
CALL atualizarSalario(1, 890.41);

-- SELECT * FROM Funcionario;


/* Procedimento que implementa uma transaction.
   Utilizado pelo zoo quando for necessário adicionar uma especie extremamente rara, 
   ignorando assim a quantidade disponivel no espaço. */
DELIMITER $$
CREATE PROCEDURE addEspecieRara(IN idAni Int, IN Nom VARCHAR(15), 
							    IN Especi VARCHAR(50), IN Gen VARCHAR(2),
							    IN DUltimaRefeicao DATETIME, IN Espa_idEspa INT)
BEGIN
	DECLARE Quanti INT;
    DECLARE ErroTransaction BOOL DEFAULT 0;
    DECLARE CONTINUE HANDLER FOR SQLEXCEPTION SET ErroTransaction = 1;
	
    START TRANSACTION;
    
    INSERT INTO Animal
		(idAnimal, Nome, Especie, Genero, DataUltimaRefeicao, Espaco_idEspaco)
		VALUES
			(idAni, Nom, Especi, Gen, DUltimaRefeicao, Espa_idEspa);

	UPDATE Espaco
		SET Quantidade = Quantidade + 1
			WHERE idEspaco = Espa_idEspa;
	
	IF ErroTransaction THEN
		ROLLBACK;
    ELSE
		COMMIT;
    END IF;
END 
$$
		
CALL addEspecieRara(12, NULL, 'Lynx pardinus', 'M', NULL, 1);

-- SELECT * FROM Animal;
-- SELECT * FROM Espaco;


/*
=>	RE1: O sistema deverá ser capaz de identificar os alimentos com défice de quantidade (≤ 100kg);
=>	RE2: O sistema deverá ser capaz de identificar quais os animais presentes num certo espaço;
=>	RE3: O sistema deverá ser capaz de agrupar os funcionários de acordo com a cidade e organizá-los de acordo com o salário auferido;
=>	RE4: O sistema deverá ser capaz de identificar os espaços de um tipo especifico com mais de uma certa quantidade de animais;
=>	RE5: O sistema deverá ser capaz de identificar os animais que consumiram uma grande quantidade de alimento numa única refeição;
=>	RE6: O sistema deverá ser capaz de identificar os funcionários que forneceram determinado alimento aos animais.
*/