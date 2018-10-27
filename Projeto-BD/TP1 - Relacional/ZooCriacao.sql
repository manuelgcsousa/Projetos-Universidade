-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema ZooDB
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema ZooDB
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `ZooDB` DEFAULT CHARACTER SET utf8 ;
USE `ZooDB` ;

-- -----------------------------------------------------
-- Table `ZooDB`.`Funcionario`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `ZooDB`.`Funcionario` (
  `idFuncionario` INT NOT NULL,
  `Nome` VARCHAR(45) NOT NULL,
  `Salario` DOUBLE(6,2) NULL,
  `Cidade` VARCHAR(20) NOT NULL,
  `Detalhes` VARCHAR(45) NULL,
  PRIMARY KEY (`idFuncionario`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ZooDB`.`Contacto`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `ZooDB`.`Contacto` (
  `Telemovel` INT NOT NULL,
  `Funcionario_idFuncionario` INT NOT NULL,
  PRIMARY KEY (`Telemovel`),
  CONSTRAINT `fk_Contacto_Funcionário`
    FOREIGN KEY (`Funcionario_idFuncionario`)
    REFERENCES `ZooDB`.`Funcionario` (`idFuncionario`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ZooDB`.`Alimento`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `ZooDB`.`Alimento` (
  `idAlimento` INT NOT NULL,
  `Nome` VARCHAR(20) NOT NULL,
  `Quantidade` INT NOT NULL,
  `Numero_Serie` VARCHAR(20) NOT NULL,
  PRIMARY KEY (`idAlimento`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ZooDB`.`Espaco`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `ZooDB`.`Espaco` (
  `idEspaco` INT NOT NULL,
  `Tipo` VARCHAR(20) NOT NULL,
  `Tamanho` CHAR(1) NOT NULL,
  `Quantidade` INT NOT NULL,
  PRIMARY KEY (`idEspaco`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ZooDB`.`Animal`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `ZooDB`.`Animal` (
  `idAnimal` INT NOT NULL,
  `Nome` VARCHAR(15) NULL,
  `Especie` VARCHAR(50) NOT NULL,
  `Genero` VARCHAR(2) NOT NULL,
  `DataUltimaRefeicao` DATETIME NULL,
  `Espaco_idEspaco` INT NOT NULL,
  PRIMARY KEY (`idAnimal`),
  INDEX `fk_Animal_Espaco1_idx` (`Espaco_idEspaco` ASC),
  CONSTRAINT `fk_Animal_Espaco1`
    FOREIGN KEY (`Espaco_idEspaco`)
    REFERENCES `ZooDB`.`Espaco` (`idEspaco`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ZooDB`.`Animal_has_Alimento`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `ZooDB`.`Animal_has_Alimento` (
  `Animal_idAnimal` INT NOT NULL,
  `Alimento_idAlimento` INT NOT NULL,
  `Quantidade` INT NOT NULL,
  PRIMARY KEY (`Animal_idAnimal`, `Alimento_idAlimento`),
  INDEX `fk_Animal_has_Alimento_Alimento1_idx` (`Alimento_idAlimento` ASC),
  INDEX `fk_Animal_has_Alimento_Animal1_idx` (`Animal_idAnimal` ASC),
  CONSTRAINT `fk_Animal_has_Alimento_Animal1`
    FOREIGN KEY (`Animal_idAnimal`)
    REFERENCES `ZooDB`.`Animal` (`idAnimal`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Animal_has_Alimento_Alimento1`
    FOREIGN KEY (`Alimento_idAlimento`)
    REFERENCES `ZooDB`.`Alimento` (`idAlimento`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `ZooDB`.`Funcionario_has_Alimento`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `ZooDB`.`Funcionario_has_Alimento` (
  `Funcionario_idFuncionario` INT NOT NULL,
  `Alimento_idAlimento` INT NOT NULL,
  PRIMARY KEY (`Funcionario_idFuncionario`, `Alimento_idAlimento`),
  INDEX `fk_Funcionario_has_Alimento_Alimento1_idx` (`Alimento_idAlimento` ASC),
  INDEX `fk_Funcionario_has_Alimento_Funcionario1_idx` (`Funcionario_idFuncionario` ASC),
  CONSTRAINT `fk_Funcionario_has_Alimento_Funcionario1`
    FOREIGN KEY (`Funcionario_idFuncionario`)
    REFERENCES `ZooDB`.`Funcionario` (`idFuncionario`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Funcionario_has_Alimento_Alimento1`
    FOREIGN KEY (`Alimento_idAlimento`)
    REFERENCES `ZooDB`.`Alimento` (`idAlimento`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
