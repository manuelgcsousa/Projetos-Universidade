CREATE TABLE Cliente (
  Utilizador_email varchar(50) NOT NULL, 
  telemovel        int NOT NULL, 
  morada           varchar(100) NOT NULL, 
  PRIMARY KEY (Utilizador_email));
CREATE TABLE Formulario_de_Realizacao (
  id_FormularioR              int IDENTITY NOT NULL, 
  Cliente_Utilizador_email    varchar(50) NOT NULL UNIQUE, 
  FuncionarioUtilizador_email varchar(50) NOT NULL, 
  duracao                     int NOT NULL, 
  observacoes                 varchar(100) NULL, 
  PRIMARY KEY (id_FormularioR));
CREATE TABLE Formulario_de_Satisfacao (
  id_FormularioS               int IDENTITY NOT NULL, 
  Cliente_Utilizador_email     varchar(50) NOT NULL UNIQUE, 
  Funcionario_Utilizador_email varchar(50) NOT NULL UNIQUE, 
  sugestoes                    varchar(100) NULL, 
  pontuacao                    int NULL, 
  pendente                     varchar(1) NOT NULL, 
  PRIMARY KEY (id_FormularioS));
CREATE TABLE Funcionario (
  Utilizador_email   varchar(50) NOT NULL, 
  Servico_id_servico int NOT NULL UNIQUE, 
  cc                 int NOT NULL UNIQUE, 
  telemovel          int NOT NULL, 
  IBAN               varchar(25) NOT NULL, 
  birthdate          date NOT NULL, 
  morada             varchar(100) NOT NULL, 
  PRIMARY KEY (Utilizador_email));
CREATE TABLE Funcionario_ServicosRealizados (
  Funcionario_Utilizador_email             varchar(50) NOT NULL UNIQUE, 
  ServicosRealizados_id_ServicosRealizados int NOT NULL UNIQUE, 
  PRIMARY KEY (Funcionario_Utilizador_email, 
  ServicosRealizados_id_ServicosRealizados));
CREATE TABLE Servico (
  id_servico     int IDENTITY NOT NULL, 
  nome           varchar(15) NOT NULL, 
  descricao      varchar(100) NOT NULL, 
  preco          float(4) NOT NULL, 
  precoSProduto  float(4) NOT NULL, 
  precoSProdutos float(4) NULL, 
  PRIMARY KEY (id_servico));
CREATE TABLE ServicosRealizados (
  id_ServicosRealizados    int IDENTITY NOT NULL, 
  Cliente_Utilizador_email varchar(50) NOT NULL, 
  morada                   varchar(100) NOT NULL, 
  data                     datetime NOT NULL, 
  preco                    float(4) NOT NULL, 
  dimensao                 varchar(1) NOT NULL, 
  prods                    varchar(1) NOT NULL, 
  aspectosimp              varchar(150) NULL, 
  pendente                 varchar(1) NOT NULL, 
  duracao                  int NOT NULL, 
  PRIMARY KEY (id_ServicosRealizados));
CREATE TABLE Utilizador (
  email    varchar(50) NOT NULL, 
  username varchar(20) NOT NULL UNIQUE, 
  password varchar(20) NOT NULL, 
  estatuto varchar(1) NOT NULL, 
  PRIMARY KEY (email));
ALTER TABLE Funcionario ADD CONSTRAINT FKFuncionari612998 FOREIGN KEY (Servico_id_servico) REFERENCES Servico (id_servico);
ALTER TABLE Formulario_de_Realizacao ADD CONSTRAINT FKFormulario179732 FOREIGN KEY (FuncionarioUtilizador_email) REFERENCES Funcionario (Utilizador_email);
ALTER TABLE Formulario_de_Satisfacao ADD CONSTRAINT FKFormulario936538 FOREIGN KEY (Funcionario_Utilizador_email) REFERENCES Funcionario (Utilizador_email);
ALTER TABLE Funcionario ADD CONSTRAINT FKFuncionari216562 FOREIGN KEY (Utilizador_email) REFERENCES Utilizador (email);
ALTER TABLE Cliente ADD CONSTRAINT FKCliente209000 FOREIGN KEY (Utilizador_email) REFERENCES Utilizador (email);
ALTER TABLE Formulario_de_Realizacao ADD CONSTRAINT FKFormulario590656 FOREIGN KEY (Cliente_Utilizador_email) REFERENCES Cliente (Utilizador_email);
ALTER TABLE Formulario_de_Satisfacao ADD CONSTRAINT FKFormulario286464 FOREIGN KEY (Cliente_Utilizador_email) REFERENCES Cliente (Utilizador_email);
ALTER TABLE Funcionario_ServicosRealizados ADD CONSTRAINT FKFuncionari157923 FOREIGN KEY (Funcionario_Utilizador_email) REFERENCES Funcionario (Utilizador_email);
ALTER TABLE Funcionario_ServicosRealizados ADD CONSTRAINT FKFuncionari343238 FOREIGN KEY (ServicosRealizados_id_ServicosRealizados) REFERENCES ServicosRealizados (id_ServicosRealizados);
ALTER TABLE ServicosRealizados ADD CONSTRAINT FKServicosRe591143 FOREIGN KEY (Cliente_Utilizador_email) REFERENCES Cliente (Utilizador_email);

