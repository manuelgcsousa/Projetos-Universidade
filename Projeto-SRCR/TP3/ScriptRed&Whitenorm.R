                             # SCRIPT USADO PARA OS DOIS TIPOS DE VINHO NORMALIZADOS #

set.seed(3)

# Importação das bibliotecas necessárias
library( neuralnet )
library( hydroGOF )
library( arules )
library( leaps )

# Ler dataset de treino de um CSV
trainsetNorm <- read.csv("C:\\Users\\David Sousa\\Desktop\\SRCR\\Trabalho\\TP3\\ValoresNormalizadosFinal.csv",header=TRUE,sep=";",dec=".")
 
# Definição das fórmulas RNA
# Fórmula completa
formulaRNA1 <-   quality ~ type + fixed.acidity + volatile.acidity + 
                 residual.sugar + chlorides +
                 free.sulfur.dioxide + total.sulfur.dioxide + 
                 density + pH + sulphates + alcohol


###################################################### EFETUAR OS TESTES #################################################################

# Alteração dos dados de teste

# Amostra "uniformemente distribuída"
# treinoRed <- trainsetNorm[1:800,]
# treinoWhite <- trainsetNorm[5698:6497,] #Metade do vinho tinto foi usado para treino enquanto que outra metade foi usada para teste(daí terem só sido usadas 800 unidades)
# testeSet <- trainsetNorm[801:5697,] 
# treinoSet <- rbind(treinoRed,treinoWhite)

# Amostra com dados organizados de forma aleatória
# set.seed(3)
amostra <- sample(nrow(trainsetNorm), 1600)
treinoAl  <- trainsetNorm[amostra,]               # É aleatória, mas depois usa as restantes para teste e produz melhores resultados
testeAl  <- trainsetNorm[-amostra,]

# Equilibrar os arrays com o mesmo número de vinhos 
weights <- seq(from = -1, to = 200, by = 0.01)

# Treinar a rede neuronal
# rna1 <- neuralnet(formulaRNA1, treinoAl, hidden = c(2,3,4), lifesign = "full", linear.output = TRUE, threshold = 0.1, algorithm = "backprop", learningrate = 0.001, startweights = weights )
rna1 <- neuralnet(formulaRNA1, treinoAl, hidden = c(2,1,2), lifesign = "full", linear.output = TRUE, 
                  threshold = 0.1, algorithm = "rprop+", startweights = weights )


# Desenhar rede neuronal
plot(rna1, rep = "best")

# Definir variaveis de input para teste
# teste.01 <- subset(testeAl,select=c( type, fixed.acidity, volatile.acidity, citric.acid,  residual.sugar, chlorides, total.sulfur.dioxide,  free.sulfur.dioxide,  density,  pH, sulphates, alcohol))
# teste.01 <- subset(testeAl,select=c(volatile.acidity, alcohol))
teste.01  <- subset(testeAl,select=c(type, fixed.acidity, volatile.acidity, residual.sugar, chlorides, free.sulfur.dioxide,  total.sulfur.dioxide, density,  pH,  sulphates,  alcohol))
# teste.01  <- subset(testeAl,select=c(type, fixed.acidity, citric.acid, chlorides, total.sulfur.dioxide, density, pH, sulphates, alcohol))


# testar a rede com os novos casos
rna1.resultados <- compute(rna1, teste.01)

# imprimir resultados 
# resultados <- data.frame(atual = testeSet$quality, previsao = rna1.resultados$net.result)
resultados <- data.frame(atual = testeAl$quality, previsao = rna1.resultados$net.result)

# imprimir resultados 
resultados$previsao <- round(resultados$previsao, digits = 8)

# Calcular o RMSE
rmse(c(testeAl$quality),c(resultados$previsao))
# rmse(c(testeSet$quality),c(resultados$previsao))

