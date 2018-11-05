# Importação das bibliotecas necessárias
library( leaps )

# Ler dataset de treino de um CSV
trainsetNorm <- read.csv("C:\\Users\\David Sousa\\Desktop\\SRCR\\Trabalho\\TP3\\ValoresNormalizadosFinal.csv",header=TRUE,sep=";",dec=".")


# Fórmula completa
formulaRNA1 <- quality ~ type + fixed.acidity + volatile.acidity + citric.acid +
               residual.sugar + chlorides + free.sulfur.dioxide + 
               total.sulfur.dioxide + density + pH + sulphates + alcohol

# Fórmula segundo o software weka
formulaWeka <- quality ~ volatile.acidity + alcohol

# Fórmula segundo as métricas
formulaMetr <- quality ~ type + fixed.acidity + volatile.acidity + 
               residual.sugar + chlorides +
               free.sulfur.dioxide + total.sulfur.dioxide + 
               density + pH + sulphates + alcohol 

# Fórmula segundo os dados que consideramos adequados 
formulaInfo <- quality ~ type + fixed.acidity + citric.acid + 
               chlorides + total.sulfur.dioxide + 
               density + pH + sulphates +alcohol 

# Análise das variáveis segundo o R
best.subset <- regsubsets(formulaRNA1, trainsetNorm, nvmax=12)
best.subset.summary <- summary(best.subset) 

# Utilização de metricas para determinar as variáveis gerais mais significativas
best.subset.by.adjr2 <- which.max(best.subset.summary$adjr2)
best.subset.by.adjr2

best.subset.by.cp <- which.min(best.subset.summary$cp)
best.subset.by.cp

best.subset.by.bic <- which.min(best.subset.summary$bic)
best.subset.by.bic

# Plot para a percepção das métricas
par(mfrow=c(2,2))
plot(best.subset.summary$adjr2, xlab="Número de variáveis", ylab="Adjusted RSq", type="l")
points(best.subset.by.adjr2, best.subset.summary$adjr2[best.subset.by.adjr2], col="red", cex =2, pch =20)
plot(best.subset.summary$cp, xlab="Número de variáveis", ylab="CP", type="l")
points(best.subset.by.cp, best.subset.summary$cp[best.subset.by.cp], col="red", cex =2, pch =20)
plot(best.subset.summary$bic, xlab="Número de variáveis", ylab="BIC", type="l")
points(best.subset.by.bic, best.subset.summary$bic[best.subset.by.bic], col="red", cex =2, pch =20)
