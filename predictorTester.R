install.packages("CHAID", repos="http://R-Forge.R-project.org")

library(readxl)
library(plotly)
library(corrplot)
library(caret)
library(rpart)

dataset <- read_excel("C:/Users/Pedro/Desktop/dataset.xlsx")

dataset$EDAD <- factor(dataset$EDAD,
      levels =c(which(dataset$EDAD <20),which(dataset$EDAD <40),which(dataset$EDAD <60),which(dataset$EDAD <80),which(dataset$EDAD >=80)),
      labels = c("menor q 20","menor q 40","menor q 60","menor q 80","mayor q 80"))

dataset$SEXO <-factor(dataset$SEXO, levels = c("MASC","FEME"), labels =c("1 hombre","2 mujer"))

dataset$PROCEDIMIENTO <- factor(dataset$PROCEDIMIENTO, levels = c("ANGIOPLASTIA","CIRUGIA"), labels =c("1 angio","2 cirugia"))

dataset <- dataset[which(!is.na(dataset)),]

fit1 <- rpart(PROCEDIMIENTO ~ SEXO + DIABETES + EPOC + `OBESIDAD MORBIDA`, data = dataset, parms = list(split = 'gini'))

library(rattle)
fancyRpartPlot(fit1)

index <- createDataPartition(dataset$PROCEDIMIENTO, p=0.75, list=FALSE)
trainSet <- dataset[ index,]
testSet <- dataset[-index,]

confusionMatrix(predictions,testSet[,outcomeName])














dataset.out <- glm(PROCEDIMIENTO ~ SEXO + DIABETES + EPOC + `OBESIDAD MORBIDA` + `Resumen Coronariopatia`,
                   data=dataset, family="binomial")

summary(dataset.out)

predsAll<-predict(dataset.out, type = "response")

boxplot(predsAll ~ dataset$PROCEDIMIENTO, col = c("green", "red"),
        ylab = "Probabilidad",
        xlab = "angio / cirugia")

dataset$PROCEDIMIENTO <- factor(dataset$PROCEDIMIENTO, levels = c("1 angio","2 cirugia"), labels =c(1,2))
dataset <- dataset[which(!is.na(dataset$PROCEDIMIENTO)),]

control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)

outcomeName<-'PROCEDIMIENTO'

predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)

Loan_Pred_Profile

predictors<-c("SEXO", "EDAD", "DIABETES", "OBESIDAD MORBIDA")

model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')

# modelo con una red neuronal
model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')

# modelo lineal generalizado
model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')

# modelo de incremento estocástico del gradiente
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')

