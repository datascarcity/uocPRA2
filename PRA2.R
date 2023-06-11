## ----setup, include=FALSE-----------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width=6, fig.height=4) 
setwd("D:/10-MASTER/40-SEMESTRE 4/40-PRACTICA2")
library(ggplot2)
library(ggfortify)
library(forecast)
library(dplyr)
library(car)
library(rcompanion)
library(corrplot)
library(caret)
library(pROC)


## ----dataset read preprocessing-----------------------
#Se importa el datased
dset <- read.csv(file="heart.csv")
dim(dset)
head(dset)
#Se comprueba tipo de variables importadas
sapply(dset, class)
#Copia del dataset original
dset.clean <- dset
#Comprobación Age
summary(dset.clean$age)
boxplot(dset.clean$age)
#Comprobación Sex
unique(dset.clean$sex)
dset.clean$sex <- factor(dset.clean$sex,
                         levels=c(0,1),
                         labels=c("F", "M"))
#Comprobación cp
unique(dset.clean$cp)
dset.clean$cp <- as.factor(dset.clean$cp)
#Comprobación trtbps
summary(dset.clean$trtbps)
#Comprobación chol
summary(dset.clean$chol)
boxplot(dset.clean$chol)
#Comprobación fbs
unique(dset.clean$fbs)
dset.clean$fbs <- as.logical(dset.clean$fbs)
#Comprobación restecg
unique(dset.clean$restecg)
dset.clean$restecg <- as.factor(dset.clean$restecg)
#Comprobación thalachh
summary(dset.clean$thalachh)
#Comprobación exng
unique(dset.clean$exng)
dset.clean$exng <- as.logical(dset.clean$exng)
#Comprobación oldpeak
summary(dset.clean$oldpeak)
#Comprobación slope
unique(dset.clean$slp)
dset.clean$slp <- as.logical(dset.clean$slp)
#Comprobación caa
unique(dset.clean$caa)
summary(dset.clean$caa)
#Comprobación thal
unique(dset.clean$thall)
dset.clean$thall <- as.factor(dset.clean$thall)
#Comprobación output
unique(dset.clean$output)
dset.clean$output <- as.factor(dset.clean$output)
#Comprobación tipos variables final
sapply(dset.clean, class)


## ----data cleaning------------------------------------
#Se buscan los valores duplicados
#duplicated(dset.clean)
dset.clean[duplicated(dset.clean),]

dim(dset.clean)
dset.clean <- unique(dset.clean)
dim(dset.clean)

#Se buscan los NAs
which(is.na(dset.clean))

#Se visualiza la distribución de algunas variables cuantitativas: age, chol, trtbps
ggplot(dset.clean, aes(x=age)) + 
  geom_histogram(binwidth = 5, fill = "white", colour = "black") +
  ggtitle("Histograma de distribución de age - binwidth = 5")
#Se detecta una distribución unimodal con asimetría  la izquierda
ggplot(dset.clean, aes(x=chol)) + 
  geom_histogram(binwidth = 20, fill = "white", colour = "black") +
  ggtitle("Histograma de distribución de chol - binwidth = 20")
#Se detecta una distribución que se aproxima bastante a auna normal,  unimodal, con una cola a la derecha bastante larga, con varias clases vacías y outliers a la derecha
ggplot(dset.clean, aes(x=trtbps)) + 
  geom_histogram(binwidth = 10, fill = "white", colour = "black") +
  ggtitle("Histograma de distribución de trtbps - binwidth = 10")
#Se detecta una distribución que se aproxima bastante a auna normal,  unimodal, asimétrica con cola a la derecha

#Se visualiza la distribución de algunas variables cualitativas
#Se puede ver que hay una mayoría de sujetos de sexo masculino
pie(table(dset.clean$sex), labels=paste(c("F", "M"), ": ", table(dset.clean$sex)))
table(dset.clean$sex)

#Se puede ver que la mayoría de los pacientes no presenta vasos sanguinéos afectados
pie(table(dset.clean$caa), labels=paste(c("0", "1", "2", "3", "4"), ": ", table(dset.clean$caa)))
table(dset.clean$caa)


## ----subset-------------------------------------------
dset.clean.over50 <- subset(dset.clean, age >= 50)
dset.clean.under50 <- subset(dset.clean, age < 50)
summary(dset.clean.over50$age)
summary(dset.clean.under50$age)

dset.clean$U50 <- with(dset.clean, ifelse(age < 50, TRUE, FALSE))
dset.clean$U50 <- as.factor(dset.clean$U50)

dset.clean$levchol <- as.factor(with(dset.clean, ifelse(chol <= 200, "N",
                                             ifelse(chol <= 240, "H", "R"))))

dset.clean.M <- subset(dset.clean, sex == "M")
dset.clean.F <- subset(dset.clean, sex == "F")
summary(dset.clean.M$sex)
summary(dset.clean.F$sex)


## ----normality homoscedasticity chol------------------
summary(dset.clean.under50$chol)
length((dset.clean.under50$chol))
length((dset.clean.over50$chol))

leveneTest(chol ~ U50, data=dset.clean)

ggplot(dset.clean, aes(x = U50, y=chol))+
  geom_boxplot()

leveneTest(chol ~ sex, data=dset.clean)
ggplot(dset.clean, aes(x = sex, y=chol))+
  geom_boxplot()


## ----chol hypotesis-----------------------------------
t.test(dset.clean$chol[dset.clean$sex=="M"], dset.clean$chol[dset.clean$sex=="F"], alternative = "two.sided", var.equal=FALSE)

t.test(dset.clean$chol[dset.clean$U50==TRUE], dset.clean$chol[dset.clean$U50==FALSE], alternative = "two.sided", var.equal=TRUE)


## ----correlation--------------------------------------
shapiro.test(dset.clean$age)
shapiro.test(dset.clean$chol)
shapiro.test(dset.clean$trtbps)

qqnorm(dset.clean$age, pch = 1)
qqnorm(dset.clean$chol, pch = 1)
qqnorm(dset.clean$trtbps, pch = 1)

ggplot(dset.clean, aes(x=age, y=trtbps))+
  geom_point()
cor(dset.clean$age, dset.clean$trtbps, method="spearman")
cor.test(dset.clean$age, dset.clean$trtbps, method="spearman")

ggplot(dset.clean, aes(x=age, y=chol))+
  geom_point()
cor(dset.clean$age, dset.clean$chol, method="spearman")
cor.test(dset.clean$age, dset.clean$chol, method="spearman")

ggplot(dset.clean, aes(x=chol, y=trtbps))+
  geom_point()
cor(dset.clean$chol, dset.clean$trtbps, method="spearman")
cor.test(dset.clean$chol, dset.clean$trtbps, method="spearman")



## ----correlation cathegorical vars--------------------
table.sex.cp <- table(dset.clean$sex, dset.clean$cp)
table.sex.cp
prop.table.sex.cp <- prop.table(table.sex.cp)
prop.table.sex.cp
cramerV(table.sex.cp)

barplot(table.sex.cp, main = "Sexo y Dolor Pecho", xlab= "Tipo de dolor", ylab="Pacientes", col = c("lightblue", "gray"), legend.text = TRUE )


table.sex.output <- table(dset.clean$sex, dset.clean$output)
table.sex.output
cramerV(table.sex.output)
barplot(table.sex.output, main = "Sexo y Predicción", xlab= "Predicción", ylab="Pacientes", col = c("lightblue", "gray"), legend.text = TRUE )

table.levchol.output <- table(dset.clean$levchol, dset.clean$output)
table.levchol.output
cramerV(table.levchol.output)
barplot(table.levchol.output, main = "Nivel de colesterol y Predicción", xlab= "Predicción", ylab="Pacientes", col = c("lightblue", "gray", "red"), legend.text = TRUE )



## ----anova--------------------------------------------
#Test de normalidad Shapiro-Wilk
shapiro.test(dset.clean$thalachh)
hist(dset.clean$thalachh)

#Test de homogeneidad de varianzas de Fligner-Killeen
fligner.test(thalachh ~ levchol, data=dset.clean)

#Test de contraste de hipótesis entre más de dos grupos de Kruskal-Wallis
kruskal.test(thalachh ~ levchol, data=dset.clean)



## ----logistic regression------------------------------
#Generación de los dos conjuntos de entrenamiento (80%) y de testing (%20)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(dset.clean), replace = TRUE, prob=c(0.8,0.2))
dset.train <- dset.clean[sample,]
dset.test <- dset.clean[!sample,]

#Se genera el modelo de regresión logística utilizando el conjunto de entrenamiento
dset.model <- glm(formula = output ~ age + sex + cp + trtbps + chol + fbs + restecg + thalachh + exng + oldpeak + slp + caa + thall, family = binomial (link=logit), data = dset.train)

summary(dset.model)

#Se puede ver que algunas de las variables no son significativas, se pueden por tanto excluir del modelo
dset.model1 <- glm(formula = output ~ sex + cp + chol + thalachh + exng + oldpeak + caa, family = binomial (link=logit), data = dset.train)

summary(dset.model1)
#Se calcula la colinealidad del modelo, no se aprecia colinealidad entre las variables
vif(dset.model1)

#Cálculo de las Odds-Ratio
dset.model1.OR <- exp(coefficients(dset.model1))
dset.model1.OR

#Se calcula la predicción de la probabilidad ajustada de output con los datos del conjunto de testing
dset.model1.predicted <- predict(dset.model1, dset.test, type="response")
#Se define un valor límite para discriminar entre pacientes de riesgo y no
threshold <- 0.5
#Se asigna dicho valor a una variable dicotómica
dset.model1.predicted.class <- as.factor(ifelse(dset.model1.predicted>threshold,1,0))
dset.model1.predicted.class
#Se genera un dataframe con la variable dicotómica ajustada y la observada
performance.data <- data.frame(observed=dset.test$output, predicted=dset.model1.predicted.class)

#Se calcula la matriz de confusión
performance.data.cm <- confusionMatrix(data=performance.data$predicted, reference=performance.data$observed)
performance.data.cm

#Se estima la curva ROC y el área debajo de la curva
dset.model1.predicted.total <- predict(dset.model1, dset.clean, type="response")
curva.roc <- roc(dset.clean$output, dset.model1.predicted.total)
plot(curva.roc)
auc(curva.roc)
dset.model1

model1.sumresiduals <- sum(residuals(dset.model1, type="pearson")^2)
model1.null <- dset.model1$df.null
model1.residual <- dset.model1$df.residual

dset.model1.pchisq <- 1-pchisq(model1.sumresiduals, model1.null-model1.residual)
dset.model1.pchisq



## ----write csv----------------------------------------
write.csv(dset.clean, "dset_clean.csv")
knitr::purl()


## ----table--------------------------------------------
Contribuciones <- c("Investigación previa", "Redacción de las respuestas", "Desarrollo del Código", "Participación en el vídeo")
Firma <- c("MR", "MR", "MR", "MR")

tasks <- data.frame(Contribuciones, Firma)
tasks

knitr::kable(tasks, "pipe")

