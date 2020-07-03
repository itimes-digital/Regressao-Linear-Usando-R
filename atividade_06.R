library(car)
library(boot)
library(magrittr)
library(tibble)

#install.packages('xlsx')
#install.packages('forcats')
library(xlsx)
library(forcats)

#install.packages('QuantPsyc')
library(MASS)
#install.packages('norm')
library(QuantPsyc)

#install.packages('haven')
library(haven)

data <- read.xlsx('D:/estudo-machine-learning/Machine Learning in R Curso Completo de Regressão Linear/data/budget.xlsx', sheetName = 'tab1');

model5 <- lm(data$Health.Spending ~ -1 + data$Leisure.Spending + data$Spending.on.education)
summary(model5)

data$residuals <- model5$residuals
data$stand.residuals <- rstandard(model5)
data$stand.residuals <- round(data$stand.residuals, digits = 3)

# Valores que estiverem fora da distribuição normal, já que o grau de confiança é de 95%
data$large.residuals <- data$stand.residuals > 1.96 | data$stand.residuals < -1.96

# verifica se os dados são simétricos

hist(data$stand.residuals)

summary(data$stand.residuals)
summary(data$residuals)


plot(model5)

qplot(sample=data$stand.residuals, stat = 'qq') + 
  labs(x='N(0,1)', y = 'Resíduos')


# resíduos leatórios

library(car)

durbinWatsonTest(model5)

#H0: os resíduos são aleatórios (p>0,05)

#H1: os resíduos não são aleatórios (p<0,05)

summary(model5)

