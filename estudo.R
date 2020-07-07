library(car)
library(boot)
library(magrittr)
library(tibble)
library(xlsx)
library(forcats)
library(MASS)
library(QuantPsyc)
library(haven)

data <- read.xlsx('D:/estudo-machine-learning/Regressao-Linear-Usando-R/data/budget.xlsx', sheetName = 'tab1');

data <- as_tibble(data)

# factor determina como uma variável categórica
prof <- factor(data$profession)

# com o levels foi definido os valores nessas categorias 
levels(prof) <- c("Doctor", "Engineer")

# Comparação de quantidade de valores antes inputar os dados na base
table(prof)
table(data$profession)

data$profession <- prof

marital_status <- factor(data$Marital.status)

# Alterando o factor para coloca categorias
levels(marital_status) <- c("Single", "Married", "Divorced", "Widower")

table(marital_status)
table(data$Marital.status)

data$Marital.status <- marital_status

income_v2 <- factor(data$income)

levels(income_v2) <- c("< 1 SM", "1 - 3 SM", "3 - 5 SM", "> 5 SM")

table(income_v2)
table(data$income)

data$income <- income_v2


model <- lm(Health.Spending ~ Leisure.Spending + Spending.on.education, data = data)
summary(model)

data_num <- data[, c('Health.Spending', 'Leisure.Spending', 'Spending.on.education')]

cor(data_num)


