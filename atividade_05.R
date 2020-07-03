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


plot(model5)
#install.packages('ggplot2')
library(ggplot2)

# Valor ajustato/previsto de Y
data$fitted <- model5$fitted.values


data[, c("Health.Spending", "fitted")]

ggplot(data = data, aes(fitted, stand.residuals)) + 
  geom_point() +
  geom_smooth(colour = 'Blue')

ggplot(data = data, aes(fitted, stand.residuals)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "Red") +
  labs(x = 'Valores ajustados', y = 'Resíduos padronizados')







