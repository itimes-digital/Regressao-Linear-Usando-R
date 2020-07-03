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


#Modelo sem beta0 ou intercepto porque o p-value é maior que 0,05
model5 <- lm(data$Health.Spending ~ -1 + data$Leisure.Spending + data$Spending.on.education)
summary(model5)

#Verificar outliers
?rstandard

data$residuals <- model5$residuals
data$stand.residuals <- rstandard(model5)
data$stand.residuals <- round(data$stand.residuals, digits = 3)

# Valores que estiverem fora da distribuição normal, já que o grau de confiança é de 95%
data$large.residuals <- data$stand.residuals > 1.96 | data$stand.residuals < -1.96

sum(data$large.residuals)

data[data$large.residuals,c('N','Health.Spending', 'stand.residuals', 'residuals')]

# Margem com resíduos próximo a 1.96
data$large.residuals99 <- data$stand.residuals > 2.1 | data$stand.residuals < -2.1
sum(data$large.residuals99)
data[data$large.residuals99,c('N','Health.Spending', 'stand.residuals', 'residuals')]

plot(data$stand.residuals)

plot(data$stand.residuals, type = 'o')


summary(model5)

# Coefficients:
#                            Estimate Std. Error t value Pr(>|t|)    
# data$Leisure.Spending       0.45634    0.05814   7.849 2.12e-12 ***
# data$Spending.on.education  0.46374    0.07811   5.937 2.98e-08 ***

# Valor da primeira coluna para saúde é (Y real) 94.1 e o previsto é (y^ ajustado) 98.8346
y <- (0.45634 * 117.5) + (0.46374 * 97.5);y

# 118 é um outlier para analisar ponto influente
data_temp <- data[-c(118),]

model6_sem_pontos_influentes <- lm(formula = Health.Spending ~ -1 + Leisure.Spending + 
     Spending.on.education, data = data_temp)

summary(model6_sem_pontos_influentes)

# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# Leisure.Spending       0.45608    0.05709   7.989 1.06e-12 ***
# Spending.on.education  0.46073    0.07670   6.007 2.19e-08 ***

# Valor da primeira coluna para saúde é 94.1 e o previsto é 98.51057
y <- (0.45608 * 117.5) + (0.46073 * 97.5);y


# 118, 98 e 72 são outliers para analisar os pontos influentes
data_temp <- data[-c(118, 98, 72),]

model7_sem_pontos_influentes <- lm(formula = Health.Spending ~ -1 + Leisure.Spending + 
                                    Spending.on.education, data = data_temp)

summary(model7_sem_pontos_influentes)

# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# Leisure.Spending       0.43588    0.05597   7.788 3.29e-12 ***
# Spending.on.education  0.48833    0.07564   6.456 2.66e-09 ***

# Valor da primeira coluna para saúde é 94.1 e o previsto é 98.82807
y <- (0.43588 * 117.5) + (0.48833 * 97.5);y
