data <- read.xlsx('D:/estudo-machine-learning/Machine Learning in R Curso Completo de Regressão Linear/data/budget.xlsx', sheetName = 'tab1');

plot(data$Leisure.Spending, data$Health.Spending)

#Criação de um modelo de regressão simples
#lm(y~x)
model1 <- lm(data$Health.Spending ~ data$Leisure.Spending)
summary(model1)

#a = intercepto
#b = beta coeficiente
#y = a + b1.x
#y <- 27.14 + 0.69x

names(data)
model2 <- lm(Health.Spending ~ Leisure.Spending + Spending.on.education, data = data)
summary(model2)

#y = a + b1.x1 + b2.x1
#y <- 18.168 + 0.40251 * x1 + 0.44838 * x2

#O B0 ou intercepto é a média de Y
mean(data$Health.Spending)
median(data$Health.Spending)

model3 <- lm(Health.Spending ~ years.of.study + 
              Leisure.Spending + 
              Spending.on.education +
              number.of.children, 
              data = data)
summary(model3)

#Estatistica wald
#H0: B0 = 0 p-value > 0,05 este torna-se não significativo, já que é o p-value do intercepto é maior que 5%
#H1: B0 != 0 p-value < 0,05


model4 <- lm(Health.Spending ~ + 
               Leisure.Spending + 
               Spending.on.education,
             data = data)
summary(model4)


summary(model3)

#Baseado nos valores do modelo3
#Intercepto com p-value acima de 0,05%, não siginificativo, então será -1
model5 <- lm(data$Health.Spending ~ -1 + data$Leisure.Spending + data$Spending.on.education)
summary(model5)

#y = 0.45634 * Leisure + 0.46374 * education

#install.packages('QuantPsyc')
library(MASS)
library(boot)
#install.packages('norm')
library(QuantPsyc)


lm.beta(model5)
# Beta padronizado
# data$Leisure.Spending data$Spending.on.education 
# 0.5231479                  0.4538041 

# Aumento no desvio padrão dessas variáveis acarreta no aumento dos gastos com saúde
sd(data$Leisure.Spending) # Cada 86.49391 gasto com lazer
0.5231479 * sd(data$Health.Spending)# Então gasta-se 40.11057 com saúde

sd(data$Spending.on.education) # Cada 75.02905 gasto com educação
0.4538041 * sd(data$Health.Spending)# Então gasta-se 34.79387 com saúde

#Então gastos com lazer tem maior importância no modelo tendo maior influência na variável Y
#34.79387 / 40.11057 = 0,867 * 100 = 86,7 - 100 = 13,3%

# Isto é, entre lazer e educação, existe uma diferença de 11.46486 de desvio padrão 
# que acarreta em saúde
86.49391 - 75.02905 = 11.46486

# Cada 1 real gasto com lazer ou educação sobre os coeficientes, podem...
y = 0.45634 * lazer + 0.46374 * educação

# Estimativas do intervalo de confiança
# Verifica a variabilidade dos dados do grau de confiança de 95%
# Se houver valores que passe por zero entre mínimo e máximo, a variável deve ser descartada
confint(model5)

# ...influenciar em um valor mínimo ou máximo sobre Y com  95% de confiança
#                                2.5 %    97.5 %
#data$Leisure.Spending        0.3412042 0.5714672
#data$Spending.on.education   0.3090691 0.6184083

# Comparar modelos com ANOVA e analisar seu grau de importância

model_a <- lm(data$Health.Spending ~ -1 + data$Leisure.Spending + data$Spending.on.education)
model_b <- lm(data$Health.Spending ~ data$Leisure.Spending + data$Spending.on.education)
model_c <- lm(data$Health.Spending ~ data$Leisure.Spending + data$Spending.on.education + data$Age)
model_d <- lm(data$Health.Spending ~ data$Leisure.Spending + data$Spending.on.education + data$Age + data$years.of.study)
model_e <- lm(data$Health.Spending ~ data$Leisure.Spending + data$Spending.on.education + data$Age + data$years.of.study + data$number.of.children)

anova(model_d, model_e)
anova(model_c, model_d)
anova(model_b, model_c)
anova(model_a, model_b)
# Modelo A tem um p-value menor que 0,05% e sem intercepto é o melhor modelo analisado
