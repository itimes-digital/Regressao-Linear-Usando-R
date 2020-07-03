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

data[data$large.residuals,c('N','Health.Spending', 'stand.residuals', 'residuals')]

#Distancia de cookie
head(cooks.distance(model5))
data$cooks.distance <- cooks.distance(model5)


#install.packages('plotly')
#install.packages('magrittr')
library(plotly)
library(magrittr)
library(ggplot2)
library(stats)
library(graphics)

# Exibe gráfico com valores da distância de cook
fig <- plot_ly(data = data, y = ~cooks.distance, type = 'scatter') %>% layout(title='Cooks Distance')
fig

#Exibe os pontos maiores do que 1
data[data$cooks.distance > 1, ]

data[data$large.residuals, c("N", "cooks.distance")]

# realiza a distancia dfbeta
data$dfbeta1 <- dfbeta(model5)[,1]
data$dfbeta2 <- dfbeta(model5)[,2]

head(data$dfbeta1)
head(data$dfbeta2)

# realiza a distancia dfbeta padronizados
data$dfbeta1_st <- dfbetas(model5)[,1]
data$dfbeta2_st <- dfbetas(model5)[,2]

head(data$dfbeta1_st)
head(data$dfbeta2_st)


names(data)

data[,16:19]

plot_ly(data = data,  y = ~dfbeta1, type = 'scatter') %>% layout(title = 'DFBETA 1')

# pontos muito deslocados: 52, 98, 7, 87 e 106

plot_ly(data = data, y = ~dfbeta2, type = 'scatter') %>% layout(title = 'DFBETA 2')

# pontos muito deslocados: 52, 98

plot_ly(data = data,  y = ~dfbeta1_st, type = 'scatter') %>% layout(title = 'DFBETA 1 padronizado')

# pontos muito deslocados: 52, 98, 7, 87, 106

plot_ly(data = data,  y = ~dfbeta2_st, type = 'scatter') %>% layout(title = 'DFBETA 2 padronizado')

# pontos muito deslocados: 52, 98


data[data$large.residuals, c("N", "dfbeta1", "dfbeta2", "dfbeta1_st", "dfbeta2_st")]
# Não pode retirar do modelo o 7 e o 98, pois são pontos influentes

model5

data_sem_item7 <- data
data_sem_item98 <- data

1 / 0.0000000000000000000001

# retirada de linha da amostra
data_sem_item7 <- data_sem_item7[-c(7),]
data_sem_item98 <- data_sem_item98[-c(98),]

model_sem_item <- lm(formula = data_sem_item7$Health.Spending ~ -1 + data_sem_item7$Leisure.Spending + 
                       data_sem_item7$Spending.on.education)

model_sem_item

# definindo novos betas, 
# após retirada de cada elemento e analisando as diferenças padronizadas
data$dffits <- dffits(model5)

plot_ly(data = data, y = ~dffits, type = 'scatter') %>% layout(title = 'DFFIT Padronizado')
# pontos influentes são 7, 52 e 98

data[data$large.residuals, c("N", "dffits")] #98 e #7

# Valores de alavancagem
data$laverage <- hatvalues(model5)

plot_ly(data=data, y=~laverage, type ='scatter') %>% layout(title="Laverage")

# linha de corte de alavancagem
# ((k + 1) / N) | k = variáveis independentes
# De acordo com o modelo temos duas variáveis independentes 
# E 120 observações -> (2 + 1)/120 = 0,025, resultado pode ser multiplicado por 2 ou 3

#(2 + 1)/120 = 0,025 * 2 #111 e #67
#(2 + 1)/120 = 0,025 * 3 #67

data[data$laverage > 0.025, c('N', 'laverage')]
data[data$laverage > 0.05, c('N', 'laverage')]
data[data$laverage > 0.075, c('N', 'laverage')]

data[data$large.residuals, c('N', 'laverage')] #98 é um ponto influente e de alavancagem


#Razão de covariância
data$cvr <- covratio(model5)

#próximo de 1 não interfere na variação das estimatvas dos parâmetros

# Limite superior da covariância
# 1 + [3 * (k + 1) / n] -> ficará mais disperso e pode prejudar a estimativa

# Limite inferior da covariância
# 1 - [3 * (k + 1) / n] -> melhora a precisão, pois diminui a variabilidade

plot_ly(data = data, y = ~cvr, type = 'scatter') %>% layout(title="CVR")

# Limite superior da covariância
1 + (3 * (2 + 1) / 120)

# Limite inferior da covariância
1 - (3 * (2 + 1) / 120)

data[data$cvr > 1.075, ] #67 e #111 a retirada prejudica o modelo
data[data$cvr < 0.925, ] # nenhum foi influente para baixo com possiblidade de retirada.

data[data$cvr > 1.075 | data$cvr < 0.925, c('N', 'cvr')]

data[data$large.residuals, c('N', 'cvr')]


influence.measures(model5)

#Multicolinearidade
cor.test(data$Leisure.Spending, data$Spending.on.education)

cor.test(data$Leisure.Spending, data$Health.Spending)
#lazer vs saúde = 0.7821115

cor.test(data$Health.Spending, data$Spending.on.education)
#saúde vs educação = 0.77825

vif(model5)
# Afeta o modelo já que a variáveis X tem forte correlação - VIF >= 10
#data$Leisure.Spending data$Spending.on.education 
#     15.51017                   15.51017

VIF <- vif(model5)
tolerancia <- 1 / VIF

mean(VIF)

