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

data <- as_tibble(data)

# variável numérica será mudado para factor
str(data$profession)

# factor determina como uma variável categórica
prof <- factor(data$profession)

str(prof)

# com o levels foi definido os valores nessas categorias 
levels(prof) <- c("Doctor", "Engineer")

prof

# Comparação de quantidade de valores antes inputar os dados na base
table(prof)
table(data$profession)

data$profession <- prof

str(data$Marital.status)
marital_status <- factor(data$Marital.status)
str(marital_status)

# Alterando o factor para coloca categorias
levels(marital_status) <- c("Single", "Married", "Divorced", "Widower")

table(marital_status)
table(data$Marital.status)

data$Marital.status <- marital_status

str(data$income)

income_v2 <- factor(data$income)
str(income_v2)

levels(income_v2) <- c("< 1 SM", "1 - 3 SM", "3 - 5 SM", "> 5 SM")

income_v2

table(income_v2)
table(data$income)

data$income <- income_v2

# 0 não tem NA
sum(is.na(data))

names(data)

data_num <- data[, c(2,6,8,9,10,11)]

#correlação
cor(data_num)

pairs(data_num)

library(ggplot2)
#install.packages('GGally')
library(GGally)

#define graficamente as correlações
ggcorr(data_num, label = T)

#install.packages('corrplot')
library(corrplot)

r <- cor(data_num)

corrplot(r, method = 'circle')

#install.packages('corrgram')
library(corrgram)

corrgram(data_num)

corrgram(data_num, 
         lower.panel = panel.pts,
         upper.panel = panel.conf,
         diag.panel = panel.density)

#Funções do help de pairs
?pairs()

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan")
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(data_num, diag.panel = panel.hist, upper.panel = panel.cor)

pairs(data_num, 
      diag.panel = panel.hist, 
      upper.panel = panel.cor,
      lower.panel = panel.smooth)

panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
          cex = 1, col.line = 'red') 
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)){
    abline(lm(y[ok]~x[ok]), col = col.line)
  }
}

pairs(data_num, 
      diag.panel = panel.hist, 
      upper.panel = panel.cor,
      lower.panel = panel.lm)

ggpairs(data_num, lower = list(continuous = 'smooth'))

data

names(data)

data_num2 <- data[, c(3, 2,6,8,9,10,11)]

ggpairs(data_num2, columns = 2:7, aes(colour = profession))

install.packages('car')
install.packages('boot')
install.packages('carData')
library(car)
library(openxlsx)

#Correlação linear por grupo de profissões
spm(~ Health.Spending + Leisure.Spending + Spending.on.education | profession, 
    data_num2, by.group = T)

# Sem agrupamento
spm(~ Health.Spending + Leisure.Spending + Spending.on.education, 
    data_num2)

names(data_num2)





