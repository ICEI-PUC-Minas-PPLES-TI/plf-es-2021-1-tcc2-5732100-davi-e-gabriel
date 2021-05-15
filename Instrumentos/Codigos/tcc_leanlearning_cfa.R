library(lavaan)
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(chemometrics)){ install.packages("chemometrics"); require(chemometrics) }
if(!require(Hmisc)){ install.packages("Hmisc"); require(Hmisc) }
if(!require(plspm)){ install.packages("plspm"); require(plspm) }
if(!require(psych)){ install.packages("psych"); require(psych) }
if(!require(psy)){ install.packages("psy"); require(psy) }
if(!require(nFactors)){ install.packages("nFactors"); require(nFactors) }
if(!require(corrplot)){ install.packages("corrplot"); require(corrplot) }
if(!require(fmsb)){ install.packages("fmsb"); require(fmsb) }
if(!require(lavaan)){ install.packages("lavaan"); require(lavaan) }
if(!require(lavaan.shiny)){ install.packages("lavaan.shiny"); require(lavaan.shiny) }
if(!require(semPlot)){ install.packages("semPlot"); require(semPlot) }
if(!require(slfm)){ install.packages("slfm"); require(slfm) }
if(!require(MVN)){ install.packages("mvn"); require(MVN) }

# selecione e execute o script todo na primeira vez
# nas próximas vezes, selecione somente as linhas abaixo desta

setwd("C:/Users/gsant/Downloads") # coloque aqui a o caminho da pasta do .csv

df <- read.table("Respostas_fechadas_alunos_compiladas.csv", # coloque aqui o nome do .csv
                   header = TRUE,
                   sep = ",")

attach(df) # comente esta linha depois da primeira execução também
S <- cbind(X1, X2, X3)
A <- cbind(X4, X5, X6)
M <- cbind(X7, X8, X9)

dtF <- cbind(S,A,M)

ds_model <- " satisfaction  =~ X1 + X2 + X3
              adequation =~ X4 + X5 + X6
              market =~ X7 + X8 + X9 "

fit <- cfa(ds_model, data = dtF)
summary(fit, fit.measures = TRUE)


#### Separando os estimadores

Est_finalT <- parameterEstimates(fit, ci = FALSE, standardized = TRUE,rsq=TRUE)

# Carga fatorial (coluna std.all), peso (coluna est)
Est_Aux_finalT <- Est_finalT %>% filter(op == "=~")
Est_Aux_finalT %>% arrange(std.all)


# calculate cronbach's alpha...
# library(psych)
# library(dplyr)
alpha(S)
alpha(A)
alpha(M)

# confiabilidade composta
sl <- standardizedSolution(fit) 
sl <- sl$est.std[sl$op == "=~"] 
names(sl) <- names(dtF) 
re_S <- 1 - sl[1:3]^2 
CC_S <-  sum(sl[1:3])^2 / (sum(sl[1:3])^2 + sum(re_S))
re_A <- 1 - sl[4:6]^2 
CC_A <-  sum(sl[4:6])^2 / (sum(sl[4:6])^2 + sum(re_A))
re_M <- 1 - sl[7:9]^2 
CC_M <-  sum(sl[7:9])^2 / (sum(sl[7:9])^2 + sum(re_M))


# Desenhando o caminho do modelo
semPaths(fit, "std", title = TRUE)

# melhor design
semPaths(fit, "est", edge.label.cex = 1.0,
         color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                      man = rgb(155, 253, 175, maxColorValue = 255)), 
         mar = c(4, 2, 4, 2), fade=FALSE)