

#Aula 9 - Diagnósticos de Resíduos 2
install.packages("lmtest")
install.packages("aTSA")
library(lmtest)
library(aTSA)
library(agricolae)                                                    #Carrega o pacote agricolae
library(normtest)                                                     #Carrega o pacote normtest
library(readxl) 



data(jocci)                                                       #Department of commerce commodity price index - USA
View(jocci)
JOCCI <- as.data.frame(jocci)                                     #Y é o Index Jocci . dy é a variação (expressa pela diferença) em logaritmos.
joccits <- ts(JOCCI$dy,start = 1959,frequency = 12)
plot(joccits, main="Índice Jocci", xlab="Ano", ylab="Indice")

#Definindo Formato dos Modelos
ar6model <- dy~dy1+dy2+dy3+dy4+dy5+dy6
ar5model 
ar4model 
ar3model 
ar2model 
ar1model 

#Executando os Teste LM-Breuch-Godfrey

TesteBGAR6 <- bgtest(ar6model,data=jocci)
TesteBGAR5 <- bgtest(ar5model,data=jocci)
TesteBGAR4 <- bgtest(ar4model,data=jocci)
TesteBGAR3 <- bgtest(ar3model,data=jocci)
TesteBGAR2 <- bgtest(ar2model,data=jocci)
TesteBGAR1 <- bgtest(ar1model,data=jocci)

P_Valores_BG <- c(TesteBGAR6$p.value,
               TesteBGAR5$p.value,
               TesteBGAR4$p.value,
               TesteBGAR3$p.value,
               TesteBGAR2$p.value,
               TesteBGAR1$p.value)

Modelos <- c("ar6model","ar5model","ar4model","ar3model","ar2model","ar1model") 

Resultados <- data.frame(Modelos,P_Valores_BG)
View(Resultados)


#Teste Reset
TesteReset6 <- resettest(ar6model,data=jocci)
TesteReset5 
TesteReset4 
TesteReset3  
TesteReset2  
TesteReset1  

P_Valores_RESET <- c(TesteReset6$p.value,
                     TesteReset5$p.value,
                     TesteReset4$p.value,
                     TesteReset3$p.value,
                     TesteReset2$p.value,
                     TesteReset1$p.value)

Resultados <- data.frame(Modelos,P_Valores_BG, P_Valores_RESET)
View(Resultados)


#Carregando o arquivo xls
variacao_PIB <- read.table("c:/Econometria/variacao.xls", header = T)                 #Lê o arquivo variacao.xls na pasta c:/Econometria
variacao_PIB <- as.data.frame(variacao_PIB[,-1])                                      #Apaga a primeira coluna



#Executando os Teste ARCH-LM
      
AR1_VAR <- arima(variacao_PIB,c(1,0,0))
AR2_VAR <- arima(variacao_PIB,c(2,0,0))
arch.test(AR1_VAR)
