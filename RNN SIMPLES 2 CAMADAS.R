library(tidyverse)
library(nnfor)
library(rnn)
library(MLmetrics)

setwd

HW <- read.csv("./RESULTADO HW HIBRYD.csv", sep = ",")

HW <- HW[, c(1,2)]

# PLOTAR GRÁFICO DA TM
HW %>% ggplot(aes(x = 1:nrow(HW), y = Geração)) +
  geom_line(color = "red")


# PREPARAÇÃO DOS DADOS
periodos_anteriores <- 3

n_col <- ncol(HW)

for (i in 1:periodos_anteriores){
  for (j in 1:nrow(HW)){
    if (j - periodos_anteriores <=0){
      
    } else {
      HW[j, n_col + i] <- HW[j - i, 2]
    }
  }
}

HW_rnn <- HW[4:(nrow(HW)), -1]
names(HW_rnn) <- c("y", "x_1", "x_2", "x_3")
names(HW) <- c("Geração", "Q1", "Q2", "x_3")
#NORMALIZAR OS DADOS 
minmax_Geração <- linscale(HW_rnn$y, minmax = list(mn = 0, mx = 1))
minmax_x1 <- linscale(HW_rnn$x_1, minmax = list(mn = 0, mx = 1))
minmax_x2 <- linscale(HW_rnn$x_2, minmax = list(mn = 0, mx = 1))
minmax_x3 <- linscale(HW_rnn$x_3, minmax = list(mn = 0, mx = 1))

# CRIANDO O DATAFRAME DE TREINO E TESTE DA RNN
Y <- array(minmax_Geração$x, dim = c(nrow(HW_rnn) -12, 1))
X <- array(c(minmax_x1$x[1:(nrow(HW_rnn) - 12)],
             minmax_x2$x[1:(nrow(HW_rnn) - 12)],
             minmax_x3$x[1:(nrow(HW_rnn) - 12)]),
           dim = c(nrow(HW_rnn) - 12, 1, 3))

#TREINAMENTO DA RNN COM TRES CAMADAS OCULTAS
rnn_HW <- trainr(Y, X,
                 hidden_dim = c(50, 50),  # Número de unidades em cada camada oculta
                 learningrate = 0.5, 
                 numepochs = 800)

#REALIZANDO PREVISÕES
entrada_h <- array(c(minmax_x1$x[(nrow(HW_rnn) - 12):nrow(HW_rnn)],
                     minmax_x2$x[(nrow(HW_rnn) - 12):nrow(HW_rnn)],
                     minmax_x3$x[(nrow(HW_rnn) - 12):nrow(HW_rnn)]),
                   dim = c(nrow(HW_rnn) -(nrow(HW_rnn)- 12), 1, 3))

pred_new <- as.data.frame(predictr(rnn_HW, entrada_h))

pred_old <- as.data.frame(predictr(rnn_HW, X))

#VOLTAR OS DADOS PARA A ESCALA NATURAL
prev_desnorm <- linscale(pred_old$V1,
                         minmax = minmax_Geração$minmax,
                         rev = TRUE)

prev_desnorm_new <- linscale(pred_new$V1,
                             minmax = minmax_Geração$minmax,
                             rev = TRUE)

finalresult <- data.frame(real = HW_rnn$y[1:81],
                          pred = prev_desnorm$x)


#Transformando entrada de data em formato tidy
finalresult$id <- 1:nrow(finalresult)
finalresult %>% gather(var, value, -id) -> finalresult
finalresult <- rbind(finalresult, data.frame(id = 82:(82+length(prev_desnorm_new$x)-1),
                                             var = rep("new", length(prev_desnorm_new$x)),
                                             value = prev_desnorm_new$x))
#Plotando os resultados finais
finalresult %>% ggplot(aes(x = id,
                           y = value, colour = var))+
  geom_line(na.rm = T)+
  scale_colour_manual(values = c("red", "blue", "gray10"))

