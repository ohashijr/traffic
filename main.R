library(dplyr)
library(ggplot2)

d <- read.csv("data/datasetCompleto.csv")

rotas.id <- d$idRota %>% unique()
rotas.min <- numeric()
for(i in rotas.id){
  rotas.min  <- c(rotas.min, sort(d[d$idRota == i, "tempoSegundos"])[1:5] %>% median())
}

baseline <- data.frame(rotasId=rotas.id, tempo=rotas.min)  

d$dataAsDate <- d$dataAsDate %>% as.Date()
d$idRota <- as.factor(d$idRota)

# calculando a differença de tempos
diffAux <- rep(NA, nrow(d))
for(i in rotas.id){
  ind.rota <- which(d$idRota == i)
  diffAux[ind.rota] <- (d[ind.rota, "tempoSegundos"] - baseline[baseline$rotasId == i, "tempo"]) / 60
}

d$diffTempo <- diffAux

levels(d$diaSemana) <- c("Dom", "Qua", "Qui", "Sab", "Seg", "Sex", "Ter")

save(d, baseline, file = "data/dados.RData")

# cria dataset cumulativo 
# horas <- filtrar as horas de interesse... default tds as horas 0:23
accumulativo <- function(d, horas=0:23){
  # percorrer os ids das rotas
  aux.id <- numeric()
  aux.acu <- numeric()
  aux.dia <- as.Date(as.character())
  aux.diaSemana <- as.character()
  
  for(i in unique(d$idRota)){
    aux <- d[d$idRota == i,] # só do id
    datas <- unique(aux$dataAsDate)
    for(x in datas){
      aux.id <- c(aux.id, i)
      aux.acu <- c(aux.acu, aux[aux$dataAsDate == x & aux$hora %in% horas,"diffTempo"] %>% sum())
      aux.dia <- c(aux.dia, x %>% as.character())
      aux.diaSemana <- c(aux.diaSemana, unique(aux[aux$dataAsDate==x,"diaSemana"]) %>% as.character())
    }
  }
  
  data.frame(idRota=aux.id, acumulado=aux.acu, tempo=aux.dia, diaSemana=aux.diaSemana)
}


# retorna a ordem por rotaId, somando tds as medições da rota
topX <- function(acu, x=10){
  acumulado.geral <- numeric()
  acumulado.id <- unique(acu$idRota)
  
  for(i in acumulado.id){
    acumulado.geral <- c(acumulado.geral, acu[acu$idRota == i,"acumulado"] %>% sum)
  }
  acu.geral <- data.frame(idRota=acumulado.id, acumulado.geral)  
  acu.geral[order(acu.geral$acumulado.geral, decreasing = TRUE)[1:x],]
}



plot <- function(data, titulo="Total Diário", filename="total"){
  data$seq <- as.numeric(data$tempo)
  breakX <- unique(data$seq) 
  labelX <- as.character()
  for(b in breakX){ 
    labelX <- c(labelX, data[which(data$tempo==b)[1], "diaSemana"] %>% as.character())
  }
  xAxis <- paste(range(data$tempo) %>% as.character(), collapse = " ~ ")
  ggplot(aes(x=seq, y=acumulado, color=idRota), data=data) + geom_line() + ylab("acumulado diario min") + xlab(xAxis) +
    scale_x_continuous(breaks=breakX, labels=labelX) + ggtitle(titulo)
  ggsave(filename = paste0("plot/",filename,".png"), width = 9)
}

acu <- accumulativo(d)
top <- topX(acu)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 10: 0 ~ 23 horas", filename = "top10_0_23")

acu <- accumulativo(d, horas=0:6)
top <- topX(acu)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 10: 0 ~ 6 horas", filename = "top10_0_6")

acu <- accumulativo(d, horas=7:12)
top <- topX(acu)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 10: 7 ~ 12 horas", filename = "top10_7_12")

acu <- accumulativo(d, horas=13:18)
top <- topX(acu)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 10: 13 ~ 18 horas", filename = "top10_13_18")

acu <- accumulativo(d, horas=19:23)
top <- topX(acu)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 10: 19 ~ 23 horas", filename = "top10_19_23")

# top 20
acu <- accumulativo(d)
top <- topX(acu, x = 20)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 20: 0 ~ 23 horas", filename = "top20_0_23")

acu <- accumulativo(d, horas=0:6)
top <- topX(acu, x = 20)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 20: 0 ~ 6 horas", filename = "top20_0_6")

acu <- accumulativo(d, horas=7:12)
top <- topX(acu, x = 20)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 20: 7 ~ 12 horas", filename = "top20_7_12")

acu <- accumulativo(d, horas=13:18)
top <- topX(acu, x = 20)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 20: 13 ~ 18 horas", filename = "top20_13_18")

acu <- accumulativo(d, horas=19:23)
top <- topX(acu, x = 20)
data <- acu[acu$idRota %in% top$idRota,]
plot(data, titulo = "Total 20: 19 ~ 23 horas", filename = "top20_19_23")


