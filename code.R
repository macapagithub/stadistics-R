install.packages('Rtools')
library('Rtools')

install.packages("devtools")
devtools::install_github('https://github.com/nebulae-co/saber', force = T)
library("saber")


#Datos segundo semestre del 2011.
#data("SB11_20112")

#Data mas corta.
data('SB11_20111')


iteraciones <- 38
tamano_muestral <- 27

plot(
  mean(SB11_20112$MATEMATICAS_PUNT),
  sd(SB11_20112$MATEMATICAS_PUNT),
  pch = 20,
  cex = 4,
  col = "white"
)

for(i in seq_len(iteraciones)){
  points(
    mean(sample(SB11_20112$MATEMATICAS_PUNT, tamano_muestral)),
    sd(sample(SB11_20112$MATEMATICAS_PUNT, tamano_muestral)),
    pch = 20
    
  )
}


points(
  mean(SB11_20112$MATEMATICAS_PUNT),
  sd(SB11_20112$MATEMATICAS_PUNT),
  pch = 20,
  cex = 4,
  col = 2
)
