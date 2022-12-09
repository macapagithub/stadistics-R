install.packages('Rtools')
library('Rtools')

install.packages("devtools")
devtools::install_github('https://github.com/nebulae-co/saber', force = T)
library("saber")


#Datos segundo semestre del 2011.
#data("SB11_20112")

#Data mas corta.
data('SB11_20111')


iteraciones <- 500
tamano_muestral <- 27

plot(
  mean(SB11_20112$MATEMATICAS_PUNT),
  sd(SB11_20112$MATEMATICAS_PUNT),
  pch = 20,
  cex = 4,
  col = "white"
)

#Voy agregar puntos de media, la muestra que sacamos, de tamaño muestral 
#ya escogido, con desviación de la muestra.
#Asi creamos una nube de estimadores. 
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
  pch = 18,
  cex = 4,
  col = 'red'
)

#head(SB11_20112)


#Intervalos de confianza.

#Vamos a ver como se distribuyen el acceso a internet.
table(SB11_20112$ECON_SN_INTERNET)

tamano_muestral <- 25
iteraciones <- 90

poblac_A <- SB11_20112$FISICA_PUNT[SB11_20112$ECON_SN_INTERNET == 0]
media_pob_A <- mean(poblac_A, na.rm = TRUE)

poblac_B <- SB11_20112$FISICA_PUNT[SB11_20112$ECON_SN_INTERNET == 1]
media_pob_B <- mean(poblac_B, na.rm = TRUE)






plot(media_pob_A, media_pob_B, col = 4, pch= 20)
abline(0, 1)

for (i in seq_len(iteraciones)){
  muestra <- sample(seq_len(nrow(SB11_20111)), tamano_muestral)
  
  cuales_A <- seq_len(nrow(SB11_20111)) %in% muestra & SB11_20111$ECON_SN_INTERNET == 0
  muestra_A <- SB11_20111$FISICA_PUNT[cuales_A]
  
  media_muestral_A <- mean(muestra_A, na.rm = TRUE)
  t_test_A <- t.test(muestra_A)
  intervalo_A <- t_test_A$conf.int
  LI_A <- min(intervalo_A)
  LS_A <- max(intervalo_A)
  
  cuales_B <- seq_len(nrow(SB11_20111)) %in% muestra & SB11_20111$ECON_SN_INTERNET == 1
  muestra_B <- SB11_20111$FISICA_PUNT[cuales_B]
  
  media_muestral_B <- mean(muestra_B, na.rm = TRUE)
  t_test_B <- t.test(muestra_B)
  intervalo_B <- t_test_B$conf.int
  LI_B <- min(intervalo_B)
  LS_B <- max(intervalo_B)
  
  rect(LI_A, LI_B, LS_A, LS_B)
  
}

points(media_pob_A, media_pob_B, col = 4, pch = 20, cex = 4)



#------------
#MAs bonito
library("dplyr")
library("purrr")
library("magrittr")
library("ggplot2")
library("LaCroixColoR")


colores <- lacroix_palette("Pamplemousse")


tamano_muestral <- 30


tibble(
  muestra = replicate(iteraciones, sample_n(SB11_20112, tamano_muestral), simplify = FALSE),
  muestra_A = map(muestra, filter, ECON_SN_INTERNET == 0) %>% map(extract2, "FISICA_PUNT"),
  media_muestra_A = map_dbl(muestra_A, mean),
  t_test_A = map(muestra_A, t.test),
  intervalo_A = map(t_test_A, extract2, "conf.int"),
  LI_A = map_dbl(intervalo_A, min),
  LS_A = map_dbl(intervalo_A, max),
  muestra_B = map(muestra, filter, ECON_SN_INTERNET == 1) %>% map(extract2, "FISICA_PUNT"),
  media_muestra_B = map_dbl(muestra_B, mean),
  t_test_B = map(muestra_B, t.test),
  intervalo_B = map(t_test_B, extract2, "conf.int"),
  LI_B = map_dbl(intervalo_B, min),
  LS_B = map_dbl(intervalo_B, max)
) -> analisis

media_pob_A <- mean(pobla_A, na.rm = TRUE)
# min_A <- media_pob_A - 15*sd(pobla_A, na.rm = TRUE)/sqrt(tamano_muestral)
# max_A <- media_muestra_A + 15*sd(pobla_A, na.rm = TRUE)/sqrt(tamano_muestral)

media_pob_B <- mean(pobla_B, na.rm = TRUE)
# min_B <- media_pob_B - 15*sd(pobla_B, na.rm = TRUE)/sqrt(tamano_muestral)
# max_B <- media_pob_B + 15*sd(pobla_B, na.rm = TRUE)/sqrt(tamano_muestral)


analisis %>% 
  ggplot +
  geom_rect(aes(xmin = LI_A, xmax = LS_A, ymin = LI_B, ymax = LS_B), alpha = 0.2, fill = colores[6]) +
  # geom_point(aes(x = media_muestra_A, y = media_muestra_B), colour = colores[1], size = 1) +
  annotate("point", media_pob_A, media_pob_B, colour = colores[4], size = 5) +
  geom_abline(intercept = 0, slope = 1, colour = colores[4], size = 1) +
  # xlim(min_A, max_A) +
  # ylim(min_B, max_B) +
  theme_minimal()


#____________________
boxplot(SB11_20112$MATEMATICAS_PUNT, horizontal = TRUE)

stripchart(SB11_20112$MATEMATICAS_PUNT, method = "jitter", pch = 19, add = TRUE, col = "blue")

summary(SB11_20112$MATEMATICAS_PUNT)

