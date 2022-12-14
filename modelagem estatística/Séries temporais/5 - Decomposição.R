#####################################
###    DECOMPOSI??O E SUAVIZA??O  ###
#####################################

# Importando s?rie do pr?prio R

# ------------------------------------------------------
# EXEMPLO 1
# Concentra??o de CO2
?co2
concentracao <- co2
plot(concentracao)

# Decomposi??o
install.packages('forecast')
library(forecast)

#Aditiva
decomposicao <-   decompose(concentracao)
plot(decomposicao)

require(ggplot)
autoplot(decomposicao)

#Multiplicativa
decomposicao <-  decompose(concentracao, type = "multiplicative")
autoplot(decomposicao)


# ------------------------------------------------------
# EXEMPLO 2
# Manchas Solares
manchas_solares <- sunspots
sunspots
plot(sunspots)

# Decomposi??o
decomposicao2 <- decompose(manchas_solares)
plot(decomposicao2)

decomposicao3 <- decompose(manchas_solares, type = "mult")
plot(decomposicao3)


#A S?rie tem alguns picos e pode ser ind?cio de outlier
#podemos fazer uma suaviza??o

# Suaviza??o (outliers)
suavizacao <- tsclean(manchas_solares)
plot(suavizacao)

# Compara??o com o original
plot(manchas_solares)
lines(suavizacao, col="red")

