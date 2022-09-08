#####################################
###    DECOMPOSIÇÃO E SUAVIZAÇÃO  ###
#####################################

# Importando série do próprio R

# ------------------------------------------------------
# EXEMPLO 1
# Concentração de CO2
?co2
concentracao <- co2
plot(concentracao)

# Decomposição
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

# Decomposição
decomposicao2 <- decompose(manchas_solares)
plot(decomposicao2)

decomposicao3 <- decompose(manchas_solares, type = "mult")
plot(decomposicao3)


#A Série tem alguns picos e pode ser indício de outlier
#podemos fazer uma suavização

# Suavização (outliers)
suavizacao <- tsclean(manchas_solares)
plot(suavizacao)

# Comparação com o original
plot(manchas_solares)
lines(suavizacao, col="red")

