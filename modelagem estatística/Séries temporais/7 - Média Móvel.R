########################
###    MÉDIA MÓVEL   ###
########################


# Desvios da Temperatura Global 
# install.packages('astsa')
library(astsa)
temp_global <- ts(globtemp, start = c(1880), end = c(2015), frequency = 1)
print(temp_global)
plot(temp_global, type="l", ylab="Desvios da Temperatura",col="blue")


# Média móvel
library(forecast)
temp_global2 <- ma(temp_global, order = 7)
plot(temp_global2,col="blue",ylab="Ordem 7")

temp_global3 <- ma(temp_global, order = 20)
plot(temp_global3,col="blue",ylab="Ordem 20")

temp_global4 <- ma(temp_global, order = 51)
plot(temp_global4,col="blue",ylab="Ordem 51")

plot(temp_global)
lines(temp_global2, col = "red")
lines(temp_global3, col = "green")
lines(temp_global4, col = "blue")



