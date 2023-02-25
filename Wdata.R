# Carregar o dataset 'women'
data(women)

# Calcular media e desvio padrao para o peso e altura
mean_height <- mean(women$height)
sd_height <- sd(women$height)

mean_weight <- mean(women$weight)
sd_weight <- sd(women$weight)

# Exibir os resultados convertidos para unidade metrica
cat("Media de altura:", mean_height*2.54, "\n")
cat("Desvio padrao de altura:", sd_height*2.54, "\n")
cat("Media de peso:", mean_weight*0.45, "\n")
cat("Desvio padrao do peso:", sd_weight*0.45, "\n")
