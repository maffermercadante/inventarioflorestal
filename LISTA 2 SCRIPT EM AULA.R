notas <- c(6.80, 7.56, 2.90, 7.20, 3.70, 6.40, 9.70, 3.81, 4.49, 7.54, 7.28, 2.50, 6.39, 2.50, 6.71, 8.12, 3.36)

#DEFINICAO VARIAVEL DE INTERESSE
y <- notas

#MEDIA
ymed <- mean(y)
ymed

#VARIANCIA M^6/PARCELA
yvar <- var(y)

#DESVIO PADRAO
ydev <- sd(y)

#TAMANHO AMOSTRA
n <- length(y)

#AREA PARCELA
areaparc <- notas[1]

#NUMERO DE PARCELAS CABIVEIS
N <- 400

#VARIANCIA DA MEDIA M^6/PARCELA
yvarmed <- (yvar/n)*(1-n/N)

#DESVIO PADRAO MEDIA M³/PARCELA
ydesvmed <- sqrt(yvarmed)

#ERRO INVENTARIO      0.975 PADRAO
erro_m3 <- qt(0.960,n-1)*ydesvmed
erro_perc <- erro_m3/ymed*100
erro_perc

#ACURACIA
((6.24 - mean(y)) / 6.24) * 100

#TOTAL FAZENDA
ytotal <- yha*areafaz
erro_m3_ha <- erro_m3_ha*areafaz

#INTERVALO CONFIANCA HECTARE
#precisa executar erro_m3_ha de novo
li <- ymed-erro_m3
li
ls<- ymed+erro_m3
ls

paste0('IC: ',round(li,2), ' <= mu <= ', round(ls,2),' (alpha=8%)')
