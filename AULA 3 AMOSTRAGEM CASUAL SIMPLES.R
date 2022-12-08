parc <- read.csv2('parcelas.csv')
View(parc)

nrow(parc)

#DEFINICAO VARIAVEL DE INTERESSE
y <- parc$vcomcc

#MEDIA M³/PARCELA
ymed <- mean(y)

#VARIANCIA M^6/PARCELA
yvar <- var(y)

#DESVIO PADRAO
ydev <- sd(y)

#TAMANHO AMOSTRA
n <- length(y)

#AREA PARCELA
unique(parc$areaparc)
areaparc <- parc$areaparc[1]

#AREA POPULACAO FAZENDA HECTARES       SEM TALHOES DUPLICADOS
talhao <- subset(parc,!duplicated(talhao), c('talhao','area'))
View(talhao)
areafaz <- sum(talhao$area)

#NUMERO DE PARCELAS CABIVEIS
N <- areafaz*10000/areaparc
 
#VARIANCIA DA MEDIA M^6/PARCELA
yvarmed <- (yvar/n)*(1-n/N)

#DESVIO PADRAO MEDIA M³/PARCELA
ydesvmed <- sqrt(yvarmed)

#ERRO INVENTARIO      0.975 PADRAO
erro_m3 <- qt(0.975,n-1)*ydesvmed
erro_perc <- erro_m3/ymed*100
erro_m3_ha <- erro_m3*10000/areaparc

#MEDIA HECTARE
yha <- ymed*10000/areaparc
163/4.6     #IMA

#TOTAL FAZENDA
ytotal <- yha*areafaz
erro_m3_ha <- erro_m3_ha*areafaz

#INTERVALO CONFIANCA HECTARE
#precisa executar erro_m3_ha de novo
lit <- yha-erro_m3_ha
lst <- yha+erro_m3_ha

paste0('IC: ',round(lit,2), ' <= mu <= ', round(lst,2),'m³/ha (alpha=5%)')
