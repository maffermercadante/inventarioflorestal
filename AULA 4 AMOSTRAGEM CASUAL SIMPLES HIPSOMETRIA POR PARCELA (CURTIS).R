#AMOSTRAGEM CASUAL SIMPLES HIPSOMETRIA POR PARCELA (CURTIS)

dados <- read.csv2('fustes.csv')
View(dados)
names(dados)

hipso <- data.frame(parcela=unique(dados$parcela), bo=NA, b1=NA)
hipso

for(i in 1:nrow(hipso)){
  selarv <- subset(dados,
  parcela==hipso$parcela[i]&
  dap>0&!is.na(dap)&
   ht>0&!is.na(ht))}

ajhipso <- lm("log(ht)~I(1/dap)", selarv)

bs <- as.vector(coef(ajhipso))

hipso$bo <- bs[1]
hipso$b1 <- bs[2]

dados <- merge(dados,hipso,by="parcela")
View(dados)

#ESTIMAR ALTURAS
dados$htest <- with(dados,exp(bo+b1/dap))

#GRAFICOS
x11()
hist(dados$htest[dados$htest>0], col='red')
hist(dados$ht[dados$ht>0], col='blue', add=T)

x11()
with(dados,plot(ht~dap,xlab='DAP (cm)',ylab='Altura Total (m)', pch='*', col='green'))
with(dados,points(htest~dap,xlab='DAP (cm)',ylab='Altura Total Estimada (m)', pch='*', col='red'))

dados$htre <- dados$ht
ii <- is.na(dados$ht)|dados$ht==0
dados$htre[ii] <- dados$htest[ii]

#ESTIMAR VOLUME     VALORES COM BETAS AJUSTADOS DE VOLUME (COEFICIENTES)
dados$vicc <- with(dados,6.436159e-05*dap^1.852143*htre^9.530665e-01)

#TABELA NOVA
parc <- aggregate(list(vol=dados$vicc), 
                  list(fazenda=dados$fazenda, 
                       talhao=dados$talhao, 
                       areatal=dados$areatal, 
                       parcela=dados$parcela, 
                       areaparc=dados$areaparc),sum)
View(parc)

#AULA ANTERIOR AMOSTRAGEM CASUAL SIMPLES----------------------------------------
#TABELA NOVA
parc <- aggregate(list(vcomcc=dados$vicc), 
                  list(fazenda=dados$fazenda, 
                       talhao=dados$talhao, 
                       areatal=dados$areatal, 
                       parcela=dados$parcela, 
                       areaparc=dados$areaparc),sum)
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
talhao <- subset(parc,!duplicated(talhao), c('talhao','areaparc'))
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







