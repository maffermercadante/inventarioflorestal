#AMOSTRAGEM CASUAL SIMPLES HIPSOMETRIA GENERICA (CURTIS)

dados <- read.csv2('fustes.csv')
View(dados)
names(dados)

dados[is.na(dados)] <- 0

#MODELO GENERICO DE CURTIS
modelo <- "log(ht)~I(1/dap)+I(log(hd))"

#PEGAR DAP MAIOR QUE ZERO E NENHUM NA
selarv <- subset(dados, !is.na(ht) & dap > 0)
View(selarv)

#APAGAR E RODAR LINHAS PRA NAO DAR ERRO (dados PRIMEIRO, modelo, selarv e ajuste)
ajhipso <- lm(modelo,selarv)

sumario <- summary(ajhipso)
sumario

y <- dados$ht[is.na(dados$ht)]
#OU
y <- selarv$ht

#DERIVADA
D(expression(log(y)), 'y')
dy <- 1/y

#MEDIA GEOMETRICA   PARENTESES ANTES JA RODA DIRETO
(medgeo <- exp(mean(log(dy))))

#sumario$sigma ERRO PADRAO RESIDUAL

#INDICE FURNIVAL
(IF <- 1/medgeo*sumario$sigma)

#PERCENTUAL
IFperc <- IF/mean(y)*100

#ESTIMANDO ALTURAS TOTAIS
(bs <- as.vector(coef(ajhipso)))
dados$htest <- exp(bs[1]+bs[2]/dados$dap+bs[3]*log(dados$hd))
#OU
dados$htest <- with(dados,exp(bs[1]+bs[2]/dap+bs[3]*log(hd)))

#HTEST EH ESTIMADA E HTRE EH RELATIVA

#dados$htest[dados$dap==0|is.na(dap)]<-0]
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










