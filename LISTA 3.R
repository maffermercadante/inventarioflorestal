dados <- read.csv2('fustes.csv')
colnames(dados)[11] <- "hd"     #MUDAR NOME DA COLUNA DE HDOM PARA HD
View(dados)
names(dados)

cub <- read.csv2('cubagem.csv')
View(cub)

#HIPSOMETRIA--------------------------------------------------------------------
#MODELO GENERICO DE CURTIS
modelo <- "log(ht) ~  I(log(hd)) + I(1/dap)"

#PEGAR DAP MAIOR QUE ZERO E NENHUM NA
selarv <- subset(dados, !is.na(ht) & dap > 0)
View(selarv)

#APAGAR E RODAR LINHAS PRA NAO DAR ERRO (dados PRIMEIRO, modelo, selarv e ajuste)
ajhipso <- lm(modelo,selarv)

sumario <- summary(ajhipso)
sumario
options(digits=10)  

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

#COEFICIENTE DETERMINACAO
R2adj <- sumario$adj.r.squared 

#COEFICIENTE DETERMINACAO %
R2adjp <- round(R2adj * 100, 2)

#VOLUMETRIA---------------------------------------------------------------------
modelo1 <- "log(vcomsc) ~ I(log(dap)) + I(log(ht))"

#ARVORES CUBADAS
selcub <- subset(cub, !is.na(ht) & dap > 0 & ht > 0)

#AJUSTE
ajvol <- lm(modelo1, selcub)

#RESUMO
volume <- summary(ajvol)
coef(volume)
options(digits=10)  

#FURNIVAL
x <- cub$vcomsc
D(expression(log(x)),'x')
dx <- 1/x

medgeov <- exp(mean(log(dx)))

#INDICE FURNIVAL M
furnival <- 1/medgeov * volume$sigma

#INDICE FURNIVAL %
furnivalperc <- furnival/mean(x) * 100

#R2 AJUSTADO
r2 <- volume$adj.r.squared

#COEFICIENTE DETERMINACAO %
R2adjp_vol <- round(r2 * 100, 2)

#ESTATISTICAS POR PARCELA-------------------------------------------------------
#DADOS DE VOLUME POR PARCELA   PACOTE MAGRITTR
parc <- arv %>% group_by(talhao,area,parcela,areaparc) %>% summarize(vol = sum(vcomscest))

#VARIAVEL INTERESSE
z <- parc$vol

#MEDIA M3 POR PARCELA
zmed <- mean(z)

#VARIANCIA
zvar <- var(z)

#DESVIO PADRAO
zdesv <- sd(z)
#ou
zdesv <- sqrt(zvar)

#TAMANHO AMOSTRA
n <- length(z)

#AREA PARCELA
areaparc <- mean(parc$areaparc)

#AREA FAZENDA
talhao <- subset(parc, !duplicated(talhao), c("talhao", "area"))

areafaz <- sum(talhao$area)

#INTENSIDADE AMOSTRAL
ia <- areafaz/n

#PARCELAS CABIVEIS
N <- areafaz*10000/areaparc

#VARIANCIA MEDIA
zvarmed <- (zvar/n) * (1-n/N)

#ERRO PADRAO MEDIA
zdesvmed <- sqrt(zvarmed)

#ERRO INVENTARIO
erroinv <- qt(0.975,n-1) * zdesvmed

#ERRO INVENTARIO %
erroinvperc <- erroinv/zmed *100

#INTERVALO CONFIANCA
cat(paste(round(zmed - erroinv, 2), '<= total <=',
          round(zmed + erroinv, 2), 'm³/ha\n'));

#ESTATISTICAS POR HECTARE-------------------------------------------------------
#MEDIA POR HECTARE
zha <- zmed*(10000/areaparc)

#ERRO INVENTARIO POR HECTARE
erroinvha <- erroinv * (10000/areaparc)

#INTERVALO CONFIANCA
cat(paste(round(zha - erroinvha, 2),'<= total <=',
          round(zha + erroinvha, 2),'m³/ha\n'));

#ESTATISTICAS POR POVOAMENTO----------------------------------------------------
#TOTAL POPULACIONAL
ztot <- zha * areafaz

#ERRO INVENTARIO POR POLULACAO
erroinvtot <- erroinvha * areafaz

#INTERVALO CONFIANCA
cat(paste(round(ztot-erroinvtot,2), '<= total <=',
          round(ztot+erroinvtot,2), 'm³\n'))
