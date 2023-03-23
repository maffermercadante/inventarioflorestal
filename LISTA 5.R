areafaz <- 22

total_arvores <- 14725 + 17100

dados <- data.frame(estrato = c(rep("A", 5), rep("B", 5)),
                    area = c(rep(12, 5), rep(10, 5)),
                    parcela = c(1:10),
                    area_parcela = 285,
                    n_arv_parc = c(35, 34, 33, 34, 33,50, 53, 49, 48, 53),
                    volume = c(3.60, 3.79, 3.79, 3.40, 3.36,6.19, 6.29, 5.80, 5.64, 5.59))

areaparc <- dados$area_parcela[1]

#VARIAVEL DE INTERESSE
y <- dados$volume

N <- areafaz*10000/areaparc

#A)AMOSTRAGEM CASUAL SIMPLES---------------------------------------------------

estatisticas_acs <- as.data.frame(cmrinvflor::estats_acs(vy = y, nt = N, sig = 3))

#ERRO
estatisticas_acs$eperc

#MEDIA POR HECTARE (m3/ha) 
yha <- estatisticas_acs$ymed * 10000 / areaparc

#ERRO INVENTARIO POR HECTARE (m3/ha)
erroinvha <- estatisticas_acs$eunid * 10000 / areaparc 

#TOTAL POPULACIONAL (m3/fazenda: total) #VOLUME COMERCIAL
ytot <- yha * areafaz;

#ERRO INVENTARIO POPULACAO (m3/fazenda) 
erroinvpop <- erroinvha * areafaz

#INTERVALO CONFIANCA POPULACAO(m3)
cat(paste(round(ytot-erroinvpop,0), " m3", "<= total populacional <=", 
          round(ytot+erroinvpop,0), " m3", sep=''))


#B)ESTIMADOR DE RAZAO ACS-------------------------------------------------------

#VARIAVEL AUXILIAR = NUMERO ARVORES POR PARCELA
x <- dados$n_arv_parc

#GRAFICO
plot(x, y,
     xlim = c(0, max(x)), 
     ylim = c(0, max(y)),
     xlab = "área da parcela (ha)",
     ylab = "volume (m3)",
     pch = "*",
     col = "red")

ajlin <- lm(y ~ x)
lines(x, predict(ajlin))
summary(ajlin)

# b0 não foi significativo, reta crescente, e passa pela origem,
# portanto utilizarei a acs estimador de razão.

#MEDIA ESTIMADA DA VARIAVEL AUXILIAR (x)
xme <- mean(x)

#MEDIA POPULACIONAL VARIAVEL AUXILIAR
xm <- total_arvores/N

#MEDIA ESTIMADA DA VARIAVEL DE INTERESSE (y)
yme <- mean(y)

n <-length(y)

#ESTIMADRO DE RAZAO
R <- yme/xme 

#varr=Variância entre valores observados e os estimados pelo estimador de razão
varr <- (sum( y^2 ) - 2 * R * sum(y * x) + R^2 * sum( x^2))/(n - 1)

#CALCULOS COM ESTIMADOR DE RAZAO

#MEDIA POR PARCELA (m3/parc)
ymraz <- R * xm

#VARIANCIA DA MEDIA ESTIMADA
varmedia <- (1 - n / N)* (1 / n) * varr

#ERRO PADRAO DA MEDIA
erro_pad_media <- sqrt(varmedia)

#ERRO DO INVENTARIO (m3/parc) - alpha de 3%:
erro_inv <- qt(0.985, n - 1) * erro_pad_media

#ERRO INVENTARIO (%)
erro_inv_perc <- erro_inv/ ymraz * 100

#TOTAL POPULACIONAL (m3)
ytraz <- ymraz * N 
ext_ha <- 10000/dados$area_parc[1]
erroinvtot <- erro_inv * ext_ha * areafaz

#INTERVALO CONFIANCA POPULACAO
cat(paste(
  round(ytraz - erroinvtot, 0),'<= total <=', 
  round(ytraz + erroinvtot, 0),'m³\n'))

#C)AMOSTRAGEM CASUAL ESTRATIFICADA --------------------------------------------------------

#DUPLICANDO VALORES
dados$vary <- dados$volume 

#alpha = 3%
sig <- 0.03

#MEDIAS POR ESTRATO
estrato <- with(dados,aggregate(
                  list(areaest = area,areaparc = area_parcela,ym = vary),
                  list(estrato = estrato),mean))

#NUMEROS DE PARCELA POR ESTRATO
calc <- with(dados,aggregate(
               list(anj = area_parcela),
               list(estrato = estrato),length))

estrato <- merge(estrato, calc)

#VARIANCIA E DESVIO PADRAO POR ESTRATO
calc <- with(dados,aggregate(
               list(s2y = vary),
               list(estrato = estrato),var))

calc$sy <- sqrt(calc$s2y)

estrato <- merge(estrato, calc)

#NUMERO DE PARCELAS CABIVEIS POR ESTRATO
estrato$pnj <- with(estrato, areaest*10000/areaparc)

#NOMEANDO A POPULACAO
estrato$populacao <- 1

#AREA, NUMERO DE AMOSTRAS E NUMERO DE AMOSTRAS CABIVEIS NA POPULACAO
populacao <- with(estrato,aggregate(
                    list(area = areaest,an = anj,pn = pnj),
                    list(populacao = populacao),sum))

estrato <- merge(populacao, estrato)

#PESO DE CADA ESTRATO
estrato$pwj <- with(estrato, pnj/pn)

#CALCULO MEDIA ESTRATIFICADA
calc <- with(estrato,aggregate(
               list(ymstr = pwj*ym),
               list(populacao = populacao),sum))

populacao <- merge(populacao, calc)

#VARIANCIA DA MEDIA ESTRATIFICADA
calc <- with(estrato,aggregate(
               list(calc1 = (pwj^2)*s2y/anj, 
                    calc2 = (pwj*s2y)/pn),
               list(populacao = populacao),sum))

calc$s2ystr<- calc$calc1 - calc$calc2
calc$calc1 <- NULL
calc$calc2 <- NULL

populacao <- merge(populacao, calc)

#CALCULO GRAU DE LIBERDADE EFETIVO
estrato$calcgl <- with(estrato, pnj * (pnj - anj)/anj)

calc <- with(estrato,aggregate(
               list(calc1 = calcgl * s2y,calc2 = (calcgl*s2y)^2 / (anj - 1)),
               list(populacao = populacao),sum))

calc$gle <- with(calc, calc1^2/calc2)
calc$calc1 <-NULL
calc$calc2 <-NULL

populacao <- merge(populacao, calc)
populacao$systr <- sqrt(populacao$s2ystr)

populacao$errounid <- with(populacao, qt(1-sig/2,gle)*systr)
populacao$erroperc <- with(populacao, errounid/ymstr*100)
populacao$erroperc

total <- with(populacao, ymstr * pn)
etotal <- with(populacao, errounid * pn)

#INTERVALO DE CONFIANCA POPULACAO
litot <- total - etotal
lstot <- total + etotal

print(paste('IC:',round(litot),'<=T<=',round(lstot),'m³',sep=''))

estrato <- subset(dados,
                  !duplicated(estrato),
                  select = c("estrato", "area"))
amostra <- dados[, c('estrato', 'parcela', 'area_parcela', 'vary')]

#ERRO EPERC DO ACE
ace <- as.data.frame(cmrinvflor::estats_ace(estrato, amostra, sig=sig*100, fc_dim=1/10000))
totace <- with(ace, ymstr*np)
errototace <- with(ace, eunid*np)
litotace <- totace-errototace
lstotace <- totace+errototace

print(paste('IC:',round(litotace),'<=T<=',round(lstotace),'m³',sep=''))
