#DADOS--------------------------------------------------------------------------

parc <- read.csv2("parcelas.csv")

#COLUNAS E VARIAVEIS-------------------------------------------------------------

#ARREA PARCELA M2
parc$area_parc <- 400
area_parc <- 400

#ARE POVOAMENTO 1 ROTACAO HECTARE
parc$area_faz <- 327
area_faz <- 327

#VOLUME TOTAL
vtcc <- 49081

#ESTATISTICAS 5% de sign. 1 ROTACAO---------------------------------------------

#VARIAVEL INTERESSE
y <- parc$vcomcc1

#PARCELAS CABIVEIS DENTRO DA POPULACAO
N <- (area_faz * 10000) / area_parc

#TABELA ESTATISTICAS
estatisticas_acs <- as.data.frame(cmrinvflor::estats_acs(vy = y, sig =5, nt = N))

#TAMANHO AMOSTRA
n <- length(y)

#VAR Y
yvar <- var(y)

#VARIANCIA MEDIA
yvarmed <- (yvar/n) * (1 - n/N)

#MEDIA POR HECTARE 
yha <- estatisticas_acs$ymed * 10000 / area_parc

#ERRO INVENTARIO HECTARE
erroinvha <- estatisticas_acs$eunid * 10000 / area_parc 

#TOTAL POPULACIONAL E VOLUME COMERCIAL
ytot <- yha * area_faz;

#ERRO INVENTARIO PARA A POPULACAO 
erroinvpop <- erroinvha * area_faz

#DIFERENCA PERCENTUAL ENTRE VOLUME ENTREGUE E VOLUME ESTIMADO
acuracia <- ((vtcc-ytot)/vtcc) * 100

#INTERVALO CONFIANCA POPULACAO
cat(paste(round(ytot-erroinvpop,0), " m3", "<= total populacional <=", round(ytot+erroinvpop,0), " m3", sep=''))

#SEGUNDA ROTACAO----------------------------------------------------------------
x <- parc$vcomcc1
y <- parc$vcomcc2

#GRAFICO
plot(x, y,
     xlim = c(0,max(x)), 
     ylim = c(0,max(y)),
     xlab = "área da parcela (ha)",
     ylab = "volume (m3)",
     pch = "*",
     col = "red")

ajlin <- lm(y ~ x)
lines(x, predict(ajlin))
summary(ajlin)

# b0 não foi significativo, e apesar de crescente, a reta passa pela origem,
# portanto utilizarei a acs estimador de razão.

#MEDI ESTIMADA VARIAVEL AUXILIAR (x)
xme <- mean(x)

#MEDIA POPULACIONAL VARIAVEL AUXILIAR
xm <- vtcc/N

#MEDIA ESTIMADA VARIAVEL INTERESSE (y)
yme <- mean(y)

n <-length(y)

#ESTIMADOR RAZAO
R <- yme/xme 

#varr=variancia entre valores observados e os estimados
#     pelo estimador de razao
varr <- (sum( y^2 ) - 2 * R * sum(y * x) + R^2 * sum( x^2))/(n - 1)

#CALCULOS COM ESTIMADOR DE RAZAO ---------------------------------------

#MEDIA POR PARCELA
ymraz <- R * xm

#VARIANCIA DA MEDIA ESTIMADA
varmedia <- (1 - n / N)* (1 / n) * varr

#ERRO PADRAO MEDIA
erro_pad_media <- sqrt(varmedia)

#ERRO INVENTARIO M3
erro_inv <- qt(0.975, n - 1) * erro_pad_media

#ERRO INVENTARIO %
erro_inv_perc <- erro_inv/ ymraz * 100

#TOTAL POPULACIONAL M3
ytraz <- ymraz * N 
ext_ha <- 10000/parc$area_parc[1]
erroinvtot <- erro_inv * ext_ha * parc$area_faz[1] 

#INTERVALO CONFIANCA POPULACAO
cat(paste(round(ytraz - erroinvtot, 0),'<= total <=', round(ytraz + erroinvtot, 0),'m³\n'))
