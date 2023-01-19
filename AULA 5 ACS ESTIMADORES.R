#ACS COM ESTIMADOR DE RAZAO PARA A MEDIA----------------------------------------

parc <- read.csv2('dados_raz_reg.csv')
View(parc)

#USO DE VARIAVEL X E Y

#AREA DA PARCELA DE M2 PARA HECTARE
x=parc$areaparc/10000
y=parc$vtcc

#GRAFICO
plot(x,y,xlab='área parcela',ylab='volume total com casca',pch='*',col='red',xlim=c(0,max(x)),ylim=c(0,max(y)))

#AJUSTE
aj <- lm(y~x)
summary(aj)

#BETA ZERO EH SIGNIFICATIVO TERIA QUE SER MENOR QUE 0,05 (AQUI ELE EH 0,5)

xme <- mean(x)
nt <- parc$larg_frag[1]/parc$larg_parc[1] #NUMERO TOTAL DE PARCELAS CABIVEIS
xm <- parc$areafrag[1]/nt

yme <- mean(y)

#ESTIMADOR DE RAZAO
R <- yme/xme
n <- length(x) #PARCELAS LANCADAS

#varr = variancia entre valores observados e os estimados pelo estimador de razao

varr <- (sum(y^2)-2*R*sum(y*x)+R^2*sum(x^2))/(n-1)

#MEDIA POR PARCELA (M³/PARCELA)
ymraz <- R*xm

#VARIANCIA DA MEDIA
varmedia <- (varr/n)*(1-n/nt)

#ERRO PADRAO DA MEDIA
erro_pad_media <- sqrt(varmedia)

#ERRO INVENTARIO
erro_inventario <- qt(0.975,n-1)*erro_pad_media

#ERRO INVERTARIO PERCENTUAL
erro_inv_perc <- erro_inventario/ymraz*100

#TOTAL POPULACIONAL M³
ytraz <- ymraz*nt

#TOTAL POPULACIONAL HECTARE
ymraz_ha <- ytraz/parc$areafrag[1]

#ACS COM ESTIMADOR DE REGRESSAO PARA A MEDIA------------------------------------
parc <- read.csv2('dados_raz_reg.csv')
View(parc)

#USO DE VARIAVEL X E Y

#AREA DA PARCELA DE M2 PARA HECTARE
x=parc$areaparc/10000
y=parc$vtcc

#GRAFICO
plot(x,y,xlab='área parcela',ylab='volume total com casca',pch='*',col='red',xlim=c(0,max(x)),ylim=c(0,max(y)))

#AJUSTE
aj <- lm(y~x)
lines(x,predict(aj))
summary(aj)

#BETA ZERO EH SIGNIFICATIVO TERIA QUE SER MENOR QUE 0,05 (AQUI ELE EH 0,5)

xme <- mean(x)
nt <- parc$larg_frag[1]/parc$larg_parc[1] #NUMERO TOTAL DE PARCELAS CABIVEIS
xm <- parc$areafrag[1]/nt

yme <- mean(y)

#ESTIMADOR DE REGRESSAO
R <- yme/xme
n <- length(x) #PARCELAS LANCADAS

b <- sum((y-yme)*(x-xme))/sum((x-xme)^2)
#OU
b <- coef(aj)[2]

#varyx = porcao da variancia de y que nao pode ser explicada pela relacao linear com x

varyx <- sum((y-yme-b*(x-xme))^2)/(n-2)   #PRA CONFERIR summary(aj)$sigma^2
#OU
varyz <- anova(aj)$'Mean Sq'[2]

#MEDIA POR PARCELA (M³/PARCELA)
ymreg <- yme+b*(xm-xme)

#VARIANCIA DA MEDIA
varmedia <- (varyx/n)*(1-n/nt)

#ERRO PADRAO DA MEDIA
erro_pad_media <- sqrt(varmedia)

#ERRO INVENTARIO
erro_inventario <- qt(0.975,n-1)*erro_pad_media

#ERRO INVERTARIO PERCENTUAL
erro_inv_perc <- erro_inventario/ymreg*100

#TOTAL POPULACIONAL M³
ytreg <- ymreg*nt

#TOTAL POPULACIONAL HECTARE
ymreg_ha <- ytreg/parc$areafrag[1]

