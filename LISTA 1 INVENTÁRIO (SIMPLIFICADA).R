dados=read.csv2('cubagem.csv')
View(dados)
names(dados)

#BERKHOUT-----------------------------------------------------------------------
modelo<-'vcomsc ~ I(b0 * dap^b1)'

(ajnlin=nls(modelo,dados,start=list(b0=pi/40000*0.45,b1=2)))
coef(ajnlin)
(sumario<-summary(ajnlin))

#ERRO PADRAO RESIDUAL
(syx<-sumario$sigma)
#SYX PERENTUAL
(syxperc<-syx/mean(dados$vcomsc)*100)

cat(paste('Erro padrão residual: ', round(syxperc,2),'%',sep=''))

plot(dados$dap,dados$vcomsc,xlab='dap(cm)',ylab='vcomsc(m³)',pch='*',col='green')

points(dados$dap,predict(ajnlin), pch='.',col='red')

legend('topleft',legend=c('vcomsc_obs','vcomsc_est'), pch=c('*','*'),col=c('green','red'),bty='n', text.col=c('green','red'));

#VALORES ESTIMDOS
(dados$vcomsc_est=predict(ajnlin))
#OU
(bs=as.vector(coef(ajnlin)))
dados$vicc_est=with(dados,bs[1]*dap^bs[2]*ht^bs[3])

#RESIDUOS
dados$res=residuals(ajnlin)

x11()
par(mfrow=c(2,2))
library(fBasics)
qqnormPlot(dados$res)
#qqnorm(dados$res)

dados$res_padronizado<-dados$res/sumario$sigma

with(dados,plot(vcomsc_est,res_padronizado,pch='.',xlab='Volume estimado (m³)',ylab='Resíduos padronizados',col='red',ylim=c(-2.5,2.5)))

abline(h=0)

#INVERSA DISTRIBUICAO T
(tinf<-qt(0.025,nrow(dados)-1))
#t(alpha=5 e [n-1] gl)
(tsup<-qt(0.975,nrow(dados)-1))   

abline(h=tinf,lty=2)
abline(h=tsup,lty=2)

#INVERSA DISTRIBUICAO T
(tinf<-qt(0.005,nrow(dados)-1)) 
#t(alpha=5 e [n-1] gl)
(tsup<-qt(0.995,nrow(dados)-1)) 

abline(h=tinf,lty=2, col='blue')
abline(h=tsup,lty=2, col='blue')

#SPURR--------------------------------------------------------------------------
modelo1<-'vcomsc ~ I(dap^2 * ht)'

ajuste=lm(formula=modelo1,data= dados)

summary(ajuste)
coef(ajuste)

#ERRO PADRO RESIDUAL
syx<-summary(ajuste)$sigma 
#SYX PERCENTUAL
syxperc<-syx/mean(dados$vcomsc)*100 

anova(ajuste)

cat(paste('Erro padrão residual: ', round(syxperc,2),'%',sep=''))

sumario_spurr <- summary(ajuste)

R2adj_spurr <- round(sumario_spurr$adj.r.squared, digits = 2)


plot(dados$dap,dados$vcomsc, xlab='dap(cm)',ylab='vcomsc(m³)', pch='*',col='green')

points(dados$dap,predict(ajuste), pch='.',col='red')

legend('topleft',legend=c('vcomsc_obs','vcomsc_est'), pch=c('*','*'),col=c('green','red'),bty='n',text.col=c('green','red'))

#VALORES ESTIMADOS
(dados$vcomsc_est=predict(ajuste))

#RESIDUOS
dados$res=residuals(ajuste)

x11()
par(mfrow=c(2,2))
library(fBasics)
qqnormPlot(dados$res)
#qqnorm(dados$res)

dados$res_padronizado<-dados$res/sumario_spurr$sigma;

with(dados,plot(vcomsc_est,res_padronizado,pch='.', xlab='Volume estimado (m³)',ylab='Resíduos padronizados',col='red',ylim=c(-2.5,2.5)))

abline(h=0)

#INVERSA DISTRIBUICAO T
(tinf<-qt(0.025,nrow(dados)-1))
#t(alpha=5 e [n-1] gl) 
(tsup<-qt(0.975,nrow(dados)-1))

abline(h=tinf,lty=2)
abline(h=tsup,lty=2)

#INVERSA DISTRIBUICAO T
(tinf<-qt(0.005,nrow(dados)-1))
#t(alpha=5 e [n-1] gl)
(tsup<-qt(0.995,nrow(dados)-1))  

abline(h=tinf,lty=2, col='blue')
abline(h=tsup,lty=2, col='blue')

#SPUR LOG-----------------------------------------------------------------------
modelo2<-'log(vcomsc) ~ I(log(dap^2 * ht))'

ajuste=lm(formula=modelo2,data= dados)

summary(ajuste)
coef(ajuste)

#ERRO PADRAO RESIDUAL
syx<-summary(ajuste)$sigma 
#SYX PERCENTUAL
syxperc<-syx/mean(dados$vcomsc)*100 

y<-dados$vcomsc
D(expression(log(y)),'y')
dy<-1/y
#MEDIA GEOMETRICA
medgeo<-exp(mean(log(dy)))
#medgeo<-prod(dy)^(1/length(dy))

#M3
ind_furnival<-1/medgeo*syx
#PORCENTAGEM
ind_furnival_percentual<-ind_furnival/mean(dados$vcomsc)*100

anova(ajuste)

cat(paste('Erro padrão residual: ', round(syxperc,2),'%',sep=''))

sumario_spurr <- summary(ajuste)

R2adj_spurr <- round(sumario_spurr$adj.r.squared, digits = 2)


plot(dados$dap,dados$vcomsc,xlab='dap(cm)',ylab='vcomsc(m³)', pch='*',col='green')

points(dados$dap,predict(ajuste),pch='.',col='red')

legend('topleft',legend=c('vcomsc_obs','vcomsc_est'),pch=c('*','*'),col=c('green','red'),bty='n',text.col=c('green','red'))

#VALORES ESTIMADOS
(dados$vcomsc_est=predict(ajuste))

#RESIDUOS
dados$res=residuals(ajuste)

x11()
par(mfrow=c(2,2))
library(fBasics);
qqnormPlot(dados$res)
#qqnorm(dados$res)

dados$res_padronizado<-dados$res/sumario_spurr$sigma

with(dados,plot(vcomsc_est,res_padronizado,pch='.',xlab='Volume estimado (m³)',ylab='Resíduos padronizados',col='red',ylim=c(-2.5,2.5)))

abline(h=0)

#INVERSA DISTRIBUICAO T
(tinf<-qt(0.025,nrow(dados)-1))
#t(alpha=5 e [n-1] gl)
(tsup<-qt(0.975,nrow(dados)-1))  

abline(h=tinf,lty=2)
abline(h=tsup,lty=2)

#INVERSA DISTRIBUICAO T
(tinf<-qt(0.005,nrow(dados)-1))
#t(alpha=5 e [n-1] gl)
(tsup<-qt(0.995,nrow(dados)-1)) 

abline(h=tinf,lty=2, col='blue')
abline(h=tsup,lty=2, col='blue')


#SCHUMACHER & HALL--------------------------------------------------------------
modelo3 <- 'vcomsc~b0*dap^b1*ht^b2'

ajuste=nls(formula='vcomsc~b0*dap^b1*ht^b2', start=list(b0=pi/40000*0.45,b1=2,b2=1),data=dados)

summary(ajuste)
coef(ajuste)

#ERRO ADRAO RESIDUAL
syx<-summary(ajuste)$sigma 
#SYX PERCENTUAL
syxp<-syx/mean(dados$vcomsc)*100 

dados$vcomsc_est<-predict(ajuste)
dados$res<-dados$vcomsc-dados$vcomsc_est
#OU
dados$res<-residuals(ajuste)

#identical(dados$res,residuals(ajuste))

graphics.off()
plot(x = dados$vcomsc_est,y = dados$res,col='red',pch='.',xlab='Volume estimado [m³]',ylab='Resíduos padronizados [m³]')
abline(h=0)

res_padronizado<-dados$res/syx

x11()
par(mfrow=c(2,2))
library(fBasics)
qqnormPlot(dados$res)

plot(x = dados$vcomsc_est,y = res_padronizado,col='red',pch='.',xlab='Volume estimado (m³)',ylab='Resíduos padronizados')
     
abline(h=0)

tinf<-qt(0.025,nrow(dados)-1)
tsup<-qt(0.975,nrow(dados)-1)

abline(h=tinf, lty=2)
abline(h=tsup, lty=2)

#SCHUMACHER & HALL LOG---------------------------------------------------------- 
modelo4 <- 'I(log(vcomsc))~I(log(dap))+I(log(ht))'

ajuste=lm(formula='I(log(vcomsc))~I(log(dap))+I(log(ht))', data=dados)

summary(ajuste)
coef(ajuste)

#ERRO PADRAO RESIDUAL
syx<-summary(ajuste)$sigma

y<-dados$vcomsc
D(expression(log(y)),'y')
dy<-1/y
#MEDIA GEOMETRICA
medgeo<-exp(mean(log(dy)))
#medgeo<-prod(dy)^(1/length(dy))

#M3
ind_furnival<-1/medgeo*syx
#PORCENTAGEM
ind_furnival_percentual<-ind_furnival/mean(dados$vcomsc)*100; 

sumario_spurr <- summary(ajuste)

R2adj_spurr <- round(sumario_spurr$adj.r.squared, digits = 2)


anova(ajuste)

dados$vcomsc_est<-exp(predict(ajuste))
dados$res<-dados$vcomsc-dados$vcomsc_est

graphics.off()
plot(x = dados$vcomsc_est,y = dados$res,col='red',pch='.',xlab='Volume estimado [m³]',ylab='Resíduos padronizados [m³]')

abline(h=0)

res_padronizado<-dados$res/ind_furnival

x11();
par(mfrow=c(2,2))
library(fBasics)
qqnormPlot(dados$res)
plot(x = dados$vcomsc_est,y = res_padronizado,col='red',pch='.',xlab='Volume estimado (m³)',ylab='Resíduos padronizados')

abline(h=0)

tinf<-qt(0.025,nrow(dados)-1)
tsup<-qt(0.975,nrow(dados)-1)

abline(h=tinf, lty=2)
abline(h=tsup, lty=2)
#TAKATA-------------------------------------------------------------------------

#LINEARIZACAO PARA OBTER BETAS
takata <- 'I((dap^2*ht)/vcomsc)~(dap)'
ajnlin <- lm(takata,dados)
summary(ajnlin)
coef(ajnlin)

modelo5 <- 'vcomsc ~ I((dap^2 * ht) / (b0 + b1 * dap))'

#colocar os valores encontrados na linearizacao nos b0 e b1
(ajnlin=nls(modelo5,dados,start=list(b0 = 32397.89550, b1 = 92.70418)))
coef(ajnlin)
(sumario<-summary(ajnlin))

#ERRO PADRO RESIDUAL
(syx<-sumario$sigma)
#SYX PERCENTUAL
(syxperc<-syx/mean(dados$vcomsc)*100)

cat(paste('Erro padrão residual: ', round(syxperc,2),'%',sep=''))

plot(dados$dap,dados$vcomsc,xlab='dap(cm)',ylab='vcomsc(m³)',pch='*',col='green')

points(dados$dap,predict(ajnlin), pch='.',col='red')

legend('topleft',legend=c('vcomsc_obs','vcomsc_est'),pch=c('*','*'),col=c('green','red'),bty='n',text.col=c('green','red'))

#VALORES ESTIMADOS
(dados$vcomsc_est=predict(ajnlin))
#OU
(bs=as.vector(coef(ajnlin)))
dados$vicc_est=with(dados,bs[1]*dap^bs[2]*ht^bs[3])

#RESIDUOS
dados$res=residuals(ajnlin)

x11()
par(mfrow=c(2,2))
library(fBasics)
qqnormPlot(dados$res)
#qqnorm(dados$res)

dados$res_padronizado<-dados$res/sumario$sigma

with(dados,plot(vcomsc_est,res_padronizado,pch='.',xlab='Volume estimado (m³)',ylab='Resíduos padronizados',col='red',ylim=c(-2.5,2.5)))

abline(h=0)

#INVERSA DITRIBUICAO T
(tinf<-qt(0.025,nrow(dados)-1))
#t(alpha=5 e [n-1] gl)
(tsup<-qt(0.975,nrow(dados)-1))  

abline(h=tinf,lty=2)
abline(h=tsup,lty=2)

#INVERSA DISTRIBUICAO T
(tinf<-qt(0.005,nrow(dados)-1))
#t(alpha=5 e [n-1] gl)
(tsup<-qt(0.995,nrow(dados)-1))

abline(h=tinf,lty=2, col='blue')
abline(h=tsup,lty=2, col='blue')