cub <- read.csv2("cubagem.csv")
View(cub)

#FAZER GRAFICO
cor(cub[,c("dap" , "ht" , "vicc")])
cub$dap2ht <- cub$dap^2*cub$ht
cor(cub[,c("dap" , "ht" , "dap2ht" , "vicc")])

par(mfrow=c(1,3)) #RODAR TODOS OS GRAFICOS JUNTOS, SELECIONAR TUDO

with(cub,plot(dap,vicc, col=2 , pch="*"))

with(cub,plot(ht,vicc, col=3 , pch="*"))

# MELHOR MODELO
with(cub,plot(dap2ht,vicc, col=4 , pch="*"))

#AJUSTE DO MODELO
ajuste=lm(formula='vicc~I(dap^2*ht)', data=cub)

summary(ajuste)
syx <- summary(ajuste)$sigma #ERRO EM METRO CUBICO
syxp <- syx/mean(cub$vicc)*100 #ERRO EM PORCENTAGEM

anova(ajuste)

#COMO ESCOLHER O MELHOR MODELO?
#VERIFICAR GRAFICO DE RESIDUOS
#VERIFICAR MENOR ERRO RESIDUAL
#ANALISE DE VARIANCIA EM SUMMARY

bs <- coef(ajuste)
cub$vicc_est <- with(cub,bs[1]+bs[2]*dap^2*ht) #RODAR O CUB
cub$vicc_est <- predict(ajuste)

cub$res <- cub$vicc-cub$vicc_est #RODAR O CUB
cub$res <- residuals(ajuste)


#GRAFICOS
with(cub,plot(vicc_est,res , col=5 , pch="°" , xlab='vicc_est [m³]' , ylab='resíduos [m³]'))

abline(h=0) #LINHA NO MEIO

plot(cub$vicc_est,rstudent(ajuste) , col=5 , pch="°" , xlab='vicc_est [m³]' , ylab='resíduos [m³]')

#LIMITES ERRO
tinf <- qt(0.025,nrow(cub)-1) #NROW É n-1
tsup <- qt(0.975,nrow(cub)-1)

#LINHAS LIMITES
abline(h=tinf, lty=2)
abline(h=tsup, lty=2)

