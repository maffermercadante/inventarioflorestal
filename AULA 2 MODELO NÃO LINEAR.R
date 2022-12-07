cub <- read.csv2("cubagem.csv")
View(cub)

ajuste=nls(formula='vicc~b0*dap^b1*ht^b2',start=list(b0=pi/40000*0.45,b1=2,b2=1), data=cub)

summary(ajuste)

syx <- summary(ajuste)$sigma #ERRO EM METRO CUBICO
syx <- syx/mean(cub$vicc)*100 #ERRO EM PORCENTAGEM

cub$vicc_est <- predict(ajuste)
cub$res <- cub$vicc-cub$vicc_est

res_padronizado <- cub$res/syx

#GRAFICO
with(cub,plot(vicc_est,res , col=5 , pch="°" , xlab='vicc_est [m³]' , ylab='resíduos [m³]'))
abline(h=0) #LINHA NO MEIO
plot(cub$vicc_est,res_padronizado , col=5 , pch="°" , xlab='vicc_est [m³]' , ylab='resíduos [m³]')

#LIMITES ERRO
tinf <- qt(0.025,nrow(cub)-1) #NROW EH n-1
tsup <- qt(0.975,nrow(cub)-1)

#LINHAS LIMITES
abline(h=tinf, lty=2)
abline(h=tsup, lty=2)

#OUTLIER
ii <- identify(cub$vicc_est,res_padronizado)

cub <- cub[-ii,] #SUBSTITUI OS PONTOS ANORMAIS

cub_sem_outlier <- cub[-ii,]
nrow(cub)
nrow(cub_sem_outlier)

ii <- abs(rstudent(ajuste))>tsup
ii <- (1:lenght(ii))[ii]

cub <- cub_sem_outlier
