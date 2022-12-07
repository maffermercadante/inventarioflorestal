cub <- read.csv2("cubagem.csv")
View(cub)

ajuste=lm(formula='I(log(vicc))~I(log(dap))+I(log(ht))', data=cub)

summary(ajuste)

syx <- summary(ajuste)$sigma #ERRO EM METRO CUBICO

y <- cub$vicc
D(expression(log(y)),'y') #DERIVADA
dy <- 1/y

medgeo <- exp(mean(log(dy))); #MEDIA GEOMETRICA

ind_furnival <- 1/medgeo*syx #M³
ind_furnival_percentual <- ind_furnival/mean(cub$vicc)*100; #%

cub$vicc_est <-exp(predict(ajuste))

cub$res <- cub$vicc-cub$vicc_est

res_padronizado <- cub$res/ind_furnival

#GRAFICO
with(cub,plot(vicc_est,res , col=5 , pch="°" , xlab='vicc_est [m³]' , ylab='resíduos [m³]'))
abline(h=0) #LINHA NO MEIO
plot(cub$vicc_est,res_padronizado , col=5 , pch="°" , xlab='vicc_est [m³]' , ylab='resíduos [m³]')

#LIMITES ERRO
tinf <- qt(0.025,nrow(cub)-1) #NROW EH n-1
tsub <- qt(0.975,nrow(cub)-1)

#LINHAS LIMITES
abline(h=tinf, lty=2)
abline(h=tsub, lty=2)

#OUTLIER
ii <- identify(cub$vicc_est,res_padronizado)

cub <- cub[-ii,] #SUBSTITUI OS PONTOS ANORMAIS

cub_sem_outlier <- cub[-ii,]
nrow(cub)
nrow(cub_sem_outlier)

ii <- abs(rstudent(ajuste))>tsup
ii <- (1:lenght(ii))[ii]

cub <- cub_sem_outlier
