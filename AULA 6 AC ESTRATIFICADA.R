#AC ESTRATIFICADA
dados <- read.csv2('parcelas.csv')
View(dados)

names(dados)

dados$vary <- dados$vcom #RENOMEAR VARIAVEL
sig <- 0.05 #ALFA = 5%

#MEDIAS POR ESTRATO
estrato <- aggregate(list(areaest=dados$areaest, 
                          areaparc=dados$areaparc, 
                          ym=dados$vary),list(estrato=dados$estrato),mean)

#NUMERO DE PARCELAS POR ESTRATOS
calc <- aggregate(list(nj=dados$parc), list(estrato=dados$estrato),length)
estrato <- merge(estrato,calc)

#VARIANCIA E DESVIO PADRAO POR ESTRATO

#VARIANCIA
calc <- aggregate(list(s2y=dados$vary), list(estrato=dados$estrato),var)

#DESVIO PADRAO
calc$sy <- sqrt(calc$s2y)

estrato <- merge(estrato,calc)

#NUMERO DE PARCELAS CABIVEIS POR ESTRATO
estrato$Nj <- estrato$areaest*10000/estrato$areaparc

#VARIAVEL POPULACAO
estrato$populacao <- 1

#AREA, NUMERO DE AMOSTRAS E  NUMERO DE AMOSTRAS CABIVEIS NA POPULACAO
populacao <- aggregate(list(area=estrato$areaest, 
                            n=estrato$nj, 
                            N=estrato$Nj),list(populacao=estrato$populacao),sum)

#JUNTAR ESTRATO E POPULACAO PARA CALCULAR PESO DOS ESTRATOS
estrato <- merge(populacao,estrato)

#PESO DE CADA ESTRATO (SOMATORIO DE TODOS TEM QUE DAR 1)
estrato$pwj <- estrato$Nj/estrato$N
sum(estrato$pwj)

#MEDIA ESTRATIFICADA
calc <- aggregate(list(ymstr=estrato$ym*estrato$pwj), 
                  list(populacao=estrato$populacao),sum)

populacao <- merge(populacao,calc)

#VARIANCIA MEDIA ESTRATIFICADA
calc <- with(estrato,aggregate(list(calc1=Nj^2*s2y/nj, 
                                    calc2=Nj*s2y), 
                  list(populacao=populacao),sum))

populacao <- merge(populacao,calc)
populacao$s2ytr=with(populacao,(1/N^2)*(calc1-calc2))

#CALC SAO VALORES TEMPORARIOS E PODEM SER RETIRADOS
populacao$calc1 <- NULL
populacao$calc2 <- NULL

#DESVIO DA MEDIA ESTRATIFICADA
populacao$s2ytr=sqrt(populacao$s2ytr)

#GRAU DE LIBERDADE EFETIVO
estrato$gl <- with(estrato,Nj*(Nj-nj)/nj)

calc <- with(estrato,aggregate(list(calc1=gl*s2y, 
                                    calc2=(gl*s2y)^2/(nj-1)), 
                               list(populacao=populacao),sum))


calc$gle <- with(calc,calc1^2/calc2)

#CALC SAO VALORES TEMPORARIOS E PODEM SER RETIRADOS
calc$calc1 <- NULL
calc$calc2 <- NULL

populacao <- merge(populacao,calc)

#ERRO
populacao$errounid <- with(populacao,qt(1-sig/2,gle)*systr)

#ERRO %
populacao$erroperc <- with(populacao,errounid/ymstr*100)

#MEDIA DAS PARCELAS
calc <- aggregate(list(areaparc=estrato$areaparc), 
                  list(populacao=estrato$populacao),mean)

populacao <- merge(populacao,calc)

#LIMITE INFERIOR E SUPERIOR
populacao$li_ha <- with(populacao,(ymtr-errounid)*10000/areaparc)
populacao$li_ha <- with(populacao,(ymtr+errounid)*10000/areaparc)






