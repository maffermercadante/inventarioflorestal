library(rgdal);
library(raster);
library(geoR);

#ARVQUIVOS DE ENTRADA
shpplt<-shapefile('plantio.shp'); #ABRINDO ARQUIVO VETORIAL

#shpplt<-shapefiles::read.shp('plantio.shp')

#TRANSFORMAÇÃO DATUM
shpplt<-spTransform(shpplt,CRS('+init=epsg:31982'))

#GRAFICO
graphics.off(); x11(); 
plot(shpplt);

areafaz<-sum(area(shpplt))/10000; #AREA FAZENDA M2

proj4string(shpplt) #GEORREFERENCIAMENTO

#RESULTADOS POR PARCELA
invparc<-read.csv2('invparc.csv', stringsAsFactors = F);
invparc<-invparc[,c('x','y','vcom')];
coordinates(invparc)<-~x+y; #DETERMINAR COORDENADAS
#proj4string(invparc) <- CRS('+init=epsg:4326'); #ADICIONANDO PROJECAO
#invparc<-spTransform(invparc,CRS('+init=epsg:31983'))

plot(invparc,add= TRUE)
points(invparc, col='red', pch=16);

#INFORMACOES GERAIS
resolucao<-10;

#CONVERSAO DOS DADOS PARA O FORMATO GEODATA
vgeo<-as.geodata(invparc);

#CRIACAO RASTER AREA DE INTERESSE
rasterplt<-raster(shpplt,resolution=resolucao);
rasterplt<-rasterize(shpplt,rasterplt);
reskrige<-as.data.frame(rasterToPoints(rasterplt));

#ANALISE VARIOGRAFICA
svar<-variog(vgeo);

graphics.off(); 
plot(svar, xlab='distância (m)', ylab='Semivariância(m^6)');

svar<-variog(vgeo, max.dist = 1500); #DEPOIS RODAR GRAFICO DE NOVO

#AJUSTE COM MINIMOS QUADRADOS
tau<-0; #EFEITO PEPITA
sigma<-1500-tau;#CONTRIBUICAO (VALOR MAX - TAU)
phi<-800; #DISTANCIA

ajuste<-variofit(svar,ini=c(sigma,phi), nugget=tau, cov.model = 'exp');
lines(ajuste);

#KRIGAGEM CONVENCIONAL
kvgeo<-krige.conv(vgeo,loc=as.matrix(reskrige[,1:2]),krige=krige.control(obj=ajuste));
reskrige$vcom<-kvgeo$predict;

#EXPORTANDO DOS RESULTADOS DA KRIGAGEM
coordinates(reskrige)=~x+y;
gridded(reskrige)<-T;
reskrige$layer<-NULL;
proj4string(reskrige) <- proj4string(shpplt); #ADICIONANDO PROJECAO
reskrige <- raster(reskrige); 
writeRaster(reskrige,'reskrige.tif',"GTiff", overwrite=TRUE);

#EXIBICAO DO MAPA
reskrige <- mask(reskrige, shpplt, inverse=FALSE) #RECORTANDO O RASTER COM O PLANTIO
graphics.off();
plot(reskrige, legend=TRUE, asp=1, main="KRIGAGEM: VCOM(m³/ha)");
plot(shpplt,lwd=2,border='darkblue',col='transparent',add=TRUE,asp=1);
points(invparc, pch = '+', cex=0.6)

#sp::compassRose(identify(reskrige,plot=F));
sp::compassRose(x=416500, y=7678500,cex=0.5)