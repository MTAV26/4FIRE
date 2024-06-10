#script 1

rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(RColorBrewer)
library(maps)
library(oce)
library(terra)
library(rworldmap)
library(rnaturalearth)
library(fields)
library(maptools)
library(RColorBrewer)
library(viridis)
library(verification)
data(wrld_simpl)

dir_oss ="C:/Users/Usuario/Dropbox/4FIRE_V2/data/"
dir_results ="C:/Users/Usuario/Dropbox/4FIRE_V2/results/"
dir_out="C:/Users/Usuario/Dropbox/4FIRE_V2/plots/"
dir_mask="C:/Users/Usuario/Dropbox/4FIRE/data/mask_season/"
# source('C:/Users/Usuario/Dropbox/4FIRE/script/model_BA/image_mask.R')
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "inout.RData"))

data(coastlineWorld)
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

####################################################
# Mascara de Oceanos
####################################################
ocean <- rnaturalearth::ne_download(scale = 10, type = 'ocean', category = 'physical', returnclass = "sf")
ocean <- sf::st_transform(ocean, crs = crs)

worldmap <- rworldmap::getMap(resolution = "coarse") # Load countries 
raster::crs(worldmap)
worldmap <- sp::spTransform(worldmap,crs) # Project to Robinson

lati <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135,-90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lati, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lati, proj = crs) # graticules 



## fix parameters
## fix parameters
years = 2001:2020 #antes en el original: 2001:2019
# num_ens = 25
# seasons = c("DJF","MAM","JJA","SON")
season = c("JJA")

# for (isea in 1:length(seasons )) {
  # season = seasons [isea]
  # print(season)
  

if (season == 'JJA') {
  first_month = 6
  last_month = 8
} else if (season == 'MAM') {
  first_month = 3
  last_month = 5
} else if (season == 'SON') {
  first_month = 9
  last_month = 11
} else if (season == 'DJF') {
  first_month = 12
  last_month = 2
} else {
  print('no season, call terminator')
}

load(paste0(dir_oss, "/BA_200101_202012_nat.RData"))
ni = dim(BA)[1]
nj = dim(BA)[2]

#############################################################################
#   BA >= 10
############################################################################## 

BAS = array(NA, dim = c(ni, nj, length(years)))
for (i in 1:ni) {
  for (j in 1:nj) {
    for (iyear in 1:length(years)) {
      if (season == 'DJF' && first_month == 12) {
        i1 = (iyear - 1) * 12
        i2 = (iyear - 1) * 12 + last_month
      } else {
        i1 = (iyear - 1) * 12 + first_month
        i2 = (iyear - 1) * 12 + last_month
      }
      BAS[i, j, iyear] = sum(BA[i, j, i1:i2], na.rm = TRUE)
    }
  }
}

mask = array(NA, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    if (length(which(BAS[i, j, ] > 0)) >= 10) {
      mask[i, j] = 1
    }
  }
}

for (i in 1:dim(BAS)[1]) {
  for (j in 1:dim(BAS)[2]) {
      BAS[i, j, ] <- (BAS[i, j,]*mask[i,j]) 
  }
}
image(BAS[,,1])
# save(BAS, file = file.path(dir_results,paste("BAS_",season,".RData", sep = "")))

#############################################################################
# ANOMALIA DEL BA PARA BA >= 10
##############################################################################

BA_anomaly = array(NA, dim = c(ni, nj, length(years)))
for (i in 1:dim(BAS)[1]) {
  for (j in 1:dim(BAS)[2]) {
      BA_anomaly[i, j, ] <-ifelse(BAS[i, j, ] > median(BAS, na.rm = TRUE), 1, 0)
  }
}

image.plot(lon,lat,BA_anomaly[,,18])
# save(BA_anomaly, file = file.path(dir_results,paste("anomaly_mask_",season,".RData", sep = "")))

# }


porcentajes_anuales <- numeric(dim(BA_anomaly)[3])
for (i in 1:dim(BA_anomaly)[3]) {
  porcentajes_anuales[i] <- 100 * (sum(BA_anomaly[,,i] == 1, na.rm = TRUE)) / sum(!is.na(BA_anomaly[,,i]))
}
porcentajes_anuales
# Encontrar el índice del máximo y mínimo porcentaje
# Encontrar el índice del máximo y mínimo porcentaje
(indice_maximo <- which.max(porcentajes_anuales))
(indice_minimo <- which.min(porcentajes_anuales))
# Calcular los años correspondientes a los índices máximo y mínimo
(año_maximo <- 2001 + indice_maximo - 1)
(año_minimo <- 2001 + indice_minimo - 1)

BAS_km= (BAS/1000000) # m2 to km2
BAS_I = BAS_km[,,indice_maximo]
# rm(BAS)

BASI = BAS_I *NA
BASI[(BAS_I  >=25000)] = 1
BASI[(BAS_I  < 25000) & (BAS_I  >= 20000)] = 2
BASI[(BAS_I  < 20000) & (BAS_I  >= 15000)] = 3
BASI[(BAS_I  < 15000) & (BAS_I  >= 10000)] = 4
BASI[(BAS_I  < 10000) & (BAS_I  >=5000)] = 5

BASI[(BAS_I  < 5000) & (BAS_I  >=4000)] = 6
BASI[(BAS_I  < 4000) & (BAS_I  >=3000)] = 7
BASI[(BAS_I  < 3000) & (BAS_I  >=2000)] = 8
BASI[(BAS_I  < 2000) & (BAS_I  >=1000)] = 9
BASI[(BAS_I  < 1000) & (BAS_I  >=500)] = 10

BASI[(BAS_I  < 500) & (BAS_I  >= 400)] = 11
BASI[(BAS_I  < 400) & (BAS_I  >= 300)] = 12
BASI[(BAS_I  < 300) & (BAS_I  >= 200)] = 13
BASI[(BAS_I  < 200) & (BAS_I  >= 100)] = 14
BASI[(BAS_I  < 100) & (BAS_I  >= 0)] = 15
BAS_1=BASI
rm(BASI, BAS_I)

brk_BA <- seq(0, 16, length.out = 16)
color_start <- "#FFC96C"  # Amarillo oscuro
color_middle1 <- "#FF9900"  # Naranja
color_middle2 <- "#FF0000"  # Rojo
color_middle3 <- "#800080"  # Violeta
color_end <- "#000000"  # Negro

gradual_palette <- c(
  colorRampPalette(c(color_start, color_middle1))(4),
  colorRampPalette(c(color_middle1, color_middle2))(4),
  colorRampPalette(c(color_middle2, color_middle3))(4),
  colorRampPalette(c(color_middle3, color_end))(3)
)

load(paste0(dir_results, "sig_JJA_RFOR_MSWEP.RData"))
mask_sig=best_sig
mask_sig[best_sig<=0.01]=NA
col_no_mod=c('#555555')
dev.off()
#=============================================================================== BA
# pdf(paste(dir_out,"BA_2012_JJA.pdf", sep=""), width=11, height=7.5)
split.screen(rbind(c(0, 1, 0.1, 1), c(0, 1, 0, 0.1)))
split.screen(c(1, 1), screen = 1) -> ind
screen(ind[1])
par(oma = c(0.8, 0.8, 0.8, 0.2), mar = rep(0.5, 4))

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_BA, 
        drawBox = FALSE, border =NA,
        main = paste("Burned area (JJA; 2012)",sep=""),cex.main = 2.5,line = -1, 
        adj = 0.5)
mapImage(lon,lat, BAS_1, col= rev(gradual_palette) ,breaks = brk_BA)
mapImage(lon, lat, mask_sig, col=  col_no_mod)
plot(ocean, col = "lightblue", add =TRUE)
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules

split.screen(c(1,1), screen=2)-> ind
c2<- range(1, 15)
RA<-1:15
screen( ind[1])
image.plot(zlim = c2,legend.only = TRUE, smallplot = c(.15, .85, .4, 1),
           col = (gradual_palette),
           horizontal = TRUE,axis.args = list(at = RA, 
              labels = c("0 to 100", "100 to 200", "200 to 300", "300 to 400", 
                         "400 to 500","500 to 1k", "1k to 2k", "2k to 3k", 
                         "3k to 4k", "4k to 5k", "5k to 10k", "10k to 15k", 
                         "15k to 20k", "20k to 25k", "> 25k"),
          las = 1, cex.axis = 0.5, srt = 45))  
close.screen( all=TRUE)
dev.off()

#=============================================================================== BAA
col_AN=c( '#932667', 'darkorange')
Etiq_AN <- c("Positive", "Negative")
brk_AN <- seq(0,1, length.out = 3)

pdf(paste(dir_out,"BAA_2012_JJA.pdf", sep=""), width=11, height=7.5)
split.screen(rbind(c(0, 1, 0.1, 1), c(0, 1, 0, 0.1)))
split.screen(c(1, 1), screen = 1) -> ind
screen(ind[1])
par(oma = c(0.8, 0.8, 0.8, 0.2), mar = rep(0.5, 4))

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_AN, 
        drawBox = FALSE, border =NA,
        main = paste("Burned area anomaly (JJA; 2012)",sep=""),cex.main = 2.5,line = -1, 
        adj = 0.5)
mapImage(lon,lat, BA_anomaly[,,indice_maximo], col= rev(col_AN) ,breaks = brk_AN)
mapImage(lon, lat, mask_sig, col=  col_no_mod)
plot(ocean, col = "lightblue", add =TRUE)
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules

split.screen(c(1,1), screen=2)-> ind
c2<- range(1, 4)
RA<-1:4
col_AN=c( '#932667', 'darkorange', '#555555', "white")
Etiq_AN <- c("Positive", "Negative", "No model", "No data")
screen( ind[1])
image.plot(zlim = c2,
           legend.only = TRUE,
           smallplot = c(.15, .85, .4, 1),
           col = rev(col_AN),
           horizontal = TRUE,
           axis.args = list(at = RA, labels = rev(Etiq_AN),
           las = 1, cex.axis = 2, srt = 45))  
close.screen( all=TRUE)
dev.off()


################################################################################
# DROP and 4FIRE
################################################################################
load(paste0(dir_results, "prob_obs_", season ,"_RFOR_MSWEP.RData"))
mask2 <- is.na(predBAobs)
image(mask_sig)
image(predBAobs[,,1])

dim(predBAobs)

# dim(best_sig)
# Aplicar la máscara a BAS: asignar NA donde mask es TRUE
# BAS[mask2] <- NA
# dim(BAS)
# dim(predBAobs)
# image.plot(lon, lat, BAS[,,1])
image.plot(lon, lat, predBAobs[,,1])
predBAobs[best_sig>=0.01]= NA #media de las probabilidades

# install.packages("colorRamps")
library(colorRamps)
# Obtener la paleta de 9 colores
original_colors = brewer.pal(9, "YlOrBr")
# Interpolar para obtener 10 colores
color_interpolator = colorRampPalette(original_colors)
ten_color_palette = color_interpolator(10)
# barplot(rep(1, 10), col=ten_color_palette, space=0)
brk2 <- seq(0, 1, length.out = 11)

# print(colors)
# col_AN=c( '#932667', 'darkorange')
# Etiq_AN <- c("Positive", "Negative")
# brk_AN <- seq(0,1, length.out = 3)

dev.off()
# pdf(paste(dir_out,"probability_obs_2012_JJA.pdf", sep=""), width=11, height=7.5)
split.screen(rbind(c(0, 1, 0.1, 1), c(0, 1, 0, 0.1)))
split.screen(c(1, 1), screen = 1) -> ind
screen(ind[1])
par(oma = c(0.8, 0.8, 0.8, 0.2), mar = rep(0.5, 4))

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk2, 
        drawBox = FALSE, border =NA,
        main = paste("Probability BA anomaly (JJA; 2012)",sep=""),cex.main = 2.5,line = -1, 
        adj = 0.5)
mapImage(lon,lat, predBAobs[,,indice_maximo], col= ten_color_palette ,breaks = brk2)
mapImage(lon, lat, mask_sig, col=  col_no_mod)
plot(ocean, col = "lightblue", add =TRUE)
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules

split.screen(c(1,1), screen=2)-> ind

c2<- range(1, 12)
RA<-1:12

col_nomod_prob= c("white", '#555555', "#FFFFE5", "#FFF7C0", "#FEE79A", "#FECE65", "#FEAC39", "#F6861F",
                  "#E1640E", "#C04602", "#933204", "#662506")
# col_AN=c( '#932667', 'darkorange', '#555555', "white")
 Etiq_AN <- c("No data", "No model", "0.0|0.1", "0.1|0.2", "0.2|0.3", "0.3|0.4",
              "0.4|0.5", "0.5|0.6", "0.6|0.7", "0.7|0.8","0.8|0.9", "0.9|1")
screen( ind[1])
image.plot(zlim = c2,
           legend.only = TRUE,
           smallplot = c(.15, .85, .4, 1),
           col = col_nomod_prob,
           # breaks = brk2 ,
           horizontal = TRUE,
            axis.args = list(at = RA, 
                             labels = Etiq_AN,
                             las = 1, cex.axis = 0.8, srt = 45)
           )  
close.screen( all=TRUE)
dev.off()


mask3=best_sig
mask3[!is.na(mask3)]=1
roc_drop = array(NA, dim = c(ni, nj))
# roc_4fire = array(NA, dim = c(ni, nj))
BA = predBAobs*NA
for (i in 1:dim(BAS)[1]) {
  for (j in 1:dim(BAS)[2]) {
    BAS[i, j, ] <- (BAS[i, j,]*mask3[i,j]) 
  }
}
dim(BAS)
# BA[i, j,] <-  ifelse(BAS[i, j,] > median(BAS[i, j,], na.rm = TRUE), 1, 0)

image(best_sig)
image(predBAobs[,,2])
roc1 = vector()
roc2 = vector()
 for (i in 1:ni) {
   for (j in 1:nj) {
    if (!is.na(best_sig[i, j])) {
      print(paste0("lon; ",i, "; lat: ", j))
      # 
       if (i == 50 && j == 27)  { next }
       if (i == 124 && j == 28) { next }
       if (i == 133 && j == 25) { next }
      BA[i, j,] <-  ifelse(BAS[i, j,] > median(BAS[i, j,], na.rm = TRUE), 1, 0)
      
      if (any(!is.na(BA[i, j, ]) & !is.na(predBAobs[i, j, ]))) {
      roc1 = roc.plot(BA[i, j,], predBAobs[i, j,], na.rm=T)
       roc_drop[i, j] = roc1$plot.data[which.max(roc1$plot.data[, 2, 1] - roc1$plot.data[, 3, 1]), 1, 1]
      # roc_drop[i, j] =  roc1$plot.data[which.max(roc1$plot.data[, 2, 1] - roc1$plot.data[, 3, 1]), 2, 1]
      # roc_drop[i, j] =roc1$plot.data[which.max(roc1$plot.data[, 2, 1] - roc1$plot.data[, 3, 1]), 3, 1]
      }
    }  
  }
}



plot(rev(roc1$plot.data[, 2, 1] ))
image.plot(lon, lat, roc_drop)
world(add=T)
print(length(BA[i, j, ]))
print(sum(!is.na(BA[i, j, ])))
print(length(PredBAobs_ens[i, j, ]))
print(sum(!is.na(PredBAobs_ens[i, j, ])))

image.plot(lon, lat,roc_drop)
image.plot(lon, lat,roc_4fire)

# save(roc_drop, file = paste0(dir_out, "roc_drop_",season, ".RData"))
# save(roc_4fire, file = paste0(dir_out, "roc_4fire_",season, ".RData"))

alarm_drop=predBAobs*NA
for (i in 1:ni) {
  for (j in 1:nj) {
    if (!is.na(mask[i, j])) {
      # alarm_4fire[i, j,] <-  ifelse(PredBApre_ens[i, j,] >= roc_4fire[i, j], 1, 0)
      alarm_drop[i, j,] <-  ifelse(predBAobs[i, j,] >= roc_drop[i, j], 1, 0)
    }
  }
}

col_AN=c("#f6d746", "red")
EtiqA <- c("Alarm", "No alarm")
brk_AN <- seq(0,1, length.out = 3)
dev.off()
pdf(paste(dir_out,"alarm_obs_2012_JJA.pdf", sep=""), width=11, height=7.5)
split.screen(rbind(c(0, 1, 0.1, 1), c(0, 1, 0, 0.1)))
split.screen(c(1, 1), screen = 1) -> ind
screen(ind[1])
par(oma = c(0.8, 0.8, 0.8, 0.2), mar = rep(0.5, 4))

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_AN, 
        drawBox = FALSE, border =NA,
        main = paste("Burned area anomaly (JJA; 2012)",sep=""),cex.main = 2.5,line = -1, 
        adj = 0.5)
mapImage(lon,lat, alarm_drop[,,indice_maximo], col= col_AN ,breaks = brk_AN)
mapImage(lon, lat, mask_sig, col=  col_no_mod)
plot(ocean, col = "lightblue", add =TRUE)
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules

split.screen(c(1,1), screen=2)-> ind
c2<- range(1, 4)
RA<-1:4
col_AN=c( "red","#f6d746",'#555555', "white")
Etiq_AN <- c("Alarm", "No alarm", "No model", "No data")
screen( ind[1])
image.plot(zlim = c2,
           legend.only = TRUE,
           smallplot = c(.15, .85, .4, 1),
           col = rev(col_AN),
           horizontal = TRUE,
           axis.args = list(at = RA, labels = rev(Etiq_AN),
                            las = 1, cex.axis = 2, srt = 45))  
close.screen( all=TRUE)
dev.off()

#===============================================================================

ni = dim(BA_anomaly)[1]
nj = dim(BA_anomaly)[2]

BAA_only_model_4f = array(NA, dim = c(ni, nj, length(years)))
BAA_only_model_dr = array(NA, dim = c(ni, nj, length(years)))


for (i in 1:ni) {
  for (j in 1:nj) {
      BAA_only_model_4f[i, j,] <- BA_anomaly[i, j,]*mask[i, j]
      BAA_only_model_dr[i, j,] <- BA_anomaly[i, j,]*mask[i, j]
  }
}

rate_table = array(data = NA, dim = c( dim(BA_anomaly)[1],  dim(BA_anomaly)[2],  dim(BA_anomaly)[3]))
rate_table_4f = array(data = NA, dim = c( dim(BA_anomaly)[1],  dim(BA_anomaly)[2],  dim(BA_anomaly)[3]))
rate_table_dr = array(data = NA, dim = c( dim(BA_anomaly)[1],  dim(BA_anomaly)[2],  dim(BA_anomaly)[3]))

for (im in 1:dim(BA_anomaly)[3]) {
  for (i in 1:dim(BA_anomaly)[1]) {
    for (j in 1:dim(BA_anomaly)[2]) {
      
        aux  = BAA_only_model_dr[i, j, im]
        aux1 = alarm_drop[i, j, im]
      
      if (!is.na(aux)!=0 & !is.na(aux1)!=0 ) {
        if(aux== 1 &  aux1 == 1){ 
          rate_table[i, j, im] = 1 # hits
        } else if (aux == 0 &  aux1 == 1){
          rate_table[i, j, im] = 2 # false rate
        } else if (aux == 1 &  aux1 == 0){
          rate_table[i, j, im] = 3 #misses
        } else if (aux == 0 &  aux1 == 0){
          rate_table[i, j, im] = 4 # observed no
        }
      }
    }
  }
}


# Contar la frecuencia de cada valor en la matriz
frecuencias <- table(rate_table[,,2], useNA = "ifany")

(frecuencias)
# Calcular el número total de elementos no NA
total_no_na <- sum(!is.na(rate_table[,,2]))

# Calcular el porcentaje de cada valor
porcentaje_1 <- round((frecuencias[1] / total_no_na) * 100, 0)
porcentaje_2 <- round((frecuencias[2] / total_no_na) * 100, 0)
porcentaje_3 <- round((frecuencias[3] / total_no_na) * 100, 0)
porcentaje_4 <- round((frecuencias[4] / total_no_na) * 100, 0)
porcentaje_na <- round((frecuencias[NA] / total_no_na) * 100, 0)

# Imprimir los porcentajes
cat("Porcentaje de hits:", porcentaje_1, "%\n")
cat("Porcentaje de false alarms:", porcentaje_2, "%\n")
cat("Porcentaje de misses:", porcentaje_3, "%\n")
cat("Porcentaje de correct negatives:", porcentaje_4, "%\n")



colores_etiquetas <- c(
   # 'lightgreen', 'red', 'darkred', 'darkgreen')
  'lightgreen', 
  # 'red',
  # 'darkred', 
  # "#377eb8",
  "#ffcc00",
  "#ff7f40",
  # "blue3",
  'darkgreen')

colores_asociados <- colores_etiquetas[1:4]
brk_rate <- seq(1,4, length.out = 5)
dev.off()
pdf(paste(dir_out,"aciertos_obs_2012_JJA.pdf", sep=""), width=11, height=7.5)
split.screen(rbind(c(0, 1, 0.1, 1), c(0, 1, 0, 0.1)))
split.screen(c(1, 1), screen = 1) -> ind
screen(ind[1])
par(oma = c(0.8, 0.8, 0.8, 0.2), mar = rep(0.5, 4))

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_rate, 
        drawBox = FALSE, border =NA,
        main = paste("Burned area anomaly (JJA; 2012)",sep=""),cex.main = 2.5,line = -1, 
        adj = 0.5)
mapImage(lon,lat, rate_table[,,indice_maximo], col= colores_etiquetas ,breaks = brk_rate)
mapImage(lon, lat, mask_sig, col=  col_no_mod)
plot(ocean, col = "lightblue", add =TRUE)
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules

split.screen(c(1,1), screen=2)-> ind
c2<- range(1, 6)
RA<-1:6
col_AN=c( 
   'lightgreen', 
   # 'red',
   # 'darkred', 
   # "#377eb8",
     "#ffcc00",
     "#ff7f40",
   # "blue3",
   'darkgreen',
   '#555555', "white")


Etiq_AN <- c(paste0("True negatives (",porcentaje_4, "%)"),
                    paste0("False positives (",porcentaje_3, "%)"),
                           paste0("False negatives (",porcentaje_2, "%)"),
                                  paste0("True positives (",porcentaje_1, "%)"),
                                         "No model",  "No data")
screen( ind[1])
image.plot(zlim = c2,
           legend.only = TRUE,
           smallplot = c(.15, .85, .4, 1),
           col = rev(col_AN),
           horizontal = TRUE,
           axis.args = list(at = RA, labels = rev(Etiq_AN),
                            las = 1, cex.axis = 0.7, srt = 45))  
close.screen( all=TRUE)
dev.off()


cat("Porcentaje de hits:", porcentaje_1, "%\n")
cat("Porcentaje de false alarms:", porcentaje_2, "%\n")
cat("Porcentaje de misses:", porcentaje_3, "%\n")
cat("Porcentaje de correct negatives:", porcentaje_4, "%\n")












# 
# 
# 
# 
# 
# 
# 
# #############################################################################
# # MASCARA DEL MODELO DONDE LA BA  >= 10 Y EL MODELO CLIBA NO ES SIGNIFICATIVO
# ##############################################################################
# 
# load(paste(dir_out_pre, "roc_4FIRE_",version,".RData", sep="")) #best_roc_drop
# mask_4f_sig= roca_4fire*NA
# mask_4f_sig[!is.na(roca_4fire)]=2
# mask_4f_sig[is.na(roca_4fire)]=1
# mask_4f_sig[mask_4f_sig== 2]=NA
# mask_4f_sig=mask_4f_sig*mask1
# 
# load(paste(dir_out_obs, "roc_DROP_",version,".RData", sep="")) #best_roc_drop
# mask_dr_sig= roca_drop*NA
# mask_dr_sig[!is.na(roca_drop)]=2
# mask_dr_sig[is.na(roca_drop)]=1
# mask_dr_sig[mask_dr_sig== 2]=NA
# mask_dr_sig=mask_dr_sig*mask1
# # save(mask_dr_sig, file = paste0(dir_app, "mask_no_mod_drop_",season, ".RData"))
# # save(mask_4f_sig, file = paste0(dir_app, "mask_no_mod_4fire_",season, ".RData"))
# 
# }
# 
# 
# dim(BA_anomaly)
# 
# num_valores_1 <- sum(matriz == 1)
# 
# # median_BAS <- apply(BAS, c(1,2), median, na.rm = TRUE)  # Calcula la mediana de BAS
# # median_BAS[median_BAS == 0] = NA
# # 
# # 
# # a<-median_BAS*mask
# # rm(median_BAS)
# # image.plot(lon, lat, a)
# # median_mask=a
# # rm(a)
# # save(median_mask, file = file.path(dir_app,paste("Median_MASK_",season,".RData", sep = "")))
# # 
# # dim(mask)
# # Crea una matriz de la misma dimensión que BAS para almacenar la anomalía de área quemada
# 
# 
# 
# load(paste0(dir_app, "anomaly_mask_DJF.RData"))
# dim(BA_anomaly)
# image.plot(lon, lat, BA_anomaly[,,20])
# 
# dim(BAS)
# load(paste0(dir_app, "/mask_MAM.RData"))
# summary(as.vector(BAS))
# 
# load(paste0(dir_app, "preBA_4fire_MAM_FDR.RData"))
# PredBAobs_ens=(apply(predBAobs, c(1, 2, 3), mean, na.rm = TRUE))
# dim(PredBAobs_ens)
# 
# 
# ####################################################
# ### DROP POR ESTACIONES
# ####################################################
# load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
# load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
# load(file.path(dir_drop, "inout.RData"))
# 
# 
# anni = 1981:2020
# mesi = rep(1:12, length(anni))
# 
# 
#   mes= which(mesi == 2)
#   mes2 <- mes[mes >= (2001 - 1981) * 12 + 1]
#   mes= which(mesi == 5)
#   mes5 <- mes[mes >= (2001 - 1981) * 12 + 1]
#   mes= which(mesi == 8)
#   mes8 <- mes[mes >= (2001 - 1981) * 12 + 1]
#   mes= which(mesi == 11)
#   mes11 <- mes[mes >= (2001 - 1981) * 12 + 1]
# 
# time_scale=c(3,6,12)
# 
# for (isc in 1:length(time_scale)) {
#   sc = time_scale[isc]
#   print(sc)
#   
#   load(file.path(paste0(dir_drop,"SPI",sc,"_ENS_1981_2020.RData", sep="")))
#   
#     spi2=spi[,,mes2]
#     spi5=spi[,,mes5]
#     spi8=spi[,,mes8]    
#     spi11=spi[,,mes11]
# 
#     save(spi2, file = file.path(dir_app,paste("SPI_",sc,"_DJF_ENS.RData", sep = "")))
#     save(spi5, file = file.path(dir_app,paste("SPI_",sc,"_MAM_ENS.RData", sep = "")))
#     save(spi8, file = file.path(dir_app,paste("SPI_",sc,"_JJA_ENS.RData", sep = "")))
#     save(spi11, file = file.path(dir_app,paste("SPI_",sc,"_SON_ENS.RData", sep = "")))
#     
# }
# 
# 
# 
# load(paste0(dir_app, "mask_DJF.RData")) #the ensemble mean
# BAS= (BAS/10000) 
# summary(as.vector(BAS))
# 
# 
# 
