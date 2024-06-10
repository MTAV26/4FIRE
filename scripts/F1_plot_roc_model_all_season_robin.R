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

dir_oss ="C:/Users/Usuario/Dropbox/4FIRE_V2/data/"
dir_results ="C:/Users/Usuario/Dropbox/4FIRE_V2/results/"
dir_out="C:/Users/Usuario/Dropbox/4FIRE_V2/plots/"
dir_mask="C:/Users/Usuario/Dropbox/4FIRE/data/mask_season/"
                                              
## load SPI
fname <- file.path(dir_oss, 'DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat

##########################################################
#Let todas las mascaras
##########################################################

load(paste0(dir_mask, "mask_DJF.RData"))
mask[mask == 0]=NA
mask_djf=mask
image(mask_djf)
rm(mask)
load(paste0(dir_mask, "mask_MAM.RData"))
mask[mask==0]=NA
mask_mam=mask
rm(mask)
load(paste0(dir_mask, "mask_JJA.RData"))
mask[mask==0]=NA
mask_jja=mask
rm(mask)
load(paste0(dir_mask, "mask_SON.RData"))
mask[mask==0]=NA
mask_son=mask
rm(mask)

##########################################################
#Let todas estaciones de roca PRED BA OBS
##########################################################
method="RLOG"
# method="RFOR"

load(paste0(dir_results, "/roc_area_DJF_",method,"_MSWEP.RData"))
hist(best_roc)
load(paste0(dir_results, "/sig_DJF_",method,"_MSWEP.RData"))
hist(best_sig)
best_roc[best_sig >= 0.01] = NA
roca_djf=best_roc
hist(roca_djf)
sig_djf=best_sig
hist(sig_djf)
rm(best_roc, best_sig)
final_mask_djf <- ifelse((!is.na(mask_djf) & !is.na(sig_djf)), 1, NA)

load(paste0(dir_results, "/roc_area_MAM_",method,"_MSWEP.RData"))
load(paste0(dir_results, "/sig_MAM_",method,"_MSWEP.RData"))
best_roc[best_sig >= 0.01] = NA
roca_mam=best_roc
sig_mam=best_sig
rm(best_roc, best_sig)
final_mask_mam <- ifelse((!is.na(mask_mam) & !is.na(sig_mam)), 1, NA)

load(paste0(dir_results, "/roc_area_JJA_",method,"_MSWEP.RData"))
load(paste0(dir_results, "/sig_JJA_",method,"_MSWEP.RData"))
best_roc[best_sig >= 0.01] = NA
roca_jja=best_roc
sig_jja=best_sig
rm(best_roc, best_sig)
final_mask_jja <- ifelse((!is.na(mask_jja) & !is.na(sig_jja)), 1, NA)

load(paste0(dir_results, "/roc_area_SON_",method,"_MSWEP.RData"))
load(paste0(dir_results, "/sig_SON_",method,"_MSWEP.RData"))
best_roc[best_sig >= 0.01] = NA
roca_son=best_roc
sig_son=best_sig
rm(best_roc, best_sig)
final_mask_son <- ifelse((!is.na(mask_son) & !is.na(sig_son)), 1, NA)


load(paste0(dir_oss, "/BA_200101_202012_nat.RData"))
data(coastlineWorld)
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

worldmap <- rworldmap::getMap(resolution = "coarse") # Load countries 
raster::crs(worldmap)
worldmap <- sp::spTransform(worldmap,crs) # Project to Robinson

lati <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135,-90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lati, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lati, proj = crs) # graticules 

####################################################
# Mascara de Oceanos
####################################################
shapename <- read_sf('C:/Users/Usuario/Dropbox/capas/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha

brk_cor <- seq(0.75, 1, length.out = 6)
col_cor <- c( "#E6F3E3", "#B7E2B1", "#6AB86F", "#227E3B", "#00441B")

# close.screen( all=TRUE)
dev.off()

pdf(paste(dir_out,"Figure2_",method,".pdf"), width=9, height=6.25)

set.panel()
split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
split.screen(c(2,2), screen=1)-> ind
zr<- range(-1,1)
screen( ind[1])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 

#===============================================================================
tit <-
  paste('a) AUC MSWEP (DJF): 
Skilfull surface (', round(length(which(!is.na(roca_djf)))/length(which(!is.na(final_mask_djf)))*100,0),'%); median (', round((median(roca_djf, na.rm = TRUE)),2),")", sep="")

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, 
        border=NA,  # Ajuste clave para no dibujar los límites
        main = paste(tit, sep=""), 
        cex.main = 0.8,line = -1.5, adj = 0.5)
mapImage(lon, lat, mask_djf, col= "orange")
mapImage(lon,lat, roca_djf, col= col_cor, breaks = brk_cor)
plot(ocean, col = adjustcolor("lightblue", alpha = 1), add = TRUE)
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 

#===============================================================================
screen( ind[2])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 

tit <-
  paste('b) AUC MSWEP (MAM): 
Skilfull surface (', round(length(which(!is.na(roca_mam)))/length(which(!is.na(final_mask_mam)))*100,0),'%); median (', round((median(roca_mam, na.rm = TRUE)),2),")", sep="")

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, border = NA,
        main = paste(tit, sep=""), 
        cex.main = 0.8,line = -1.5, adj = 0.5)
mapImage(lon, lat, mask_mam, col= "orange")
mapImage(lon,lat, roca_mam, col= col_cor, breaks = brk_cor)
plot(ocean, col = adjustcolor("lightblue", alpha = 1), add = TRUE)
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 

#===============================================================================
screen( ind[3])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 

tit <-
  paste('c) AUC MSWEP (JJA): 
Skilfull surface (', round(length(which(!is.na(roca_jja)))/length(which(!is.na(final_mask_jja)))*100,0),'%); median (', round((median(roca_jja, na.rm = TRUE)),2),")", sep="")

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, border = NA,
        main = paste(tit, sep=""), 
        cex.main = 0.8,line = -1.5, adj = 0.5)
mapImage(lon, lat, mask_jja, col= "orange")
mapImage(lon,lat, roca_jja, col= col_cor, breaks = brk_cor)
plot(ocean, col = adjustcolor("lightblue", alpha = 1), add = TRUE)
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 

#===============================================================================
screen( ind[4])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 

tit <-
  paste('d) AUC MSWEP (SON): 
Skilfull surface (', round(length(which(!is.na(roca_son)))/length(which(!is.na(final_mask_son)))*100,0),'%); median (', round((median(roca_son, na.rm = TRUE)),2),")", sep="")

mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_cor, drawBox = FALSE, border = NA,
        main = paste(tit, sep=""), 
        cex.main = 0.8,line = -1.5, adj = 0.5)
mapImage(lon, lat, mask_son, col= "orange")
mapImage(lon,lat, roca_son, col= col_cor, breaks = brk_cor)
plot(ocean, col = adjustcolor("lightblue", alpha = 1), add = TRUE) 
plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules 
#===============================================================================
zz<- range(0.75, 1) # s
split.screen(c(1,2), screen=2)-> ind2
screen( ind2[1])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
image.plot(zlim=zz,legend.only=TRUE, smallplot=c(.25,.99, .69,.99),
           col=col_cor, breaks = brk_cor, horizontal = TRUE)

screen( ind2[2])
par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
nombres_paleta <- c("No climate effect", "No data")
image.plot(zlim=zr,legend.only=TRUE, smallplot=c(.25,.75, .69,.99),col=c("orange", "white"), 
           horizontal = TRUE,axis.args = list(at = c(zr,zr[length(zr)]+diff(zr)[1]),
                                              labels=c(nombres_paleta,'No model')))

close.screen( all=TRUE)
dev.off()



