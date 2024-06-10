rm(list = ls())
graphics.off()
gc()

library(pracma)
library(ncdf4)
library(fields)
library(maptools)
library(RColorBrewer)
library(viridis)



source('C:/Users/Usuario/Dropbox/4FIRE/script/model_BA/image_mask.R')
data(wrld_simpl)

where='miguel'
if (where == 'mac') {
  dir_data='~/Dropbox/estcena/scripts/miguel/4FIRE/data/'
  dir_out = '~/Dropbox/estcena/scripts/miguel/4FIRE/out/'
  dir_drop='/Users/marco/Documents/dati/fire_climate_data/climate_fire/datos/drop_spi/'
} else if (where == 'miguel') {
  dir_data = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/'
  dir_out="C:/Users/Usuario/Dropbox/4FIRE_V2/results/"
  dir_mask="C:/Users/Usuario/Dropbox/4FIRE/data/mask_season/"
}

fname<-file.path(dir_data, '/DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat
ni=length(lon)
nj=length(lat)
ilat = which(lat > -60 & lat < 75)
ilon = which(lon > -165 & lon < 180)



# seasons= c("DJF")
# seasons= c("DJF", "MAM", "JJA", "SON")

library(sp) # loads sp library too # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(maps)
library(oce)
library(ggplot2)
library(terra)
library(raster)
library(rgdal)
library(rworldmap)
library(rnaturalearth)

##########################################################
#creo una mascara de todo el globo para tapar los paises
##########################################################
dir_oss="C:/Users/Usuario/Dropbox/4DROP/data"
fname <- file.path(dir_oss, 'MSWEP_1981_2021.nc')
obs.nc <- nc_open(fname)
obs <- ncvar_get(obs.nc, "precipitation")
obs[obs == "-9999.f"] <- NA
obs1 = apply(obs, c(1, 2), mean)
load(file.path(dir_drop, "inout.RData"))
obs_mask= obs1*inout
obs_mask[!is.na(obs_mask)]=1
image.plot(lon, lat, obs_mask)
dev.off()


season = c("DJF","MAM","JJA", "SON")

for (seas in 1:length(season)) {
  seaJJA = season[seas]

  version = paste0(seaJJA,'_RFOR')

  
  load(paste0(dir_mask, "mask_",seaJJA,".RData"))
  mask_jja=mask
  rm(mask)

  load(paste0(dir_out, "sig_",version,"_MSWEP.RData"))
  
    
  # load(paste0(dir_out, "best_m_spi_a_fin_",version,"_MSWEP.RData"))
  # best_m_spi_a_fin[best_sig>=0.01]=NA
  # x1=best_m_spi_a_fin
  
  load(paste0(dir_out, "best_t_spi_a_fin_",version,"_MSWEP.RData"))
  best_t_spi_a_fin[best_sig>=0.01]=NA
  x1=best_t_spi_a_fin
  # 
  

  # load(paste0(dir_out, "cor_",version,"_MSWEP.RData"))
  # x1 <- ifelse(rho < 0, 1, 0)
  # x1[best_sig>=0.01]=NA

  conteo<-table(x1)
  tot<-sum(conteo)
  print(round(conteo/tot*100, 1))
}

