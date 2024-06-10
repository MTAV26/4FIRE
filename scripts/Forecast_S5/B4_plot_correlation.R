
rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(maps)
library(s2dverification)
library(ggplot2)
library(reshape)

library(pracma)
library(verification)
library(psych)
library(sf)
# install.packages("oce")
library(oce)
library(raster)
# install.packages("terra")
library(terra)
library(raster)
# install.packages("graticule")
library(graticule)
library(rgdal)
library(rworldmap)
# install.packages("rnaturalearth")
library(rnaturalearth)

source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")


dir_drop = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/'
dir_s5 = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/SEAS5/'
dir_out= 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/SEAS5/'

brk <- seq(-1, 1, 0.2)
col <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))

brk_cor <- seq(-1, 1, 0.2)
col_cor <- (colorRampPalette(brewer.pal(11, "PRGn"))(10))



load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))
lonGPCP = lon
latGPCP = lat
lat2 = lat[which(lat > -60 & lat < 85)]
anni = 2000:2020
mesi = rep(1:12, length(anni))
# 


####################################################
# Proyecci?n de Robinson y mapa base (robin)
####################################################
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
# shapename <- read_sf('~/Dropbox/estcena/miguel/capa_oceanos/ne_10m_ocean.shp')
ocean <- sf::st_transform(shapename , crs = crs) # cha
rm(shapename)


# datasets =c("GPCC")
datasets = c(
  # 'CPC',
  # 'GPCC',
  # 'PRECL',
  # 'ERA5',
  # 'JRA55',
  # 'NCEP',
  # 'MERRA2',
  # 'CAMS_OPI',
  # 'CHIRPS',
  # 'GPCP',
  'MSWEP')


start_date=05
ms="05"
#sc=c(6)
time_scale = c(3,6,12)
# mes_forecast = c(
#   "NOV",
#   "DEC",
#   "JAN",
#   "FEB")

mes_forecast = c(
  "MAY",
  "JUN",
  "JUL",
  "AUG")


  ##############################################
      ## PLOT WITH PROJECT ROBINSON
  ##############################################
      # dev.off()
      
      pdf(paste(dir_out,"/COR_DROP_JJA_MSWEP_all.pdf",sep = ""),width=11.5, height=12)
      # set.panel() 
      # split.screen( rbind(c(0, 1,.1,1), c(0,1,0,.1)))
      # split.screen(c(4,3), screen=1)-> ind
      
      
      # zr<- range(0,1)
      
      set.panel() 
      split.screen( rbind(c(0, 1,.1,1), c(0,1,0,.1)))
      split.screen(c(4,3), screen=1)-> ind
      zr<- range(0,1)
      
      contador <- 0
      
    for (mes_for in 1:length(mes_forecast)) {
        est = mes_forecast[mes_for]
        print(est)
        
        ##########----plot 1---#############
      
      for (isc in 1:length(time_scale)) {
        sc = time_scale[isc]
        print(sc)
        
        contador <- contador + 1
        
            for (idata in 1:length(datasets)) {
              dataset = datasets[idata]
              print(dataset)
            
            # load(file.path(dir_out, paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,"_original.RData", sep = "") ))
              
            
            load(file.path(dir_out, paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,".RData", sep = "")))
            load(file.path(dir_out, paste("/PVAL_S5_spi",sc,"_",ms,"_",est,"_",dataset,".RData", sep = "")))
            
        
            pvalue1 =pvalue_adj2
            pvalue1[pvalue1 <= 0.01] = NA
            pvalue1[pvalue1 >= 0.01] = -1
            
            pvalue1[is.infinite(pvalue1)]=NA
            pvalue1[is.na(pvalue1)]=NA
            colns<-c("#FFE4C4")
            

      
      interaction_number <- (mes_for  - 1) * 3 + isc 
      screen( ind[interaction_number])
        # screen( ind[contador])
        par(oma=c( 0,0,0,0),mar=rep(0, 4)) 
        
        
        if(interaction_number == 1){
          panel = "a"
        } else if (interaction_number == 2){
          panel = "b"
        } else if (interaction_number == 3){
          panel = "c"
        } else if (interaction_number == 4){
          panel = "d"
        } else if (interaction_number == 5){
          panel = "e"
        } else if (interaction_number == 6){
          panel = "f"
        } else if (interaction_number == 7){
          panel = "g"
        } else if (interaction_number == 8){
          panel = "h"
        } else if (interaction_number == 9){
          panel = "i"
        } else if (interaction_number == 10){
          panel = "j"
        } else if (interaction_number == 11){
          panel = "k"
        } else if (interaction_number == 12){
          panel = "l"
        }
        
        
        tit <-
          paste(
            panel,') COR for SPI',sc, ' in ',est,', ',dataset,' (S5) against MSWEP \n Start date: ',
            month.name[ start_date],
            ' - Period: 1993/2020', sep=""
          )
        
        mapPlot(coastlineWorld, col="white",
                projection="+proj=robin", 
                breaks = brk_cor, drawBox = FALSE, 
                main = tit, cex.main = 0.8,line = -1.5, adj = 0.5)
         
        # mapImage(lon, lat, obs_mask, col= "white")
         mapImage(lon, lat2, corre2, col= col_cor, breaks = brk_cor)
         mapImage(lon, lat2, pvalue1, col = colns)
         
         # Encontrar las coordenadas en la matriz para el valor -1 en pvalue1
         # indices <- which(pvalue1 == -1, arr.ind = TRUE)
         # # Obtener las coordenadas de fila y columna
         # filas <- indices[, 1]
         # columnas <- indices[, 2]
        plot(ocean, col = "lightblue", add =TRUE)
        plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules
        }
      }
    }

      # zr<- seq(0,1, 0.5)
      zz<- range(-1, 1) # s
      split.screen(c(1,2), screen=2)-> ind2
      screen( ind2[1])
      
      image.plot(zlim=zz,legend.only=TRUE, smallplot=c(.25,.95, .6,.9),
                 col=col_cor, breaks = brk_cor, horizontal = TRUE)
      
      screen( ind2[2])
      # zr<- range(0,1, 0.5)
      nombres_paleta <- c("No significative", "No data")
      image.plot(zlim=zr,legend.only=TRUE, smallplot=c(.25,.75, .6,.9),col=c("#FFE4C4","white"),
                 horizontal = TRUE,
                  axis.args = list(at = c(zr,zr[length(zr)]+diff(zr)[1]),
                                                    labels=c(nombres_paleta,'No model')))
      
      
      # zr <- seq(-1, 1, 1)
      # nombres_paleta <- c("No significative", "No data", "Ocean")
      # image.plot(zlim = zr, legend.only = TRUE, smallplot = c(.25, .75, .6, .9),
      #            col = c("#FFE4C4", "white", "lightblue"), horizontal = TRUE,
      #            axis.args = list(at = c(1:4),
      #                             labels = nombres_paleta[1:3], "no model"))
      
      close.screen( all=TRUE)
      dev.off()     
      
      
      
      
      
      
      
      
      
      
      
      
      