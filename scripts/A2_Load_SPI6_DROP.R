#script 4

rm(list = ls())
graphics.off()
gc()

#library(ncdf)
library(sp)
library(maptools) # loads sp library too
library(RColorBrewer) # creates nice color schemes
library(classInt) # finds class intervals for continuous variables
library(fields)
library(s2dverification)
library(maps)
library(pracma)
library(verification)
library(psych)

source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")

## fixed parameters
# dir_drop = 'C:/Users/Usuario/OneDrive/Escritorio/4DROP/DATA/drop/'
# dir_ensD= 'C:/Users/Usuario/OneDrive/Escritorio/4DROP/RESULTADOS/ens_drop_V2'

#dir_drop = '/home/miguel/4DROP/DROP/'
dir_oss = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/'
dir_drop = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/'

data(wrld_simpl)
time_scale = c(6)


## fix parameters
anni = 1993:2020
mesi = rep(1:12, length(anni))
anno_2000 = which(anni == 1993)
mese_ind = which(mesi == 01)
last_month = mese_ind[anno_2000]
#mesi_8 = which(mesi == 8)

##
load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))

load(file.path(dir_drop, "inout.RData"))


## load data
pred = array(data = NA, dim = c(length(lon), length(lat), length(mesi), 11))
dim(pred)

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  
  nam <- paste("spi", sc, sep = "")
  print(nam)
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_GPCP_1993_2020.RData", sep = "") ))
  aux = get(nam)
  pred[, , , 1] = aux[, ,]
  
  load(file.path(
    paste(dir_drop,"SPI",sc,"_CAMS_OPI_1993_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 2] = aux[, ,]
  
  load(file.path(
    paste(
      dir_drop,"SPI",sc,"_CHIRPS_1993_2020.RData",sep = "") ))
  aux = get(nam)
  pred[, , , 3] = aux[, ,]
  
  load(file.path(
    paste(
      dir_drop,"SPI", sc,"_CPC_1993_2020.RData",sep = "")  ))
  
  aux = get(nam)
  pred[, , , 4] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_GPCC_1993_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 5] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_JRA55_1993_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 6] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_PRECL_1993_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 7] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_ERA5_1993_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 8] = aux[, ,]
  
  load(file.path(
    paste(dir_drop, "SPI", sc, "_NCEP_1993_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 9] = aux[, ,]
  
  load(file.path(
    paste(dir_drop,"SPI", sc, "_MERRA2_1993_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 10] = aux[, ,]
  
  load(file.path(
    paste(dir_drop,"SPI", sc, "_MSWEP_1993_2020.RData", sep = "")  ))
  aux = get(nam)
  pred[, , , 11] = aux[, ,]
  
  rm(aux)
  aux=pred[,,1:last_month,]
  rm(pred)
  pred=aux
  rm(aux)
  
  spi = (apply(pred, c(1, 2, 3), mean, na.rm = TRUE))
  save(spi, file = paste0(dir_out,"/DROP_SPI", sc,"_ENS_no_scaled.RData"))
  
  for (i in 1:dim(pred)[1]) {
    for (j in 1:dim(pred)[2]) {
      if (sum(!is.na(spi[i,j,]))!=0) {
        spi[i,j,]=scale(spi[i,j,])
      }
    }
  }
  
  # image.plot(lon, lat, spi[, , last_month])
  # plot(wrld_simpl, add = TRUE)
  
  save(spi, file = paste0(dir_out, "/DROP_SPI", sc,"_ENS.RData"))
  
  
  # # 
  # spi_sd = apply(pred, c(1, 2, 3), sd, na.rm = TRUE)
  # save(spi_sd, file = paste0(dir_out, "/DROP_SPI", sc,"_ENS_SPREAD.RData"))
  
  # points <- expand.grid(lon, lat)
  # pts = SpatialPoints(points, proj4string = CRS(proj4string(wrld_simpl)))
  # ii <- !is.na(over(pts, wrld_simpl))
  # inout = ii[, 1]
  # dim(inout) <- c(nrow(pred), ncol(pred))
  # inout[inout == 0] = NA
  
  # 
  # 
  # spi_prob = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
  # for (im in 1:dim(pred)[3]) {
  #   ## Plot Probability Moderate Drought
  #   for (i in 1:dim(pred)[1]) {
  #     for (j in 1:dim(pred)[2]) {
  #       aux = pred[i, j,im,]
  #       spi_prob[i, j,im] = (sum(aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux)))
  #       
  #     }
  #   }
  #   spi_prob[, ,im]=spi_prob[, ,im]*inout
  # } 
  # save(spi_prob, file = paste0(dir_out, "/DROP_PROB", sc,"_DROP.RData"))
  # 
  # 
  # 
  # 
  # ## traffic light
  # spi_tl = array(data = NA, dim = c(length(lon), length(lat), dim(pred)[3]))
  # for (im in 1:dim(pred)[3]) {
  #   for (i in 1:dim(pred)[1]) {
  #     for (j in 1:dim(pred)[2]) {
  #       # aux = pred[i, j, im,]
  #       
  #       aux = spi[i, j, im] #without ENS
  #       aux1 =spi_prob[i, j, im]
  #       
  #       if (sum(!is.na(aux))!=0) {
  #         if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) > 0 &
  #             sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) <= 0.25)  {
  #           spi_tl[i, j, im] = 2 #yellow code
  #         } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) > 0.25 &
  #                    sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) <= 0.75)  {
  #           spi_tl[i, j, im] = 3 #orange code
  #         } else if (sum(aux[!is.na(aux)] <= -1.3) / sum(!is.na(aux1)) > 0.75)  {
  #           spi_tl[i, j, im] = 4 #red code
  #         
  #         } else  if (sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux1)) > 0 &
  #                     sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux1)) <= 0.5)  { 
  #           spi_tl[i, j, im] = 2 #yellow code
  #         } else if (sum(aux[!is.na(aux)] > -1.30 & aux[!is.na(aux)] <= -0.8) / sum(!is.na(aux1)) > 0.5)  {
  #           spi_tl[i, j, im] = 3 #orange code
  #           
  #         } else if (sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux1)) > 0.5)  {
  #           spi_tl[i, j, im] = 2 #yellow code
  #           
  #         } else  if (sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux1)) > 0 &
  #                     sum(aux[!is.na(aux)] > -0.8 & aux[!is.na(aux)] <= -0.5) / sum(!is.na(aux1)) <= 0.5)  {
  #          
  #            spi_tl[i, j, im] = 1 #green code
  #         } else  {
  #           spi_tl[i, j, im] = 1 #green code
  #         }
  #       }
  #     }
  #   }
  #   spi_tl[, , im] = spi_tl[, , im] * inout
  # } 
  # 
  # save(spi_tl, file = paste0(dir_out, "/DROP_TRAF_LIG_", sc,"_DROP.RData"))
}



