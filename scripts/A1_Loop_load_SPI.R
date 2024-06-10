#script 1

rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(sp)
library(maptools) # loads sp library too
library(fields)
library(maps)
library(SPEI)
library(RColorBrewer)

data(wrld_simpl)


##
anni = 1981:2020
mesi = rep(1:12, length(anni))
# anno_2000 = which(anni == 1993)
# mese_aug = which(mesi == 1)
# mese_aug2000 = mese_aug[anno_2000]

fechas <- seq(as.Date("1981-01-01"), as.Date("2020-12-01"), by = "month")
i_1993=which(fechas=="1993-01-01")
i_last=which(fechas=="2020-12-01")

# Crear un vector de fechas con el primer día de cada mes
# dir_oss = '/home/miguel/4DROP/data'
# dir_drop = '/home/miguel/4DROP/DROP'

dir_oss = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/'
dir_drop = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/'
#dir_oss = '~/Dropbox/4DROP/data'



data(wrld_simpl)

load(file.path(dir_drop, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_drop, "lat_GPCP_1981_2017.RData"))

points <- expand.grid(lon, lat)
data(wrld_simpl)
pts = SpatialPoints(points, proj4string = CRS(proj4string(wrld_simpl)))
ii <- !is.na(over(pts, wrld_simpl))
inout = ii[, 1]
dim(inout) <- c(length(lon), length(lat))
inout[inout == 0] = NA

image.plot(lon, lat, inout)
plot(wrld_simpl, add = TRUE)

# datasets=c('MSWEP')
datasets = c('ERA5',
             'CHIRPS',
             'CPC',
             'PRECL',
             'CAMS_OPI',
             'GPCC',
             'GPCP',
             'JRA55',
             'NCEP',
             'MSWEP',
             'MERRA2')

## load data and spi calculation
for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  
  if (dataset == "JRA55") {
    # dir_spi='/data/disk1/JRA55'
    # fname<-file.path(dir_spi, 'JRA55_1981_2020_25_monthly.nc')
    fname <- file.path(dir_oss, 'JRA55_1981_2021_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prlr")
  } else if (dataset == "GPCP") {
    # dir_spi='/data/disk1/GPCPv2_3/'
    # fname <- file.path(dir_spi, 'gpcp_cdr_v23rB1_1981_2020.nc')
    fname <- file.path(dir_oss, 'gpcp_cdr_v23rB1_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "GPCC") {
    # dir_spi='/data/disk1/GPCCv2018'
    fname <- file.path(dir_oss, 'prec_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-99999.9921875"] <- NA
  } else if (dataset == "CAMS_OPI") {
    # dir_spi='/data/disk1/CAMS_OPI/cams_opi_v0208'
    # fname<-file.path(dir_spi, 'camsopi_timecorrect-2.5-1981-2020.nc')
    fname <-
      file.path(dir_oss,
                'camsopi_timecorrect-2.5-1981-2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prcp")
    # obs=obs[,,1:(dim(obs)[3]-12)]
    obs[obs == "-9.99e+08"] <- NA
    obs[, , 63] = NA * obs[, , 63] #BE CAREFULL, 198603 is missing
    
  } else if (dataset == "PRECL") {
    # dir_spi='/data/disk1/PRECL'
    fname <-
      file.path(dir_oss, 'precip.mon.mean.2.5x2.5.1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "rain")
  } else if (dataset == "CPC") {
    # dir_spi='/data/disk1/CPC_GLOBAL_PRECIP'
    fname <- file.path(dir_oss, 'precip_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs = obs[, , -dim(obs)[3]] #eliminate current month
    obs[obs == "-9.96920996838687e+36"] <- NA
  } else if (dataset == "CHIRPS") {
    # dir_spi='/data/disk1/CHIRPS'
    fname <- file.path(dir_oss, 'chirps-v2.0.monthly-2.5.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "ERA5") {
    # dir_spi='/data/disk1/ERA5'
    # fname<-file.path(dir_spi, 'ERA5-drop.nc')
    fname <- file.path(dir_oss, 'ERA5-drop.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "tp")
    obs[obs == "-32767s"] <- NA
  } else if (dataset == "NCEP") {
    # dir_spi='/data/disk1/NCEP'
    fname <- file.path(dir_oss, 'prate_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prate")
  } else if (dataset == "MERRA2") {
    # dir_spi='/data/disk1/MERRA2'
    fname <- file.path(dir_oss, 'MERRA2_1981_2021_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "PRECTOTLAND")
    obs[obs == "999999986991104"] <- NA
  } else if (dataset == "MSWEP") {
    fname <- file.path(dir_oss, 'MSWEP_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precipitation")
    obs[obs == "-9999.f"] <- NA
    
    } else {
    print('dataset not known')
    }
  
    
    obs = obs[, ncol(obs):1, ]
    
    dum = obs
    dum[1:(nrow(obs) / 2), , ] = obs[(nrow(obs) / 2 + 1):nrow(obs), , ]
    dum[(nrow(obs) / 2 + 1):nrow(obs), , ] = obs[1:(nrow(obs) / 2), , ]
    rm(obs)
    obs = dum
    rm(dum)
    
    #image.plot(lon, lat, obs[,,100])
    #plot(wrld_simpl, add = TRUE)
    
    ## keep only 1993-2020 period
    obs=obs[,,i_1993:i_last]
    for (i in 1:dim(obs)[3]) {
      obs[, , i] = inout * obs[, , i]
    }
    
    aux=apply(obs,c(1,2),mean,na.rm=TRUE)
    for (i in 1:dim(obs)[3]) {
      obs[,,i] = aux/aux * obs[,,i]
    }
    
    spi3 = obs * NA
    spi6 = obs * NA
    spi12 = obs * NA
    
    for (i in 1:length(lon)) {
      print(paste0('grid ',i,' of ',length(lon)))   
      for (j in 1:length(lat)) {
        #if (!is.na(inout[i, j])) {
        if (!is.na(aux[i, j]/aux[i, j])) {

          
          dum <- spi(obs[i, j,], 3, na.rm = TRUE)
          spi3[i, j,] = dum$fitted
          rm(dum)
          
          dum <- spi(obs[i, j,], 6, na.rm = TRUE)
          spi6[i, j,] = dum$fitted
          rm(dum)
          
          dum <- spi(obs[i, j,], 12, na.rm = TRUE)
          spi12[i, j,] = dum$fitted
          rm(dum)
          
        }
      }
    }
    
    spi3[is.infinite(spi3)] <- NA
    spi3[is.na(spi3)] <- NA
    
    spi6[is.infinite(spi6)] <- NA
    spi6[is.na(spi6)] <- NA
    
    spi12[is.infinite(spi12)] <- NA
    spi12[is.na(spi12)] <- NA
    
    # ## plot
    # anno_2000 = which(anni == 2020)
    # mese_aug = which(mesi == 8)
    # mese_aug2000 = mese_aug[anno_2000]
    # # 
    # image.plot(lon, lat, spi6[, , mese_aug2000])
    # plot(wrld_simpl, add = TRUE)
    
    
    
    ### SAVE
    #save(spi1, file = file.path(dir_data, paste0("SPI1_", dataset, "_1981_2020.RData")))
    save(spi3, file = file.path(dir_drop, paste0("SPI3_", dataset, "_1993_2020.RData")))
    save(spi6, file = file.path(dir_drop, paste0("SPI6_", dataset, "_1993_2020.RData")))
    save(spi12, file = file.path(dir_drop, paste0("SPI12_", dataset, "_1993_2020.RData")))
}


#load(file.path('C:/Users/migue/OneDrive/Escritorio/4drop/data/spi6_datasets/SPI6_GPCC_1981_2020.RData'))
dim(spi6)
image.plot(lon, lat, spi3[,,3])
plot(wrld_simpl, add = TRUE)
#(vector = as.numeric(spi6[,,479]))
#summary(vector)


 
