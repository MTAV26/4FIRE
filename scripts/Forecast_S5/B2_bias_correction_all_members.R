rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(fields)
library(maps)
library(maptools)

data(wrld_simpl)

# where = 'onfire'
start_date = 08
anni = 1993:2020
anni_test = 2001:2020
mesi_test = rep(1:12, length(anni_test))

i0=which(match(anni, 2001)==1)

anni81 = 1981:2020
mesi81 = rep(1:12, length(anni))

fechas <- seq(as.Date("1981-01-01"), as.Date("2020-12-01"), by = "month")
i_1993=which(fechas=="1993-01-01")
i_last=which(fechas=="2020-12-01")
fechas93 <- seq(as.Date("1993-01-01"), as.Date("2020-12-01"), by = "month")
i_test=which(fechas93=="2001-01-01")

mesi = rep(1:12, length(anni))
mesi_test = rep(1:12, length(anni_test))





if (start_date == 11) {
  target_season = 'NDJF'
  dates = c(11,12,1,2)

} else if (start_date == 2) {
  target_season = 'FMAM'
  dates = c(2,3,4,5)

} else if (start_date == 5) {
  target_season = 'MJJA'
  dates = c(5,6,7,8)
  

} else if (start_date == 8) {
  target_season = 'ASON'
  # dates = c(8,9,10,11)
  dates = c(8)
}

num_ens = 25
nome_variable = 'tp'

# if (where == 'mac') {
#   dir_oss = '/Users/marco/Documents/dati/4DROP/'
#   dir_s5 = '/Users/marco/Documents/dati/SEAS5/'
#   dir_out = '/Users/marco/Documents/dati/obs/DROP/'
#   load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
#   load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
# } else if (where == 'climax') {
#   dir_oss = '/home/marco/4drop/'
#   dir_out = '/home/marco/4drop/'
#   dir_s5 = '/Users/marco/Documents/dati/SEAS5/'
#   load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
#   load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
# } else if (where == 'onfire') {
#   dir_oss = '/diskonfire/DROP/'
#   dir_out = '/diskonfire/4FIRE/'
#   dir_s5 = '/diskonfire/4FIRE/'
#   load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
#   load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
# }

dir_oss='C:/Users/Usuario/Dropbox/4FIRE_V2/DROP/'
dir_s5='C:/Users/Usuario/Dropbox/4FIRE_V2/SEAS5/'
dir_out ='C:/Users/Usuario/Dropbox/4FIRE_V2/SEAS5/'
# 

load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
CRS.new <-
  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(wrld_simpl) <- CRS.new
points <- expand.grid(lon, lat)
pts = SpatialPoints(points)
proj4string(pts) <- CRS.new
ii <- !is.na(over(pts, wrld_simpl))
inout = ii[, 1]
dim(inout) <- c(length(lon), length(lat))

inout[inout == 0] = NA

image.plot(lon, lat, inout)
plot(wrld_simpl, add = TRUE)
# 
datasets = c(
  # 'CAMS_OPI',
  # 'CHIRPS',
  # 'CPC',
  # 'ERA5',
  # 'GPCC',
  # 'GPCP',
  # 'JRA55',
  # 'MERRA2',
  # 'MSWEP',
  # 'NCEP',
  'PRECL'
)

# datasets=c("CAMS_OPI")
# mesi = rep(1:12, length(anni))
# mesi = rep(1:12, length(anni))


for (idata in 1:length(datasets)) {
  dataset = datasets[idata]
  print(dataset)
  
  if (dataset == "JRA55") {
    fname <- file.path(dir_oss, 'JRA55_1981_2021_25_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prlr")
  } else if (dataset == "GPCP") {
    fname <- file.path(dir_oss, 'gpcp_cdr_v23rB1_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "GPCC") {
    fname <- file.path(dir_oss, 'prec_1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-99999.9921875"] <- NA
  } else if (dataset == "CAMS_OPI") {
    fname <-
      file.path(dir_oss,
                'camsopi_timecorrect-2.5-1981-2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prcp")
    # obs=obs[,,1:(dim(obs)[3]-12)]
    obs[obs == "-9.99e+08"] <- NA
    obs[, , 63] = NA * obs[, , 63] #BE CAREFULL, 198603 is missing
  } else if (dataset == "PRECL") {
    fname <-
      file.path(dir_oss, 'precip.mon.mean.2.5x2.5.1981_2021.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "rain")
  } else if (dataset == "CPC") {
    fname <- file.path(dir_oss, 'precip_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs = obs[, , -dim(obs)[3]] #eliminate current month
    obs[obs == "-9.96920996838687e+36"] <- NA
  } else if (dataset == "CHIRPS") {
    fname <- file.path(dir_oss, 'chirps-v2.0.monthly-2.5.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "precip")
    obs[obs == "-9999"] <- NA
  } else if (dataset == "ERA5") {
    fname <- file.path(dir_oss, 'ERA5-drop.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "tp")
    obs[obs == "-32767s"] <- NA
  } else if (dataset == "NCEP") {
    fname <- file.path(dir_oss, 'prate_1981_2021_monthly.nc')
    obs.nc <- nc_open(fname)
    obs <- ncvar_get(obs.nc, "prate")
  } else if (dataset == "MERRA2") {
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
  
  obs = obs[, ncol(obs):1,]
  dum = obs
  dum[1:(nrow(obs) / 2), ,] = obs[(nrow(obs) / 2 + 1):nrow(obs), ,]
  dum[(nrow(obs) / 2 + 1):nrow(obs), ,] = obs[1:(nrow(obs) / 2), ,]
  rm(obs)
  obs = dum
  rm(dum)
  
  dim(obs)
  ## keep only 1993-2020 period
  prec = obs[, , i_1993:i_last]
  

  for (i in 1:dim(prec)[3]) {
    prec[, , i] = inout * prec[, , i]
  }
  
  aux = apply(prec, c(1, 2), mean, na.rm = TRUE)
  # image.plot(lon, lat, aux / aux)
  # plot(wrld_simpl, add = TRUE)
  
  for (i in 1:dim(prec)[3]) {
    prec[, , i] = aux / aux * prec[, , i]
  }
  
  # image.plot(lon, lat, apply(prec, c(1, 2), mean, na.rm = TRUE))
  # plot(wrld_simpl, add = TRUE)
  
  ## load s5
  load(paste0(
    dir_s5,
    'SEASONAL5.TP.',
    sprintf('%02d', start_date),
    '_all_members.Rdata'
  ))
  
  
  dim(s5)
  # s5 = array(NA, dim = c(length(lon), length(lat),length(dates),length(anni),num_ens))
  
  # image.plot(lon, lat, apply(s5, c(1, 2), mean))
  # plot(wrld_simpl, add = TRUE)
  
  s5_ens = apply(s5, c(1, 2, 3, 4), mean, na.rm = T)
  dim(s5_ens)
  image.plot(lon, lat, s5_ens[,,1,28])

  
  # cc  
  
  ## bias correction
  s5_adj = array(NA, dim = c(
    length(lon),
    length(lat),
    length(dates),
    length(anni),
    num_ens
  ))
  
  dim(prec)
  dim(s5)
  dim(s5_adj)
  dim(s5_ens)
  
  for (ilead in 1:length(dates)) {
    cat('Processing month', dates[ilead], '\n')
    
    mesi_calibration = which(mesi == dates[ilead])

    ind = 1:length(mesi_calibration)
    cat('ind', ind, '\n')
    

    for (iyear in i0:length(anni)) {
      itest_s5 =ind[iyear]
      cat('itest_s5', ind[iyear], '\n')

      itrain_obs = mesi_calibration[-iyear]
      cat('itrain_obs', mesi_calibration[-iyear], '\n')

      itrain_s5 = ind[-iyear]
      cat('itrain_s5', ind[-iyear], '\n')
      cat('·············, \n')
        
        for (i in 1:length(lon)) {
          for (j in 1:length(lat)) {
            if (!is.na(inout[i, j])) {
              x_train = prec[i, j, itrain_obs]
              y_train <- s5_ens[i, j, ilead, itrain_s5]

            bias = mean(x_train, na.rm = TRUE) / mean(y_train, na.rm = TRUE)

            for (iens in 1:num_ens) {
              
              y_test <- s5[i, j, ilead, itest_s5, iens]
              s5_adj[i, j, ilead, itest_s5, iens] = y_test * bias

            }
          }
        }
      }
    }
  }
# 
#   
#   
#   save(s5_adj,
#        file = paste0(
#          dir_out,
#          'SEASONAL5.TP.',
#          sprintf('%02d', start_date),
#          "_",target_season, "_",
#          dataset,
#          '_adj.Rdata'
#        ))
#   
#   # image.plot(lon, lat, apply(s5_ens, c(1, 2), mean, na.rm = T))
#   # plot(wrld_simpl, add = TRUE)
#   # #
#   # image.plot(lon, lat, apply(s5_adj, c(1, 2), mean, na.rm = T))
#   # plot(wrld_simpl, add = TRUE)
#   
#   # image.plot(lon, lat, apply(obs[,,], c(1, 2), mean,na.rm=T))
#   # plot(wrld_simpl, add = TRUE)
#   #
#   
 }




dim(s5_adj)

summary(as.vector(s5_adj[,,2,9,25]))
summary(as.vector(s5_adj[,,2,1,24]))
summary(as.vector(s5_adj[,,2,1,15]))
summary(as.vector(s5_adj[,,2,1,13]))
summary(as.vector(s5_adj[,,2,1,8]))


summary(as.vector(s5_adj[,,4,20,20]))
summary(as.vector(s5_adj[,,1,17,22]))
summary(as.vector(s5_adj[,,1,13,22]))
summary(as.vector(s5_adj[,,1,4,22]))
summary(as.vector(s5_adj[,,1,28,25]))
# 
# 
# 
# dim(s5_adj)
# image.plot(lon, lat, s5_adj[,,1,21,25])
# 
# 
# dim(prec)
# dim(s5_ens)
