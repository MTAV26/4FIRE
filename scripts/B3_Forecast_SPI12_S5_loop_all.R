rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(fields)
library(maps)
library(maptools)
library(SPEI)

data(wrld_simpl)


where = 'onfire'
anni = 2000:2020


mesi = rep(1:12, length(anni))

## fix parameters
num_ens = 25
nome_variable = 'TP'
if (where == 'mac') {
  dir_oss = '/Users/marco/Documents/dati/4DROP/'
  dir_s5 = '/Users/marco/Documents/dati/SEAS5/'
  dir_out = '/Users/marco/Documents/dati/obs/DROP/'
} else if (where == 'climax') {
  dir_oss = '/home/marco/4drop/'
  dir_out = '/home/marco/4drop/'
  dir_s5 = '/disk2/SEAS5/'
} else if (where == 'tormenta') {
  dir_oss = '/home/marco/4DROP/'
  dir_out = '/home/marco/4DROP/'
  dir_s5 = '/home/marco/4DROP/'
} else if (where == 'onfire') {
  dir_oss = '/diskonfire/DROP/'
  dir_s5 = '/diskonfire/4FIRE/'
  dir_out  = '/diskonfire/4FIRE/'
  load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
  load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
}

# dir_oss='C:/Users/Usuario/Dropbox/4FIRE/data/DROP/'
# dir_s5='C:/Users/Usuario/Dropbox/4FIRE/data/SEAS5/'
# dir_out ='C:/Users/Usuario/Dropbox/4FIRE/data/SEAS5/'

# load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
# load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
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



datasets = c(
  'CAMS_OPI',
  'CHIRPS',
  'CPC',
  'ERA5',
  'GPCC',
  'GPCP',
  'JRA55',
  'MERRA2',
  'MSWEP',
  'NCEP',
  'PRECL'
)


start_dates =c(8,11,2)

for (istart_date in 1:length(start_dates)) {
  start_date = start_dates[istart_date]
  
  if (start_date == 2) {
    target_season = 'FMAM'
    
  } else if (start_date == 5) {
    target_season = 'MJJA'
    
    
  } else if (start_date == 8) {
    target_season = 'ASON'
    
    
  } else if (start_date == 11) {
    target_season = 'NDJF'
    
  }
  
  
  ## load data and spi calculation
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
    

    ## keep only 2000-2020 period en total 21 años
    prec = obs[, , 229:480]
    
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = inout * prec[, , i]
    }
    
    aux = apply(prec, c(1, 2), mean, na.rm = TRUE)
    image.plot(lon, lat, aux / aux)
    plot(wrld_simpl, add = TRUE)
    
    for (i in 1:dim(prec)[3]) {
      prec[, , i] = aux / aux * prec[, , i]
    }
    
    mesi_start = which(mesi == 1)
    
    
    #
    load(file = paste0(
      dir_s5,
      'SEASONAL5.TP.',
      sprintf('%02d', start_date),
      "_", target_season, "_",
      dataset,
      '_adj.Rdata'
    ))
  
    
    ## predict spi12 for august, july and august are predicted, march, april, may, june, observed
    ## spei forecast
    spi12pred4 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3], num_ens))
    spi12pred3 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3], num_ens))
    spi12pred2 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3], num_ens))
    spi12pred1 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3], num_ens))
    
    
    
    for (ianni in 1:length(anni)) {
      if (target_season == 'DJFM' && ianni == 1) {
        next
      }

      
      anno_for = which(anni == anni[ianni])
      mese_start = mesi_start[anno_for]
      
      
      #resampling dati storici
      for (ires in 1:num_ens) {
        # print(mese_start)
        PRE_FOR4 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], length(anni) * 12))
        PRE_FOR4 = prec
        
        
        PRE_FOR3 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], length(anni) * 12))
        PRE_FOR3 = prec
        
        PRE_FOR2 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], length(anni) * 12))
        PRE_FOR2 = prec
        
        PRE_FOR1 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], length(anni) * 12))
        PRE_FOR1 = prec
        
        if (target_season == 'NDJF' && start_date == 11) {
          PRE_FOR4[, , (mese_start  - 2):(mese_start + 11)] = NA
          PRE_FOR3[, , (mese_start  - 2):(mese_start + 11)] = NA
          PRE_FOR2[, , (mese_start  - 2):(mese_start + 11)] = NA
          PRE_FOR1[, , (mese_start  - 2):(mese_start + 11)] = NA
          
        } else {
          PRE_FOR4[, , (mese_start + start_date - 1):(mese_start + 11)] = NA
          PRE_FOR3[, , (mese_start + start_date - 1):(mese_start + 11)] = NA
          PRE_FOR2[, , (mese_start + start_date - 1):(mese_start + 11)] = NA
          PRE_FOR1[, , (mese_start + start_date - 1):(mese_start + 11)] = NA
        }
        
        
        if (start_date == 5) {
          PRE_FOR4[, , (mese_start + 4)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR4[, , (mese_start + 5)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR4[, , (mese_start + 6)] = s5_adj[, , 3, ianni, ires]
          PRE_FOR4[, , (mese_start + 7)] = s5_adj[, , 4, ianni, ires]
          
          PRE_FOR3[, , (mese_start + 4)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR3[, , (mese_start + 5)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR3[, , (mese_start + 6)] = s5_adj[, , 3, ianni, ires]
          
          PRE_FOR2[, , (mese_start + 4)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR2[, , (mese_start + 5)] = s5_adj[, , 2, ianni, ires]
          
          PRE_FOR1[, , (mese_start + 4)] = s5_adj[, , 1, ianni, ires]
          
          
        }  else if (start_date == 8) {
          PRE_FOR4[, , (mese_start + 7)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR4[, , (mese_start + 8)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR4[, , (mese_start + 9)] = s5_adj[, , 3, ianni, ires]
          PRE_FOR4[, , (mese_start + 10)] = s5_adj[, , 4, ianni, ires]
          
          PRE_FOR3[, , (mese_start + 7)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR3[, , (mese_start + 8)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR3[, , (mese_start + 9)] = s5_adj[, , 3, ianni, ires]
          
          PRE_FOR2[, , (mese_start + 7)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR2[, , (mese_start + 8)] = s5_adj[, , 2, ianni, ires]
          
          PRE_FOR1[, , (mese_start + 7)] = s5_adj[, , 1, ianni, ires]
          
        }  else if (start_date == 11) {
          PRE_FOR4[, , (mese_start - 2)] = s5_adj[, , 1, ianni - 1, ires]
          PRE_FOR4[, , (mese_start - 1)] = s5_adj[, , 2, ianni - 1, ires]
          PRE_FOR4[, , (mese_start)]     = s5_adj[, , 3, ianni - 1, ires]
          PRE_FOR4[, , (mese_start + 1)] = s5_adj[, , 4, ianni - 1, ires]
          
          PRE_FOR3[, , (mese_start - 2)] = s5_adj[, , 1, ianni - 1, ires]
          PRE_FOR3[, , (mese_start - 1)] = s5_adj[, , 2, ianni - 1, ires]
          PRE_FOR3[, , (mese_start)]     = s5_adj[, , 3, ianni - 1, ires]
          
          PRE_FOR2[, , (mese_start - 2)] = s5_adj[, , 1, ianni - 1, ires]
          PRE_FOR2[, , (mese_start - 1)] = s5_adj[, , 2, ianni - 1, ires]
          
          PRE_FOR1[, , (mese_start - 2)] = s5_adj[, , 1, ianni - 1, ires]
          
        }  else if (start_date == 2) {
          PRE_FOR4[, , (mese_start + 1)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR4[, , (mese_start + 2)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR4[, , (mese_start + 3)] = s5_adj[, , 3, ianni, ires]
          PRE_FOR4[, , (mese_start + 4)] = s5_adj[, , 4, ianni, ires]
          
          PRE_FOR3[, , (mese_start + 1)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR3[, , (mese_start + 2)] = s5_adj[, , 2, ianni, ires]
          PRE_FOR3[, , (mese_start + 3)] = s5_adj[, , 3, ianni, ires]
          
          PRE_FOR2[, , (mese_start + 1)] = s5_adj[, , 1, ianni, ires]
          PRE_FOR2[, , (mese_start + 2)] = s5_adj[, , 2, ianni, ires]
          
          PRE_FOR1[, , (mese_start + 1)] = s5_adj[, , 1, ianni, ires]
          
          
        } else {
          print('start date not known')
        }  
        
        ## calculate SPI
        spitmp4 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3]))
        spitmp3 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3]))
        spitmp2 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3]))
        spitmp1 = array(data = NA, dim = c(dim(prec)[1], dim(prec)[2], dim(prec)[3]))
        
        
        
        for (i in 1:dim(prec)[1]) {
          print(paste0(
            "anni: ",
            anni[ianni], "; start date: ",start_date,
            "; ",dataset, "; ensemble ",
            ires,
            ' grid ',
            i,
            ' of ',
            length(lon)
          ))
          # cat("loop", anni[ianni], "\n")
          # cat("loop", ires, "\n")
          #
          for (j in 1:dim(prec)[2]) {
            if (!is.na(inout[i, j])) {
              
              dum4 <- spi(PRE_FOR4[i, j,], 12, na.rm = TRUE)
              dum3 <- spi(PRE_FOR3[i, j,], 12, na.rm = TRUE)
              dum2 <- spi(PRE_FOR2[i, j,], 12, na.rm = TRUE)
              dum1 <- spi(PRE_FOR1[i, j,], 12, na.rm = TRUE)
              
              spitmp4[i, j,] = dum4$fitted
              spitmp3[i, j,] = dum3$fitted
              spitmp2[i, j,] = dum2$fitted
              spitmp1[i, j,] = dum1$fitted
              
              rm(dum4, dum3, dum2, dum1)
              
            }
          }
        }
        
        

        if (target_season == 'MJJA') {
          
          spi12pred4[, , mese_start + 7, ires] = spitmp4[, , mese_start + 7]
          spi12pred3[, , mese_start + 6, ires] = spitmp3[, , mese_start + 6]
          spi12pred2[, , mese_start + 5, ires] = spitmp2[, , mese_start + 5]
          spi12pred1[, , mese_start + 4, ires] = spitmp1[, , mese_start + 4]
          
        }else if (target_season == 'ASON') {
          
          spi12pred4[, , mese_start + 10, ires] = spitmp4[, , mese_start + 10]
          spi12pred3[, , mese_start + 9, ires] = spitmp3[, , mese_start + 9]
          spi12pred2[, , mese_start + 8, ires] = spitmp2[, , mese_start + 8]
          spi12pred1[, , mese_start + 7, ires] = spitmp1[, , mese_start + 7]
          
        } else if (target_season == 'NDJF') {
          spi12pred4[, , mese_start - 2, ires] = spitmp4[, , mese_start - 2]
          spi12pred3[, , mese_start - 1, ires] = spitmp3[, , mese_start - 1]
          spi12pred2[, , mese_start    , ires] = spitmp2[, , mese_start    ]
          spi12pred1[, , mese_start + 1, ires] = spitmp1[, , mese_start + 1]
          
        } else if (target_season == 'FMAM') {
          spi12pred4[, , mese_start + 4, ires] = spitmp4[, , mese_start + 4]
          spi12pred3[, , mese_start + 3, ires] = spitmp3[, , mese_start + 3]
          spi12pred2[, , mese_start + 2, ires] = spitmp2[, , mese_start + 2]
          spi12pred1[, , mese_start + 1, ires] = spitmp1[, , mese_start + 1]
          
        }
      }
    }
    
    if (target_season == 'MJJA') {
      save(spi12pred4, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_AUG_", dataset, ".RData", sep = "") ))
      save(spi12pred3, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_JUL_", dataset, ".RData", sep = "") ))
      save(spi12pred2, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_JUN_", dataset, ".RData", sep = "") ))
      save(spi12pred1, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_MAY_", dataset, ".RData", sep = "") ))
      
    }else if (target_season == 'ASON') {
      save(spi12pred4, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_NOV_", dataset, ".RData", sep = "") ))
      save(spi12pred3, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_OCT_", dataset, ".RData", sep = "") ))
      save(spi12pred2, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_SEP_", dataset, ".RData", sep = "") ))
      save(spi12pred1, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_AUG_", dataset, ".RData", sep = "") ))
      
    } else if (target_season == 'NDJF') {
      
      save(spi12pred4, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_FEB_", dataset, ".RData", sep = "") ))
      save(spi12pred3, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_JAN_", dataset, ".RData", sep = "") ))
      save(spi12pred2, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_DEC_", dataset, ".RData", sep = "") ))
      save(spi12pred1, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_NOV_", dataset, ".RData", sep = "") ))
      
    } else if (target_season == 'FMAM') {
      
      save(spi12pred4, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_MAY_", dataset, ".RData", sep = "") ))
      save(spi12pred3, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_APR_", dataset, ".RData", sep = "") ))
      save(spi12pred2, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_MAR_", dataset, ".RData", sep = "") ))
      save(spi12pred1, file = file.path(dir_out, paste("spi12SEAS5_", sprintf("%02d", start_date),"_FEB_", dataset, ".RData", sep = "") ))
      
    }
  }
}
