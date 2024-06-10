# Clear workspace and graphical devices
rm(list = ls())
graphics.off()
gc()

# Load necessary libraries
library(ncdf4)
library(fields)
# install.packages("randomForest")
library(randomForest)
library(verification)


where = 'chispita'

if (where == 'miguel') {
  dir_data = "C:/Users/Usuario/Dropbox/4FIRE_V2/data/"
  dir_out = "C:/Users/Usuario/Dropbox/4FIRE_V2/results/"
} else if (where == 'chispita') {
  dir_data = "/home/miguel/Dropbox/4FIRE_V2/data/"
  dir_out = "/home/miguel/Dropbox/4FIRE_V2/results/"
}

#===============================================================================
# load lon lat
#===============================================================================

fname <- file.path(dir_data, 'DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat

#===============================================================================
# load BA
#===============================================================================

load(paste0(dir_data, "/BA_200101_202012_nat.RData"))
ni = dim(BA)[1]
nj = dim(BA)[2]

#===============================================================================
# to select -----
#===============================================================================
seasons = c(
  # 'JJA',
    # 'DJF'
     #'MAM'
     'SON'
    )
dataset = c('MSWEP')

load(file.path(paste0(dir_data, "SPI3_", dataset, "_1993_2020.RData")))
load(file.path(paste0(dir_data, "SPI6_", dataset, "_1993_2020.RData")))
load(file.path(paste0(dir_data, "SPI12_", dataset, "_1993_2020.RData")))

spi3[is.infinite(spi3)] = NA
spi6[is.infinite(spi6)] = NA
spi12[is.infinite(spi12)] = NA

for (iseas in 1:length(seasons)) {
  season = seasons[iseas]
  print(season)
  
  years = 2001:2020
  version = paste0(season, "_RFOR")
  
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
  
  
  BAS = array(0, dim = c(ni, nj, length(years)))
  for (i in 1:ni) {
    for (j in 1:nj) {
      for (iyear in 1:length(years)) {
        if (season == 'DJF' && first_month == 12) {
          i1 = (iyear - 1) * 12
          i2 = (iyear - 1) * 12 + last_month
          # print(paste0(season, ": ", i1:i2))
        } else {
          i1 = (iyear - 1) * 12 + first_month
          i2 = (iyear - 1) * 12 + last_month
          # print(paste0(season, ": ", i1:i2)) 
        }
        BAS[i, j, iyear] = sum(BA[i, j, i1:i2], na.rm = TRUE)
      }
    }
  }
  
  if (season == "DJF"){
    BAS=BAS[,,BAS[3]-1]
    # years=2002:2020
  }
  
  mask = array(0, dim = c(ni, nj))
  for (i in 1:ni) {
    for (j in 1:nj) {
      # print(length(BA[i,j,]==0))
      if (length(which(BAS[i, j,] > 0)) >= 10) {
        mask[i, j] = 1
      }
    }
  }
  
  rm(BA)
  BA = BAS * NA
  #===============================================================================
  # main loop
  #===============================================================================
  
  years_spi_a = 1993:2020
  if (season == "DJF") {
    years = 2001:2020
  } else {
    years = 2000:2020  #el area quemada solo tiene datos hasta 2020
  }
  
  iok_spi_a = match((years[1] - 1):years[length(years)], years_spi_a) #we need also 1999
  spi_a3 = spi3[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] * 12)]
  spi_a6 = spi6[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] *
                                                       12)]
  spi_a12 = spi12[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] * 12)]
  
  if (season == "DJF") {
    years = 2002:2020
  } else {
    years = 2001:2020  #el area quemada solo tiene datos hasta 2020
  }
  
  spi_a = array(NA, dim = c(ni, nj, dim(spi_a3)[3], 3))
  spi_a[, , , 1] = spi_a3
  spi_a[, , , 2] = spi_a6
  spi_a[, , , 3] = spi_a12
  
  rm(spi_a3)
  rm(spi_a6)
  rm(spi_a12)
  spi_a[is.infinite(spi_a)] = NA
  
  if (season == "DJF") {
    num_iter = length((first_month - 26):last_month) * 3
  } else{
    num_iter = length((first_month - 14):last_month) * 3
  }
  
  # Output the number of iterations for debugging and verification
  print(num_iter)
  
  # Initialize arrays for storing best results
  best_roc = array(NA, dim = c(ni, nj))
  best_sig = array(NA, dim = c(ni, nj))
  best_m_spi_a_fin = array(NA, dim = c(ni, nj))
  best_t_spi_a_fin = array(NA, dim = c(ni, nj))
  best_x1_fin = array(NA, dim = c(ni, nj, length(years)))
  
  # Loop through all geographic coordinates
  for (i in 1:length(lon)) {
    for (j in 1:length(lat)) {
      if (mask[i, j] == 1) {  # Process only if the mask condition is met
        # Arrays to hold the metrics for each iteration
        roca = array(NA, dim = c(num_iter))
        sig = array(NA, dim = c(num_iter))
        best_m_spi_a = array(NA, dim = c(num_iter))
        best_t_spi_a = array(NA, dim = c(num_iter))
        best_x1_1 = array(NA, dim = c(num_iter, length(years)))
        best_x1 = array(NA, dim = c(num_iter, length(years)))
        
        k = 0  # Counter for iterations
        for (im_spi_a in  (if (season == 'DJF' && first_month == 12) {
          (first_month - 26):(last_month)
        } else {
          (first_month - 14):last_month})) {
          #-5
          #antecedente
          for (isc_spi_a in 1:3) {
            k = k + 1
            print(paste0(
              'lon ',
              i,
              '/',
              ni,
              '; lat ',
              j,
              '/',
              nj,
              '; step ',
              k,
              '/',
              num_iter
            ))
            
            spi_a_aux  = vector()
            
            if (im_spi_a <= -12) {
              im_ok = 24 + im_spi_a
              dum = spi_a[i, j, 1:(dim(spi_a)[3] - 24), isc_spi_a] # 240
              spi_a_aux = dum[seq(im_ok, length(dum), 12)]
            } else if (im_spi_a > -12 & im_spi_a <= 0) {
              im_ok = 12 + im_spi_a
              dum = spi_a[i, j, 13:(dim(spi_a)[3] - 12), isc_spi_a] # 252
              spi_a_aux = dum[seq(im_ok, length(dum), 12)]
            } else {
              im_ok = im_spi_a
              dum = spi_a[i, j, 25:dim(spi_a)[3], isc_spi_a]  # 264
              spi_a_aux = dum[seq(im_ok, length(dum), 12)]
            }
            
            if (length(which(is.na(spi_a_aux))) >= 1) {
              next
            }
            
            if (length(unique(spi_a_aux)) <= 2) {
              next
            }
            
            pre1 = vector()
            BAS_train = vector()
            
            for (iy in 1:length(years)) {
              BAS_train = BAS[i, j,-iy]
              BA_train <-
                ifelse(BAS_train > median(BAS_train, na.rm = TRUE), 1, 0)
              spi_a_train = spi_a_aux[-iy]
              spi_a_test = spi_a_aux[iy]
              
              mydata_train = data.frame("y" = (BA_train), "x1" = spi_a_train)
              mydata_test = data.frame("x1" = (spi_a_test))
              
              # Ensure that 'y' is a factor
              mydata_train$y <- as.factor(mydata_train$y)
              # Fit a random forest model
              fit1 <- randomForest(y ~ x1, data = mydata_train, ntree=500, importance=TRUE)
              # Predict using the random forest model
              # We use type="prob" to get predicted probabilities for each class
              pre1[iy] <- predict(fit1, newdata = mydata_test, type = "prob")[, "1"]
              
              # Retrieve and store importance scores
              imp_scores <- importance(fit1)
              best_x1_1[k, iy] = imp_scores["x1", "MeanDecreaseAccuracy"]
            }
            
            # Añades la siguiente condición para verificar si hay NA en pre2
            if (any(is.na(pre1))) {
              next  # Salta a la siguiente iteración si hay valores NA en pre2
            }
            
            BA[i, j, ] <- ifelse(BAS[i, j, ] > median(BAS[i, j, ], na.rm = TRUE), 1, 0)
            roc1 = roc.area(BA[i, j, ], pre1)
            
            roca[k] = roc1$A
            sig[k] = roc1$p.value
            best_x1[k, ] = best_x1_1[k, ]
            best_t_spi_a[k] = isc_spi_a
            best_m_spi_a[k] = im_spi_a - last_month
            
          }
        }
      
  
        if (length(which(is.na(roca))) == length(roca)) {
          next
        }
        
        dum = max((roca), na.rm = TRUE)
        idx = which((roca) == dum, arr.ind = TRUE)
        idx = idx[1]
        
        best_roc[i, j] = roca[idx]
        best_sig[i, j] = sig[idx]
        
        best_m_spi_a_fin[i, j] = best_m_spi_a[idx]
        best_t_spi_a_fin[i, j] = best_t_spi_a[idx]
        
        best_x1_fin[i, j,] = best_x1[idx,]
        
      }
    }
  }
  
  

save(best_roc, file = paste0(dir_out, "roc_area_", version, "_", dataset, ".RData"))
save(best_sig, file = paste0(dir_out, "sig_", version, "_", dataset, ".RData"))
save(best_m_spi_a_fin, file = paste0(dir_out, "best_m_spi_a_fin_", version, "_", dataset, ".RData"))
save(best_t_spi_a_fin,file = paste0(dir_out, "best_t_spi_a_fin_", version, "_", dataset, ".RData"))
save(best_x1_fin, file = paste0(dir_out, "best_x1_", version, "_", dataset, ".RData"))

}

# library(fields)
# image.plot(lon,lat,best_roc)
# world(add = TRUE, col = "black")
# 
# best_roc2=best_roc
# best_roc2[best_sig > 0.05] = NA
# image.plot(lon,lat,best_roc2)
# world(add = TRUE, col = "black")
# 
# length(which(best_sig>0.05))
# length(which(best_sig<=0.05))
