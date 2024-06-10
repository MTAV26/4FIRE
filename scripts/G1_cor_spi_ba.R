# Clear workspace and graphical devices
rm(list = ls())
graphics.off()
gc()

# Load necessary libraries
library(ncdf4)
library(fields)
library(randomForest)
library(verification)

dir_oss ="C:/Users/Usuario/Dropbox/4FIRE_V2/data/"
dir_results ="C:/Users/Usuario/Dropbox/4FIRE_V2/results/"
dir_out="C:/Users/Usuario/Dropbox/4FIRE_V2/plots/"
dir_mask="C:/Users/Usuario/Dropbox/4FIRE/data/mask_season/"

fname <- file.path(dir_oss, 'DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat


load(paste0(dir_oss, "/BA_200101_202012_nat.RData"))
ni = dim(BA)[1]
nj = dim(BA)[2]

dataset='MSWEP'
years = 2001:2020
season = 'DJF'
method = 'RFOR'


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
      } else {
        i1 = (iyear - 1) * 12 + first_month
        i2 = (iyear - 1) * 12 + last_month
      }
      BAS[i, j, iyear] = sum(BA[i, j, i1:i2], na.rm = TRUE)
    }
  }
}

if (season == "DJF") {
  BAS = BAS[, , BAS[3] - 1]
}

mask = array(0, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    if (length(which(BAS[i, j, ] > 0)) >= 10) {
      mask[i, j] = 1
    }
  }
}

rm(BA)
BA = BAS * NA

# Load SPI data
load(file.path(paste0(
  dir_oss, "SPI3_", dataset, "_1993_2020.RData"
)))
load(file.path(paste0(
  dir_oss, "SPI6_", dataset, "_1993_2020.RData"
)))
load(file.path(paste0(
  dir_oss, "SPI12_", dataset, "_1993_2020.RData"
)))

spi3[is.infinite(spi3)] = NA
spi6[is.infinite(spi6)] = NA
spi12[is.infinite(spi12)] = NA

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

load(paste0(dir_results, "roc_area_",  season,"_",method, "_",dataset, ".RData"))
load(paste0(dir_results, "sig_", season,"_",method, "_", dataset, ".RData"))
load(paste0(dir_results, "best_m_spi_a_fin_", season,"_",method, "_", dataset, ".RData"))
load(paste0(dir_results, "best_t_spi_a_fin_",season,"_",method, "_", dataset, ".RData"))

100*length(which(best_sig<0.01))/(length(which(best_sig<0.01))+length(which(best_sig>=0.01)))

rho = NA*best_m_spi_a_fin
sig = rho

# Loop through all geographic coordinates
for (i in 1:length(lon)) {
  for (j in 1:length(lat)) {
    if (!is.na(best_sig[i, j]) && best_sig[i, j] <= 0.01) {  # Process only if the mask condition is met
      # Arrays to hold the metrics for each iteration
      k = 0  # Counter for iterations
    
      im_spi_a = best_m_spi_a_fin[i,j]+ last_month
    
      spi_a_aux  = vector()
      
      if (im_spi_a <= -12) {
        im_ok = 24 + im_spi_a
        dum = spi_a[i, j, 1:(dim(spi_a)[3] - 24), best_t_spi_a_fin[i,j]] # 240
        spi_a_aux = dum[seq(im_ok, length(dum), 12)]
      } else if (im_spi_a > -12 & im_spi_a <= 0) {
        im_ok = 12 + im_spi_a
        dum = spi_a[i, j, 13:(dim(spi_a)[3] - 12), best_t_spi_a_fin[i,j]] # 252
        spi_a_aux = dum[seq(im_ok, length(dum), 12)]
      } else {
        im_ok = im_spi_a
        dum = spi_a[i, j, 25:dim(spi_a)[3], best_t_spi_a_fin[i,j]]  # 264
        spi_a_aux = dum[seq(im_ok, length(dum), 12)]
      }
      
      if (length(which(is.na(spi_a_aux))) >= 1) {
        next
      }
      
      if (length(unique(spi_a_aux)) <= 2) {
        next
      }
      
      aux=cor.test(BAS[i,j,],spi_a_aux,method="spearman")
      rho[i,j]=aux$estimate
      sig[i,j]=aux$p.value
  
    }
  }
}

image.plot(lon,lat,rho)
world(add = TRUE, col = "black")

save(rho, file = paste0(dir_results, "cor_", season,"_",method, "_", dataset, ".RData"))

# best_rho=rho
# best_rho[sig > 0.01] = NA
# 
# image.plot(lon,lat,best_rho)
# world(add = TRUE, col = "black")
# 
# best_roc2=best_roc
# best_roc2[best_sig > 0.05] = NA
# image.plot(lon,lat,best_roc2)
# world(add = TRUE, col = "black")
# 
# length(which(best_sig>0.05))
# length(which(best_sig<=0.05))





