rm(list = ls())
graphics.off()
gc()
# install.packages("randomForest")
library(randomForest)
library(ncdf4)
library(fields)
library(verification)
library(RColorBrewer)

where = 'miguel'
if (where == 'chispita') {
  dir_oss = '/home/miguel/Dropbox/4FIRE_V2/data/'
  dir_s5 = '/home/miguel/Dropbox/4FIRE_V2/data/SEAS5/'
  dir_out = '/home/miguel/Dropbox/4FIRE_V2/results/'
  # source('/home/miguel/Dropbox/4FIRE/script/Model_BA/image_mask.R')
} else if (where == 'miguel') {
  dir_oss = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/'
  dir_s5 = 'C:/Users/Usuario/Dropbox/4FIRE_V2/data/SEAS5/'
  dir_out = 'C:/Users/Usuario/Dropbox/4FIRE_V2/results/'
  #source('C:/Users/Usuario/Dropbox/4FIRE/script/model_BA/image_mask.R')
} else if (where == 'onfire') {
  dir_oss = "/home/miguel/4FIRE_V2/data/"
  dir_model = "/diskonfire/results/4FIRE/"
  dir_s5 = '/home/miguel/4FIRE_V2/SEAS5/'
  dir_out = "/diskonfire/results/4FIRE/"
  #source('C:/Users/Usuario/Dropbox/4FIRE/script/model_BA/image_mask.R')
}

## fix parameters
anni = 1993:2020
mesi = rep(1:12, length(anni))
anno_2001 = which(anni == 2001)
mese_ind = which(mesi == 01)
first_month_serie = mese_ind[anno_2001]
years = 2001:2020 #antes en el original: 2001:2019
num_ens = 25
dataset = 'MSWEP'
season = 'JJA'
version = 'RFOR'


# version = paste0(season)
if (season == 'JJA') {
  first_month = 6
  last_month = 8
  START = "05"
  M1 = "AUG"
  M2 = "JUL"
  M3 = "JUN"
  M4 = "MAY"
} else if (season == 'MAM') {
  first_month = 3
  last_month = 5
  START = "02"
  M1 = "MAY"
  M2 = "APR"
  M3 = "MAR"
  M4 = "FEB"
} else if (season == 'SON') {
  first_month = 9
  last_month = 11
  START = "08"
  M1 = "NOV"
  M2 = "OCT"
  M3 = "SEP"
  M4 = "AUG"
} else if (season == 'DJF') {
  first_month = 12
  last_month = 2
  START = "11"
  M1 = "FEB"
  M2 = "JAN"
  M3 = "DEC"
  M4 = "NOV"
} else {
  print('no season, call terminator')
}

load(paste0(dir_oss, "/BA_200101_202012_nat.RData"))
load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
ni = dim(BA)[1]
nj = dim(BA)[2]

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
      #print(i1:i2)
      BAS[i, j, iyear] = sum(BA[i, j, i1:i2], na.rm = TRUE)
    }
  }
}

if (season == "DJF") {
  BAS = BAS[, , BAS[3] - 1]
  years = 2002:2020
}

mask = array(NA, dim = c(ni, nj))
for (i in 1:ni) {
  for (j in 1:nj) {
    if (length(which(BAS[i, j,] > 0)) >= 10) {
      mask[i, j] = 1
    }
  }
}
BA = BAS * NA


load(paste0(dir_out, "sig_",season,"_", version, "_", dataset, ".RData"))
hist(best_sig)
best_sig[best_sig>=0.01]=NA
mask_model = best_sig / best_sig
image(mask_model)

#####################################################


load(file.path(paste0(
  dir_oss, "SPI3_", dataset, "_1993_2020.RData"
)))
load(file.path(paste0(
  dir_oss, "SPI6_", dataset, "_1993_2020.RData"
)))
load(file.path(paste0(
  dir_oss, "SPI12_", dataset, "_1993_2020.RData"
)))

  years_spi_a = 1993:2020
  if(season == "DJF"){
    years = 2001:2020  
  } else {
    years = 2000:2020  #el area quemada solo tiene datos hasta 2020
  }
  iok_spi_a = match((years[1] - 1):years[length(years)], years_spi_a) #we need also 1999
  
  spi_a3 = spi3[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] * 12  )]
  spi_a6 = spi6[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] * 12  )]
  spi_a12 = spi12[, , (((iok_spi_a[1] - 1) * 12) + 1):(iok_spi_a[length(iok_spi_a)] * 12)]
  
  if(season == "DJF"){
    years = 2002:2020  
  } else {
    years = 2001:2020  #el area quemada solo tiene datos hasta 2020
  }
  
  spi_a = array(NA, dim = c(ni, nj, dim(spi_a3)[3], 3))
  spi_a[, , , 1] = spi_a3
  spi_a[, , , 2] = spi_a6
  spi_a[, , , 3] = spi_a12
  rm(spi_a3, spi_a6, spi_a12)
  spi_a[is.infinite(spi_a)] = NA
  
  spipred = array(NA, dim = c(ni, nj, length(years), num_ens, 4, 3))
  
  
  
  
  if (season == "DJF") {
    load(paste0(dir_s5, "spi3SEAS5_11_FEB_", dataset, ".RData"))
    spi3pred4[is.infinite(spi3pred4)] = NA
    aux = spi3pred4[, , first_month_serie:dim(spi3pred4)[3], ]
    spipred[, , , , 4, 1] = aux[, , seq(last_month + 12, (dim(aux)[3]), 12), ]
    
    load(paste0(dir_s5, "spi3SEAS5_11_JAN_", dataset, ".RData"))
    spi3pred3[is.infinite(spi3pred3)] = NA
    aux = spi3pred3[, , first_month_serie:dim(spi3pred3)[3], ]
    spipred[, , , , 3, 1] = aux[, , seq(last_month + 11, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi3SEAS5_11_DEC_", dataset, ".RData"))
    spi3pred2[is.infinite(spi3pred2)] = NA
    aux = spi3pred2[, , first_month_serie:dim(spi3pred2)[3], ]
    spipred[, , , , 2, 1] = aux[, , seq(first_month, (dim(aux)[3] - 12), 12), ]
    
    load(paste0(dir_s5, "spi3SEAS5_11_NOV_", dataset, ".RData"))
    spi3pred1[is.infinite(spi3pred1)] = NA
    aux = spi3pred1[, , first_month_serie:dim(spi3pred1)[3], ]
    spipred[, , , , 1, 1] = aux[, , seq(first_month - 1, (dim(aux)[3] -
                                                            12), 12), ]
    
    ##############################################################################
    
    load(paste0(dir_s5, "spi6SEAS5_11_FEB_", dataset, ".RData"))
    spi6pred4[is.infinite(spi6pred4)] = NA
    aux = spi6pred4[, , first_month_serie:dim(spi6pred4)[3], ]
    spipred[, , , , 4, 2] = aux[, , seq(last_month + 12 , dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi6SEAS5_11_JAN_", dataset, ".RData"))
    spi6pred3[is.infinite(spi6pred3)] = NA
    aux = spi6pred3[, , first_month_serie:dim(spi6pred3)[3], ]
    spipred[, , , , 3, 2] = aux[, , seq(last_month + 11, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi6SEAS5_11_DEC_", dataset, ".RData"))
    spi6pred2[is.infinite(spi6pred2)] = NA
    aux = spi6pred2[, , first_month_serie:dim(spi6pred2)[3], ]
    spipred[, , , , 2, 2] = aux[, , seq(first_month, (dim(aux)[3] - 12), 12) , ]
    
    load(paste0(dir_s5, "spi6SEAS5_11_NOV_", dataset, ".RData"))
    spi6pred1[is.infinite(spi6pred1)] = NA
    aux = spi6pred1[, , first_month_serie:dim(spi6pred1)[3], ]
    spipred[, , , , 1, 2] = aux[, , seq(first_month - 1, (dim(aux)[3] -
                                                            12), 12), ]
    
    ###############################################################################
    
    load(paste0(dir_s5, "spi12SEAS5_11_FEB_", dataset, ".RData"))
    spi12pred4[is.infinite(spi12pred4)] = NA
    aux = spi12pred4[, , first_month_serie:dim(spi12pred4)[3], ]
    spipred[, , , , 4, 3] = aux[, , seq(last_month + 12, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi12SEAS5_11_JAN_", dataset, ".RData"))
    spi12pred3[is.infinite(spi12pred3)] = NA
    aux = spi12pred3[, , first_month_serie:dim(spi12pred3)[3], ]
    spipred[, , , , 3, 3] = aux[, , seq(last_month + 11, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi12SEAS5_11_DEC_", dataset, ".RData"))
    spi12pred2[is.infinite(spi12pred2)] = NA
    aux = spi12pred2[, , first_month_serie:dim(spi12pred2)[3], ]
    spipred[, , , , 2, 3] = aux[, , seq(first_month, (dim(aux)[3] - 12), 12), ]
    
    load(paste0(dir_s5, "spi12SEAS5_11_NOV_", dataset, ".RData"))
    spi12pred1[is.infinite(spi12pred1)] = NA
    aux = spi12pred1[, , first_month_serie:dim(spi12pred1)[3], ]
    spipred[, , , , 1, 3] = aux[, , seq(first_month - 1, (dim(aux)[3] -
                                                            12), 12), ]
    
  } else {
    ##############################################################################
    load(paste0(
      dir_s5,
      paste0("spi3SEAS5_", START, "_", M1, "_", dataset, ".RData")
    ))
    spi3pred4[is.infinite(spi3pred4)] = NA
    dim(spi3pred4)
    aux = spi3pred4[, , first_month_serie:dim(spi3pred4)[3], ]  #13:252
    spipred[, , , , 4, 1] = aux[, , seq(last_month, dim(aux)[3], 12), ]  # [1]   8  20  32  44  56  68  80  92 104 116 128 140 152 164 176 188 200 212 224 236
    # spipred[, , , , 4, 1] = aux[, , seq(last_month +12, dim(aux)[3], 12),]  # [1]   8  20  32  44  56  68  80  92 104 116 128 140 152 164 176 188 200 212 224 236
    
    load(paste0(dir_s5, "spi3SEAS5_", START, "_", M2, "_", dataset, ".RData"))
    spi3pred3[is.infinite(spi3pred3)] = NA
    aux = spi3pred3[, , first_month_serie:dim(spi3pred3)[3], ]
    spipred[, , , , 3, 1] = aux[, , seq(last_month - 1, dim(aux)[3], 12), ] #[1] 7  19  31  43  55  67  79  91 103 115 127 139 151 163 175 187 199 211 223 235
    # spipred[, , , , 3, 1] = aux[, , seq(last_month + 11, dim(aux)[3], 12),]
    
    load(paste0(dir_s5, "spi3SEAS5_", START, "_", M3, "_", dataset, ".RData"))
    spi3pred2[is.infinite(spi3pred2)] = NA
    aux = spi3pred2[, , first_month_serie:dim(spi3pred2)[3], ]
    spipred[, , , , 2, 1] = aux[, , seq(last_month - 2, dim(aux)[3], 12), ] # [1]   6  18  30  42  54  66  78  90 102 114 126 138 150 162 174 186 198 210 222 234
    
    load(paste0(dir_s5, "spi3SEAS5_", START, "_", M4, "_", dataset, ".RData"))
    spi3pred1[is.infinite(spi3pred1)] = NA
    aux = spi3pred1[, , first_month_serie:dim(spi3pred1)[3], ]
    spipred[, , , , 1, 1] = aux[, , seq(last_month - 3, dim(aux)[3], 12), ] # [1]   5  17  29  41  53  65  77  89 101 113 125 137 149 161 173 185 197 209 221 233
    ##############################################################################
    load(paste0(dir_s5, "spi6SEAS5_", START, "_", M1, "_", dataset, ".RData"))
    spi6pred4[is.infinite(spi6pred4)] = NA
    aux = spi6pred4[, , first_month_serie:dim(spi6pred4)[3], ]
    spipred[, , , , 4, 2] = aux[, , seq(last_month, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi6SEAS5_", START, "_", M2, "_", dataset, ".RData"))
    spi6pred3[is.infinite(spi6pred3)] = NA
    aux = spi6pred3[, , first_month_serie:dim(spi6pred3)[3], ]
    spipred[, , , , 3, 2] = aux[, , seq(last_month - 1, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi6SEAS5_", START, "_", M3, "_", dataset, ".RData"))
    spi6pred2[is.infinite(spi6pred2)] = NA
    aux = spi6pred2[, , first_month_serie:dim(spi6pred2)[3], ]
    spipred[, , , , 2, 2] = aux[, , seq(last_month - 2, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi6SEAS5_", START, "_", M4, "_", dataset, ".RData"))
    spi6pred1[is.infinite(spi6pred1)] = NA
    aux = spi6pred1[, , first_month_serie:dim(spi6pred1)[3], ]
    spipred[, , , , 1, 2] = aux[, , seq(last_month - 3, dim(aux)[3], 12), ]
    ##############################################################################
    load(paste0(dir_s5, "spi12SEAS5_", START, "_", M1, "_", dataset, ".RData"))
    spi12pred4[is.infinite(spi12pred4)] = NA
    aux = spi12pred4[, , first_month_serie:dim(spi12pred4)[3], ]
    spipred[, , , , 4, 3] = aux[, , seq(last_month, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi12SEAS5_", START, "_", M2, "_", dataset, ".RData"))
    spi12pred3[is.infinite(spi12pred3)] = NA
    aux = spi12pred3[, , first_month_serie:dim(spi12pred3)[3], ]
    spipred[, , , , 3, 3] = aux[, , seq(last_month - 1, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi12SEAS5_", START, "_", M3, "_", dataset, ".RData"))
    spi12pred2[is.infinite(spi12pred2)] = NA
    aux = spi12pred2[, , first_month_serie:dim(spi12pred2)[3], ]
    spipred[, , , , 2, 3] = aux[, , seq(last_month - 2, dim(aux)[3], 12), ]
    
    load(paste0(dir_s5, "spi12SEAS5_", START, "_", M4, "_", dataset, ".RData"))
    spi12pred1[is.infinite(spi12pred1)] = NA
    aux = spi12pred1[, , first_month_serie:dim(spi12pred1)[3], ]
    spipred[, , , , 1, 3] = aux[, , seq(last_month - 3, dim(aux)[3], 12), ]
  }
  
  
  ## load models parameters
  load(paste0(dir_out, "best_m_spi_a_fin_", season,"_",version, "_", dataset, ".RData"))
  load(paste0(dir_out, "best_t_spi_a_fin_", season,"_",version, "_", dataset, ".RData"))
  
  roca_pred = best_sig * NA
  sig_pred = roca_pred
  
  roca_obs = roca_pred
  sig_obs = roca_pred
  
  dim(spi6pred1)
  
  predBA = array(NA, dim = c(ni, nj, length(years)))
  predBAobs = array(NA, dim = c(ni, nj, length(years)))
  
  for (i in 1:length(lon)) {
    for (j in 1:length(lat)) {
      # for (i in 13) {
      #   for (j in 62) {
      if (!is.na(best_m_spi_a_fin[i, j]) & !is.na(mask_model[i, j])) {
        spi_a_aux  = rep(NA, length(years))  # 20
        if (!is.na(best_m_spi_a_fin[i, j])) {
          
          im_spi_a = best_m_spi_a_fin[i, j] + last_month# 8
          print(paste0("lon = ",i,"; lat = ", j, "; im_spi_a: ", im_spi_a,  "/; best_m_spi_a_fin: ", best_m_spi_a_fin[i, j], "/; first_month: ", first_month))
          
          if (im_spi_a <= -12) {
            im_ok = 24 + im_spi_a   #24
            dum = spi_a[i, j, 1:(dim(spi_a)[3] - 24), best_t_spi_a_fin[i, j]]  #1:240
            spi_a_aux = dum[seq(im_ok, length(dum), 12)]  #mesi_12
          } else if (im_spi_a > -12 & im_spi_a <= 0) {
            im_ok = 12 + im_spi_a
            dum = spi_a[i, j, 13:(dim(spi_a)[3] - 12), best_t_spi_a_fin[i, j]] #13:252
            spi_a_aux = dum[seq(im_ok, length(dum), 12)]
          } else {
            im_ok = im_spi_a
            dum = spi_a[i, j, 25:dim(spi_a)[3], best_t_spi_a_fin[i, j]] #25:264
            spi_a_aux = dum[seq(im_ok, length(dum), 12)]
          }
          
          
          
          if (length(which(is.na(spi_a_aux))) == length(spi_a_aux)) {
            next
          }
        }
        
        if ((best_m_spi_a_fin[i, j] + 4)
            > 0) {
          spifor = array(NA, dim = c(length(years), num_ens))
          spifor = spipred[i, j, , , best_m_spi_a_fin[i, j] + 4, best_t_spi_a_fin[i, j]]
        }
        
        pre_obs = vector()
        pre = vector()
        BAS_train = vector()
        
        for (iy in 1:length(years)) {
          BAS_train = BAS[i, j, -iy]
          BA_train <-ifelse(BAS_train > median(BAS_train, na.rm = TRUE),1,0)
          
          spi_a_train = spi_a_aux[-iy]
          spi_a_test = spi_a_aux[iy]
          
          # mydata_train = data.frame("y" = (BA_train), "x1" = spi_a_train)
          mydata_train = data.frame(y = as.factor(BA_train), x1 = spi_a_train)
          
          fit1 <- randomForest(y ~ x1, data = mydata_train, ntree = 500)
          pre_obs[iy] = predict(fit1, newdata = data.frame(x1 = spi_a_test), type = "prob")[, "1"]
          
          if (!is.na(best_m_spi_a_fin[i, j]) & best_m_spi_a_fin[i, j]  < -3)  {
            pre[iy] = predict(fit1, newdata = data.frame(x1 = spi_a_test), type = "prob")[, "1"]
          } else {
            
            dum_pre = vector()
            for (iens in 1:num_ens) {
              # dum_pre[iens] = fit$coefficients[1] + fit$coefficients[2] * spifor[iy, iens]
              dum_pre[iens] = predict(fit1, newdata = data.frame(x1 = spifor[iy, iens]), type = "prob")[, "1"]
            }
            pre[iy] = mean(dum_pre, na.rm = TRUE)
          } 
        }
        
        
        # Añades la siguiente condición para verificar si hay NA en pre2
        if (any(is.na(pre))) {
          next  # Salta a la siguiente iteración si hay valores NA en pre2
        }
        
        predBAobs[i,j,]=pre_obs
        predBA[i,j,]=pre
        
        
        BA[i, j,] <-  ifelse(BAS[i, j,] > median(BAS[i, j,], na.rm = TRUE), 1, 0)
      
        roc_pred = roc.area(BA[i, j,], pre)
        roca_pred[i, j] = roc_pred$A
        sig_pred[i, j] = roc_pred$p.value
        
        roc_obs = roc.area(BA[i, j,], pre_obs)
        roca_obs[i, j] = roc_obs$A
        sig_obs[i, j] = roc_obs$p.value
      }
    }
  }

  
  image.plot(lon, lat, predBA[,,2])
  
  length(which(sig_obs<0.01))/length(which(!is.na(best_m_spi_a_fin)))
  length(which(sig_pred<0.01))/length(which(!is.na(best_m_spi_a_fin)))

  best_roc_pred = roca_pred
  best_sig_pred = sig_pred
  best_roc_pred[best_sig_pred >= 0.01] = NA
  length(which(!is.na(best_roc_pred)))/length(which(!is.na(best_m_spi_a_fin)))
  best_sig_pred[best_sig_pred >= 0.01] = NA

  # save(best_roc_pred, file = paste0(dir_out, "roc_area_pred_", season ,"_", version, "_", dataset, ".RData"))
  # save(best_sig_pred, file = paste0(dir_out, "sig_pred_",season ,"_", version, "_", dataset, ".RData"))
  
  save(predBAobs, file = paste0(dir_out, "prob_obs_", season ,"_", version, "_", dataset, ".RData"))
  save(predBA, file = paste0(dir_out, "prob_pred_",season ,"_", version, "_", dataset, ".RData"))
  
  
  
  

  image.plot(lon, lat, roca_obs)
  world(add = TRUE, col = "black")

image.plot(lon, lat, best_roc_pred)
world(add = TRUE, col = "black")
length(which(!is.na(best_roc_pred)))/length(which(!is.na(best_m_spi_a_fin)))
