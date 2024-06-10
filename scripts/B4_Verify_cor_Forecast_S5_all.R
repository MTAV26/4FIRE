
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

anni = 2000:2020
mesi = rep(1:12, length(anni))
# 
# datasets =c("GPCC")
dataset = c(
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
time_scale = c(3,6,12)
mes_forecast = c(
"MAY",
"JUN",
"JUL",
"AUG")
# mes_forecast = c("MAY")
# sc= 6
# dataset="MSWEP"
# est="FEB"

for (isc in 1:length(time_scale)) {
  sc = time_scale[isc]
  print(sc)
  load(file.path(dir_drop,paste("SPI", sc, "_",dataset,"_1993_2020.RData", sep = "")))
  
  if(sc == 3){
    spi=spi3
  } else if (sc == 6){
    spi = spi6 
  } else if (sc ==12){
    spi = spi12
  }
  # spi = spi[, , 229:480]
  
  # for (idata in 1:length(datasets)) {
  #   dataset = datasets[idata]
  #   print(dataset)
  

  for (mes_for in 1:length(mes_forecast)) {
    est = mes_forecast[mes_for]
    print(est)
  

    if (est =="FEB") {
      mesi_8 =  which(mesi == 02)
    } else if (est =="MAR") {
      mesi_8 =  which(mesi == 03)
    } else if (est =="APR") {
      mesi_8 =  which(mesi == 04)
    } else if (est =="MAY"){
      mesi_8 =  which(mesi == 05)
    } else if (est =="JUN") {
      mesi_8 =  which(mesi == 06)
    } else if (est =="JUL") {
      mesi_8 =  which(mesi == 07)
    } else if (est =="AUG") {
      mesi_8 =  which(mesi == 08)
    } else if (est =="SEP") {
      mesi_8 =  which(mesi == 09)
    } else if (est =="OCT") {
      mesi_8 =  which(mesi == 10)
    } else if (est =="NOV") {
      mesi_8 =  which(mesi == 11)
    } else if (est =="DEC") {
      mesi_8 =  which(mesi == 12)
    } else if (est =="JAN") {
      mesi_8 =  which(mesi == 01)
    }
    
    
    
  
  ens = spi[,,mesi_8]  
  dim(ens)

  # dev.off()
  
  load(file.path(dir_s5, paste('spi',sc,'SEAS5_',ms,'_',est,"_",dataset,".RData",sep = "")))
  # summary(as.vector(spi6pred4))

  
  # load(file.path(dir_s5, paste("SPI6SEAS5_11_DEC_MSWEP.RData",sep = "")))
  # summary(as.vector(spi6pred2))
  
  
  # image.plot(lon,lat,spi6pred2[,,25,22] )
  if (sc == "3" & est =="MAY"){
    pred =  spi3pred1
  } else if (sc == "6" & est =="MAY") {
    pred =  spi6pred1
  } else if (sc == "12" & est =="MAY") {
    pred =  spi12pred1

  } else if (sc == "3" & est =="JUN"){
    pred =  spi3pred2
  } else if (sc == "6" & est =="JUN") {
    pred =  spi6pred2
  } else if (sc == "12" & est =="JUN") {
    pred =  spi12pred2


  } else if (sc == "3" & est =="JUL"){
    pred =  spi3pred3
  } else if (sc == "6" & est =="JUL") {
    pred =  spi6pred3
  } else if (sc == "12" & est =="JUL") {
    pred =  spi12pred3


  } else if (sc == "3" & est =="AUG"){
    pred =  spi3pred4
  } else if (sc == "6" & est =="AUG") {
    pred =  spi6pred4
  } else if (sc == "12" & est =="AUG") {
    pred =  spi12pred4
  } else {
    print('dataset not known')
  }
  
  
  # if (sc == "3" & est =="AUG"){
  #   pred =  spi3pred1
  # } else if (sc == "6" & est =="AUG") {
  #   pred =  spi6pred1
  # } else if (sc == "12" & est =="AUG") {
  #   pred =  spi12pred1
  #   
  # } else if (sc == "3" & est =="SEP"){
  #   pred =  spi3pred2
  # } else if (sc == "6" & est =="SEP") {
  #   pred =  spi6pred2
  # } else if (sc == "12" & est =="SEP") {
  #   pred =  spi12pred2
  #   
  #   
  # } else if (sc == "3" & est =="OCT"){
  #   pred =  spi3pred3
  # } else if (sc == "6" & est =="OCT") {
  #   pred =  spi6pred3
  # } else if (sc == "12" & est =="OCT") {
  #   pred =  spi12pred3
  #   
  #   
  # } else if (sc == "3" & est =="NOV"){
  #   pred =  spi3pred4
  # } else if (sc == "6" & est =="NOV") {
  #   pred =  spi6pred4
  # } else if (sc == "12" & est =="NOV") {
  #   pred =  spi12pred4
  # } else {
  #   print('dataset not known')
  # }
  
  
  # if (sc == "3" & est =="FEB"){
  #   pred =  spi3pred1
  # } else if (sc == "6" & est =="FEB") {
  #   pred =  spi6pred1
  # } else if (sc == "12" & est =="FEB") {
  #   pred =  spi12pred1
  #   
  # } else if (sc == "3" & est =="MAR"){
  #   pred =  spi3pred2
  # } else if (sc == "6" & est =="MAR") {
  #   pred =  spi6pred2
  # } else if (sc == "12" & est =="MAR") {
  #   pred =  spi12pred2
  #   
  #   
  # } else if (sc == "3" & est =="APR"){
  #   pred =  spi3pred3
  # } else if (sc == "6" & est =="APR") {
  #   pred =  spi6pred3
  # } else if (sc == "12" & est =="APR") {
  #   pred =  spi12pred3
  #   
  #   
  # } else if (sc == "3" & est =="MAY"){
  #   pred =  spi3pred4
  # } else if (sc == "6" & est =="MAY") {
  #   pred =  spi6pred4
  # } else if (sc == "12" & est =="MAY") {
  #   pred =  spi12pred4
  # } else {
  #   print('dataset not known')
  # }
  
  
  
  # if (sc == "3" & est =="NOV"){
  #   pred =  spi3pred1
  # } else if (sc == "6" & est =="NOV") {
  #   pred =  spi6pred1
  # } else if (sc == "12" & est =="NOV") {
  #   pred =  spi12pred1
  #   
  # } else if (sc == "3" & est =="DEC"){
  #   pred =  spi3pred2
  # } else if (sc == "6" & est =="DEC") {
  #   pred =  spi6pred2
  # } else if (sc == "12" & est =="DEC") {
  #   pred =  spi12pred2
  #   
  #   
  # } else if (sc == "3" & est =="JAN"){
  #   pred =  spi3pred3
  # } else if (sc == "6" & est =="JAN") {
  #   pred =  spi6pred3
  # } else if (sc == "12" & est =="JAN") {
  #   pred =  spi12pred3
  #   
  #   
  # } else if (sc == "3" & est =="FEB"){
  #   pred =  spi3pred4
  # } else if (sc == "6" & est =="FEB") {
  #   pred =  spi6pred4
  # } else if (sc == "12" & est =="FEB") {
  #   pred =  spi12pred4
  # } else {
  #   print('dataset not known')
  # }
  
  pred[is.infinite(pred)]=NA

  
  data = apply(pred[,,mesi_8,], c(1,2,3), mean, na.rm=TRUE)
  data[is.infinite(data)]=NA
  data[is.na(data)] <- NA
  
  dim(data)
  dim(ens)
  
  summary(as.vector(data))
  summary(as.vector(ens))
  
  ni = dim(data)[1]
  nj = dim(data)[2]
  nt = dim(ens)[3]
  
  corre <- matrix(data = NA,nrow = ni, ncol = nj)
  pvalue <- matrix(data = NA, nrow = ni, ncol = nj)
  
  
  for (i in 1:ni) {
    for (j in 1:nj) {
      
      OK <- complete.cases(ens[i, j,], data[i, j,])
      x <- ens[i, j, OK]
      y <- data[i, j, OK]
      n <- length(x)
      #if (n >= anniok * 12) {
      if (n >= nt*0.9) {
        
        dum = CorrMIO((x), (y), method = 'pearson', pval = TRUE )
        corre[i, j] = as.numeric(dum)[1]
        pvalue[i, j] <- as.numeric(dum)[4]
        rm(dum)
        
      }
      
      rm(OK, n, x, y)
    }
  }
  
  
  
  
  # pvalue_adj = p.adjust(pvalue, method = "fdr", n = length(pvalue[!is.na(pvalue)]))
  pvalue_adj = matrix(pvalue, nrow = ni, ncol = nj)
  #
  lat2 = lat[which(lat > -60 & lat < 85)]
  corre2 = corre[, which(lat > -60 & lat < 85)]
  pvalue2 = pvalue[, which(lat > -60 & lat < 85)]
  pvalue_adj2 = pvalue_adj[, which(lat > -60 & lat < 85)]
  
  
  ## plot 1
  
  # postscript(
  #   file.path(dir_out,paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,"_original.eps",sep = "")),
  #   paper = "special",
  #   width = 11,
  #   height = 7,
  #   horizontal = T
  # )
  # 
  # 
  # layout(matrix(c(1, 2), 1, 2, byrow = T), widths = c(11, 1.5))
  # par(oma = c(1, 1, 4, 1))
  # tit <-
  #   paste(
  #     'Correlation for SPI',sc, ' in ',est,', ',dataset,' (S5) against DROP\n Start date: ',
  #     month.name[ start_date],
  #     ' - Period: 2000/2020\n (points: p<0.05 )', sep=""
  #   )
  # 
  # 
  # mioplot(corre2, lon,lat2,
  #         toptitle = '',sizetit = 0.8,
  #         brks = brk_cor,  cols = col_cor,
  #         axelab =F, filled.continents = FALSE,
  #         drawleg = F,  
  #         # dots = pvalue2 <= 0.05,
  #         dots2 = pvalue_adj2 <= 0.05 
  # )
  # title(tit, line = 0.5, outer = T)
  # ce = 1.4
  # ColorBar(
  #   brks = brk,
  #   cols = col,
  #   vert = T
  # )
  # 
  save(corre2, file = file.path(dir_out, paste("COR_S5_spi",sc,"_",ms,"_",est,"_",dataset,".RData", sep = "") ))
  save(pvalue_adj2, file = file.path(dir_out, paste("PVAL_S5_spi",sc,"_",ms,"_",est,"_",dataset,".RData", sep = "") ))
  # dev.off()
  }
  # }
}

