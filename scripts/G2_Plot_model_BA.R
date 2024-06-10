rm(list = ls())
graphics.off()
gc()
library(ncdf4)
library(RColorBrewer)
library(maps)
library(oce)
library(terra)
library(rworldmap)
library(rnaturalearth)
# library(sp) # loads sp library too # creates nice color schemes
# library(classInt) # finds class intervals for continuous variables
# library(maps)
# library(oce)
# library(ggplot2)
# library(terra)
# library(raster)
# library(rgdal)
# library(rworldmap)
# library(rnaturalearth)
# library(pracma)
# library(ncdf4)
library(fields)
# library(maptools)
# library(RColorBrewer)
# library(viridis)
source('C:/Users/Usuario/Dropbox/4FIRE/script/model_BA/image_mask.R')

dir_oss ="C:/Users/Usuario/Dropbox/4FIRE_V2/data/"
dir_results ="C:/Users/Usuario/Dropbox/4FIRE_V2/results/"
dir_out="C:/Users/Usuario/Dropbox/4FIRE_V2/plots/"
dir_mask="C:/Users/Usuario/Dropbox/4FIRE/data/mask_season/"


fname<-file.path(dir_oss, '/DROP_3_1981_2020.nc')
obs.nc <- nc_open(fname)
obs.nc$dim$lon$vals -> lon
obs.nc$dim$lat$vals -> lat

data(coastlineWorld)
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

####################################################
# Mascara de Oceanos
####################################################
ocean <- rnaturalearth::ne_download(scale = 10, type = 'ocean', category = 'physical', returnclass = "sf")
ocean <- sf::st_transform(ocean, crs = crs)

worldmap <- rworldmap::getMap(resolution = "coarse") # Load countries 
raster::crs(worldmap)
worldmap <- sp::spTransform(worldmap,crs) # Project to Robinson

lati <- c(-90, -45, 0, 45, 90)
long <- c(-180, -135,-90, -45, 0, 45, 90, 135, 180)
labs <- graticule::graticule_labels(lons = long, lats = lati, xline = -180, yline = 90, proj = crs) # labels for the graticules 
lines <- graticule::graticule(lons = long, lats = lati, proj = crs) # graticules 

# 
# load(paste0(dir_mask, "mask_JJA.RData"))
# mask_jja=mask
# rm(mask)
# season = c("JJA")
season = c("DJF", "MAM", "JJA", "SON")
# FDR=1

# seaJJA = c("JJA")

for (seas in 1:length(season)) {
  seaJJA = season[seas]

  load(paste0(dir_mask, "mask_",seaJJA,".RData"))
  mask_jja=mask
  rm(mask)
  
  load(paste0(dir_results, "sig_",seaJJA,"_RFOR_MSWEP.RData"))
  load(paste0(dir_results, "best_m_spi_a_fin_",seaJJA,"_RFOR_MSWEP.RData"))
  best_m_spi_a_fin[best_sig>=0.01]=NA
  load(paste0(dir_results, "best_t_spi_a_fin_",seaJJA,"_RFOR_MSWEP.RData"))
  best_t_spi_a_fin[best_sig>=0.01]=NA
  load(paste0(dir_results, "cor_",seaJJA,"_RFOR_MSWEP.RData"))
  rho[best_sig>=0.01]=NA
  # load(paste0(dir_out, "roc_area_",version,"_MSWEP.RData"))
  # load(paste0(dir_out, "sig_",version,"_MSWEP.RData"))
  
 # image.plot(lon, lat, rho)
  # colns<-c("#34495E")
  colns<-c("#E0E0E0")
  mask_jja[(mask_jja==0)]=NA
  # image(mask_jja)
  # # col_c2 <- c("white","grey", "#8E0152", "#C72582", "#E284B7", "#F4C3E1" ,"#FAEAF2", "#EFF6E5")
  # # col_c2 <- c("white", "grey", "#EFF6E5",  "#DBEEEC", "#97D6CD", "#45A39A", "#066B63", "#003C30")
  # 
  # col_coef <- c("#8E0152", "#C72582", "#E284B7", "#F4C3E1" ,"#FAEAF2", 
  #             "#EFF6E5",  "#DBEEEC", "#97D6CD", "#45A39A", "#066B63", "#003C30")
  # ##############################################################################
  # #COEFICIENTE SPI AC JJA
  # ##############################################################################
  # dev.off()
  # pdf(paste(dir_out,"beta_AC_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  pdf(paste(dir_out,"cor_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  split.screen(c(1,1), screen=1)-> ind
  screen( ind[1])
  par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  par(mar=rep(0.6, 4))

  max_coef=1
  min_coef=-max_coef
  brk_coef <- seq(min_coef, max_coef, length.out = 11)
  col_coef <-(colorRampPalette(brewer.pal(length(brk_coef), "BrBG"))(length(brk_coef) -1))
  # best_x1_fin_fin=apply(best_x1_fin, c(1, 2), mean, na.rm = TRUE)
  mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_coef, drawBox = FALSE, border =NA,
          main = paste("COR (",seaJJA,")",sep=""),
          # main = bquote(italic("COR") ~ "(" ~ .(seaJJA) ~ ")"),
          cex.main = 2.5,
          line = -1, adj = 0.5)
  mapImage(lon, lat, mask_jja, col=  colns)
  mapImage(lon,lat, rho, col= col_coef ,breaks = brk_coef)

  plot(ocean, col = "lightblue", add =TRUE)
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules
  # text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
  # text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)

  split.screen(c(1,1), screen=2)-> ind
  c2<- range(-1, 1)
  # brk_coef <- c(-3.1, -2.4, -1.8, -1.2, -0.6, 0, 0.6, 1.2, 1.8, 2.4, 3.1)

  screen( ind[1])
  image.plot(zlim=  c2,
             legend.only=TRUE,
             smallplot=c(.15, .85, .4, 1),
             col=  col_coef,
             horizontal = TRUE)

  close.screen( all=TRUE)
  dev.off()
  # 
  # 
  # # ##############################################################################
  # # #time scale SPI AC JJA
  # # ##############################################################################
  # 
# 
  # pdf(paste(dir_out,"time_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  # split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  # split.screen(c(1,1), screen=1)-> ind
  # screen( ind[1])
  # par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  # par(mar=rep(0.6, 4))
  # 
  # brk_spi <- seq(1, 3, length.out = 4)
  # # col_spi = c("#D98880", "#F9F89C", "#ABEBC6")
  # col_spi = c("#FFCCCC", "#D98880", "#8B0000")
  # mapPlot(coastlineWorld , col="white", projection="+proj=robin",breaks = brk_spi, drawBox = FALSE, border = NA,
  #         main = paste("t (",seaJJA,")",sep=""),
  #         # main = bquote(italic("t") ~ "(" ~ .(seaJJA) ~ ")"),
  #         cex.main = 2.5,
  #         line = -1, adj = 0.5)
  # mapImage(lon, lat, mask_jja, col=  colns)
  # mapImage(lon,lat, best_t_spi_a_fin, col= col_spi, breaks = brk_spi)
  # 
  # plot(ocean, col = "lightblue", add =TRUE)
  # plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules
  # # text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=0.8) # plots longitude labels
  # # text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=0.8)
  # 
  # 
  # split.screen(c(1,1), screen=2)-> ind
  # screen( ind[1])
  # zr<- range(1, 5)
  # 
  # brk_spi1 <- c(1,2,3, 4, 5)
  # 
  # # col_spi1<-c("white", ""#E0E0E0"", "#D98880", "#F9F89C", "#ABEBC6")
  # col_spi1<-c("white", "#E0E0E0","#FFCCCC", "#D98880", "#8B0000")
  # image.plot(zlim=zr,
  #            legend.only=TRUE,
  #            # smallplot=c(.15,.85, .7,1),
  #            smallplot=c(.15, .85, .4, 1),
  #            col=col_spi1,
  #            horizontal = TRUE,
  #            axis.args = list(at = brk_spi1,
  #                             labels=c('No BA', "No model",'SPI3','SPI6','SPI12')))
  # 
  # 
  # close.screen( all=TRUE)
  # dev.off()
  # # 
  # 
  # 
  # # ##############################################################################
  # # #mes SPI AC JJA
  # # ##############################################################################
# dev.off()
  
  # main = expression(italic("m") ~ " (AC; " * seaJJA * ")", sep="")
  
  
  pdf(paste(dir_out,"mes_AC_",seaJJA,".pdf", sep=""), width=11, height=7.5)
  split.screen( rbind(c(0, 1,.06,1), c(0,1,0,.06)))
  split.screen(c(1,1), screen=1)-> ind
  screen( ind[1])
  par(oma=c(0.8,0.8,0.8,0.2),mar=rep(0.5, 4))
  par(mar=rep(0.6, 4))
# image.plot(lon, lat, best_m_spi_a_fin)
  
  # inicio <- "#004000"  # Verde muy oscuro
  # fin <- "#CCFFCC"    # Verde muy claro
  # 
  # # Crear una función para generar la paleta
  # paleta_azules <- colorRampPalette(c(inicio, fin))
  # # Generar 13 colores en formato hexadecimal
  # colores_azules <- paleta_azules(13)
  # 
  # inicio <- "#E6E6FA"  # Lila muy claro (Lavender)
  # fin <- "#9370DB"    # Lila medio oscuro (Medium Purple)
  # inicio <- "#B57EDC"  # Lila claro pero más saturado (Lavender Blue)
  # fin <- "#4B0082"   
  # 
  # # Crear una función para generar la paleta
  # paleta_rojos <- colorRampPalette(c(inicio, fin))
  # 
  # # Generar 4 colores en formato hexadecimal
  # colores_rojos <- paleta_rojos(4)
  
   # brk_mes <- seq(-14, -2, length.out = 13)
  brk_mes <- seq(-17, 0, length.out = 18)
  col_mes <- c("#004000", "#114F11", "#225F22", "#336F33", "#447F44", "#558F55",
               "#669F66", "#76AF76", "#88BF88", "#99CF99",
               "#AADFAA", "#BBEFBB", "#CCFFCC", "#B57EDC", "#9154BE", "#6E2AA0", "#4B0082")
  
  
  # col_mes <- c("#5e4fa3", "#6669a3", "#6f82a3", "#779ca4", "#80b5a4", 
  #                        "#88cfa4", "#9fcc99", "#b7c88e",  "#cec584", "#e6c179", 
  #                        "#fdbe6e", "#ea9865", "#d7725c", "#c44d54", "#b1274b", 
  #                        "#9e0142", "#654321")
  

  mapPlot(coastlineWorld , col="white", projection="+proj=robin",
          breaks = brk_mes,
          drawBox = FALSE,
          border = NA,
          main = paste(main = paste("m (",seaJJA,")",sep="")),
          # main = bquote(italic("m") ~ "(" ~ .(seaJJA) ~ ")"),
          # main = bquote(bold(italic("m")) ~ "(" ~ .(seaJJA) ~ ")"),
          cex.main = 2.5,
          line = -1, adj = 0.5)

  # mapImage(lon, lat2, pvalue_adj2)
  # mapImage(lon, lat, obs_mask, col= "white")
  mapImage(lon, lat, mask_jja, col=  colns)
  mapImage(lon,lat, best_m_spi_a_fin, col=   col_mes 
           , breaks = brk_mes
           )

  plot(ocean, col = "lightblue", add =TRUE)
  plot(lines, lty = 5, col = "black", add = TRUE) # plots graticules

  # text(subset(labs, labs$islon), lab = parse(text = labs$lab[labs$islon]), pos = 3, xpd = NA, cex=1) # plots longitude labels
  # text(subset(labs, !labs$islon), lab = parse(text = labs$lab[!labs$islon]), pos = 2, xpd = NA, cex=1)

  
  split.screen(c(1,1), screen=2)-> ind
  screen( ind[1])
  zc<- range(-18, 0)
  # col_mes1 <- c("white", "grey","#5e4fa3", "#6669a3", "#6f82a3", "#779ca4", "#80b5a4", 
  #                        "#88cfa4", "#9fcc99", "#b7c88e", "#cec584", "#e6c179", 
  #                        "#fdbe6e", "#ea9865", "#d7725c", "#c44d54", "#b1274b", 
  #                        "#9e0142", "#654321")
  
  # col_mes1 <- c("white", ""#E0E0E0"","#004000", "#114F11", "#225F22", "#336F33", "#447F44", "#558F55",
                # "#669F66", "#76AF76", "#88BF88", "#99CF99",
                # "#AADFAA", "#BBEFBB", "#CCFFCC", "#FFCCCC", "#D88888", "#B14444",
                # "#8B0000")
  col_mes1 <- c("white", "#E0E0E0","#004000", "#114F11", "#225F22", "#336F33", "#447F44", "#558F55",
                "#669F66", "#76AF76", "#88BF88", "#99CF99",
                "#AADFAA", "#BBEFBB", "#CCFFCC",  "#B57EDC", "#9154BE", "#6E2AA0", "#4B0082")
  

  brk_mes <- c(0, -1, -2, -3,-4, -5, -6, -7,-8, -9, -10, -11,-12, -13, -14, -15, -16, -17, -18)
  meses_label = c("No data", "No model", "-16", "-15", "-14", "-13", "-12", "-11",
                  "-10", "-9", "-8", "-7","-6", "-5", "-4","-3", "-2",
                  "-1", "M")
  # if (seaJJA =="DJF") {
  #   meses_label = c("F", "J", "D", "N", "O", "S", "A", "J",
  #                   "J", "M", "A", "M","F", "J", "D","N", "O",
  #                   "", "")
  #                   # "No model", "No BA")
  #   
  # } else if (seaJJA =="MAM") {
  #   meses_label = c( "M","A", "M", "F","J", "D", "N", "O",
  #                    "S", "A", "J", "J","M", "A", "M","F", "J",
  #                    "", "")
  #   # "No model", "No BA")
  # } else if (seaJJA =="JJA") {
  #   meses_label = c( "A","J", "J", "M","A", "M", "F", "J",
  #                    "D", "N", "O", "S", "A", "J", "J","M", "A",
  #                    "", "")
  #   # "No model", "No BA")
  # } else if (seaJJA =="SON") {
  #   meses_label = c( "N","O", "S", "A","J", "J", "M", "A",
  #                    "M", "F", "J", "D","N", "O", "S","A", "J",
  #                    "", "")
  #   meses_label = c( "M","-1", "-2", "-3","-4", "-5", "-6", "-7",
  #                    "-8", "-9", "-10", "-11","-12", "-13", "-14","-15", "-16",
  #                    "", "")
  #   # "No model", "No BA")
  # }
  
  image.plot(zlim=zc,
             legend.only=TRUE,
             # smallplot=c(.15,.85, .7,1),
             smallplot=c(.15, .85, .4, 1),
             col=col_mes1,
             horizontal = TRUE, axis.args = list(at = brk_mes,
             labels= rev(meses_label),
             cex.axis = 1))
  close.screen( all=TRUE)
  dev.off()
}

