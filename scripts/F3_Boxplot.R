#############################
#ROC AREA CLIMATOLOGY
############################

rm(list = ls())
graphics.off()
gc()

source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot_stat.R")
source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot.R")

# dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/'
dir_out= 'C:/Users/Usuario/Dropbox/4FIRE_V2/results/'
dir_out1= 'C:/Users/Usuario/Desktop/4FIRE_V2/plots/'
# dir_out1= 'C:/Users/Usuario/Dropbox/4FIRE/Figuras/'
# seasons= c("DJF", "MAM", "JJA", "SON")

bs=c("roc_area")

  load(paste(dir_out,bs,'_DJF_RFOR_MSWEP.RData', sep = ""))
  load(paste(dir_out,'sig_DJF_RFOR_MSWEP.RData', sep = ""))
  best_roc[best_sig>=0.01]=NA
  corre_box1 = as.vector(best_roc)
  
  load(paste(dir_out,bs,'_DJF_RLOG_MSWEP.RData', sep = ""))
  load(paste(dir_out,'sig_DJF_RLOG_MSWEP.RData', sep = ""))
  best_roc[best_sig>=0.01]=NA
  corre_box2 = as.vector(best_roc)
#  image(best_roc)
  
  load(paste(dir_out,bs,'_MAM_RFOR_MSWEP.RData', sep = ""))
  load(paste(dir_out,'sig_MAM_RFOR_MSWEP.RData', sep = ""))
  best_roc[best_sig>=0.01]=NA
  corre_box3 = as.vector(best_roc)
  
  load(paste(dir_out,bs,'_MAM_RLOG_MSWEP.RData', sep = ""))
  load(paste(dir_out,'sig_MAM_RLOG_MSWEP.RData', sep = ""))
  best_roc[best_sig>=0.01]=NA
  corre_box4 = as.vector(best_roc)
#  image(best_roc)
  
  load(paste(dir_out,bs,'_JJA_RFOR_MSWEP.RData', sep = ""))
  load(paste(dir_out,'sig_JJA_RFOR_MSWEP.RData', sep = ""))
  best_roc[best_sig>=0.01]=NA
  corre_box5 = as.vector(best_roc)
  
  load(paste(dir_out,bs,'_JJA_RLOG_MSWEP.RData', sep = ""))
  load(paste(dir_out,'sig_JJA_RLOG_MSWEP.RData', sep = ""))
  best_roc[best_sig>=0.01]=NA
  corre_box6 = as.vector(best_roc)
#  image(best_roc)
  
  load(paste(dir_out,bs,'_SON_RFOR_MSWEP.RData', sep = ""))
  load(paste(dir_out,'sig_SON_RFOR_MSWEP.RData', sep = ""))
  best_roc[best_sig>=0.01]=NA
  corre_box7 = as.vector(best_roc)
  
  load(paste(dir_out,bs,'_SON_RLOG_MSWEP.RData', sep = ""))
  load(paste(dir_out,'sig_SON_RLOG_MSWEP.RData', sep = ""))
  best_roc[best_sig>=0.01]=NA
  corre_box8 = as.vector(best_roc)
#  image(best_roc)
  
  
  plot_data <-
    data.frame(
      corre_box1,
      corre_box2,
      corre_box3,
      corre_box4,
      corre_box5,
      corre_box6,
      corre_box7,
      corre_box8
      )
  #     corre_box[, 10]
  #   )
  
  Porcentajes=c(66, 43, 
                68, 44,
                70, 47, 
                68 ,48)
  
  colores=c("green", "blue", "green", "blue", "green", "blue", "green", "blue")
  # setEPS()
  # postscript(
  #   file.path(
  #     dir_out1,
  #     paste("BOX_ROC_obs_",season,".eps", sep = "")
  #     #paste("boxplot_corre_spi_", sc, "_ENS.eps", sep = "")
  #   ),
  #   horiz = FALSE,
  #   onefile = FALSE,
  #   width = 8.5,
  #   height = 5.5
  # )
  dev.off()
  # Ajustes de márgenes
  # par(oma = c(3, 4, 2, 4), mar = c(7, 4, 4, 6))
  par(oma = c(1, 1, 1, 1), mar = c(5, 4, 4, 4))
  # Crear el boxplot y obtener las posiciones de las cajas
  # Crear el boxplot
  box_positions <- boxplot(
    plot_data,
    outline = F,
    las = 2,
    ylim = c(0.5, 1),
    names = c('DJF (RF)', 'DJF (LR)',
              'MAM (RF)', 'MAM (LR)',
              'JJA (RF)', 'JJA (LR)',
              'SON (RF)', 'SON (LR)'),
    main = 'Model with observation',
    ylab = 'AUC',
    cex.main = 1.5,
    cex.lab = 1,
    col = c("green", "blue", "green", "blue", "green", "blue", "green", "blue")
  )
  # Obtener las posiciones centrales de los boxplots
  box_centers <- box_positions$stats[3, ]

  grid_lines <- 0.5:8.5
  box_centers <- box_positions$stats[3, ]
  grid(nx = NA, ny = NULL, lty = 2, col = "cornsilk2")
  abline(v = grid_lines, col = "cornsilk2", lty = 2)

  # Añadir eje secundario para el gráfico de barras
  par(new = TRUE)
  plot(1:length(Porcentajes), Porcentajes, type = "n", ylim = c(0, 200), axes = FALSE, xlab = "", ylab = "")
  axis(4, at = pretty(range(0, 200)), col.axis = "blue", las = 2)
  mtext("Skilfull surface (%)", side = 4, line = 3, col = "blue")
  
  # Dibujar las barras en posiciones específicas usando rect
  bar_width <- 0.25  # Ancho de las barras
  bar_centers <- c(1.45,
                   2.3,
                   3.2,
                   4.05,
                   4.95,
                   5.8,
                   6.65,
                   7.55)
  
  for (i in 1:length(Porcentajes)) {
    rect(
      xleft = bar_centers[i] - bar_width / 2,
      ybottom = 0,
      xright = bar_centers[i] + bar_width / 2,
      ytop = Porcentajes[i],
      col = adjustcolor(colores[i], alpha.f = 1),  # Aplicar transparencia
      border = T
    )
  }

  # Ajustar etiquetas del gráfico de barras
  text(
    x = bar_centers,
    y = Porcentajes + 10,
    labels = paste0(Porcentajes, "%"),
    col = "blue",
    cex = 0.8
  )
  
  
  
  
  ####-----------------------------------
  
  #############################
  #ROC AREA CLIMATOLOGY
  ############################

  rm(list = ls())
  graphics.off()
  gc()
  
  source("C:/Users/Usuario/Dropbox/4DROP/script/Common/CorrMIO.R")
  source("C:/Users/Usuario/Dropbox/4DROP/script/Common/ColorBarM.R")
  source("C:/Users/Usuario/Dropbox/4DROP/script/Common/mioplot_global.R")
  source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot_stat.R")
  source("C:/Users/Usuario/Dropbox/4DROP/script/Common/my_boxplot.R")
  
  # dir_out= 'C:/Users/Usuario/Dropbox/4FIRE/data/model_obs_FDR/'
  dir_out= 'C:/Users/Usuario/Dropbox/4FIRE_V2/results/'
  dir_out1= 'C:/Users/Usuario/Desktop/4FIRE_V2/plots/'
  # dir_out1= 'C:/Users/Usuario/Dropbox/4FIRE/Figuras/'
  # seasons= c("DJF", "MAM", "JJA", "SON")
  
  bs=c("roc_area")
  
  load(paste(dir_out,bs,'_pred_DJF_RFOR_MSWEP.RData', sep = ""))
  corre_box1 = as.vector(best_roc_pred)
  
  load(paste(dir_out,bs,'_pred_DJF_RLOG_MSWEP.RData', sep = ""))
  corre_box2 = as.vector(best_roc_pred)
  #  image(best_roc)
  
  load(paste(dir_out,bs,'_pred_MAM_RFOR_MSWEP.RData', sep = ""))
  corre_box3 = as.vector(best_roc_pred)
  
  load(paste(dir_out,bs,'_pred_MAM_RLOG_MSWEP.RData', sep = ""))
  corre_box4 = as.vector(best_roc_pred)
  #  image(best_roc)
  
  load(paste(dir_out,bs,'_pred_JJA_RFOR_MSWEP.RData', sep = ""))
  corre_box5 = as.vector(best_roc_pred)
  
  load(paste(dir_out,bs,'_pred_JJA_RLOG_MSWEP.RData', sep = ""))
  corre_box6 = as.vector(best_roc_pred)
  #  image(best_roc)
  
  load(paste(dir_out,bs,'_pred_SON_RFOR_MSWEP.RData', sep = ""))
  corre_box7 = as.vector(best_roc_pred)
  
  load(paste(dir_out,bs,'_pred_SON_RLOG_MSWEP.RData', sep = ""))
  corre_box8 = as.vector(best_roc_pred)
  #  image(best_roc)
  
  
  plot_data <-
    data.frame(
      corre_box1,
      corre_box2,
      corre_box3,
      corre_box4,
      corre_box5,
      corre_box6,
      corre_box7,
      corre_box8
    )
  #     corre_box[, 10]
  #   )
  
  Porcentajes=c(42, 23, 
                48, 29,
                48, 30, 
                49 ,31)
  # setEPS()
  # postscript(
  #   file.path(
  #     dir_out1,
  #     paste("BOX_ROC_obs_",season,".eps", sep = "")
  #     #paste("boxplot_corre_spi_", sc, "_ENS.eps", sep = "")
  #   ),
  #   horiz = FALSE,
  #   onefile = FALSE,
  #   width = 8.5,
  #   height = 5.5
  # )
  colores=c("green", "blue", "green", "blue", "green", "blue", "green", "blue")
  # dev.off()
  # Ajustes de márgenes
  # par(oma = c(3, 4, 2, 4), mar = c(7, 4, 4, 6))
  par(oma = c(1, 1, 1, 1), mar = c(5, 4, 4, 4))
  # Crear el boxplot y obtener las posiciones de las cajas
  # Crear el boxplot
  box_positions <- boxplot(
    plot_data,
    outline = F,
    las = 2,
    ylim = c(0.5, 1),
    names = c('DJF (RF)', 'DJF (LR)',
              'MAM (RF)', 'MAM (LR)',
              'JJA (RF)', 'JJA (LR)',
              'SON (RF)', 'SON (LR)'),
    main = 'Model with predictions',
    ylab = 'AUC',
    cex.main = 1.5,
    cex.lab = 1,
    col = c("green", "blue", "green", "blue", "green", "blue", "green", "blue")
  )
  # Obtener las posiciones centrales de los boxplots
  box_centers <- box_positions$stats[3, ]
  
  grid_lines <- 0.5:8.5
  box_centers <- box_positions$stats[3, ]
  grid(nx = NA, ny = NULL, lty = 2, col = "cornsilk2")
  abline(v = grid_lines, col = "cornsilk2", lty = 2)
  
  # Añadir eje secundario para el gráfico de barras
  par(new = TRUE)
  plot(1:length(Porcentajes), Porcentajes, type = "n", ylim = c(0, 200), axes = FALSE, xlab = "", ylab = "")
  axis(4, at = pretty(range(0, 200)), col.axis = "blue", las = 2)
  mtext("Skilfull surface (%)", side = 4, line = 3, col = "blue")
  
  # Dibujar las barras en posiciones específicas usando rect
  bar_width <- 0.25  # Ancho de las barras
  bar_centers <- c(1.45,
                   2.3,
                   3.2,
                   4.05,
                   4.95,
                   5.8,
                   6.65,
                   7.55)
  
  for (i in 1:length(Porcentajes)) {
    rect(
      xleft = bar_centers[i] - bar_width / 2,
      ybottom = 0,
      xright = bar_centers[i] + bar_width / 2,
      ytop = Porcentajes[i],
      col = adjustcolor(colores[i], alpha.f = 1),  # Aplicar transparencia
      border = T
    )
  }
  
  # Ajustar etiquetas del gráfico de barras
  text(
    x = bar_centers,
    y = Porcentajes + 10,
    labels = paste0(Porcentajes, "%"),
    col = "blue",
    cex = 0.8
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  