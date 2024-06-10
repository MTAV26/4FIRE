# Definir los datos
correlation_data <- data.frame(
  Seasons = c("DJF", "MAM", "JJA", "SON"),
  POSITIVE = c(44.7, 45, 45, 48.6),
  NEGATIVE = c(55.3, 55, 55, 51.4)
)

# Crear el primer gráfico (a) Correlation
par(mfrow=c(3,1), mar=c(5,4,4,2))

barplot(t(as.matrix(correlation_data[,2:3])), beside=TRUE, col=c("#A3E4D7", "#F5CBA7"), ylim=c(0,60),
        names.arg=correlation_data$Seasons, legend=c("POSITIVE", "NEGATIVE"), 
        main="(a) Correlation", xlab="Seasons", ylab="Value (%)")

# Definir los datos para el segundo gráfico
data2 <- data.frame(
  m = c(-16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, "M"),
  DJF = c(4.7, 5.2, 5.4, 4.3, 4.1, 4.3, 4.9, 3.9, 5.2, 4.3, 7.1, 5.6, 4.3, 6.2, 9, 8.4, 13.1),
  MAM = c(4.7, 5.5, 4.7, 3, 4.6, 4.7, 3.8, 5, 5.7, 4.1, 6.9, 5.8, 7.5, 5.5, 8.2, 9.5, 10.6),
  JJA = c(4.2, 4.6, 5.1, 4.4, 4.2, 6, 4.5, 6.9, 7.1, 5.3, 5.6, 5.3, 4.9, 4.8, 6, 9.9, 11.3),
  SON = c(5.1, 5.3, 4, 4.8, 5.5, 4.4, 5.6, 6.4, 5.9, 4.9, 6.3, 3.2, 4, 6, 8.2, 11.7, 8.6)
)

# Crear el segundo gráfico (b) Month (m)
barplot(t(as.matrix(data2[,2:5])), beside=TRUE, col=c("#A3E4D7", "#A9DFBF", "#F9E79F", "#F5CBA7"), 
        ylim=c(0,15), names.arg=data2$m, legend=c("DJF", "MAM", "JJA", "SON"),
        main="(b) Month (m)", xlab="m", ylab="Value (%)", args.legend=list(x="top"))

# Definir los datos para el tercer gráfico
data3 <- data.frame(
  Seasons = c("DJF", "MAM", "JJA", "SON"),
  SPI3 = c(40.4, 37.1, 33, 40.9),
  SPI6 = c(29.9, 35.9, 36.5, 33.7),
  SPI12 = c(29.7, 27, 30.5, 25.4)
)

# Crear el tercer gráfico (c) SPI (t)
barplot(t(as.matrix(data3[,2:4])), beside=TRUE, col=c("#FFCCCC", "#D98880", "#8B0000"), 
        ylim=c(0,50), names.arg=data3$Seasons, legend=c("SPI3", "SPI6", "SPI12"), 
        main="(c) SPI (t)", xlab="Seasons", ylab="Value (%)")

# Guardar la gráfica combinada en un archivo PDF
# pdf("combined_plots.pdf", width=8, height=14)
par(mfrow=c(3,1), mar=c(5,4,4,2))

barplot(t(as.matrix(correlation_data[,2:3])), beside=TRUE, col=c("#A3E4D7", "#F5CBA7"), ylim=c(0,60),
        names.arg=correlation_data$Seasons, legend=c("POSITIVE", "NEGATIVE"), 
        main="(a) Correlation", xlab="Seasons", ylab="Value (%)")

barplot(t(as.matrix(data2[,2:5])), beside=TRUE, col=c("#A3E4D7", "#A9DFBF", "#F9E79F", "#F5CBA7"), 
        ylim=c(0,15), names.arg=data2$m, legend=c("DJF", "MAM", "JJA", "SON"),
        main="(b) Month (m)", xlab="m", ylab="Value (%)", args.legend=list(x="top"))

barplot(t(as.matrix(data3[,2:4])), beside=TRUE, col=c("#FFCCCC", "#D98880", "#8B0000"), 
        ylim=c(0,50), names.arg=data3$Seasons, legend=c("SPI3", "SPI6", "SPI12"), 
        main="(c) SPI (t)", xlab="Seasons", ylab="Value (%)")

dev.off()

