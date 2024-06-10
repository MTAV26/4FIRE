
# Instalar y cargar las librerías necesarias
# if (!require(ggplot2)) install.packages("ggplot2")
# if (!require(tidyr)) install.packages("tidyr")
# if (!require(patchwork)) install.packages("patchwork")
library(ggplot2)
library(tidyr)
library(patchwork)

# Definir y crear los datos para el primer gráfico
correlation_data <- data.frame(
  Seasons = c("DJF", "MAM", "JJA", "SON"),
  POSITIVE = c(44.7, 45, 45, 48.6),
  NEGATIVE = c(55.3, 55, 55, 51.4)
)

# Transformar los datos a formato largo
correlation_long <- pivot_longer(correlation_data, cols = c(POSITIVE, NEGATIVE), 
                                 names_to = "Correlation", values_to = "Percentage")

# Asegurar el orden correcto de las estaciones y de las correlaciones
correlation_long$Seasons <- factor(correlation_long$Seasons, levels = c("DJF", "MAM", "JJA", "SON"))
correlation_long$Correlation <- factor(correlation_long$Correlation, levels = c("POSITIVE", "NEGATIVE"))

# Crear la gráfica de barras
plot1 <- ggplot(correlation_long, aes(x = Seasons, y = Percentage, fill = Correlation)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = c("POSITIVE" = "#A3E4D7", "NEGATIVE" = "#F5CBA7")) +
  labs(x = "Seasons", y = "Value (%)", title = "(a) Correlation") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title = "Correlation")) +
  scale_y_continuous(limits = c(0, 60))  # Ajustar el eje y a 60 para mostrar ambas correlaciones

# Definir y crear los datos para el segundo gráfico
data2 <- data.frame(
  m = c(-16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, "M"),
  DJF = c(4.7, 5.2, 5.4, 4.3, 4.1, 4.3, 4.9, 3.9, 5.2, 4.3, 7.1, 5.6, 4.3, 6.2, 9, 8.4, 13.1),
  MAM = c(4.7, 5.5, 4.7, 3, 4.6, 4.7, 3.8, 5, 5.7, 4.1, 6.9, 5.8, 7.5, 5.5, 8.2, 9.5, 10.6),
  JJA = c(4.2, 4.6, 5.1, 4.4, 4.2, 6, 4.5, 6.9, 7.1, 5.3, 5.6, 5.3, 4.9, 4.8, 6, 9.9, 11.3),
  SON = c(5.1, 5.3, 4, 4.8, 5.5, 4.4, 5.6, 6.4, 5.9, 4.9, 6.3, 3.2, 4, 6, 8.2, 11.7, 8.6)
)

data_long2 <- pivot_longer(data2, cols = -m, names_to = "Season", values_to = "Value")

data_long2$m <- factor(data_long2$m, levels = c(-16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, "M"))

data_long2$Season <- factor(data_long2$Season, levels = c("DJF", "MAM", "JJA", "SON"))

col_mes <- c("#004000", "#114F11", "#225F22", "#336F33", "#447F44", "#558F55",
             "#669F66", "#76AF76", "#88BF88", "#99CF99",
             "#AADFAA", "#BBEFBB", "#CCFFCC", "#B57EDC", "#9154BE", "#6E2AA0", "#4B0082")

x_labels <- c("-16", "-15", "-14", "-13", "-12", "-11", "-10", "-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "*M*")

plot2 <- ggplot(data_long2, aes(x = m, y = Value, fill = Season)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7, color = "black") +
  scale_fill_manual(values = c("DJF" = "#A3E4D7", "MAM" = "#A9DFBF", "JJA" = "#F9E79F", "SON" = "#F5CBA7")) +
  labs(x = "m", y = "Value (%)", title = "(b) Month (m)") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_markdown(angle = 45, hjust = 1, vjust = 1, color = col_mes),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_x_discrete(labels = x_labels) +
  guides(fill = guide_legend(title = "Season")) +
  scale_y_continuous(labels = percent_format(scale = 1), expand = c(0, 0), limits = c(0, 15))

# Definir y crear los datos para el tercer gráfico
data3 <- data.frame(
  Seasons = c("DJF", "MAM", "JJA", "SON"),
  SPI3 = c(40.4, 37.1, 33, 40.9),
  SPI6 = c(29.9, 35.9, 36.5, 33.7),
  SPI12 = c(29.7, 27, 30.5, 25.4)
)

# Transformar los datos a formato largo
data_long3 <- pivot_longer(data3, cols = c(SPI3, SPI6, SPI12), names_to = "SPI", values_to = "Value")

# Asegurar el orden correcto de las estaciones
data_long3$Seasons <- factor(data_long3$Seasons, levels = c("DJF", "MAM", "JJA", "SON"))

# Asegurar el orden correcto de las variables SPI
data_long3$SPI <- factor(data_long3$SPI, levels = c("SPI3", "SPI6", "SPI12"))

# Crear la gráfica de barras
plot3 <- ggplot(data_long3, aes(x = Seasons, y = Value, fill = SPI)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  scale_fill_manual(values = c("SPI3" = "#FFCCCC", "SPI6" = "#D98880", "SPI12" = "#8B0000")) +
  labs(x = "Seasons", y = "Value (%)", title = "(c) SPI (t)") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title = "SPI"))

# Combinar los tres gráficos en una sola imagen
combined_plot <- plot1 / plot2 / plot3

# Guardar el gráfico combinado en un archivo PDF
ggsave("C:/Users/Usuario/Dropbox/4FIRE_V2/plots/combined_plots.pdf", combined_plot, width = 8, height = 14)

# Mostrar el gráfico combinado
print(combined_plot)


