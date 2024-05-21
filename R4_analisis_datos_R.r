install.packages("ggplot2")
library(ggplot2)

data <- list(
  HC = c(44.9, 45.6, 45.4, 44.9, 44.3),
  HI = c(39.2, 47.0, 50.9, 52.9, 56.4),
  HT = c(93.5, 93.1, 93.2, 92.9, 92.5),
  HTP = c(43.7, 52.1, 49.5, 47.3, 45.9),
  U6E = c(51.3, 47.0, 45.3, 45.0, 43.0),
  UI6E = c(57.4, 59.5, 63.9, 65.8, 70.1),
  UCHE = c(51.3, 52.2, 46.8, 46.7, 44.6),
  UITI = c(12.8, 14.7, 20.4, 23.7, 27.2),
  UIFH = c(29.1, 20.5, 16.7, 13.4, 10.7),
  UTC6E = c(71.5, 73.6, 72.2, 73.5, 75.1)
)

alfa <- 0.2
valores_suavi <- list()

for (llave in names(data)) {
  valor_nuevo <- alfa * data[[llave]][length(data[[llave]])] + (1 - alfa) * data[[llave]][length(data[[llave]]) - 1]
  valores_suavi[[llave]] <- valor_nuevo
  data[[llave]] <- c(data[[llave]], valor_nuevo)
}

for (llave in names(valores_suavi)) {
  cat(sprintf("%s para el 2020: %.2f\n", llave, valores_suavi[[llave]]))
}

annos <- c(2015, 2016, 2017, 2018, 2019, 2020)

ggplot(data.frame(annos = rep(annos, length(data)), value = unlist(data), variable = rep(names(data), each = length(annos))), aes(x = annos, y = value, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = "year", y = "Valor del Indicador") +
  theme_bw() +
  theme(legend.position = "bottom")