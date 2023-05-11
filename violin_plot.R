# Violin plot de los tiempos de cada circuito comparado con la media del resto
# dado un rango de años determinado

# Load ggplot2 package
library(ggplot2)

# Create example data
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100), rnorm(100, mean = 2), rnorm(100, mean = -1))
)

# Create violin plot
ggplot(data, aes(y = group, x = value, fill = group)) + 
  geom_violin(color = NA) +
  geom_jitter(width = 0.2, alpha = 0.2) +
  scale_fill_discrete(name = "Group") +
  labs(title = "Violin Plot Example", x = "Group", y = "Value")


ggplot(data = iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_violin(draw_outline = FALSE)

head(data)


violin_data <- read.csv("datos/data_for_violin.csv")
colores_equipos <- read.csv("datos/colores_equipos.csv")

# Vamos a recibir los siguientes inputs:
# circuitId, mejor equipo, rango de años

# Puedo tomar la distribución del tiempo, o una gráfica de líneas en el tiempo 
# comparando con el resto de los equipos en promedio

circuit_id <- 32
best_team <- "Mercedes"
anio_inicio <- 2000
anio_fin <- 2022
n_std <- 3

data_best <- filter(
  violin_data, 
  year >= anio_inicio & year <= anio_fin & circuitId == circuit_id & team == best_team
)
mean_sec <- mean(data_best$seconds)
sd_sec <- sd(data_best$seconds)
data_best <- data_best[data_best$seconds > mean_sec - n_std * sd_sec & data_best$seconds < mean_sec + n_std * sd_sec, ]

data_others <- filter(
  violin_data,
  year >= anio_inicio & year <= anio_fin & circuitId == circuit_id & team != best_team
)
data_others$team <- "Others"
mean_sec <- mean(data_others$seconds)
sd_sec <- sd(data_others$seconds)
data_others <- data_others[data_others$seconds > mean_sec - n_std * sd_sec & data_others$seconds < mean_sec + n_std * sd_sec, ]

zusammen <- rbind(data_best, data_others)

v <- ggplot(zusammen, aes(y = team, x = seconds, fill = team)) +
  geom_violin(color = NA) +
  # geom_boxplot() +
  scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD",
                               "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B", "Others"="darkgray")) +
  labs(
    title = "Distribución de tiempo",
    x = "Tiempo (s)",
    y = circuit_id
  )
v
ggplotly(v)

a <- zusammen %>%
  group_by(year, team) %>%
  summarise(avg_time = mean(seconds)) %>%
  ggplot(aes(x = year, y = avg_time, color = team)) +
  geom_line() +
  geom_point() 
ggplotly(a)

violin_data <- read.csv("datos/data_for_violin.csv")
get_violin_plot <- function(circuit_id, best_team, anio_inicio, anio_fin) {
  n_std <- 3
  nombre_circuito <- filter(violin_data, circuitId == circuit_id)$name[1]
  
  # Sacamos los datos del mejor equipo en el circuito seleccionado
  data_best <- filter(
    violin_data, 
    year >= anio_inicio & year <= anio_fin & circuitId == circuit_id & team == best_team
  )
  mean_sec <- mean(data_best$seconds)
  sd_sec <- sd(data_best$seconds)
  data_best <- data_best[data_best$seconds > mean_sec - n_std * sd_sec & data_best$seconds < mean_sec + n_std * sd_sec, ]
  
  # Sacamos los datos del resto de los equipos
  data_others <- filter(
    violin_data,
    year >= anio_inicio & year <= anio_fin & circuitId == circuit_id & team != best_team
  )
  if (nrow(data_others) > 0) {
    data_others$team <- "Others"
    mean_sec <- mean(data_others$seconds)
    sd_sec <- sd(data_others$seconds)
    data_others <- data_others[data_others$seconds > mean_sec - n_std * sd_sec & data_others$seconds < mean_sec + n_std * sd_sec, ]
  }

  # Juntamos ambos datasets
  zusammen <- rbind(data_best, data_others)
  
  # Hacemos la gráfica
  v <- ggplot(zusammen, aes(y = team, x = seconds, fill = team)) +
    geom_violin(color = NA) +
    scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD",
                                 "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B", "Others"="darkgray")) +
    labs(
      title = "Distribución de tiempo",
      x = "Tiempo (s)",
      y = nombre_circuito
    )
  
  return(v)
}

get_violin_plot(17, "Ferrari", 1950, 2022)
