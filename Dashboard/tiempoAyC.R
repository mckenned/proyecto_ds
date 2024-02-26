# Cargar los datos
datos <- read.csv("../limpieza/limpio/controles_espana_familia.csv")

# Filtrar las filas donde METAL no está vacío
datos <- datos %>% filter(METAL != "")

# Convertir la columna FECHA a tipo de dato Date
datos$FECHA <- as.Date(datos$FECHA, format = "%m/%d/%Y")

# Calcular la diferencia de tiempo entre anillamiento y control
datos <- datos %>%
  group_by(METAL) %>%
  mutate(tiempo_diferencia = ifelse(MODO == "C", FECHA - lag(FECHA), NA))

