# Lee el archivo CSV
datos <- read.csv("../data/controles_espana.csv")

# Convertir las columnas DIA, MES y AÑO a una fecha
datos$Fecha <- as.Date(paste(datos$DIA, datos$MES, datos$AÑO, sep="-"), format="%d-%m-%Y")

# Separar los datos de anillamiento y control
datos_A <- datos[datos$MODO == "A",]
datos_C <- datos[datos$MODO == "C",]

# Unir los dos dataframes por la columna METAL
datos_final <- merge(datos_A, datos_C, by="METAL", suffixes=c("_anillamiento", "_control"))

# Calcular la diferencia de tiempo
datos_final$Tiempo <- as.numeric(datos_final$Fecha_control - datos_final$Fecha_anillamiento)

# Calcular la media del tiempo por especie
media_especie <- aggregate(Tiempo ~ ESPECIE_anillamiento, datos_final, mean)

# Ordenar las especies por la media del tiempo
media_especie <- media_especie[order(media_especie$Tiempo), ]

# Crear el gráfico de barras horizontal
ggplot(media_especie, aes(x=ESPECIE_anillamiento, y=Tiempo)) +
  geom_bar(stat="identity", fill="blue") +
  coord_flip() +  # Hace el gráfico horizontal
  labs(y="Media del tiempo entre anillamiento y control (días)", x="Especies", title="Media del tiempo entre anillamiento y control por especie") +
  theme_minimal()