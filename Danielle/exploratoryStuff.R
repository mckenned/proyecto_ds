library(data.table)

# data <- fread("limpieza/limpio/anillamiento.csv")
setwd("~/Programming/erasmusCourses/DS/Final Project/proyecto_ds")
data <- fread("data/combined_anillamiento.csv")

# duplicate_anilla <- unique_data$Anilla[duplicated(unique_data$Anilla)]

# 656 down to 555 unique locations with lowercase correction too
data$NombreLocalidad <- tolower(data$NombreLocalidad)
data$NombreLocalidad <- gsub("la puebla del rio", "puebla del rio", data$NombreLocalidad)
data$NombreLocalidad <- gsub("la puebla del río", "puebla del rio", data$NombreLocalidad)
data$NombreLocalidad <- gsub("coria del río", "coria del rio", data$NombreLocalidad)
data$NombreLocalidad <- gsub("reserva biologica de donana", "reserva biologica de doñana", data$NombreLocalidad)
data$NombreLocalidad <- gsub("san juan del prto", "san juan del puerto", data$NombreLocalidad)
data$NombreLocalidad <- gsub("sanlucar d guadiana", "sanlucar de guadiana", data$NombreLocalidad)
data$NombreLocalidad <- gsub("sanlúcar de guadiana", "sanlucar de guadiana", data$NombreLocalidad)

# 304 down to 216 unique municipalities with lowercase correction too
data$MUNICIPIO <- tolower(data$MUNICIPIO)
data$MUNICIPIO <- gsub("la puebla del rio", "puebla del rio", data$MUNICIPIO)
data$MUNICIPIO <- gsub("la puebla del río", "puebla del rio", data$MUNICIPIO)
data$MUNICIPIO <- gsub("alcalá de los gazules", "alcala de los gazules", data$MUNICIPIO)
data$MUNICIPIO <- gsub("alcolea del río", "alcolea del rio", data$MUNICIPIO)
data$MUNICIPIO <- gsub("coria del río", "coria del rio", data$MUNICIPIO)
data$MUNICIPIO <- gsub("gibraleón", "gibraleon", data$MUNICIPIO)
data$MUNICIPIO <- gsub("jerez de la frta", "jerez de la frontera", data$MUNICIPIO)
data$MUNICIPIO <- gsub("la palam del condado", "la palma del condado", data$MUNICIPIO)
data$MUNICIPIO <- gsub("los palacios-", "los palacios", data$MUNICIPIO)
data$MUNICIPIO <- gsub("p.n. doñana", "p. nac. de doñana", data$MUNICIPIO)
data$MUNICIPIO <- gsub("puerto de  santa maria", "puerto de santa maria", data$MUNICIPIO)
data$MUNICIPIO <- gsub("puerto de santamaria", "puerto de santa maria", data$MUNICIPIO)
data$MUNICIPIO <- gsub("puerto santa maria", "puerto de santa maria", data$MUNICIPIO)
data$MUNICIPIO <- gsub("sanlucar de barrameda.", "sanlucar de barrameda", data$MUNICIPIO)
data$MUNICIPIO <- gsub("santa olalla de  cala", "santa olalla de cala", data$MUNICIPIO)
data$MUNICIPIO <- gsub("villamartín", "villamartin", data$MUNICIPIO)

# Reduced from 32 to 24
data$NombreAnillador <- tolower(data$NombreAnillador)
data$NombreAnillador <- iconv(data$NombreAnillador, to = "ASCII//TRANSLIT")
data$NombreAnillador <- gsub("'", "", data$NombreAnillador)
data$NombreAnillador <- gsub("~", "", data$NombreAnillador)


freq_ani <- table(data$NombreAnillador)
freq_ani <- as.data.frame(freq_ani)

freq_loc <- table(data$NombreLocalidad)
freq_loc <- as.data.frame(freq_loc)

freq_munip <- table(data$MUNICIPIO)
freq_munip <- as.data.frame(freq_munip)

freq_esp <- table(data$NombreEspecie)
freq_esp <- as.data.frame(freq_esp)


unique_Munips <- unique(data[, c("NombreLocalidad", "MUNICIPIO")])

# Then, calculate the frequencies of NombreLocalidad
freq_table <- table(data$NombreLocalidad)
freq_df <- as.data.frame(freq_table)

# Add MUNICIPIO column to freq_df based on unique_data
# freq_df$MUNICIPIO <- unique_data$MUNICIPIO[match(freq_df$Var1, unique_data$NombreLocalidad)]
