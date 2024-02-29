# Use anillamiento for data
freq_ani <- table(data$NombreAnillador)
freq_ani <- as.data.frame(freq_ani)

freq_loc <- table(data$NombreLocalidad)
freq_loc <- as.data.frame(freq_loc)

freq_munip <- table(data$MUNICIPIO)
freq_munip <- as.data.frame(freq_munip)

freq_esp <- table(data$NombreEspecie)
freq_esp <- as.data.frame(freq_esp)

