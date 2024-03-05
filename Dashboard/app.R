library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggmap)
library(lubridate)  # Para la función mdy()
register_google("AIzaSyB5mUIt_VgX0xIQHDwwdFNtP9nUFHw5t8U")

# Cargar los datos para el histograma
datos <- read.csv("controles_espana_familia_hist.csv")
anillamientos <- read.csv("anillamiento_familia.csv")
# Ruta para el archivo de coordenadas
coordenadas_file <- "controles_familia_extranjero_coordenadas.csv"

# Cargar los datos para el mapa si el archivo existe, si no, calcular las coordenadas
if (file.exists(coordenadas_file)) {
  datos_mapa <- read.csv(coordenadas_file)
} else {
  datos_mapa <- read.csv("controles_extranjeros_familia_mapa.csv")
  coordenadas <- geocode(as.character(datos_mapa$LOCALIDAD))
  datos_mapa$LATITUD <- coordenadas$lat
  datos_mapa$LONGITUD <- coordenadas$lon
  # Guardar los datos con las coordenadas en un archivo CSV
  write.csv(datos_mapa, coordenadas_file, row.names = FALSE)
}
translate_month <- function(month) {
  months_english <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  months_spanish <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  if (month %in% months_english) {
    return(months_spanish[match(month, months_english)])
  } else if(month %in% months_spanish){
    return(months_english[match(month, months_spanish)])
  }else{
    return("Invalid month") 
  }
}


# Filtrar las filas donde METAL no está vacío para el histograma
datos <- datos %>% filter(METAL != "")

# Convertir la columna FECHA a tipo de dato Date para el histograma
datos$FECHA <- mdy(datos$FECHA)

# Calcular la diferencia de tiempo entre anillamiento y control para el histograma
datos <- datos %>%
  group_by(METAL) %>%
  mutate(tiempo_diferencia = ifelse(MODO == "C", FECHA - lag(FECHA), NA))

# Define UI
ui <- fluidPage(
  titlePanel("Anillamiento y Control de Aves"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tab != 'imagen'",
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO HISTOGRAMA CALCULO TIEMPO POR ESPECIE, FAMILIA ORDEN Y LOCALIDAD ENTRE ANILLAMIENTO Y CONTROL   #
        #   (PREGUNTA DAVID)                                                                                        #
        #############################################################################################################
        h4("Filtro para Histograma de tiempo y locadidad entre anillamiento y control"),
        selectInput("orden_histo", "Orden:", choices = unique(datos$ORDEN)),
        checkboxInput("aplicar_filtro_orden", "Solo filtrar por orden", value = FALSE),
        selectInput("familia_histo", "Familia:", choices = NULL),
        checkboxInput("aplicar_filtro_familia", "Solo filtrar por familia", value = FALSE),
        selectInput("especie_histo", "Especie:", choices = NULL),
        selectInput("lugar_histo", "Lugar:", choices = NULL),
        checkboxInput("aplicar_filtro_lugar", "Filtrar resultados por localidades", value = FALSE),
        selectInput("tiempo", "Filtrar por tiempo:", choices = c("Menos de 1 año", "1 a 2 años", "2 a 5 años", "5 o más")),
        checkboxInput("aplicar_filtro_tiempo", "Filtrar resultados por tiempo", value = FALSE),
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO PARA MAPA DE REPRESENTACIÓN DEL ANILLAMIENTO Y CONTROL DE AVES POR ESPECIE, FAMILIA Y ORDEN     #
        #    (PREGUNTA GENERAL)                                                                                     #
        #############################################################################################################
        h4("Filtro para Mapa"),
        selectInput("orden_mapa", "Orden:", choices = unique(datos_mapa$ORDEN)),
        selectInput("familia_mapa", "Familia:", choices = NULL),
        selectInput("especie_mapa", "Especie:", choices = NULL),
        checkboxInput("aplicar_filtros", "Aplicar filtros", value = TRUE),
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO PARA HISTOGRAMA DE ANILLAMIENTO DE ESPECIE POR MESES (PREGUNTA MATTHEW)                         #
        #                                                                                                           #
        #############################################################################################################
        h4("Filtro para Histograma de anillamientos de especie por meses"),
        # Nuevo selector de entrada para la especie
        selectInput("especie_anillamiento", "Especie de Anillamiento:",
                    choices = unique(anillamientos$NombreEspecie)),
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO PARA HISTOGRAMA DE ANILLAMIENTO DE ESPECIES POR MES (PREGUNTA MATTHEW)                          #
        #                                                                                                           #
        #############################################################################################################
        h4("Filtro para Histograma de anillamientos de especies por mes"),
        # Nuevo selector de entrada para el mes
        selectInput("mes_anillamiento", "Mes de Anillamiento:",
                    choices = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO PARA HISTOGRAMA DE ANILLAMIENTO DE ESPECIE POR AÑOS (PREGUNTA MATTHEW)                          #
        #                                                                                                           #
        #############################################################################################################
        h4("Filtro para Histograma de anillamientos de especie por años"),
        # Nuevo selector de entrada para la especie
        selectInput("especie_anillamiento_año", "Especie de Anillamiento:",
                    choices = unique(anillamientos$NombreEspecie)),
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO PARA HISTOGRAMA DE ANILLAMIENTO DE ESPECIES POR AÑO (PREGUNTA MATTHEW)                          #
        #                                                                                                           #
        #############################################################################################################
        h4("Filtro para Histograma de anillamientos de especies por año"),
        # Nuevo selector de entrada para el año
        numericInput("año_anillamiento", "Número de filtro:", value = NULL, min = 1900),
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO PARA HISTOGRAMA DE ANILLADOR POR MESES (PREGUNTA PABLO)                                         #
        #                                                                                                           #
        #############################################################################################################
        h4("Filtro para Histograma de anillador por meses"),
        # Nuevo selector de entrada para el anillador
        selectInput("anillador_anillamiento_mes", "Anillador:",
                    choices = unique(anillamientos$NombreAnillador)),
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO PARA HISTOGRAMA DE ANILLADORES POR MES (PREGUNTA PABLO)                                         #
        #                                                                                                           #
        #############################################################################################################
        h4("Filtro para Histograma de anillador por meses"),
        # Nuevo selector de entrada para el mes
        selectInput("anillador_meses", "Mes de Anillamiento:",
                    choices = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
        #############################################################################################################
        #                                                                                                           #
        #    FILTRO PARA HISTOGRAMA DE ANILLAMIENTO DE ESPECIE POR AÑOS (PREGUNTA PABLO)                            #
        #                                                                                                           #
        #############################################################################################################
        h4("Filtro para Histograma de anillador por años"),
        # Nuevo selector de entrada para la especie
        selectInput("anillador_anillamiento_años", "Anillador:",
                    choices = unique(anillamientos$NombreAnillador))
      ),
      
    ),
    mainPanel(
      tabsetPanel(
        id = "tab",
        tabPanel("Histograma tiempo Anillamiento y Control", plotOutput("histograma")),
        tabPanel("Mapa", leafletOutput("mapa")),
        tabPanel("Tasa de anillamiento", imageOutput("imagen")),
        tabPanel("Histograma de especie por meses", plotOutput("histograma_meses")),
        tabPanel("Histograma de especies por mes", plotOutput("histograma_especies_meses")),
        tabPanel("Histograma de especie por años", plotOutput("histograma_años")),
        tabPanel("Histograma de especies por año", plotOutput("histograma_especies_años")),
        tabPanel("Histograma de anillador por meses", plotOutput("histograma_anillador_meses")),
        tabPanel("Histograma de anilladores por mes", plotOutput("histograma_anilladores_mes")),
        tabPanel("Histograma de anillador por años", plotOutput("histograma_anillador_años")),
        
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Filtrar datos según selecciones para el histograma
  datos_filtrados_histo <- reactive({
    if(input$aplicar_filtro_lugar == TRUE){
      if(input$aplicar_filtro_orden == TRUE){
        datos_filtrados <- datos %>%
          filter(ORDEN == input$orden_histo) %>%
          filter(PROVINCIA == input$lugar_histo)
      }else if(input$aplicar_filtro_familia == TRUE){
        datos_filtrados <- datos %>%
          filter(ORDEN == input$orden_histo) %>%
          filter(FAMILIA == input$familia_histo) %>%
          filter(PROVINCIA == input$lugar_histo)
      }else{
        datos_filtrados <- datos %>%
          filter(ORDEN == input$orden_histo) %>%
          filter(FAMILIA == input$familia_histo) %>%
          filter(ESPECIE == input$especie_histo) %>%
          filter(PROVINCIA == input$lugar_histo)
      }
    }else{
      if(input$aplicar_filtro_orden == TRUE){
        datos_filtrados <- datos %>%
          filter(ORDEN == input$orden_histo) 
      }else if(input$aplicar_filtro_familia == TRUE){
        datos_filtrados <- datos %>%
          filter(ORDEN == input$orden_histo) %>%
          filter(FAMILIA == input$familia_histo)
      }else{
        datos_filtrados <- datos %>%
          filter(ORDEN == input$orden_histo) %>%
          filter(FAMILIA == input$familia_histo) %>%
          filter(ESPECIE == input$especie_histo) 
      }
    }
    
    
    if(input$aplicar_filtro_tiempo == TRUE){
      # Filtrar por tiempo para el histograma
      if (input$tiempo == "Menos de 1 año") {
        datos_filtrados <- datos_filtrados %>%
          filter(tiempo_diferencia < 365)
      } else if (input$tiempo == "1 a 2 años") {
        datos_filtrados <- datos_filtrados %>%
          filter(tiempo_diferencia >= 365 & tiempo_diferencia < 730)
      } else if (input$tiempo == "2 a 5 años") {
        datos_filtrados <- datos_filtrados %>%
          filter(tiempo_diferencia >= 730 & tiempo_diferencia < 1825)
      } else if (input$tiempo == "5 o más") {
        datos_filtrados <- datos_filtrados %>%
          filter(tiempo_diferencia >= 1825)
      }
      
    }
    
    return(datos_filtrados)
  })
  output$histograma_anillador_años <- renderPlot({
    anillamientos$FechaCaptura <- as.Date(anillamientos$FechaCaptura)
    anillamientos$AñoCaptura <- year(anillamientos$FechaCaptura)
    anillamientos <- anillamientos[anillamientos$AñoCaptura > 1700, ]
    anillamientos <- na.omit(anillamientos)
    
    anillamientos_anillador <- anillamientos[, (names(anillamientos) %in% c("NombreAnillador", "FechaCaptura", "AñoCaptura"))]
    anilladores_name_filter <- input$anillador_anillamiento_años
    print(anilladores_name_filter)
    anillamientos_anillador <- subset(anillamientos_anillador, tolower(NombreAnillador) == tolower(anilladores_name_filter))
    anillamientos_anillador <- na.omit(anillamientos_anillador)
    
    min_year <- min(anillamientos$AñoCaptura)
    max_year <- max(anillamientos$AñoCaptura)
    
    hist(anillamientos_anillador$AñoCaptura,
         xlab = "Año",
         ylab = "Frecuencia",
         main = paste(anilladores_name_filter, "capturas por año"),
         breaks = seq(min_year - 0.5, max_year + 0.5, by = 1))
  })
  output$histograma_anillador_meses <- renderPlot({
    anillamientos_anillador <- anillamientos[, (names(anillamientos) %in% c("NombreAnillador", "FechaCaptura"))]
    anilladores_name_filter <- input$anillador_anillamiento_mes
    anillamientos_anillador <- subset(anillamientos_anillador, tolower(NombreAnillador) == tolower(anilladores_name_filter))
    anillamientos_anillador$FechaCaptura <- as.Date(anillamientos_anillador$FechaCaptura)
    anillamientos_anillador$MesCaptura <- month(anillamientos_anillador$FechaCaptura)
    anillamientos_anillador <- na.omit(anillamientos_anillador)
    
    hist(anillamientos_anillador$MesCaptura,
         breaks = seq(1, 12, by = 1),  # specify breaks for each month
         xlab = "Mes",
         ylab = "Frecuencia",
         main = paste(anilladores_name_filter, "capturas por mes"))
  })
  #Procesamiento anillamientos anilladores/mes
  output$histograma_anillador_meses <- renderPlot({
    anillamientos_anillador <- anillamientos[, (names(anillamientos) %in% c("NombreAnillador", "FechaCaptura"))]
    anilladores_name_filter <- input$anillador_anillamiento_mes
    anillamientos_anillador <- subset(anillamientos_anillador, tolower(NombreAnillador) == tolower(anilladores_name_filter))
    anillamientos_anillador$FechaCaptura <- as.Date(anillamientos_anillador$FechaCaptura)
    anillamientos_anillador$MesCaptura <- month(anillamientos_anillador$FechaCaptura)
    anillamientos_anillador <- na.omit(anillamientos_anillador)
    
    hist(anillamientos_anillador$MesCaptura,
         breaks = seq(1, 12, by = 1),  # specify breaks for each month
         xlab = "Mes",
         ylab = "Frecuencia",
         main = paste(anilladores_name_filter, "capturas por mes"))
  })
  #Procesamiento anillamientos especies/mes
  output$histograma_anilladores_mes <- renderPlot({
    anillamientos_anillador <- anillamientos[, (names(anillamientos) %in% c("NombreAnillador", "FechaCaptura"))]
    month_filter <- input$mes_anillamiento
    mes <- translate_month(month_filter)
    month_number <- match(mes, month.name)
    anillamientos_anillador$FechaCaptura <- as.Date(anillamientos_anillador$FechaCaptura)
    anillamientos_anillador$MesCaptura <- month(anillamientos_anillador$FechaCaptura)
    anillamientos_anillador <- subset(anillamientos_anillador, MesCaptura == month_number)
    anillamientos_anillador <- na.omit(anillamientos_anillador)
    
    anilladores_counts <- table(anillamientos_anillador$NombreAnillador)
    anilladores_counts <- anilladores_counts[order(-anilladores_counts)]
    anilladores_counts <- head(anilladores_counts, 10)
    anilladores_counts <- anilladores_counts[order(anilladores_counts)]
    anilladores_names <- names(anilladores_counts)
    colors <- rainbow(length(anilladores_counts))
    
    max_count <- max(anilladores_counts)
    xlim <- c(0, ifelse(max_count > 12000, max_count, 12000))

    
    barplot(anilladores_counts,
            col = rev(colors),
            xlab = "Cantidad de capturas",
            main = paste("Capturas en", month_filter),
            horiz = TRUE,
            names.arg = FALSE,
            xlim = xlim)
    
    legend("bottomright", inset=c(0,0), legend = rev(anilladores_names), fill = colors, bg = adjustcolor("white", alpha = 0.8))
  })
  
  #Procesamiento anillamientos especies/mes
  output$histograma_meses <- renderPlot({
    anillamientos_especie <- anillamientos[, (names(anillamientos) %in% c("NombreEspecie", "FechaCaptura"))]
    species_name_filter <- input$especie_anillamiento
    anillamientos_especie <- subset(anillamientos_especie, tolower(NombreEspecie) == tolower(species_name_filter))
    anillamientos_especie$FechaCaptura <- as.Date(anillamientos_especie$FechaCaptura)
    anillamientos_especie$MesCaptura <- month(anillamientos_especie$FechaCaptura)
    anillamientos_especie <- na.omit(anillamientos_especie)
    
    hist(anillamientos_especie$MesCaptura,
         breaks = seq(1, 12, by = 1),  # specify breaks for each month
         xlab = "Mes",
         ylab = "Frecuencia",
         main = paste(species_name_filter, "capturados por mes"))
  })
  
  #Procesamiento anillamientos especies/mes
  output$histograma_especies_meses <- renderPlot({
    anillamientos_especie <- anillamientos[, (names(anillamientos) %in% c("NombreEspecie", "FechaCaptura"))]
    month_filter <- input$mes_anillamiento
    mes <- translate_month(month_filter)
    month_number <- match(mes, month.name)
    anillamientos_especie$FechaCaptura <- as.Date(anillamientos_especie$FechaCaptura)
    anillamientos_especie$MesCaptura <- month(anillamientos_especie$FechaCaptura)
    anillamientos_especie <- subset(anillamientos_especie, MesCaptura == month_number)
    anillamientos_especie <- na.omit(anillamientos_especie)
    
    species_counts <- table(anillamientos_especie$NombreEspecie)
    species_counts <- species_counts[order(-species_counts)]
    species_counts <- head(species_counts, 10)
    species_counts <- species_counts[order(species_counts)]
    species_names <- names(species_counts)
    colors <- rainbow(length(species_counts))
    
    max_count <- max(species_counts)
    xlim <- c(0, ifelse(max_count > 12000, max_count, 12000))
    
    
    
    barplot(species_counts,
            col = rev(colors),
            xlab = "Cantidad de capturas",
            main = paste("Especies capturadas en", month_filter),
            horiz = TRUE,
            names.arg = FALSE,
            xlim = xlim)
    
    legend("bottomright", inset=c(0,0), legend = rev(species_names), fill = colors, bg = adjustcolor("white", alpha = 0.8))
  })
  
  #Procesamiento anillamientos especies/mes
  output$histograma_años <- renderPlot({
    anillamientos$FechaCaptura <- as.Date(anillamientos$FechaCaptura)
    anillamientos$AñoCaptura <- year(anillamientos$FechaCaptura)
    anillamientos <- anillamientos[anillamientos$AñoCaptura > 1700, ]
    anillamientos <- na.omit(anillamientos)
    
    anillamientos_especie <- anillamientos[, (names(anillamientos) %in% c("NombreEspecie", "FechaCaptura", "AñoCaptura"))]
    species_name_filter <- input$especie_anillamiento_año
    anillamientos_especie <- subset(anillamientos_especie, tolower(NombreEspecie) == tolower(species_name_filter))
    anillamientos_especie <- na.omit(anillamientos_especie)
    
    min_year <- min(anillamientos$AñoCaptura)
    max_year <- max(anillamientos$AñoCaptura)
    
    hist(anillamientos_especie$AñoCaptura,
         xlab = "Año",
         ylab = "Frecuencia",
         main = paste(species_name_filter, "capturados por año"),
         breaks = seq(min_year - 0.5, max_year + 0.5, by = 1))
  })
  
  #Procesamiento anillamientos especies/mes
  output$histograma_especies_años <- renderPlot({
    anillamientos_especie <- anillamientos[, (names(anillamientos) %in% c("NombreEspecie", "FechaCaptura"))]
    year_filter <- input$año_anillamiento
    print(year_filter)
    anillamientos_especie$FechaCaptura <- as.Date(anillamientos_especie$FechaCaptura)
    anillamientos_especie$AñoCaptura <- year(anillamientos_especie$FechaCaptura)
    anillamientos_especie <- subset(anillamientos_especie, AñoCaptura == year_filter)
    print(anillamientos_especie)
    anillamientos_especie <- na.omit(anillamientos_especie)
    # Verificar si no se encontraron anillamientos para el año ingresado
    if (nrow(anillamientos_especie) == 0) {
      return(plot(1, type = "n", xlab = "", ylab = "", main = "No se encontraron anillamientos para ese año", axes = FALSE))
    }
    species_counts <- table(anillamientos_especie$NombreEspecie)
    species_counts <- species_counts[order(-species_counts)]
    species_counts <- head(species_counts, 10)
    species_counts <- species_counts[order(species_counts)]
    species_names <- names(species_counts)
    colors <- rainbow(length(species_counts))
    
    max_count <- max(species_counts)
    xlim <- c(0, ifelse(max_count > 5000, max_count, 5000))
    
    barplot(species_counts,
            col = rev(colors),
            xlab = "Cantidad de capturas",
            main = paste("Especies capturadas en", year_filter),
            horiz = TRUE,
            names.arg = FALSE,
            xlim = xlim)
    
    legend("bottomright", inset=c(0,0), legend = rev(species_names), fill = colors, bg = adjustcolor("white", alpha = 0.8))
  })
  # Filtrar datos según selecciones para el mapa
  datos_filtrados_mapa <- reactive({
    datos_mapa %>%
      filter(ORDEN == input$orden_mapa) %>%
      filter(FAMILIA == input$familia_mapa) %>%
      filter(ESPECIE == input$especie_mapa)
  })
  
  observeEvent(input$orden_histo, {
    updateSelectInput(session, "familia_histo", choices = unique(datos$FAMILIA[datos$ORDEN == input$orden_histo]))
    # Actualizamos las especies basadas en el nuevo orden seleccionado
    updateSelectInput(session, "especie_histo", choices = unique(datos$ESPECIE[datos$ORDEN == input$orden_histo]))
  })
  
  observeEvent(input$familia_histo, {
    updateSelectInput(session, "especie_histo", choices = unique(datos$ESPECIE[datos$FAMILIA == input$familia_histo]))
    # Actualizamos las localidades basadas en la nueva familia seleccionada
    updateSelectInput(session, "lugar_histo", choices = unique(datos$PROVINCIA[datos$FAMILIA == input$familia_histo]))
  })
  
  observeEvent(input$orden_mapa, {
    updateSelectInput(session, "familia_mapa", choices = unique(datos_mapa$FAMILIA[datos_mapa$ORDEN == input$orden_mapa]))
    updateSelectInput(session, "especie_mapa", choices = NULL)
  })
  
  observeEvent(input$familia_mapa, {
    
    updateSelectInput(session, "especie_mapa", choices = unique(datos_mapa$ESPECIE[datos_mapa$FAMILIA == input$familia_mapa]))
  })
  
  output$histograma <- renderPlot({
    ggplot(datos_filtrados_histo(), aes(x = tiempo_diferencia, y = ESPECIE, fill = METAL)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Diferencia de tiempo entre anillamiento y control",
           x = "Días",
           y = "Especie") +
      theme_minimal() +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed")
  })
  
  output$mapa <- renderLeaflet({
    # Si se ha desactivado la aplicación de filtros, mostrar todos los puntos
    if (!input$aplicar_filtros) {
      datos_filtrados_mapa <- datos_mapa
    } else {
      # Calcular la diferencia de tiempo entre anillamiento y control para cada punto
      datos_filtrados_mapa <- datos_filtrados_mapa() %>%
        arrange(METAL, DIA, MES, AÑO) %>%
        group_by(METAL) %>%
        mutate(tiempo_diferencia = c(NA, diff(as.numeric(difftime(FECHA, lag(FECHA))))))
    }
    
    mapa <- leaflet(datos_filtrados_mapa) %>%
      addTiles() %>%
      addCircleMarkers(
        ~LONGITUD, ~LATITUD,
        radius = 5,
        color = ifelse(datos_filtrados_mapa$MODO == "A", "yellow", "red"),
        fillOpacity = 0.8
      )
    
    for (i in 2:nrow(datos_filtrados_mapa)) {
      print(datos_filtrados_mapa$MODO[i] == "C" & datos_filtrados_mapa$MODO[i - 1] == "A")
      if (datos_filtrados_mapa$MODO[i] == "C" & datos_filtrados_mapa$MODO[i - 1] == "A") {
        weight <- 1
        mapa <- addPolylines(mapa, 
                             lng = c(datos_filtrados_mapa$LONGITUD[i - 1], datos_filtrados_mapa$LONGITUD[i]), 
                             lat = c(datos_filtrados_mapa$LATITUD[i - 1], datos_filtrados_mapa$LATITUD[i]),
                             color = "black",
                             weight = weight)
      }
    }
    
    mapa
  })
  
  
  # Mostrar imagen
  output$imagen <- renderImage({
    list(src = "../Danielle/images/Decomp_Acro_scir_rio_guad.png",
         contentType = "image/png",
         width = 400,
         height = 300)
  }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
