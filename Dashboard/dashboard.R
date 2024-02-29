library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggmap)
library(lubridate)  # Para la función mdy()
register_google("AIzaSyB5mUIt_VgX0xIQHDwwdFNtP9nUFHw5t8U")

# Cargar los datos para el histograma
datos <- read.csv("../limpieza/limpio/controles_espana_familia_hist.csv")
anillamientos <- read.csv("../limpieza/limpio/anillamiento_familia.csv")
# Ruta para el archivo de coordenadas
coordenadas_file <- "controles_familia_extranjero_coordenadas.csv"

# Cargar los datos para el mapa si el archivo existe, si no, calcular las coordenadas
if (file.exists(coordenadas_file)) {
  datos_mapa <- read.csv(coordenadas_file)
} else {
  datos_mapa <- read.csv("../limpieza/limpio/controles_extranjeros_familia_mapa.csv")
  coordenadas <- geocode(as.character(datos_mapa$LOCALIDAD))
  datos_mapa$LATITUD <- coordenadas$lat
  datos_mapa$LONGITUD <- coordenadas$lon
  # Guardar los datos con las coordenadas en un archivo CSV
  write.csv(datos_mapa, coordenadas_file, row.names = FALSE)
}

# Filtrar las filas donde METAL no está vacío para el histograma
datos <- datos %>% filter(METAL != "")

# Convertir la columna FECHA a tipo de dato Date para el histograma
datos$FECHA <- mdy(datos$FECHA)

# Calcular la diferencia de tiempo entre anillamiento y control para el histograma
datos <- datos %>%
  group_by(METAL) %>%
  mutate(tiempo_diferencia = ifelse(MODO == "C", FECHA - lag(FECHA), NA))

# Definir UI
ui <- fluidPage(
  titlePanel("Anillamiento y Control de Aves"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tab != 'imagen'",
        h4("Filtro para Histograma de tiempo entre anillamiento y control"),
        selectInput("orden_histo", "Orden:", choices = unique(datos$ORDEN)),
        selectInput("familia_histo", "Familia:", choices = NULL),
        selectInput("especie_histo", "Especie:", choices = NULL),
        selectInput("tiempo", "Filtrar por tiempo:", choices = c("Menos de 1 año", "1 a 2 años", "2 a 5 años", "5 o más"))
      ),
      h4("Filtro para Mapa"),
      selectInput("orden_mapa", "Orden:", choices = unique(datos_mapa$ORDEN)),
      selectInput("familia_mapa", "Familia:", choices = NULL),
      selectInput("especie_mapa", "Especie:", choices = NULL),
      numericInput("año_desde", "Año Desde:", min(datos_mapa$AÑO)),
      numericInput("año_hasta", "Año Hasta:", max(datos_mapa$AÑO)),
      checkboxInput("aplicar_filtros", "Aplicar filtros", value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        id = "tab",
        tabPanel("Histograma tiempo Anillamiento y Control", plotOutput("histograma")),
        tabPanel("Mapa", leafletOutput("mapa")),
        tabPanel("Imagen", imageOutput("imagen"))
      )
    )
  )
)

# Definir server
server <- function(input, output, session) {
  
  # Filtrar datos según selecciones para el histograma
  datos_filtrados_histo <- reactive({
    datos_filtrados <- datos %>%
      filter(ORDEN == input$orden_histo) %>%
      filter(FAMILIA == input$familia_histo) %>%
      filter(ESPECIE == input$especie_histo)
    
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
    
    return(datos_filtrados)
  })
  
  # Filtrar datos según selecciones para el mapa
  datos_filtrados_mapa <- reactive({
    datos_mapa %>%
      filter(ORDEN == input$orden_mapa) %>%
      filter(FAMILIA == input$familia_mapa) %>%
      filter(ESPECIE == input$especie_mapa) %>%
      filter(AÑO >= input$año_desde & AÑO <= input$año_hasta)
  })
  
  # Actualizar selección de familias y especies según orden seleccionado para el histograma
  observeEvent(input$orden_histo, {
    updateSelectInput(session, "familia_histo", choices = unique(datos$FAMILIA[datos$ORDEN == input$orden_histo]))
    updateSelectInput(session, "especie_histo", choices = NULL)
  })
  
  observeEvent(input$familia_histo, {
    updateSelectInput(session, "especie_histo", choices = unique(datos$ESPECIE[datos$FAMILIA == input$familia_histo]))
  })
  
  # Actualizar selección de familias y especies según orden seleccionado para el mapa
  observeEvent(input$orden_mapa, {
    updateSelectInput(session, "familia_mapa", choices = unique(datos_mapa$FAMILIA[datos_mapa$ORDEN == input$orden_mapa]))
    updateSelectInput(session, "especie_mapa", choices = NULL)
  })
  
  observeEvent(input$familia_mapa, {
    updateSelectInput(session, "especie_mapa", choices = unique(datos_mapa$ESPECIE[datos_mapa$FAMILIA == input$familia_mapa]))
  })
  
  # Crear histograma
  output$histograma <- renderPlot({
    ggplot(datos_filtrados_histo(), aes(x = tiempo_diferencia, y = ESPECIE, fill = METAL)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Diferencia de tiempo entre anillamiento y control",
           x = "Días",
           y = "Especie") +
      theme_minimal() +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed")
  })
  
  # Crear el mapa
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
    
    # Agregar puntos al mapa
    mapa <- leaflet(datos_filtrados_mapa) %>%
      addTiles() %>%
      addCircleMarkers(
        ~LONGITUD, ~LATITUD,
        radius = 5,
        color = ifelse(datos_filtrados_mapa$MODO == "A", "yellow", "red"),
        fillOpacity = 0.8
      )
    
    # Agregar líneas entre los puntos de anillamiento y control
    for (i in 2:nrow(datos_filtrados_mapa)) {
      if (datos_filtrados_mapa$MODO[i] == "C" & datos_filtrados_mapa$MODO[i - 1] == "A") {
        # Calcular el grosor de la línea proporcional a la diferencia de tiempo
        weight <- 1 + datos_filtrados_mapa$tiempo_diferencia[i] / 365  # Ajusta según necesites
        # Agregar la línea entre los puntos de anillamiento y control
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
    list(src = "ruta/a/la/imagen.png",
         contentType = "image/png",
         width = 400,
         height = 300)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
