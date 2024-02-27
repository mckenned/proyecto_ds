library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggmap)
register_google("AIzaSyB5mUIt_VgX0xIQHDwwdFNtP9nUFHw5t8U")

# Cargar los datos para el histograma
datos <- read.csv("../limpieza/limpio/controles_espana_familia.csv")

# Cargar los datos para el mapa
datos_mapa <- read.csv("../limpieza/limpio/controles_extranjeros_familia.csv")

# Filtrar las filas donde METAL no está vacío para el histograma
datos <- datos %>% filter(METAL != "")

# Filtrar las filas donde METAL no está vacío para el mapa
datos_mapa <- datos_mapa %>% filter(METAL != "")

# Convertir la columna FECHA a tipo de dato Date para el histograma
datos$FECHA <- as.Date(datos$FECHA, format = "%m/%d/%Y")

# Calcular la diferencia de tiempo entre anillamiento y control para el histograma
datos <- datos %>%
  group_by(METAL) %>%
  mutate(tiempo_diferencia = ifelse(MODO == "C", FECHA - lag(FECHA), NA))

# Extraer las coordenadas de las localidades para el mapa
coordenadas <- geocode(as.character(datos_mapa$LOCALIDAD))

# Agregar las coordenadas al dataframe de los datos del mapa
datos_mapa$LATITUD <- coordenadas$lat
datos_mapa$LONGITUD <- coordenadas$lon

# Definir UI
ui <- fluidPage(
  titlePanel("Anillamiento y Control de Aves"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tab != 'imagen'",
        h4("Filtro para Histograma de tiempo entre anillamiento y control"),
        selectInput("orden", "Orden:", choices = unique(datos$ORDEN)),
        selectInput("familia", "Familia:", choices = NULL),
        selectInput("especie", "Especie:", choices = NULL),
        selectInput("tiempo", "Filtrar por tiempo:", choices = c("Menos de 1 año", "1 a 2 años", "2 a 5 años", "5 o más"))
      ),
      h4("Filtro para Mapa"),
      numericInput("año_desde", "Año Desde:", min(datos_mapa$AÑO)),
      numericInput("año_hasta", "Año Hasta:", max(datos_mapa$AÑO))
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
  datos_filtrados <- reactive({
    datos_filtrados <- datos %>%
      filter(ORDEN == input$orden) %>%
      filter(FAMILIA == input$familia) %>%
      filter(ESPECIE == input$especie)
    
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
      filter(AÑO >= input$año_desde & AÑO <= input$año_hasta)
  })
  
  # Actualizar selección de familias y especies según orden seleccionado para el histograma
  observeEvent(input$orden, {
    updateSelectInput(session, "familia", choices = unique(datos$FAMILIA[datos$ORDEN == input$orden]))
    updateSelectInput(session, "especie", choices = NULL)
  })
  
  observeEvent(input$familia, {
    updateSelectInput(session, "especie", choices = unique(datos$ESPECIE[datos$FAMILIA == input$familia]))
  })
  
  # Crear histograma
  output$histograma <- renderPlot({
    ggplot(datos_filtrados(), aes(x = tiempo_diferencia, y = ESPECIE, fill = METAL)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Diferencia de tiempo entre anillamiento y control",
           x = "Días",
           y = "Especie") +
      theme_minimal() +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed")
  })
  
  # Crear el mapa
  output$mapa <- renderLeaflet({
    leaflet(datos_filtrados_mapa()) %>%
      addTiles() %>%
      addMarkers(~LATITUD, ~LONGITUD, popup = ~LOCALIDAD)
  })
  
  # Crear otro gráfico
  output$otro_grafico <- renderPlot({
    # Código para otro gráfico aquí
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
