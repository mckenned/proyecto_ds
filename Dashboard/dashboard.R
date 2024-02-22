# Cargar los paquetes necesarios
library(shiny)
library(dplyr)
library(ggplot2)

# Cargar los datos desde el archivo CSV
datos <- read.csv("../data/controles_espana.csv", header = TRUE, sep = ";")

# Calcular la fecha de anillamiento utilizando día, mes y año
datos$FECHA_ANILLAMIENTO <- as.Date(paste(datos$DIA, datos$MES, datos$AÑO, sep = "-"), format = "%d-%m-%Y")

# Calcular la diferencia de tiempo entre anillamiento y control en días
datos <- datos %>%
  group_by(METAL) %>%
  mutate(DIFERENCIA_TIEMPO = as.numeric(difftime(max(FECHA_ANILLAMIENTO), min(FECHA_ANILLAMIENTO), units = "days"))) %>%
  ungroup()


datos <- datos %>% filter(MODO == "C")

# Obtener el máximo valor de diferencia de tiempo
max_diferencia_tiempo <- max(datos$DIFERENCIA_TIEMPO)

# Definir la UI del dashboard
ui <- fluidPage(
  titlePanel("Anillamiento y Control de Aves"),
  sidebarLayout(
    sidebarPanel(
      selectInput("especie", "Especie:",
                  choices = unique(datos$ESPECIE)),
      numericInput("tiempo_min", "Diferencia de tiempo mínimo (días):",
                   min = 0, max = max_diferencia_tiempo, value = 0),
      numericInput("tiempo_max", "Diferencia de tiempo máximo (días):",
                   min = 0, max = max_diferencia_tiempo, value = max_diferencia_tiempo),
      width = 3
    ),
    mainPanel(
      plotOutput("histograma")
    )
  )
)

# Definir el servidor del dashboard
server <- function(input, output) {
  output$histograma <- renderPlot({
    datos_filtrados <- datos %>%
      filter(ESPECIE == input$especie,
             DIFERENCIA_TIEMPO >= input$tiempo_min & DIFERENCIA_TIEMPO <= input$tiempo_max)
    
    ggplot(datos_filtrados, aes(x = DIFERENCIA_TIEMPO)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      annotate("text", x = datos_filtrados$DIFERENCIA_TIEMPO, y = 0, label = datos_filtrados$DIFERENCIA_TIEMPO, vjust = -0.5, size = 3) +
      labs(title = "Diferencia de tiempo entre Anillamiento y Control",
           x = "Días de diferencia",
           y = "Frecuencia") +
      theme_minimal()
  })
}

# Ejecutar la aplicación shiny
shinyApp(ui = ui, server = server)
