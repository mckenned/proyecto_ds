# Instala y carga el paquete shiny
if(!require(shiny)) install.packages("shiny")
library(shiny)

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Mi Dashboard Interactivo"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Número de bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define la lógica del servidor
server <- function(input, output) {
  # Lee el archivo CSV
  datos <- read.csv("../data/controles_espana.csv")
  # Cambia los valores 'A' por 0 y 'C' por 1
  datos$MODO <- ifelse(datos$MODO == 'A', 0, 
                                     ifelse(datos$MODO == 'C', 1, NA))
  datos$MODO <- na.omit(datos$MODO)
  datos$MODO <- as.numeric(as.character(datos$MODO)) # Reemplaza "columna_de_interes" con el nombre de tu columna
  
  str(datos$MODO)
  output$distPlot <- renderPlot({
    x    <- datos$MODO # Reemplaza esto con el nombre de la columna de interés
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Etiqueta del eje x", # Reemplaza esto con la etiqueta del eje x
         main = "Título del gráfico") # Reemplaza esto con el título del gráfico
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
