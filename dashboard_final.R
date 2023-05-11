# Ahora vamos a hacer una gráfica que seleccionando un circuito
# regrese un barplot con los puntos obtenidos en dicho circuito por cada equipo
setwd("/Users/mariano/Documents/itam/visualizacion/Dashboard_viz")

# lista de librerías a utilizar
librerias <- c("shiny", "plotly", "shinydashboard", "leaflet", "dplyr", "sf", "DT", "ggstream")

# verificar si cada librería está instalada, e instalarla en caso contrario
for (lib in librerias) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

library(shiny)
library(plotly)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(sf)
library(DT)
library(ggstream)

###### FUNCIONES Y LECTURA DE DATOS
circuits <- read.csv('./datos/circuits.csv')
historico <- read.csv('./datos/points_per_year.csv')
colores_equipo <- read.csv('datos/colores_equipos.csv')
datos_stack <- read.csv("datos/data_for_streamgraph.csv")
violin_data <- read.csv("datos/data_for_violin.csv")

encontrar_mejor_equipo <- function(anio_inicio, anio_fin, n_top) {
  
  # Filtramos los datos para el rango de años especificado
  datos_filtrados <- filter(historico, year >= anio_inicio & year <= anio_fin)
  
  # Agrupamos los datos por circuitId y team
  # Calculamos suma total de points
  datos_agrupados <- datos_filtrados %>%
    group_by(circuitId, team) %>%
    summarise(total_points = sum(points))
  
  # Seleccionamos solo los renglones correspondientes con n_top
  datos_top <- datos_agrupados %>%
    group_by(circuitId) %>%
    top_n(n_top, total_points)
  
  # Juntamos los datos con los de circuitos para poderlos graficar
  resultado <- datos_top %>%
    merge(circuits, by = "circuitId") %>%
    merge(colores_equipo, by = "team")
  
  resultado$size_aux <- log(resultado$total_points) * 1.7
  
  return(resultado)
}

get_circuit_info <- function(longitud, latitud) {
  if(is.null(longitud)){longitud = filter(circuits, circuitId == 32)$lng}
  if(is.null(latitud)){latitud = filter(circuits, circuitId == 32)$lat}
  circuit_id <- filter(circuits, lng == longitud, lat == latitud)$circuitId
  circuit_name <- filter(circuits, lng == longitud, lat == latitud)$name
  
  return(c(circuit_id, circuit_name))
}

get_plotly_graph <- function(anio_inicio, anio_fin, circuit_id) {
  datos_filtrados <- filter(historico, year >= anio_inicio & year <= anio_fin & circuitId == circuit_id)
  circuit_name <- filter(circuits, circuitId == circuit_id)$name
  
  datos_graph <- datos_filtrados %>%
    group_by(team) %>%
    summarise(total_points = sum(points)) %>%
    merge(colores_equipo, by = "team") %>%
    arrange(desc(total_points))
  
  fig <- plot_ly(
    data = datos_graph,
    y = ~reorder(team, total_points),
    x = ~total_points,
    type = "bar",
    marker = list(color = ~color)
  ) %>%
    layout(
      xaxis = list(title = "Puntos Acumulados"),
      yaxis = list(title = "Equipo"),
      title = circuit_name
    )
  
  return(fig)
}

get_violin_plot <- function(circuit_id, anio_inicio, anio_fin) {
  n_std <- 3
  nombre_circuito <- filter(violin_data, circuitId == circuit_id)$name[1]
  
  # Sacamos los datos del mejor equipo en el circuito seleccionado
  datos_filtrados <- filter(historico, year >= anio_inicio & year <= anio_fin & circuitId == circuit_id)
  circuit_name <- filter(circuits, circuitId == circuit_id)$name
  
  datos_graph <- datos_filtrados %>%
    group_by(team) %>%
    summarise(total_points = sum(points)) %>%
    merge(colores_equipo, by = "team") %>%
    arrange(desc(total_points))
  
  best_team <- datos_graph$team[1]
  
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
  data_others$team <- "Others"
  mean_sec <- mean(data_others$seconds)
  sd_sec <- sd(data_others$seconds)
  data_others <- data_others[data_others$seconds > mean_sec - n_std * sd_sec & data_others$seconds < mean_sec + n_std * sd_sec, ]
  
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


# SHINY APP - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
ui <- dashboardPage( 
  dashboardHeader(title = "F1 Seasons"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficas", tabName = "circuitos"),
      menuItem("Datos", tabName = "datos")
    )
  ), 
  dashboardBody(
    tabItems(
      tabItem(
        "circuitos",
        fluidRow(
          column(3, sliderInput("rango_anios", "Rango años:", min = min(historico$year), max = max(historico$year), value = c(min(historico$year), max(historico$year))))
        ),
        fluidRow(
          column(7, leafletOutput("mymap", height="400px")),
          column(5, plotlyOutput("barras_equipos", height = "400px"))
        ),
        fluidRow(" "),
        fluidRow(
          column(6, plotlyOutput("stacked_graph", height = "400px")),
          column(6, plotOutput("violin", height = "400px"))
        )
      ),
      
      tabItem(
        "datos",
        fluidRow(
          column(1, ""),
          column(10, dataTableOutput("tabla")),
          column(1, "")
        )
        
      )
    )
  )
)

server <- function(input, output) { 

  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  coords <- reactive({
    return(c(input$mymap_marker_click$lng, input$mymap_marker_click$lat))
  })
  
  datos_filtrados <- reactive({
    anio_min <- input$rango_anios[1]
    anio_max <- input$rango_anios[2]
    datos_filtrados <- encontrar_mejor_equipo(anio_min, anio_max, 1)
    # print(filter(datos_filtrados, circuitId == 32)$team)
    return(datos_filtrados)
  })
  
  datos_for_stack_graph <- reactive({
    anio_min <- input$rango_anios[1]
    anio_max <- input$rango_anios[2]
    
    data_aux <- filter(datos_stack, year >= anio_min & year <= anio_max)
    return(data_aux)
  })
  
  anios <- reactive({
    return(c(input$rango_anios[1], input$rango_anios[2]))
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = datos_filtrados(),
        lng = ~lng,
        lat = ~lat,
        radius = ~size_aux,
        color = ~color,
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = paste0(
          "<b>Circuito</b>: ", datos_filtrados()$name, "<br>",
          "<b>Equipo</b>: ", datos_filtrados()$team, "<br>",
          "<b>Puntos</b>: ", datos_filtrados()$total_points
        ),
        clusterOptions = markerClusterOptions()
      )
  })
  
  output$stacked_graph <- renderPlotly({
    g <- ggplot(datos_for_stack_graph(), aes(x = year, y = porcentaje, fill = team)) +
      geom_bar(stat = "identity", width = 0.95) +
      scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD",
                                   "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")) +
      labs(x = "Año", y = "Porcentaje", fill = "Grupo") +
      theme_minimal() +
      theme(legend.position = "top")
    
    ggplotly(g)
  })
  
  # Almacena el clic
  # Se trata de variables internas
  observeEvent(input$mymap_marker_click,{
    data_of_click$clickedMarker <- input$mymap_marker_click
    aux <- input$mymap_marker_click
  })
  
  # Haz un scatterplot o un barplot dependiendo del punto seleccionado
  output$barras_equipos <- renderPlotly({
    coordenadas <- coords()
    circuit_info <- get_circuit_info(coordenadas[1], coordenadas[2])
    
    get_plotly_graph(anio_inicio = anios()[1], anios()[2], circuit_id = circuit_info[1])
  })
  
  # Graficamos el violin
  output$violin <- renderPlot({
    coordenadas <- coords()
    circuit_info <- get_circuit_info(coordenadas[1], coordenadas[2])

    # get_violin_plot(circuit_info[1], anios()[1], anios()[2])
    
    tryCatch(
      {
        # Llamada a la función get_violin_plot
        get_violin_plot(circuit_info[1], anios()[1], anios()[2])
      }, error = function(e) {
        ggplot(datos_stack, aes(x = year, y = porcentaje, fill = team)) +
          geom_stream(alpha = 1, linewidth = 0, color = "black") +
          scale_fill_manual(values = c("Red Bull"="#3671C6","Mercedes"="#6CD3BF","Aston Martin"="#358C75","Ferrari"="#F91536","Williams"="#37BEDD",
                                       "Alpine"="#2293D1","Haas"="#B6BABD","AlphaTauri"="#5E8FAA","McLaren"="#F58020","Alfa Romeo"="#C92D4B")) +
          labs(x = "", y = "Puntos", fill = "Equipo") +
          theme_minimal() +
          theme(legend.position = "top")
      }
    )
    
  })
  
  
  output$tabla <- renderDataTable({
    datos_filtrados()[, c('team', 'name', 'location', 'total_points')]
  })
}

shinyApp(ui, server)
