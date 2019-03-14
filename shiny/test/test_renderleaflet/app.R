library(shiny)

ui <- fluidPage(
  leafletOutput("map1"),
  textOutput("text_output")
)

map <- leaflet() %>% addCircleMarkers(
  lng = runif(10),
  lat = runif(10),
  layerId = paste0("marker", 1:10))


server <- function(input, output, session) {
  output$map1 <- renderLeaflet(map)
  
  observeEvent(input$map1_marker_click, {
    leafletProxy("map1", session) %>%
      removeMarker(input$map1_marker_click$id) %>% 
      addCircleMarkers(
        lng = runif(10),
        lat = runif(10),
        layerId = paste0(input$map1_marker_click$id))
  })
  
  observeEvent(input$map1_click,{
    leafletProxy("map1", session) %>%
      addCircleMarkers(
        lng = input$map1_click$lng,
        lat = input$map1_click$lat)
  })
  
  
  output$text_output = renderText({
    input$map1_click$lat
    
  })
  
  
}

app <- shinyApp(ui, server)
# }
# NOT RUN {
if (interactive()) app
# }
# NOT RUN {
# }

# ?removeMarker
