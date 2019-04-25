library(shiny);library(raster);library(rgdal);library(leaflet)



print("lib/mat.tif")
file.exists("lib/mat.tif")
mat = raster("lib/mat.tif")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Target MAT"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("target",
                     "Target MAT:",
                     min = -5,
                     max = 20,
                     value = 8,
                     step = .1),
         numericInput("sd", 
                      "Std Dev:", 
                      .6, 
                      min = .01, 
                      max = 2, 
                      step = .05),
         submitButton("Update View", icon("refresh"))
      ),
      
      mainPanel(
         # plotOutput("distPlot")
        leafletOutput("map")
      )
   )
)

# Define server logic required to draw map
server <- function(input, output) {
   suit = reactive(calc(mat, function(x) dnorm(x, input$target, input$sd)))
   # output$distPlot <- renderPlot({
   #   plot(suit())
   # })
   output$map <- renderLeaflet({
     leaflet("map") %>% addTiles() %>%
        fitBounds(-124.4125, 32.5375, -114.1292,  42.0125)
   })
   
   observe({
     leafletProxy("map") %>%
       addRasterImage(suit(),  opacity = 0.5)
       
   })
   
   
}



# Run the application 
shinyApp(ui = ui, server = server)







