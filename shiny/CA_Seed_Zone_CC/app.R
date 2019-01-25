library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(ggplot2)

# zone_mat_df = read.csv("lib/seedzone_elev_band_mat_extract_10yma.csv", stringsAsFactors = F)
zone_mat_df = read.csv("lib/seedzone_elev_band_mat_extract_hist_fut.csv", stringsAsFactors = F)
zone_mat_df$year = factor(zone_mat_df$year)
ceiling_500 = function(x) ceiling(x/500)*500
ceiling_500(c(1,2))
zone_mat_df$el_bnd_mx = ceiling_500(as.numeric(as.character(zone_mat_df$el_bnd_mx)))
zone_mat_df$el_bnd = paste0(zone_mat_df$el_bnd_mx -500, " — ", zone_mat_df$el_bnd_mx, "ft")
e = seq(0, 14000, 500)
zone_mat_df$el_bnd = factor(zone_mat_df$el_bnd, levels = paste0(e -500, " — ", e, "ft"))
table(zone_mat_df$period)

zone_mat_df[zone_mat_df$period == "2071-2100", ]


#test
# print("lib/seed zones disolve wgs84.RDS")
# file.exists("lib/seed zones disolve wgs84.RDS")
# seed_zones = readOGR("lib", "seed zones disolve wgs84") # 1.8 seconds
# saveRDS(seed_zones, "lib/seed zones disolve wgs84.RDS")
seed_zones = readRDS("lib/seed zones disolve wgs84.RDS") # .03 seconds



server <- function(input, output) {
  
  # create a reactive value that will store the click position
  # data_of_click <- reactiveValues(clickedMarker=NULL)
  data_of_click <- reactiveValues(clickedShape = NULL)
  
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    # leaflet() %>% 
    #   setView(lng=131 , lat =-25, zoom=4) %>%
    #   addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
    #   addCircleMarkers(data=data, ~x , ~y, layerId=~id, popup=~id, radius=8 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8)
    leaflet() %>% addTiles() %>%
      addPolygons(data = seed_zones, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0,
                  popup=~SEED_ZONE, layerId= ~SEED_ZONE,
                  highlightOptions = highlightOptions(color = "black", weight = 3,
                                                      bringToFront = TRUE))
    
    
  })
  
  # store the click
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    data_of_click$clickedShape <- input$map_shape_click
  })
  
  output$plot=renderPlot({
    sz = data_of_click$clickedShape$id
    if(is.null(sz)) sz = 526 # set default seed zone
    
    
    time_periods = c("1961-1970","2008-2017", "2011-2040" ,"2041-2070","2071-2100")[c(input$nineteenseventy, input$twentyseventeen,  input$twentyfourty,   input$twentyseventy, input$twentyonehundred)]
    data = zone_mat_df[zone_mat_df$period %in% time_periods,]
    
    
    ggplot(aes(y = mat, x = el_bnd, fill = period), data = subset(data, SEED_ZONE == as.numeric(sz))) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(paste("Seed Zone", sz)) + xlab("elevational band") + ylab("mean anual temperature [°C]")  + 
      theme(axis.title = element_text(size = rel(1.5))) +
      theme(axis.text = element_text(size = rel(1.3))) + 
      theme(legend.text = element_text(size = rel(1.3))) +
      theme(legend.title = element_text(size = rel(1.3))) 

  })
}


# ui <- fluidPage(
#   br(),
#   column(5,leafletOutput("map", height="600px")),
#   column(7,plotOutput("plot", height="600px"), 
#          checkboxInput("nineteenseventy", "1961-1970", TRUE),
#          checkboxInput("twentyseventeen", "2008-2017", TRUE),
#          checkboxInput("twentyfourty", "2011-2040", FALSE),
#          checkboxInput("twentyseventy", "2041-2070", FALSE),
#          checkboxInput("twentyonehundred", "2071-2100", FALSE)
#          ),
#   br()
# )


ui <- fluidPage(
  h3("Tracking historical and future climate change for the seed zones of California"),
  column(5,leafletOutput("map", height="600px"),
         h5("Select a Seed Zone")),
  column(7,plotOutput("plot", height="600px"), 
         fluidRow(
           column(3,
             h5("Select Time Periods"),
             checkboxInput("nineteenseventy", "1961-1970", TRUE),
             checkboxInput("twentyseventeen", "2008-2017", TRUE)
             ),
           column(3,
                  br(),
                  checkboxInput("twentyfourty", "2011-2040", FALSE),
                  checkboxInput("twentyseventy", "2041-2070", FALSE)
                  ),
           column(3,
                  br(),
                  checkboxInput("twentyonehundred", "2071-2100", FALSE)
           )
           )
         )
  ,
  br(),
  p("Seed zones have been in use in California since the 1940s. The currently used seed zone map was published in 1970 (Buck et al. 1970) and consists of a set of tiered seed zones, from a bioclimatic region (i.e. the Sierra Nevada) to a smaller sub-region (the northern Sierra Nevada) to an individual zone (seed zone 526). Currently seeds are transferred within seed zone, and a 500 foot elevation band (we use the term “unit” here to describe this area). Given little other guidance, and a concern for changing climate conditions, land managers have been selecting seed lots from 500 feet lower elevation within a seed zone for reforestation projects, with the hope that the seeds from what is assumed warmer climate will be better adapted to the current and future climate of the higher elevation planting site. How well does this approach match our changing climate conditions?")
)









shinyApp(ui = ui, server = server)