# seed zone bcm climate tracker ####
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(ggplot2)
library(plotly)


data_long_mat = readRDS("lib/data_long_mat.RDS")
data_long_map = readRDS("lib/data_long_map.RDS")
data_long_aet = readRDS("lib/data_long_aet.RDS")
data_long_cwd = readRDS("lib/data_long_cwd.RDS")

seed_zones = readRDS("lib/seed zones disolve wgs84.RDS") # .03 seconds



server <- function(input, output) {
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedShape = NULL)
  
  
  output$map <- renderLeaflet({
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
  
  # output$plot=renderPlot({
  #   sz = data_of_click$clickedShape$id
  #   if(is.null(sz)) sz = 526 # set default seed zone
  #   str(data_long)
  #   unique(data_long$period)
  #   
  #   time_periods = c("1981-2010","2010-2039 HDHE", "2011-2040 HDHE" ,"2040-2069 HDHE")[c(input$twentyten, input$twentythirtynine,  input$twentysixtynine,   input$twentyninentynine)]
  #   
  #   # time_periods =  c("1981-2010","2010-2039 HDHE")
  #   
  #   d = data_long[data_long$period %in% time_periods,]
  #   # sz = "526"
  #   d = d[d$sz == sz,]
  #   str(d)
  #   ggplot(aes(y = mat, x = el_bnd, fill = period), data = d) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(paste("Seed Zone", sz)) + xlab("elevational band") + ylab("mean anual temperature [°C]")  + 
  #     theme(axis.title = element_text(size = rel(1.5))) +
  #     theme(axis.text = element_text(size = rel(1.3))) + 
  #     theme(legend.text = element_text(size = rel(1.3))) +
  #     theme(legend.title = element_text(size = rel(1.3))) 
  # })
  
  output$plot=renderPlotly({
    # climate var
    if(input$var == "Temperature"){
      data_long = data_long_mat
      plot_title = "mean annual temperature [°C]"
    }
    if(input$var == "Precipitation"){
      data_long = data_long_map
      plot_title = "mean annual precipitation [mm]"
    }
    if(input$var == "Actual Evapotranspiration"){
      data_long = data_long_aet
      plot_title = "mean annual AET [mm]"
    }
    if(input$var == "Climate Water Defecit"){
      data_long = data_long_cwd
      plot_title = "mean annual CWD [mm]"
    }

    
    # time period
    # str(data_long)
    # unique(data_long$period)
    hist_time_periods = c("1921-1950", "1951-1980", "1981-2010", "1961-1970", "2009-2018")[c(input$t1921.1950, input$t1951.1980, input$t1981.2010, input$t1961.1970, input$t2009.2018)]
    
    fut_time_periods = c("2010-2039", "2040-2069", "2070-2099")[c(input$t2010.2039, input$t2040.2069, input$t2070.2099)]
    
    scenario = c("HDHE","HDLE", "WWHE", "WWLE") [match(input$scenario , c("Hot and Dry, High Emission (HDHE)", "Hot and Dry, Low Emission (HDLE)", "Warm and Wet, High Emission (WWHE)", "Warm and Wet, Low Emission (WWLE)" ) )]
      
    fut_time_periods = paste(fut_time_periods, scenario)
    time_periods =  c(hist_time_periods, fut_time_periods)
    # time_periods =  c("1981-2010","2010-2039 HDHE")
    d = data_long[data_long$period %in% time_periods,]
    
    
    # seed zone
    sz = data_of_click$clickedShape$id
    if(is.null(sz)) sz = 526 # set default seed zone
    d = d[d$sz == sz,]
    str(d)
    plot_ly(d, x = ~el_bnd, y = ~mat, color = ~period, type = "box") %>%
      layout(boxmode = "group", title = paste("Seed Zone", sz), xaxis = list(title = "elevation"), yaxis = list(title = plot_title))
  })
  
  
  
  
}






ui <- fluidPage(
  h3("Tracking historical and future climate change for the seed zones of California"),
  p("Seed zones have been in use in California since the 1940s. The currently used seed zone map was published in 1970 (Buck et al. 1970). Currently seeds are transferred within within seed zones and 500 foot elevation bands. Given little other guidance, and a concern for changing climate conditions, land managers have been selecting seed lots from 500 feet lower elevation within a seed zone for reforestation projects, with the hope that the seeds from what is assumed warmer climate will be better adapted to the current and future climate of the higher elevation planting site. How well does this approach match changing climate conditions?"),
  fluidRow(
    plotlyOutput("plot", height="600px")
  )
  ,
  fluidRow(br(),
    column(5,
           h5("Select a seed zone:"),
           leafletOutput("map", height="460px")
    ),
    column(2,
           h5("Select time periods:"),
           h5(" Historical"),
           checkboxInput("t1921.1950", "1921-1950", F),
           checkboxInput("t1951.1980", "1951-1980", F),
           checkboxInput("t1981.2010", "1981-2010", F),
           checkboxInput("t1961.1970", "1961-1970", TRUE),
           checkboxInput("t2009.2018", "2009-2018", TRUE),
           h5(" Future"),
           checkboxInput("t2010.2039", "2010-2039", FALSE),
           checkboxInput("t2040.2069", "2040-2069", FALSE),
           checkboxInput("t2070.2099", "2070-2099", FALSE)
    )
    ,
    column(3,
      h5("Select a climate variable:"),
      selectInput("var", label = NULL, choices = c("Temperature", "Precipitation", "Actual Evapotranspiration", "Climate Water Defecit")),
      h5("Select a future climate scenario:"),
      selectInput("scenario", label = NULL, choices = c("Hot and Dry, High Emission (HDHE)",
                                                        "Hot and Dry, Low Emission (HDLE)",
                                                        "Warm and Wet, High Emission (WWHE)",
                                                        "Warm and Wet, Low Emission (WWLE)" ))
    )
    )
  ,
  fluidRow(
    br(),
    p("Information on climate scenarios: Hot and Dry, High Emission (HDHE) = MIROC-ESM RCP8.5; Hot and Dry, Low Emission (HDLE) = MIROC-ESM, RCP4.5; Warm and Wet, High Emission (WWHE) = CNRM-ESM, RCP8.5; Warm and Wet, Low Emission (WWLE) = CNRM-ESM, RCP4.5" )
  )
)
  
  





  # fluidRow(column(4,
  #                 h5("Select a seed zone:"),
  #                 leafletOutput("map", height="460px"),
  #                 br(),
  #                 h5("Select a climate variable:"),
  #                 selectInput("var", label = NULL, choices = c("Temperature", "Precipitation", "Actual Evapotranspiration", "Climate Water Defecit")),
  #                 h5("Select a future climate scenario:"),
  #                 selectInput("scenario", label = NULL, choices = c("Hot and Dry, High Emission (HDHE)", 
  #                                                                   "Hot and Dry, Low Emission (HDLE)", 
  #                                                                   "Warm and Wet, High Emission (WWHE)", 
  #                                                                   "Warm and Wet, Low Emission (WWLE)" )),
  #                 p("Hot and Dry, High Emission (HDHE) = MIROC-ESM RCP8.5;
  #          Hot and Dry, Low Emission (HDLE) = MIROC-ESM, RCP4.5; Warm and Wet, High Emission (WWHE) = CNRM-ESM, RCP8.5; Warm and Wet, Low Emission (WWLE) = CNRM-ESM, RCP4.5" )
  # ),
  # column(8,
  #        # plotOutput("plot", height="600px"), 
  #        plotlyOutput("plot", height="600px"), 
  #        fluidRow(h5("Select time Periods:"),
  #                 column(3,
  #                        h5("Historical:"),
  #                        checkboxInput("t1921.1950", "1921-1950", F),
  #                        checkboxInput("t1951.1980", "1951-1980", F),
  #                        checkboxInput("t1981.2010", "1981-2010", F),
  #                        checkboxInput("t1961.1970", "1961-1970", TRUE),
  #                        checkboxInput("t2009.2018", "2009-2018", TRUE)
  #                 ),
  #                 column(3,
  #                        h5("Future:"),
  #                        checkboxInput("t2010.2039", "2010-2039", FALSE),
  #                        checkboxInput("t2040.2069", "2040-2069", FALSE),
  #                        checkboxInput("t2070.2099", "2070-2099", FALSE)
  #                 )
  #        )
  # ))













shinyApp(ui = ui, server = server)
