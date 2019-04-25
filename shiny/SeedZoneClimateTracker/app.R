# duo tool ####

library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(ggplot2)
library(plotly)
library(leaflet.esri)

# load for bcm climate tracker 
data_long_mat = readRDS("lib/data_long_mat.RDS")
data_long_map = readRDS("lib/data_long_map.RDS")
data_long_aet = readRDS("lib/data_long_aet.RDS")
data_long_cwd = readRDS("lib/data_long_cwd.RDS")
seed_zones = readRDS("lib/seed zones disolve wgs84.RDS") # .03 seconds

# load for analog climate matcher
library(shiny);library(tidyverse);library(sp);library(leaflet)
library(raster)# for projection function
clim_by_group = readRDS("lib/clim_by_group.RDS")
seed_zones_el = readRDS("lib/seedzone_elev_400m_simp.RDS")
seed_zones_el$label = paste0(seed_zones_el$SEED_ZONE, ", ", seed_zones_el$el_bnd)



ui = navbarPage("Seed Zone Climate Tracker",
                tabPanel("Analog Climate Finder",
                         h3("Tracking historical and future climate change for the seed zones of California"),
                         p("Current guidelines for seed transfer in California forestry are based on a system of seed zones and 500-ft elevation bands. Given little other guidance, and a concern for changing climate conditions, land managers have been selecting seeds from 500 ft lower elevation within a seed zone for reforestation projects, with the hope that the seeds from what is assumed warmer climate will be better adapted to the current and future climate of the higher elevation planting site. How well does this approach match changing climate conditions?"),
                         fluidRow(
                           column(width = 6,
                                  h3("Target Climate"),
                                  p("What is the target seed zone, elevation band, and climate scenario? (Where will the seeds be planted and what climate period/scenario should they be optimized for?)"),
                                  selectInput('per_dest', "Climate Scenario", c("1981-2010", "2010-2039 ENS HE", "2010-2039 HD HE",  "2010-2039 WW HE",  "2040-2069 ENS HE", "2040-2069 HD HE", "2040-2069 WW HE",  "2070-2099 ENS HE", "2070-2099 HD HE",  "2070-2099 WW HE"), selected = "2010-2039 ENS HE"),
                                  selectInput('seedzone', 'Seed zone', levels(clim_by_group$sz), selected = "526") ,
                                  selectInput('el', 'Elevation', c("", as.character(unique(subset(clim_by_group, sz == "526")$el_bnd))))
                           ),
                           
                           column(width = 6,
                                  h3("Analog Climate"),
                                  p("During what historical period are we searching for units with similar climate conditions? (What climate period are seeds adapted to?)"),
                                  selectInput('per_source', 'Historical Period', c("1901-1930", "1941-1970")),
                                  
                                  # h3("Climate Variables"),
                                  strong("What climate variables are we tracking?"),
                                  checkboxGroupInput("clim_vars", label = NULL, 
                                                     choices = list("Mean Annual Temperature" = "scaled_mat", "Mean Annual Precipitation" = "scaled_log_map", "Temperature Seasonality" = "scaled_td", "Photoperiod" = "scaled_lat"),
                                                     selected = c("scaled_mat", "scaled_log_map", "scaled_td", "scaled_lat")),
                                  sliderInput("max_dist", "Analog Climate Tolerance",
                                              min = 0, max = 1, value = .25, step = .05
                                  )
                           )
                         ),
                         
                         br(),
                         p("Click on the map or use the dropdown menus to select a target seed zone and elevation band. Units with analogous climates (i.e. locations where seeds are likely to be adapted to the target climate) are shown in green and listed below."),
                         fluidRow(
                           column(width = 8, leafletOutput("map", height="600px", width = "100%")),
                           column(width = 4, tableOutput("table"))
                         ),
                         br(),
                         p("Information on climate scenarios: Ensemble, High Emission (ENS HE) = Ensemble RCP8.5; Hot and Dry, High Emission (HD HE) = MIROC-ESM RCP8.5; Hot and Dry, Low Emission (HD LE) = MIROC-ESM, RCP4.5; Warm and Wet, High Emission (WW HE) = CNRM-ESM, RCP8.5; Warm and Wet, Low Emission (WW LE) = CNRM-ESM, RCP4.5" )
                ),
                tabPanel("Climate Tracker",
                         h3("Tracking historical and future climate change for the seed zones of California"),
                         p("Current guidelines for seed transfer in California forestry are based on a system of seed zones and 500-ft elevation bands. Given little other guidance, and a concern for changing climate conditions, land managers have been selecting seeds from 500 ft lower elevation within a seed zone for reforestation projects, with the hope that the seeds from what is assumed warmer climate will be better adapted to the current and future climate of the higher elevation planting site. How well does this approach match changing climate conditions?"),
                         fluidRow(
                           plotlyOutput("plot", height="600px")
                         )
                         ,
                         fluidRow(br(),
                                  column(5,
                                         h5("Select a seed zone:"),
                                         leafletOutput("select_seedzone_map", height="460px")
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
               
)

server <- function(input, output, session) {
  
  
  # server code for climate tracker ####
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedShape = NULL)
  
  
  output$select_seedzone_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addPolygons(data = seed_zones, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0,
                  popup=~SEED_ZONE, layerId= ~SEED_ZONE, label = ~SEED_ZONE, 
                  highlightOptions = highlightOptions(color = "black", weight = 3,
                                                      bringToFront = TRUE))
    
    
  })
  
  # store the click
  observeEvent(input$select_seedzone_map_shape_click, { # update the location selectInput on map clicks
    data_of_click$clickedShape <- input$select_seedzone_map_shape_click
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
    
    cols = c("#70C6AB", "#FC8D63", "#C8D1E6", "#E78AC3", "#ACDA60", "#FFD830", "#E7C79B", "#B8B8B8")[c(input$t1921.1950, input$t1951.1980, input$t1981.2010, input$t1961.1970, input$t2009.2018, input$t2010.2039, input$t2040.2069, input$t2070.2099)]
    
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
    plot_ly(d, x = ~el_bnd, y = ~mat, color = ~period, type = "box", colors = cols) %>%
      layout(boxmode = "group", title = paste("Seed Zone", sz), xaxis = list(title = "elevation"), yaxis = list(title = plot_title))
  })
  
  # server code for climate finder ####
  
  observeEvent(input$debug_button,{
    print("***********debug button*********")
  }
  )
  
  observe({
    req(input$map_click)
    coords = input$map_click
    coords = as.data.frame(coords)
    coordinates (coords) = ~ lng + lat
    projection(coords) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    clicked_unit = coords %over% seed_zones_el
    sz_clicked = as.character(clicked_unit$SEED_ZONE)
    updateSelectInput(session, 'seedzone', selected = sz_clicked)
    el_clicked = as.character(clicked_unit$el_bnd)
    el_sz = unique(seed_zones_el$el_bnd[seed_zones_el$SEED_ZONE == sz_clicked])
    updateSelectInput(session, 'el',  choices = el_sz, selected = el_clicked)
    leafletProxy("map") %>%
      clearShapes()
    print("db 1")
  })
  
  observeEvent(input$seedzone, {
    print("input seedzone 1")
    el_sz = unique(seed_zones_el$el_bnd[seed_zones_el$SEED_ZONE == input$seedzone])
    el_sz = c("", as.character(el_sz))
    print(paste("el not in seedzone 1:", !sum(as.character(input$el) %in% el_sz)))
    updateSelectInput(session, 'el',  choices = el_sz, selected = input$el)
    leafletProxy("map") %>%
      clearShapes()
    # if(sum(as.character(input$el) %in% el_sz)){
    #   updateSelectInput(session, 'el',  choices = el_sz, selected = input$el)
    # } else updateSelectInput(session, 'el',  choices = el_sz, selected = "")
    # print(paste("el not in seedzone 2:", !sum(as.character(input$el) %in% el_sz)))
  })
  
  
  
  target_unit = reactive({    # target unit polygon for map
    print("target_unit 1")
    seedzone = input$seedzone
    per_dest = input$per_dest
    per_source = input$per_source
    req(input$el)
    el = input$el
    print("target_unit 1.1")
    print(paste("input el:", el))
    print(paste("input sz:", seedzone))
    target_unit = seed_zones_el[seed_zones_el$el_bnd == el & seed_zones_el$SEED_ZONE == seedzone,]
    print("target_unit 1.2")
    if(length(target_unit))   target_unit$label = paste0(target_unit$SEED_ZONE, ", ", target_unit$el_bnd)
    print("target_unit 2")
    target_unit
  })
  
  
  # output$text_output = renderText({
  #   el_lev = levels(clim_by_group$el_bnd)
  #   el_sz = el_lev[el_lev %in% unique(clim_by_group$el_bnd[clim_by_group$sz == input$seedzone])]
  #   el_sz = c("", as.character(el_sz))
  #   as.character(sum(as.character(input$el) %in% el_sz))
  #   
  # })
  
  output$table <- renderTable({
    print("table 1")
    matches()
  })
  
  # matching seed zones ####
  matches = reactive({
    # seedzone = "732"
    # per_dest = "2070-2099 HD HE"
    # per_source = "1941-1970"
    # el = "3000 — 3500ft"
    
    
    print("db matches 1")
    print(input$clim_vars)
    
    clim_vars = input$clim_vars
    seedzone = input$seedzone
    per_dest = input$per_dest
    per_source = input$per_source
    req(input$el)
    el = input$el
    max_dist = input$max_dist
    
    
    clim_target = subset(clim_by_group, period == per_dest & el_bnd == el & sz == seedzone)[,c("scaled_mat","scaled_log_map", "scaled_td","scaled_lat")]
    clim_by_group_for_source_period <- subset(clim_by_group, period == per_source)
    mat_dif = clim_by_group_for_source_period$scaled_mat - clim_target$scaled_mat
    map_dif = clim_by_group_for_source_period$scaled_log_map - clim_target$scaled_log_map
    td_dif = clim_by_group_for_source_period$scaled_td - clim_target$scaled_td
    lat_dif = clim_by_group_for_source_period$scaled_lat - clim_target$scaled_lat
    
    
    


    
    multi_var_dist = sqrt(mat_dif^2 * "scaled_mat"%in% clim_vars + 
                            map_dif^2 * "scaled_map"%in% clim_vars + 
                            td_dif^2 * "scaled_td"%in% clim_vars +
                            lat_dif^2 * "scaled_lat"%in% clim_vars)
    
    
    
    
    matches = clim_by_group_for_source_period[multi_var_dist < max_dist, c(    "sz", "el_bnd") ]
    names(matches)[1] = "SEED_ZONE"
    # matches$match = T
    print(matches)
    print("db matches 2")
    matches})
  
  
  

  
  
  # el.react = reactive(input$el)
  # 
  
  # matching spatial polygons for seed zone matches
  sz_matches = reactive({
    print("sz_matches 1")
    matches = matches()
    if(nrow(matches)){
      matches = matches()
      matches$match = T
      matches2 = left_join(seed_zones_el@data, matches,  by = c("el_bnd", "SEED_ZONE"), copy = T )
      matches2$match[is.na(matches2$match)] <- F
      sz_matches = seed_zones_el[matches2$match,]
      # sz_matches = seed_zones_el[F,]
      # if(length(sz_matches)) sz_matches$label = paste0(sz_matches$SEED_ZONE, ", ", sz_matches$el_bnd)
      print("sz_matches 2")
    } else sz_matches = seed_zones_el[F,]
    sz_matches
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()  %>% setView(-119.509444, 37.229722,  zoom = 6) %>% 
      addLegend("topright", 
                colors =c("blue",  "green"),
                labels= c("Target",  "Analog"),
                # title= "",
                opacity = .9)  %>% 
      addProviderTiles("Esri", group="Relief") %>% 
      addEsriBasemapLayer(key="Imagery", autoLabels=TRUE, group="Imagery") %>% 
      addLayersControl(baseGroups = c( "Relief", "Imagery"))
    
  })
  
  observe({
    if(nrow(target_unit()) & nrow(sz_matches())){
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data = sz_matches(), color = "green", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = .5, fillColor = "green",
                    popup=~label, label= ~label,
                    highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>%
        addPolygons(data = target_unit(), color = "blue", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = .5, fillColor = "blue",
                    popup=~label, label= ~label,
                    highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) # %>% 
        # addProviderTiles("Esri", group="Relief") %>% 
        # addEsriBasemapLayer(key="Imagery", autoLabels=TRUE, group="Imagery") %>% 
        # addLayersControl(baseGroups = c( "Relief", "Imagery"))
      
    }
    
    if(length(target_unit()) & !length(sz_matches())){
      leafletProxy("map") %>%
        addTiles("Terrain") %>%
        clearShapes() %>%
        addPolygons(data = target_unit(), color = "blue", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = .5, fillColor = "blue",
                    popup=~label, label= ~label,
                    highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) # %>% 
        # addProviderTiles("Esri", group="Relief") %>% 
        # addEsriBasemapLayer(key="Imagery", autoLabels=TRUE, group="Imagery") %>% 
        # addLayersControl(baseGroups = c( "Relief", "Imagery"))
    }
    
  })
  
}


shinyApp(ui = ui, server = server)
