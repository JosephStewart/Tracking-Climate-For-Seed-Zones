# Reactive dropdown and analogous polygons

library(shiny);library(tidyverse);library(sp);library(leaflet)
library(raster)# for projection function
clim_by_group = readRDS("lib/clim_by_group.RDS")
seed_zones_el = readRDS("lib/seedzone_elev_400m_simp.RDS")
# seedzone = "525"
# el = "8500 — 9000ft"
# per_dest = "2010-2039 HDHE"
# per_source = "1921-1950"




ui = fluidPage(
  actionButton("debug_button", "Mark debug"),
  selectInput('per_source', 'What historical climate period are seeds adapted to?)', c("1921-1950", "1951-1980", "1961-1970" ))
  ,
  selectInput('per_dest', 'What climate should seeds be optimized for?', c("1981-2010","2009-2018","2010-2039 HDHE","2010-2039 HDLE", "2010-2039 WWHE", "2010-2039 WWLE", "2040-2069 HDHE", "2040-2069 HDLE", "2040-2069 WWHE", "2040-2069 WWLE", "2070-2099 HDHE", "2070-2099 HDLE", "2070-2099 WWHE", "2070-2099 WWLE"), selected = "2010-2039 HDHE")
  ,
  selectInput('seedzone', 'In what seed zone will seeds be planted?', unique(clim_by_group$sz), selected = "526")
  ,
  selectInput('el', 'At what elevation will seeds be planted?', c("", as.character(unique(subset(clim_by_group, sz == "526")$el_bnd))))
  ,
  # uiOutput('el.select')
  # ,
  # textOutput("text_output"),
  br()
  ,
  h5("The historical climates of the following units are analogous to the target climate for the planting unit:")
  ,
  leafletOutput("map", height="450px", width = "450px")
  ,
  tableOutput("table")
 
)

server = function(input, output, session){
  
  observeEvent(input$debug_button,{
    print("debug button")
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
    el_lev = levels(clim_by_group$el_bnd)
    el_sz = el_lev[el_lev %in% unique(clim_by_group$el_bnd[clim_by_group$sz == sz_clicked])]
    updateSelectInput(session, 'el',  choices = el_sz, selected = el_clicked)
    print("db 1")
  })
  
  # observeEvent(input$seedzone, {
  #   el_lev = levels(clim_by_group$el_bnd)
  #   el_sz = el_lev[el_lev %in% unique(clim_by_group$el_bnd[clim_by_group$sz == input$seedzone])]
  #   el_sz = c("", as.character(el_sz))
  #   print(paste("el not in seedzone:", !sum(as.character(input$el) %in% el_sz)))
  #   if(!sum(as.character(input$el) %in% el_sz)){
  #     updateSelectInput(session, 'el',  choices = el_sz)
  #   }
  # })
  
  
  # output$el.select = renderUI({
  #   seedzone = input$seedzone
  #   el_lev = levels(clim_by_group$el_bnd)
  #   el_sz = el_lev[el_lev %in% unique(clim_by_group$el_bnd[clim_by_group$sz == seedzone])]
  #   els = c("", as.character(el_sz))
  #   selectInput('el', 'At what elevation will seeds be planted?', els)
  # })
  
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
  
  
  matches = reactive({
    print("db matches 1")
    seedzone = input$seedzone
    per_dest = input$per_dest
    per_source = input$per_source
    req(input$el)
    el = input$el

    clim_target = subset(clim_by_group, period == per_dest & el_bnd == el & sz == seedzone)[,c("scaled_mat","scaled_log_map")]
    max_dist = .25
    clim_by_group_for_source_period <- subset(clim_by_group, period == per_source)
    mat_dif = clim_by_group_for_source_period$scaled_mat - clim_target$scaled_mat
    map_dif = clim_by_group_for_source_period$scaled_log_map - clim_target$scaled_log_map

    multi_var_dist = sqrt(mat_dif^2 + map_dif^2)

    matches = clim_by_group_for_source_period[multi_var_dist < max_dist, c(    "sz", "el_bnd") ]
    names(matches)[1] = "SEED_ZONE"
    # matches$match = T
    print("db matches 2")
    matches})
  
  
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()  %>% setView(-119.509444, 37.229722,  zoom = 5) #%>%
      # addPolygons(data = sz_matches(), color = "#444444", weight = 1, smoothFactor = 0.5,
      #             opacity = 1.0, fillOpacity = 0,
      #             popup=~label, label= ~label,
      #             highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE))
  })
  
  
  # per_source.react = reactive(iput$)
  
  # el.react = reactive(input$el)
  # 
  sz_matches = reactive({
    print("sz_matches 1")
    matches = matches()
    matches$match = T
    matches2 = left_join(seed_zones_el@data, matches,  by = c("el_bnd", "SEED_ZONE"), copy = T )
    matches2$match[is.na(matches2$match)] <- F
    sz_matches = seed_zones_el[matches2$match,]
    sz_matches$label = paste0(sz_matches$SEED_ZONE, ", ", sz_matches$el_bnd)
    print("sz_matches 2")
    sz_matches
  })

  
  target_unit = reactive({    # target unit polygon for map
    print("target_unit 1")
    seedzone = input$seedzone
    per_dest = input$per_dest
    per_source = input$per_source
    req(input$el)
    el = input$el
    target_unit = seed_zones_el[seed_zones_el$el_bnd == el & seed_zones_el$SEED_ZONE == seedzone,]
    target_unit$label = paste0("planting unit, ", target_unit$SEED_ZONE, ", ", target_unit$el_bnd)
    print("target_unit 2")
    target_unit
  })
  
  
  
  
  observe({
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(data = sz_matches(), color = "green", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = .5, fillColor = "green",
                    popup=~label, label= ~label,
                    highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)) %>%
      addPolygons(data = target_unit(), color = "blue", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = .5, fillColor = "blue",
                  popup=~label, label= ~label, 
                  highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE))

  })
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

