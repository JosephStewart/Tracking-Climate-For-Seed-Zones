library(leaflet)


leaflet("map") %>%
  addPolygons(data = seed_zones_el, color = "green", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = .5, fillColor = "green",
              popup=~label, label= ~label,
              highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE))


seed_zones_el = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/SeedZoneClimateTracker/lib/seedzone_elev_400m_simp.RDS")
