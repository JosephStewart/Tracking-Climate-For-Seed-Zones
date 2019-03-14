#
# Reactive dropdown

library(shiny)
data_long_mat = readRDS("lib/data_long_mat.RDS")
# seedzone = "091"
# el = "2500 — 3000ft"
# per = "1981-2010"
# unique(data_long_mat$el_bnd[data_long_mat$sz == seedzone])


# Define UI for application that draws a histogram
ui = bootstrapPage(
  selectInput('seedzone', 'Choose Seedzone', unique(data_long_mat$sz))
  ,
  uiOutput('seedzoneselect')
  ,
  textOutput("text_output")
  
)

server = function(input, output){
  output$seedzoneselect = renderUI({
    seedzone = input$seedzone

    el_lev = levels(data_long_mat$el_bnd)
    el_sz = el_lev[el_lev %in% unique(data_long_mat$el_bnd[data_long_mat$sz == seedzone])]
    els = c("", as.character(el_sz))
    selectInput('el', 'Elevation', els)
  })
  
  output$text_output = renderText({
    seedzone = input$seedzone
    el = input$el
    # "dd"
    mean(subset(data_long_mat, el_bnd == el & period == per & sz == seedzone)$mat)
    
    
    
    
    
    # head(data_long_mat)
    # as.character(data_long_mat$sz == seedzone)[1]
    # paste(c("",as.character(unique(data_long_mat$el_bnd[data_long_mat$sz == seedzone]))), collapse = ",")
    # paste(data_long_mat$sz == seedzone, collapse = ",")
    # paste(seedzone, el)
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)



# library(dplyr)
# 
# mat_target = subset(data_long_mat, el_bnd == el & period == per & sz == seedzone)$mat
# 
# which_match <- subset(data_long_mat, period == per) %>% 
#   group_by(sz, el_bnd) %>%
#   summarize(mean = mean(mat, na.rm = TRUE), median = median(mat, na.rm = TRUE), 
#             p = wilcox.test(mat_target, mat)$p.value
#             )
# 
# table(which_match$p > .8)
# 
# which_match[which_match$p > .8,]
# 
# 
# 
# wilcox.test(subset(data_long_mat, el_bnd == el & period == per & sz == seedzone)$mat, 
#             subset(data_long_mat, el_bnd == el & period == per & sz == "092")$mat)$p.value
# 
# 



