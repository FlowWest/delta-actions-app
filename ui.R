shinyUI(navbarPage(
  title = "Delta SDM", inverse = TRUE,
  header = tags$head(tags$style(
    type = 'text/css',
    'form.well { max-height: 800px; overflow-y: auto; }'
  ), 
  includeCSS("styles.css")),
  id = "deltaapp",
  theme = shinythemes::shinytheme("yeti"),
  # shinythemes::themeSelector(),
  tabPanel("Home", value = "home", home_ui("app")),
  tabPanel("Consequence Table",value = "consequence_table", 
           consequence_ui(("app"))), 
  tabPanel("Chinook Routing", value = "chinook_routing", chinook_ui("app")), 
  tabPanel("Smelt"), 
  tabPanel("Help")
))
