shinyUI(navbarPage(
  title = "Delta SDM", inverse = TRUE,
  header = tags$head(tags$style(
    type = 'text/css',
    'form.well { max-height: 800px; overflow-y: auto; }'
  ), 
  includeCSS("styles.css")),
  theme = shinythemes::shinytheme("yeti"),
  # shinythemes::themeSelector(),
  tabPanel("Home"),
  tabPanel("Consequence Table", 
           consequence_ui(("app"))), 
  tabPanel("Chinook", chinook_ui("app")), 
  tabPanel("Smelt"), 
  tabPanel("Help")
))
