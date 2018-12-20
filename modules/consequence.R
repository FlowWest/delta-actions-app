consequence_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 3,
      HTML('<div class="alert alert-dismissible alert-warning">
  <button type="button" class="close" data-dismiss="alert">&times;</button>
  <h4 class="alert-heading">Hello User!</h4>
  <p class="mb-0">This application is currently under construction.<a href="#" class="alert-link"></a></p>
             </div>'),
      htmlTemplate("templates/consequence-template.html", 
                   ui_variable_select_action_x2 =  
                     actionButton(ns("consequence_x2"), 
                                  label="Show details", class="show-details"), 
                   ui_variable_select_action_fish_fence=
                     actionButton(ns("consequence_fish_fence"), 
                                  label="Show details", class="show-details"),
                   ui_variable_select_action_yolo_bypass=
                     actionButton(ns("consequence__yolo_bypass"), 
                                  label="Show details", class="show-details")
      )), 
    mainPanel = mainPanel(
      width = 9,
      column(width=3, 
             selectInput(ns("dummy"), label=NULL, choices = consequence_choices)), 
      column(width = 9, 
             uiOutput(ns("consequence_results_radio_ui"))),
      column(width = 12, 
             plotlyOutput(ns("consequence_plot")))
    )
  )}

consequence_server <- function(input, output, session) {
  ns <- session$ns
  
  parse_var_options <- function(x) {
    map_chr(x, ~str_extract(., "water_availability|water_quality|delta_smelt|chinook_salmon"))
  }
  
  action_selected <- eventReactive({
    input$consequence_x2
    input$consequence_fish_fence 
    input$consequence_yolo_bypass
  }, {
    
  })
  
  consequence_result_choices <- reactive({
    x <- parse_var_options(input$consequence_x2)
    switch (x,
            "water_availability" = c("Mean Exports (SOD)", "CV Exports (SOD)", 
                                     "Mean Deliveries (NOD)", "CV Deliveries (NOD)"), 
            "water_quality" = c("Salinity", "Temperature", "Contaminants"),
            "delta_smelt" = c("Smelt abundance"),
            "chinook_salmon" = c("Fall abundance", "Winter abundance", "Spring abudance")
    )
  })
  
  output$consequence_results_radio_ui <- renderUI({
    req(input$consequence_x2)
    radioGroupButtons(ns("consequence_result"), 
                      label = NULL, 
                      choices = consequence_result_choices(), 
                      justified = TRUE, 
                      status="primary", 
                      checkIcon = list(yes = icon("ok", 
                                                  lib = "glyphicon")))
    
  })
  
  output$consequence_plot <- renderPlotly({
    mtcars %>% 
      plot_ly(x=~wt, y=~carb, type='scatter', mode='markers')
  })
  
}