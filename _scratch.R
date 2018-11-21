fluidRow(
  column(width = 12, class="col-md-6 col-md-offset-1", 
         selectInput(ns("scenario"), label=NULL, choices = scenario_choices, 
                     width = "100%")
  ),
  column(width = 12, class="col-md-3", 
         selectInput(ns("consequence"), label=NULL, choices = consequence_choices, 
                     width = "100%")),
  # column(width = 2, class="col-md-2", 
  #        actionButton(ns("consequence_help"), label="", icon = icon("question"), 
  #                     style="border-radius:10px", class="btn-primary")),
  # column(width = 12, class="col-md-2 col-md-offset-5", 
  #        selectInput(ns("consequence"), label=NULL, choices = consequence_choices, 
  #                    width = "100%")
  # ), 
  column(width = 12, class="col-md-6 col-md-offset-3",
         uiOutput(ns("consequence_results_radio_ui"))
  ), 
  column(width = 12, class="col-md-10 col-md-offset-1", 
         plotlyOutput(ns("consequence_plot"))
  ),
  tags$br(),
  tags$div(
    style="margin-top:5px;",
    column(width = 12, class="col-md-4 col-md-offset-2", 
           tags$form(class="well")
    ),
    column(width = 12, class="col-md-4", 
           tags$form(class="well")
    ))
)



consequence_server <- function(input, output, session) {
  ns <- session$ns
  
  base_data <- reactive({
    switch (input$scenario,
            "1" = 
    )
  })
  
  observeEvent(input$consequence_help, {
    showModal(ui = modalDialog(
      title="Help", 
      easyClose = TRUE, 
      size="l"
    ))
  })
  
  output$consequence_results_radio_ui <- renderUI({
    req(input$consequence)
    radioGroupButtons(ns("consequence_result"), 
                      label = NULL, 
                      choices = consequence_result_choices(), 
                      justified = TRUE, 
                      status="primary", 
                      checkIcon = list(yes = icon("ok", 
                                                  lib = "glyphicon")))
    
  })
  
  consequence_result_choices <- reactive({
    switch (input$consequence,
            "Water Availability" = c("Mean Exports (SOD)", "CV Exports (SOD)", 
                                     "Mean Deliveries (NOD)", "CV Deliveries (NOD)"), 
            "Water Quality" = c("Salinity", "Temperature", "Contaminants"),
            "Delta Smelt" = c("Smelt abundance"),
            "Chinook Salmon" = c("Fall abundance", "Winter abundance", "Spring abundance")
    )
  })
  
  output$consequence_plot <- renderPlotly({
    mtcars %>% 
      plot_ly(x=~mpg, y=~disp, type='scatter', mode='markers')
  })
  
}