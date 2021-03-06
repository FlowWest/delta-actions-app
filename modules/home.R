home_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class="jumbotron", 
      tags$h1(class="display-3","Delta SDM Tools"), 
      tags$p(class="lead", tags$em("Hello User! This application is still under construction.")),
      tags$p(class="lead", "This application compiles a set of tools for interacting 
           and visualizing model input and output for the SDM models."), 
      tags$hr()
    ),  
    tags$div(class="col-md-4", 
             tags$h4("Chinook Routing Tool"), 
             tags$hr(), 
             tags$p("This tool allows users to visualize Chinook routing in the Delta.
                    Users can insert custom values for key components of routing in 
                    the Delta and easily evaluate tradeoffs for each."), 
             actionButton(ns("goto_chinook_routing"), "View tool", 
                          class="btn-success")),
    tags$div(class="col-md-4", 
             tags$h4("Consequence Table"), 
             tags$hr(), 
             tags$p("Coming Soon")), 
    tags$div(class="col-md-4", 
             tags$h4("Smelt Routing"), 
             tags$hr(), 
             tags$p("Coming Soon")),
    tags$div(class="col-md-12",
             tags$hr(),
             helpText("To view source code for this application, as well 
                      as all the tools", tags$a(href="#", "here")))
  )
}

home_server <- function(input, output, session, x) {
  observeEvent(input$goto_chinook_routing, {
    updateNavbarPage(session = x, inputId = "deltaapp", selected = "chinook_routing")
  })
}

