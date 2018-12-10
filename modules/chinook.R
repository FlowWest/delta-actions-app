chinook_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      tags$h3("Chinook Routing"),
      helpText("Set each of the parameters below, and select run to 
           simulate Chinook routing in the delta"),
      radioButtons(ns("dcc_open"), "Delta Cross Channel Gate", 
                   choices = c("open"=1, "closed"=0), inline=TRUE),
      radioButtons(ns("hor_barr"), "Head of Old River", 
                   choices = c("barrier in place"=1, "no barrier"=0), inline=TRUE),
      radioButtons(ns("bio_fence"), "Bioacoustic fence at Sutter/Steamboar", 
                   choices = c("fence in place"=1, "no fence"=0), inline = TRUE),
      numericInput(ns("q_free"), "Freeport average daily discharge (cms)", 10),
      numericInput(ns("q_vern"), "Vernalis average daily discharge (cms)", 10),
      numericInput(ns("q_stck"), "Stockton average daily discharge (cms)", 10),
      numericInput(ns("temp_vern"), "Vernalis average daily temperature (C)", 10),
      numericInput(ns("temp_pp"), "SJR at Prisoner's Point average daily temperature (C)", 10),
      numericInput(ns("cvp_exp"), "CVP average daily exports (cms)", 10),
      numericInput(ns("swp_exp"), "SWP average daily exports (cms)", 10),
      numericInput(ns("fl"), "Fork length (mm)", 10),
      actionButton(ns("run_routing"), "run routing", class="btn-primary")
    ), 
    mainPanel(
      width = 9,
      leafletOutput(ns("chinook_routing_map")),
      plotlyOutput(ns("chinook_routing_plot"))
    )
  )
}

chinook_server <- function(input, output, session) {
  
  cmap <- colorFactor("Dark2", domain = chinook_regions$Id)
  
  chinook_routing <- reactiveVal(NULL)
  chinook_routing_locations_selected <- reactiveValues(data=NULL)
  
  observeEvent(input$run_routing, {
    
    x <- DeltaS(
      as.numeric(input$dcc_open),
      as.numeric(input$hor_barr),
      as.numeric(input$bio_fence),
      input$q_free,
      input$q_vern, 
      input$q_stck,
      input$temp_vern,
      input$temp_pp,
      input$cvp_exp,
      input$swp_exp,
      1000, 
      1000,
      input$fl)
    
    d <- data.frame(loc_id = names(x), value=x, row.names = NULL) %>% 
      left_join(chinook_routing_points, by=c("loc_id"="location_id"))
    
    
    chinook_routing(d)
  })
  
  
  observeEvent(input$chinook_routing_map_marker_click, {
    if (is.null(chinook_routing_locations_selected$data))
      chinook_routing_locations_selected$data <- input$chinook_routing_map_marker_click$id
    else if (input$chinook_routing_map_marker_click$id %in% chinook_routing_locations_selected$data)
      return()
    else 
      chinook_routing_locations_selected$data <- 
        append(chinook_routing_locations_selected$data, 
               input$chinook_routing_map_marker_click$id)
  })
  
  chinook_routing_run <- reactive({
    req(chinook_routing())
    
    chinook_routing() %>% 
      filter(loc_id %in% chinook_routing_locations_selected$data)
  })
  
  output$chinook_routing_map <- renderLeaflet({
    leaflet(chinook_regions) %>% 
      addTiles() %>% 
      addPolygons(
                  weight = 2, 
                  fillOpacity = .5, 
                  color=~cmap(Id), 
                  label=~Id) %>% 
      addCircleMarkers(data=chinook_routing_points, label=~paste(location), 
                       weight=1, fillOpacity = .5, layerId = ~location_id) %>% 
      addLegend("bottomright",pal=cmap, values=~Id)
    
  })
  
  output$chinook_routing_plot <- renderPlotly({
    validate(
      need(!is.null(chinook_routing()), "First set parameters and run, then select a point to view Chinook counts")
    )
    
    chinook_routing_locations_selected$data %>% 
      plot_ly(x=~location, y=~value, type='bar')
  })
  
}