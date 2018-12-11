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
      numericInput(ns("q_free"), "Freeport average daily discharge (cms, 150 < x < 2400)", 
                   value = 150, min = 150, max = 2400),
      numericInput(ns("q_vern"), "Vernalis average daily discharge (cms, 13.6 < x < 807)", 
                   value = 14, min = 13.6, max = 807),
      numericInput(ns("q_stck"), "Stockton average daily discharge (cms, -8.9 < x < 325.6)", 
                   value = 0, min = -8.9, max = 325.6),
      numericInput(ns("temp_vern"), "Vernalis average daily temperature (C°, 8.2 < x < 21.8)", 
                   value = 10, min = 8.2, max = 21.8),
      numericInput(ns("temp_pp"), "SJR at Prisoner's Point average daily temperature (C°, 7.8 < x < 20.8)", 
                   value = 10, min = 7.8, max = 20.8),
      numericInput(ns("cvp_exp"), "CVP average daily exports (cms)", 
                   value = 10, min = 6.1, max = 120),
      numericInput(ns("swp_exp"), "SWP average daily exports (cms)", 
                   value = 10, min = 5.2, max = 235.7),
      numericInput(ns("fl"), "Fork length (mm)", value = 10),
      actionButton(ns("clear_selected_routing_points"), "clear points", class="btn-primary")
    ), 
    mainPanel(
      width = 9,
      leafletOutput(ns("chinook_routing_map")),
      tabsetPanel(
        type = "pills",
        tabPanel("Plot", plotlyOutput(ns("chinook_routing_plot"))),
        tabPanel("Data", tableOutput(ns("chinook_routing_table")))
      )
    )
  )
}

chinook_server <- function(input, output, session) {
  
  # there is a linear correlation between the two
  observeEvent(input$q_vern, {
    updateNumericInput(session, "q_stck", 
                       max = (-4.155134 + 0.429387 * input$q_vern)*1.3,
                       min = (-4.155134 + 0.429387 * input$q_vern)*0.7)
  })
  
  observeEvent(input$clear_selected_routing_points, {
    chinook_routing_locations_selected$data <- NULL
    
    leafletProxy("chinook_routing_map") %>% 
      clearGroup("selected_points")
  })
  
  cmap <- colorFactor("Dark2", domain = chinook_regions$Id)
  
  chinook_routing <- reactiveVal(NULL)
  chinook_routing_locations_selected <- reactiveValues(data=NULL)
  
  chinook_routing_output <- reactive({
    DeltaS(
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
      10000, 
      10000,
      input$fl)
  })
  
  chinook_routing_df <- reactive({
    
    data.frame(loc_id = names(chinook_routing_output()), 
               value = chinook_routing_output(), row.names = NULL) %>% 
      left_join(chinook_routing_points, by=c("loc_id"="location_id"))
    
  })
  
  observeEvent(input$chinook_routing_map_marker_click, {
    cat(unlist(input$chinook_routing_map_marker_click))
    if (is.null(chinook_routing_locations_selected$data)) {
      cat("in the case when chinook routing is null")
      chinook_routing_locations_selected$data <- input$chinook_routing_map_marker_click$id
    } else if ((input$chinook_routing_map_marker_click$id %in% chinook_routing_locations_selected$data)) {
      cat("in the case when the marker is in the list of selected already")
      chinook_routing_locations_selected$data
    } else 
      chinook_routing_locations_selected$data <- 
        append(chinook_routing_locations_selected$data, 
               input$chinook_routing_map_marker_click$id)
  })
  
  chinook_routing_run <- reactive({
    
    chinook_routing_df() %>% 
      filter(loc_id %in% chinook_routing_locations_selected$data)
    
  })
  
  
  
  output$chinook_routing_map <- renderLeaflet({
    leaflet(chinook_regions) %>% 
      addProviderTiles(provider = providers$Esri.WorldTopoMap, group="Topo") %>%
      addProviderTiles(provider = providers$Esri.WorldImagery, group = "Imagery") %>% 
      addPolygons(
        weight = 2, 
        fillOpacity = .5, 
        fillColor = "#8c8c8c", 
        color="#8c8c8c",
        popup=~paste0("<b>", Id, "</b>"), 
        group = "Chinook Regions") %>% 
      addCircleMarkers(data=chinook_routing_points, label=~paste(location), 
                       weight=1, fillOpacity = .8, layerId = ~location_id, 
                       group = "Routing Points", color="#3a3a3a", fillColor = "#008cba") %>% 
      addLayersControl(
        baseGroups = c("Topo", "Imagery"),
        overlayGroups = c("Chinook Regions")
      )
    
  })
  
  output$chinook_routing_plot <- renderPlotly({
    
    validate(
      need(!is.null(chinook_routing_locations_selected$data),
           "First set parameters and run, then select a point to view Chinook counts")
    )
    
    
    chinook_routing_run() %>% 
      plot_ly(x=~location, y=~value, type='bar', marker=list(color="#008cba")) %>% 
      layout(xaxis = list(title=""))
    
  })
  
  output$chinook_routing_table <- renderTable({
    chinook_routing_run() %>% 
      select(value, location) %>% 
      spread(location, value)
  })
  
  observeEvent(input$chinook_routing_map_click, {
    leafletProxy("chinook_routing_map", data=chinook_routing_run()) %>% 
      clearGroup("selected_points") %>% 
      addCircleMarkers(color="red", fillColor = "#c65151", group="selected_points")
  })
  
}

















