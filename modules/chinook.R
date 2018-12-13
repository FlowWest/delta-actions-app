chinook_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      tabsetPanel(
        type = "pills",
        tabPanel("Dash", 
                 tags$h3("Chinook Routing"),
                 helpText("Set each of the parameters below, and select junctions 
                          of interest from the map"),
                 radioButtons(ns("dcc_open"), "Delta Cross Channel Gate", 
                              choices = c("open"=1, "closed"=0), inline=TRUE),
                 radioButtons(ns("hor_barr"), "Head of Old River", 
                              choices = c("barrier in place"=1, "no barrier"=0), inline=TRUE),
                 radioButtons(ns("bio_fence"), "Bioacoustic fence at Sutter/Steamboar", 
                              choices = c("fence in place"=1, "no fence"=0), inline = TRUE),
                 numericInput(ns("q_free"), "Freeport average daily discharge (cms, 150 < x < 2400)", 
                              value = 150, min = 150, max = 2400),
                 
                 # these two numeric inputs need addtional context
                 tags$div(
                   tags$div(style="display:inline-block", 
                            numericInput(ns("q_vern"), "Vernalis average daily discharge (cms, 13.6 < x < 807)", 
                                         value = 14, min = 13.6, max = 807)), 
                   tags$div(style="display:inline-block", 
                            actionButton(ns("help_with_q_vern"), label = NULL, 
                                         icon = icon("question"), 
                                         class="btn-xs btn-primary routing-help-button"))
                 ),
                 tags$div(
                   tags$div(style="display:inline-block", 
                            numericInput(ns("q_stck"), "Stockton average daily discharge (cms)", 
                                         value = 0, min = -8.9, max = 325.6)), 
                   tags$div(style="display:inline-block", 
                            actionButton(ns("help_with_q_stck"), label = NULL, 
                                         icon = icon("question"), 
                                         class="btn-xs btn-primary routing-help-button"))
                 ),
                 numericInput(ns("temp_vern"), "Vernalis average daily temperature (C°, 8.2 < x < 21.8)", 
                              value = 10, min = 8.2, max = 21.8),
                 numericInput(ns("temp_pp"), "SJR at Prisoner's Point average daily temperature (C°, 7.8 < x < 20.8)", 
                              value = 10, min = 7.8, max = 20.8),
                 numericInput(ns("cvp_exp"), "CVP average daily exports (cms)", 
                              value = 10, min = 6.1, max = 120),
                 numericInput(ns("swp_exp"), "SWP average daily exports (cms)", 
                              value = 10, min = 5.2, max = 235.7),
                 numericInput(ns("fl"), "Fork length (mm)", value = 10),
                 actionButton(ns("clear_selected_routing_points"), "clear points", class="btn-primary")), 
        tabPanel("Help", 
                 tagList(
                   tags$br(),
                   tags$p("The Chinook routing tool allows users to set parameter values at 
                       different junctions of the Delta. "), 
                   tags$br(),
                   tags$h4("How to use"), 
                   tags$hr(), 
                   tags$p("The interface to the tool is simple and requires minimal set up 
                        to use."),
                   tags$p("You can start by selecting junctions of interest from the map.
                          Upon selecting the first one, you will notice a bar plot start
                          to appear below the map. Each bar corresponds to a point selected."),
                   tags$p("Once a set of desired points have been selected, you can start
                          changing parameters. This will modify the results for Chinook routing and
                          the plot will reflect this."),
                   tags$p("You can clear all the points on the map using the 'clear points'
                          button. You can also remove only a particular point by
                          selecting it again.")
                 )
        )
      )
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
  
  observeEvent(input$temp_vern, {
    updateNumericInput(session, "temp_pp", 
                       max = (0.91483 + 0.88525 * input$temp_vern)*1.2, 
                       min = (0.91483 + 0.88525 * input$temp_vern)*0.8)
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
           "Select more than one point of interest from the map above to view a plot here")
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
      addCircleMarkers(color="red", 
                       fillColor = "#c65151",
                       layerId = ~paste0("map-",loc_id),
                       label = ~as.character(chinook_locs_id_lookup[loc_id]),
                       labelOptions = labelOptions(noHide = TRUE),
                       group="selected_points")
  })
  
  # Help modals --------------------------------------------------------
  observeEvent(input$help_with_q_vern, {
    showModal(modalDialog(
      title = "Flow at Vernalis", 
      tagList(
        tags$p("Flow at Vernallis is correlated with flow at Stockton. 
              The application will change the allowed values based on 
              a quantified version of this correlation.")), 
      easyClose = TRUE
    ))
  })
}

