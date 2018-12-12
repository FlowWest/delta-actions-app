home_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class="jumbotron", 
      tags$h1(class="display-3","Delta SDM Tools"), 
      tags$p(class="lead", "This application compiles a set of tools for interacting 
           and visualizing model input and output for the SDM models."), 
      tags$hr()
    ),  
    tags$div(class="col-md-4", 
             tags$h4("Chinook Routing"), 
             tags$hr()),
    tags$div(class="col-md-4", 
             tags$h4("Consequence Table"), 
             tags$hr()), 
    tags$div(class="col-md-4", 
             tags$h4("Smelt Routing"), 
             tags$hr())
  )
}

home_server <- function(input, output, session) {
  
}


# HTML('<div class="list-group">
#   <a href="#" class="list-group-item list-group-item-action flex-column align-items-start active">
#     <div class="d-flex w-100 justify-content-between">
#       <h5 class="mb-1">List group item heading</h5>
#       <small>3 days ago</small>
#     </div>
#     <p class="mb-1">Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit.</p>
#     <small>Donec id elit non mi porta.</small>
#   </a>
#   <a href="#" class="list-group-item list-group-item-action flex-column align-items-start">
#     <div class="d-flex w-100 justify-content-between">
#       <h5 class="mb-1">List group item heading</h5>
#       <small class="text-muted">3 days ago</small>
#     </div>
#     <p class="mb-1">Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit.</p>
#     <small class="text-muted">Donec id elit non mi porta.</small>
#   </a>
# </div>')# HTML('<div class="list-group">
#   <a href="#" class="list-group-item list-group-item-action flex-column align-items-start active">
#     <div class="d-flex w-100 justify-content-between">
#       <h5 class="mb-1">List group item heading</h5>
#       <small>3 days ago</small>
#     </div>
#     <p class="mb-1">Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit.</p>
#     <small>Donec id elit non mi porta.</small>
#   </a>
#   <a href="#" class="list-group-item list-group-item-action flex-column align-items-start">
#     <div class="d-flex w-100 justify-content-between">
#       <h5 class="mb-1">List group item heading</h5>
#       <small class="text-muted">3 days ago</small>
#     </div>
#     <p class="mb-1">Donec id elit non mi porta gravida at eget metus. Maecenas sed diam eget risus varius blandit.</p>
#     <small class="text-muted">Donec id elit non mi porta.</small>
#   </a>
# </div>')