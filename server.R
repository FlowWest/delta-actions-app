shinyServer(function(input, output) {
  
  callModule(home_server, "app")
  callModule(consequence_server, "app")
  callModule(chinook_server, "app")
})
