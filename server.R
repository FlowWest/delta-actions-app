shinyServer(function(input, output) {

  callModule(consequence_server, "app")
  callModule(chinook_server, "app")
})
