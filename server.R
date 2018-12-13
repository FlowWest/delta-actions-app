shinyServer(function(input, output, session) {
  
  callModule(home_server, "app", x=session)
  callModule(consequence_server, "app")
  callModule(chinook_server, "app")
})
