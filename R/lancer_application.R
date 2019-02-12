lancer_application <- function() {  
  appDir <- system.file("APPLICATION", package = "RSATRAJ")  
  shiny::runApp(appDir, display.mode = "normal")
  }
