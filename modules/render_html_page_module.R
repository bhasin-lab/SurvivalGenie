
## Render HTML page -------------------------------------

htmlUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("htmlout")) %>% withSpinner()
  )
}

htmlServer <- function(input, output, session, path) {
  ns <- session$ns
  
  shinyjs::runjs(paste0("callValue('", ns(""), "');"))
  shiny::addResourcePath("data", dirname(path))
  
  output$htmlout <- renderUI({
    tags$iframe(
      id = ns("htmlout"),
      seamless = "seamless",
      src = paste0("data/", basename(path)),
      width = 1.0 * as.numeric(input$dimval$width),
      height = 0.9 * as.numeric(input$dimval$height)
    )
  })
}

