#' Run shiny application
#' 
#' @param fits a list of objects of class \code{brmsfit} that you want to compare.
#' @return a \code{shiny.appobj} object.
#' @import shiny
#' @export
#' 
influ_app <- function(fits) {
  
  n <- length(fits)
  flabs <- rep(NA, n)
  
  for (i in 1:n) {
    str <- as.character(fits[[i]]$formula)[1]
    left1 <- stringi::stri_trim_right(str = str, pattern = "[\u007E]", negate = FALSE)
    flabs[i] <- substr(x = str, nchar(left1) + 2, nchar(str))
  }
  
  ui <- navbarPage(
    title = "influ2",
    tabPanel(
      title = "Model comparisons",
      tableOutput(outputId = "table_cf"),
      selectInput(inputId = "pick_fits", label = "Select the fits", choices = flabs, selected = flabs, multiple = TRUE),
      plotOutput(outputId = "plot_cf", click = "plot_click")
    )
  )
  
  server <- function(input, output, session) {
    output$table_cf <- renderTable({
      get_bayes_R2(fits = fits)
    })
    output$plot_cf <- renderPlot({
      ii <- which(input$pick_fits %in% flabs)
      ifits <- fits[ii]
      plot_compare(fits = ifits)
    })
  }
  
  shinyApp(ui = ui, server = server)
  # shinyApp(ui = ui, server = server, ...)
}
