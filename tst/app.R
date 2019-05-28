#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(column(width=12, h2("Filering demo"))),
  fluidRow(
    column(
      width = 4,
      filterDataUI(id = "demo")
    ),
    column(width = 8, 
           DT::dataTableOutput(outputId = "subsetdata"),
           DT::dataTableOutput(outputId = "subsetdata2")
    )
  ),
  fluidRow(
    column(width = 12, 
           verbatimTextOutput(outputId = "expr"),
           verbatimTextOutput(outputId = "expr2"),
           verbatimTextOutput(outputId = "expr3"),
           verbatimTextOutput(outputId = "expr4")
           
           
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  res <- callModule(module = filterDataServer,
                    id = "demo", x = reactive(iris),
                    return_data = TRUE, show_all_filters = FALSE)
  
  
  output$expr <- renderText({
    req(res)
    if(res$filtered){
      expr_str <- format(res$expr)
      expr_str <- paste( gsub("^[ ]+", "", expr_str), collapse = "")
      
      gsub("\\&[ ]*", "&\n\t", expr_str, fixed = FALSE)
    } else NULL
  })
  output$expr2<-renderPrint({res$expr})
  output$expr3<-renderPrint({res$filtered})
  output$expr4<-renderPrint({as.character(res$expr)})
  
  
  output$subsetdata <- DT::renderDataTable({
    res$filtered_data
  })
  output$subsetdata2 <- DT::renderDataTable({
    as.character(res$expr)->char_cond
    paste(char_cond[2], char_cond[1], char_cond[3])->expr
    filter(iris, eval(parse(text=res$expr)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

