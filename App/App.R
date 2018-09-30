library(ggplot2)
library(tidyr)
library(dplyr)
library(shinyWidgets)

ui <- fluidPage(
  
  titlePanel("Linear Approximations of real-valued functions on real line"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      sliderTextInput(inputId = "xaxis", label = "x-axis limits",
                      choices = seq(-10,10, by = 1),
                      selected = c(-5,5)),
      textInput(inputId = "fct", label = "Enter function"),
      numericInput(inputId = "at", label = "Approximation at", value = 0, step = 0.1)
    ),
    mainPanel(
    
    plotOutput("plot")
    )
  
  )
)

     

server <- function(input, output, session) {
  
    output$plot <- renderPlot({
      
      function_expression <- parse(text = input$fct)
      
      fun <- function(x) { eval(parse(text = input$fct)) }
      dfun <- function(x) { eval(D(parse(text = input$fct), "x"))}
      linearization <- function(x) { 
        fa <- fun(input$at)
        dfa <- dfun(input$at)
        fa + dfa * (x - input$at)
      }
        
      df <- data.frame(x = seq(input$xaxis[1],input$xaxis[2],by = 0.01))
      df <- df %>% mutate(y =  fun(x),
                          L = linearization(x)) %>%
        gather(value_type, value, c("y","L"))
      
      ggplot(df, aes(x = x, y = value, col = value_type)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_line()
    })
}
 


shinyApp(ui = ui, server = server)