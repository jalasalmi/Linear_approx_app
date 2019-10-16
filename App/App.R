library(ggplot2)
library(tidyr)
library(dplyr)
library(shinyWidgets)



ui <- fluidPage(
  
  titlePanel("Taylor Series Approximations of real-valued functions on real line"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      sliderTextInput(inputId = "xaxis", label = "x-axis limits",
                      choices = seq(-10,10, by = 1),
                      selected = c(-5,5)),
      textInput(inputId = "fct", label = "Enter function"),
      numericInput(inputId = "at", label = "Approximation at", value = 0, step = 0.1),
      numericInput(inputId = "n", label = "Degree of approximation", value = 1, step = 1)
    ),
    mainPanel(
    
    plotOutput("plot")
    )
  
  )
)

server <- function(input, output, session) {
  
    output$plot <- renderPlot({
    
      fun <- function(x) { eval(parse(text = input$fct)) }
      dfun <- function(x, func, N) { 
        derivative <- D(parse(text = func), "x")
         if(N > 1) {
           for(n in 1:(N-1)) {
           derivative <- D(derivative, "x")
           }
         }
         eval(derivative)
         }
      approximation <- function(x, N) { 
      terms = 0
        for(n in 1:N) {
        terms <- terms + dfun(input$at, input$fct, n) / factorial(n) * (x - input$at)^n 
        }  
      
      fun(input$at) + terms 
      }
        
      df <- data.frame(x = seq(input$xaxis[1],input$xaxis[2],by = 0.01))
      df <- df %>% mutate(y =  fun(x),
                          L = approximation(x, input$n)) %>%
        gather(value_type, value, c("y","L"))
      
      ggplot(df, aes(x = x, y = value, col = value_type)) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0) +
        geom_line(size = 1.5) +
        theme_light() +
        labs(col = NULL, x = NULL, y = NULL) +
        scale_x_continuous(breaks = input$xaxis[1]:input$xaxis[2]) 
    })
}
 


shinyApp(ui = ui, server = server)