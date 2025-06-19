library(shiny)
library(rootSolve)

# Define UI for application
ui <- fluidPage(
  titlePanel("Project 1: Airline Overbooking Problem"),
  
  # Sidebar Layout
  sidebarLayout(
    sidebarPanel(
      sliderInput("N", "Number of seats (N):", min = 1, max = 400, value = 200),
      sliderInput("p", "Probability of showing up (p):", min = 0, max = 1, value = 0.95, step = 0.01),
      sliderInput("gamma", "Overbooking probability (gamma):", min = 0, max = 1, value = 0.2, step = 0.01),
      sliderInput("nposs", "Range of ticket sales (nposs):", min = 1, max = 300, value = 20)
    ),
    
    # Show plots
    mainPanel(
      plotOutput("DiscretePlot"),
      plotOutput("ContinuousPlot")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # Compute optimal tickets sold (discrete)
  optimal_discrete <- reactive({
    nposs <- seq(input$N, input$N + input$nposs, by = 1)
    fn_values <- 1 - input$gamma - pbinom(input$N, size = nposs, prob = input$p)
    nposs[which.min(abs(fn_values))]
  })
  
  # Compute optimal tickets sold (continuous)
  fn_continuous <- function(n) {
    mu_n <- n * input$p
    sigma_n <- sqrt(n * input$p * (1 - input$p))
    1 - input$gamma - pnorm(input$N + 0.5, mean = mu_n, sd = sigma_n)
  }
  
  optimal_continuous <- reactive({
    optimise(function(n) abs(fn_continuous(n)), lower = input$N, upper = input$N + input$nposs)$minimum
  })
  
  # Discrete Plot
  output$DiscretePlot <- renderPlot({
    nposs <- seq(input$N, input$N + input$nposs, by = 1)
    fn_values <- 1 - input$gamma - pbinom(input$N, size = nposs, prob = input$p)
    
    plot(nposs, fn_values, type = "p", pch = 16, col = "blue", 
         xlab = "n", ylab = "Objective",
         main = paste("Objective Vs n to find optimal tickets sold\n (", 
                      optimal_discrete(), 
                      ") gamma=", input$gamma, 
                      " N=", input$N, " discrete"))
    abline(h = 0, col = "red", lwd = 2)
    abline(v = optimal_discrete(), col = "red", lwd = 2)
  })
  
  # Continuous Plot
  output$ContinuousPlot <- renderPlot({
    nposs <- seq(input$N, input$N + input$nposs, by = 0.1) # 0.1 works best
    fn_values <- sapply(nposs, fn_continuous)
    
    plot(nposs, fn_values, type = "l", col = "black",
         xlab = "n", ylab = "Objective",
         main = paste("Objective Vs n to find optimal tickets sold\n (", 
                      round(optimal_continuous(), 4), 
                      ") gamma=", input$gamma, 
                      " N=", input$N, " continuous"))
    abline(h = 0, col = "blue", lwd = 2)
    abline(v = optimal_continuous(), col = "blue", lwd = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)