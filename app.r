library(shiny)
source(file = "R/simulate_full_matrix.R")
source(file = "R/random.R")
source(file = "R/create_histogram.R")
source(file = "R/extra_inputs.R")


# Define ui ----
ui <- fluidPage(
  titlePanel("Simulated data showcasing MNAR and MAR values"),
  sidebarLayout(
    sidebarPanel(
      
      helpText("Toogle log2 scaling data"),
      checkboxInput("transform_log2", "log2 scale", 
                    value = TRUE),
      
      helpText("Histogram settings"),
      sliderInput("breaks", "Number of breaks", 
                  min = 10, max = 100, value = 50, step = 10),
      
      helpText("Include missing data"),
      sliderInput("MNAR_threshold", "Cutoff threshold for considering MNAR", 
                          min = 5, max = 95, value = 40, step = 5),
      
      checkboxInput("MAR", "include MAR", 
                    value = TRUE),
      
      sliderInput("MAR_prob", "MAR probablity", 
                  min = 0.05, max = 0.50, value = 0.1, step = 0.05),
      
      checkboxInput("MNAR", "include MNAR", 
                    value = TRUE),
      
      sliderInput("MNAR_prob", "MNAR probability", 
                  min = 0.25, max = 0.75, value = 0.5, step = 0.05)
    ),
    mainPanel(
      
      plotOutput(outputId = "histogram"),
      
      actionButton('recalc_full_matrix', "Recalculate the matrix")
    ))
)

# Define server logic ----
server <- function(input, output) {
  
  full_matrix <- reactiveVal(simulate_full_matrix())
  
  observeEvent(input$recalc_full_matrix, {
    full_matrix(simulate_full_matrix())
  })
  
  # Create histogram ----
  output$histogram <- renderPlot({
    create_histogram(
      full_matrix=full_matrix(),
      MNAR_threshold=input$MNAR_threshold,
      MAR_prob=input$MAR_prob,
      MNAR_prob=input$MNAR_prob,
      MAR=input$MAR,
      MNAR=input$MNAR,
      transform_log2=input$transform_log2,
      breaks=input$breaks)
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)