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

    # Application title
    titlePanel("Optimal Allocations in Platform Trials"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("r1",
                        "Fraction of observations in Period 1",
                        min = 0,
                        max = 0.5,
                        value = .3),
            checkboxInput("CC", "Only concurrent controls", TRUE),
            checkboxInput("NCC", "Concurrent and non-concurrent controls", TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)
    
# Define server logic required to draw a histogram
server <- function(input, output) {

  source("Optimisation case 3.R")
    output$distPlot <- renderPlot({
        plalloc(input$r1,CC=input$CC,NCC=input$NCC,savepdf=F) }, height = 800, width = 900,res=80)}

# Run the application 
shinyApp(ui = ui, server = server)
