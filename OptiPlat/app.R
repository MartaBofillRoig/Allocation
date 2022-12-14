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
          p("This app computes optimal allocation ratios in Period 2 of a platform trial with two experimental and one control arm.
            In Periods 1 and 2 the optimal allocation is in all cases 1:1 allocation."),
          p(HTML(paste("For details see ",tags$a(href="https://github.com/MartaBofillRoig/Allocation", "Bofill Roig et al. (2022)")),sep="")),
         # p(HTML(paste(tags$em("r"),tags$sub("2"),".... fraction of observations in period 2", sep = ""))),
          br(),
            sliderInput("r1",
                        HTML(paste("Proportion of observations in Period 1  (",tags$em("r"),tags$sub("1"),")",sep="")),
                        min = 0,
                        max = 0.5,
                        value = .3),
            checkboxInput("CC", "Analysis with concurrent controls only", TRUE),
            checkboxInput("NCC", "Analysis with concurrent and non-concurrent controls", TRUE),
         htmlOutput("text")
        ),
        # Show a plot of the generated distribution
        mainPanel(
           br(), 
           plotOutput("distPlot"),
           br() 
           # ,
           # p(HTML(paste("Upper graph: The gray line denotes the allocation probability 1/(2+sqrt(2)), the optimal allocation ratio 
           #   for the control group in the multi-arm trial, where both experimental treatment arms start at the beginning (",
           #              tags$em("r"),tags$sub("1"),"=0)",sep=""))),
           # p("Lower graph: The lower gray line denotes the reduction in Variance of a multi-arm trial allocation probability 1/(2+sqrt(2)), the optimal allocation ratio 
           #   for the control group in the multi-arm trial.")
      )
    )
)
    
# Define server logic required to draw the plots
server <- function(input, output) {

  output$text <- renderText({
    HTML(paste0('<div style = "background-color: #e6f2ff; width: 100%; border-radius: 20px; padding:20px;">',
                "<b>Upper graph:</b> The optimal allocation ratios for the three arms in Period 2 of the platform 
                trial as function of <em>r</em><sub>2</sub>. The upper gray line denotes the allocation probability 1/(2+sqrt(2)), 
                the optimal allocation ratio for the control group in the multi-arm trial, where both experimental treatment arms start 
                at the beginning (<em>r</em><sub>1</sub>=0).
                <br>
  				<b>Lower graph:</b> The decrease in variance (in percent) of the optimized platform trial compared to separate trials with the same total 
                sample size as the platform trial as function of <em>r</em><sub>2</sub>. The lower gray line denotes the reduction in variance of a multi-arm trial allocation probability 1/(2+sqrt(2)), the optimal allocation ratio for the control group in the multi-arm trial. "))
  }) 
  
  source("Optimisation case 3.R")
    output$distPlot <- renderPlot({
        plalloc(input$r1,CC=input$CC,NCC=input$NCC,savepdf=F) }, height = 800, width = 900,res=80)}

# Run the application 
shinyApp(ui = ui, server = server)
