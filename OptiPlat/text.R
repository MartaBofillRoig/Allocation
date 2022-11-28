# output$Explanation <- renderText({  
#   HTML(paste0('<div style = "background-color: #e6f2ff; width: 100%; border-radius: 20px; padding:20px;">', 
#               "Upper graph: The gray line denotes the allocation probability 1/(2+sqrt(2)), the optimal allocation ratio for the control group in the multi-arm trial, where both experimental treatment arms start at the beginning (r1=0). 
# 				Lower graph: The lower gray line denotes the reduction in Variance of a multi-arm trial allocation probability 1/(2+sqrt(2)), the optimal allocation ratio for the control group in the multi-arm trial. "))
# }) 


output$text <- renderUI({
  fruits[input$index] <- paste("<b>",fruits[input$index],"</b>")
  HTML(paste(fruits))
})