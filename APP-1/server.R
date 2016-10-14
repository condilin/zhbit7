server<- shinyServer(function(input,output){
output$textDisplay <- renderText(
  paste0("You said ", input$comment," there are",
         nchar(input$comment)," charater in this.")
)
})