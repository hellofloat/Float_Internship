
# Shiny makes it easy to turn analytical analysis into stylish and interactive web apps
# There are two files we create --- ui.r and server.r
# ui.r -- control the layout, appeaance, widgeta that capture suer inputs
# server.r --- set of instructions that uses the suer inputs, process them adn produces required output


# structure of ui.r

library(shiny)

# define UI for shiny application

shinyUI(fluidpage(
  
  # application title
  titlePanel(),
  
  #sidebarLayout(
  
  #sidebar panel - widgets to get data from user
  sidebarPanel(),
  
  #Main panel -- ouput , charts are dislpayed
  mainPanel()
  )
  
)
)

# structure of server.R

library(shiny)

shinyServer(
  function(input,output){
    
  }
)
