library(shiny)
load("data/out.rdata")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = colnames(statesGrowth),
                  selected = colnames(statesGrowth)[1])
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height = 1000, width = 1000)
    )
  )
))