library(shiny)
load("data/out.rdata")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({

    par(mfrow=c(4,1))
    plot(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(data[,input$var],c(predictLevel[1,input$var]+predictSeason[1,input$var]), rep(NA,11)), ylim=range(c(data[,input$var],(puLevel[,input$var]+puSeas[,input$var]),(plLevel[,input$var]+plSeas[,input$var]),c(predictLevel[1,input$var]+predictSeason[1,input$var])))
  ,ylab=paste(input$var, "Sales",sep=" "),type="l",xlab="Year")
  
    #lines(c(statesSeason[,input$var]+statesLevel[,input$var],rep(NA,12)))
    lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), predictLevel[,input$var]+predictSeason[,input$var]), col=2)
    lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), puLevel[,input$var]+puSeas[,input$var]), col=2)
    lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), plLevel[,input$var]+plSeas[,input$var]), col=2)
    
    
    plot(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(statesLevel[,input$var],predictLevel[,input$var]),ylim=range(c(statesLevel[,input$var],predictLevel[,input$var],puLevel[,input$var],plLevel[,input$var])),ylab=paste(input$var, "Deseasonilized Sales",sep=" "),type="l",xlab="Year")
   lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), puLevel[,input$var]), col=2)
   lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), plLevel[,input$var]), col=2)
   
    plot(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(statesGrowth[,input$var], predictGrowth[,input$var]), ylim=range(c(statesGrowth[,input$var], predictGrowth[,input$var],puMu[,input$var],plMu[,input$var])),ylab=paste(input$var, "Growth",sep=" "),type="l",xlab="Year")
   lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), puMu[,input$var]), col=2)
   lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), plMu[,input$var]), col=2)
   
    plot(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(statesSeason[,input$var], predictSeason[,input$var]), ylim=range(c(statesSeason[,input$var], predictSeason[,input$var], puSeas[,input$var],plSeas[,input$var])),ylab=paste(input$var, "Seasonality",sep=" "),type="l",xlab="Year")
   lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), puSeas[,input$var]), col=2)
   lines(seq(as.Date("07/01/2005", format = "%d/%m/%Y"), by = "months", length = length(data[,input$var])+12) ,c(rep(NA,length(data[,1])), plSeas[,input$var]), col=2)
    
    
    
    
  })
})