library(shiny)

ui <- fluidPage(title = "Estimating from Graphs",
                navlistPanel(              
                   tabPanel(title = "Scatter Plots",
                            plotOutput("scattplot"),
                            actionButton("NewPlot", "Make a New Plot")
                   ),
                   tabPanel(title = "X-Variable Histogram",
                            plotOutput("x_hist")
                   ),
                   tabPanel(title = "Y-Variable Histogram",
                            plotOutput("y_hist")
                   ),
                   tabPanel(title = "Statistics",
                            textOutput("xmean"),
                            textOutput("xsd"),
                            textOutput("ymean"),
                            textOutput("ysd"),
                            textOutput("r1"),
                            textOutput("rms")
                   ),
                   tabPanel(title = "Instructions",
                            tags$p("Press the Make a New Plot button to begin."),
                            tags$p("Estimate the average and SD for the x and y variables from the scatter plot.
                                   Click on the Statistics link to see if you are correct.
                                   The histograms links will show you the histograms for each
                                   variable individually."))
                            )
                            )





server <- function(input, output) {
   #set up starting values for the means, sds, etc.
   mylist <- reactiveValues(mx = 0, sx = 1, my = 0, sy = 1, xlist = c(1:100), r1 = 0, ylist = c(1:100), b1 = 1, b0 = 0, rms = 0)
   
   #These will update each time a user clicks the NewPlot button
   observeEvent(input$NewPlot, {
      mylist$mx <-runif(1, 0, 500)
      mylist$sx <- runif(1, .5, 100)
      mylist$my <- runif(1, 0, 500)
      mylist$sy <- runif(1, .5, 100)
      mylist$xlist<-rnorm(100, mylist$mx, mylist$sx)
      mylist$r1 <- runif(1, -1, 1)
      mylist$b1 <- mylist$r1 * mylist$sy / mylist$sx
      mylist$b0 <- mylist$my - mylist$b1 * mylist$mx
      mylist$rms <- sqrt(1 - (mylist$r1)^2) * mylist$sy
      for (i in 1:100) {mylist$ylist[i] = mylist$b1*mylist$xlist[i]+mylist$b0+rnorm(1,0,20)}
   })
   
   output$scattplot <- renderPlot({
      plot(mylist$xlist,mylist$ylist, main = "Scatter Plot of X and Y Variables",
           xlab = "X-variable", ylab = "Y-variable", pch=19)
   })
   output$x_hist <- renderPlot({
      hist(mylist$xlist, main = "Histogram of X-Variable",
           xlab = "X-Variable", ylab = "Proportion per X", prob=TRUE)
   })
   output$y_hist <- renderPlot({
      hist(mylist$ylist, main = "Histogram of Y-Variable",
           xlab = "Y-Variable", ylab = "Proportion per Y", prob=TRUE)
   })
   output$xmean <- renderText(paste("X-variable average =", round(mylist$mx)))
   output$xsd <- renderText(paste("X-variable SD =", round(mylist$sx)))
   output$ymean <- renderText(paste("Y-variable average =", round(mylist$my)))
   output$ysd <- renderText(paste("Y-variable SD =", round(mylist$sy)))
   output$r1 <- renderText(paste("correlation coefficient r =", round(cor(mylist$xlist, mylist$ylist),2)))
   output$rms <- renderText(paste("r.m.s. error =", round(mylist$rms, 2)))
   
}


shinyApp(ui = ui, server = server)