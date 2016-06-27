library(shiny)
library(ggplot2)

ui <- fluidPage(title = "Estimating from Graphs",
                navlistPanel(              
                  tabPanel(title = "Scatter Plots",
                           actionButton("NewPlot", "Make a New Plot"),
                           plotOutput("scattplot"),
                           checkboxInput("RegLine", label = "Draw the Regression Line", value = FALSE),
                           checkboxInput("RMSLine", label = "Show one RMS error above and below the regression line", value = FALSE)
                  ),
                  tabPanel(title = "X-Variable Histogram",
                           plotOutput("x_hist"),
                           checkboxInput("Xave", label = "Show the average", value = FALSE),
                           checkboxInput("Xsd", label = "Show one SD above and below the average", value = FALSE)
                  ),
                  tabPanel(title = "Y-Variable Histogram",
                           plotOutput("y_hist"),
                           checkboxInput("Yave", label = "Show the average", value = FALSE),
                           checkboxInput("Ysd", label = "Show one SD above and below the average", value = FALSE)
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
  mylist <- reactiveValues(mx = 50.5, sx = 29, my = 50.5, sy = 29, xlist = c(1:100), r1 = 1, ylist = c(1:100), b1 = 1, b0 = 0, rms = 0, cor = 1)
  
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
    mylist$cor <- cor(mylist$xlist, mylist$ylist)
    mylist$rms <- sqrt(1 - (mylist$cor)^2) * mylist$sy
    for (i in 1:100) {mylist$ylist[i] = mylist$b1*mylist$xlist[i]+mylist$b0+rnorm(1,0,20)}
    
    
  })
  
  output$scattplot <- renderPlot({
    dat<-data.frame(mylist$xlist, mylist$ylist)
    p <- ggplot(dat, aes(x = mylist.xlist, y = mylist.ylist)) +
      geom_point() +
      ggtitle("Scatter Plot of X- and Y-Variables") +
      scale_x_continuous("X-variable", 
                         breaks = round(seq(min(mylist$xlist), max(mylist$xlist), 
                                            (max(mylist$xlist)-min(mylist$xlist))/10)), 0) +
      scale_y_continuous("Y-variable", 
                         breaks = round(seq(min(mylist$ylist), max(mylist$ylist), 
                                            (max(mylist$ylist)-min(mylist$ylist))/10)), 0) +
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 1),
            axis.line.y = element_line(color="black", size = 1))
    if(input$RegLine) {p + geom_smooth(method = "lm", se = FALSE)} else {p}   
  })
  output$x_hist <- renderPlot({
    dat <- dat<-data.frame(mylist$xlist, mylist$ylist)
    p2<- ggplot(dat, aes(x = mylist.xlist)) +
      geom_histogram(aes(y = ..density..), fill = "gray81", col = "black",
                     binwidth = (max(mylist$xlist)-min(mylist$xlist))/10) +
      ggtitle("Histogram of X-Variable") +
      scale_x_continuous("X-Variable", 
                         breaks = round(seq(min(mylist$xlist), max(mylist$xlist), 
                                            (max(mylist$xlist)-min(mylist$xlist))/10)), 0) +
      scale_y_continuous("Proportion per X") +
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 1),
            axis.line.y = element_line(color="black", size = 1))
    avline <- geom_vline(xintercept = mean(mylist$xlist), col = "red", size = 2)
    sdline1 <- geom_vline(xintercept = (mean(mylist$xlist) - sd(mylist$xlist)), col = "blue", size = 2)
    sdline2 <- geom_vline(xintercept = (mean(mylist$xlist) + sd(mylist$xlist)), col = "blue", size = 2)
    p3 <- p2 + avline
    p4 <- p2 + avline + sdline1 + sdline2
    p5 <- p2 + sdline1 + sdline2
    if(input$Xave == TRUE & input$Xsd == FALSE) {
      p3
    } else if (input$Xave == TRUE & input$Xsd == TRUE) {
      p4
    } else if (input$Xave == FALSE & input$Xsd == TRUE) {
      p5
    } else {
      p2
    }
  })
  output$y_hist <- renderPlot({
    dat <- dat<-data.frame(mylist$xlist, mylist$ylist)
    p2<- ggplot(dat, aes(x = mylist.ylist)) +
      geom_histogram(aes(y = ..density..), fill = "gray81", col = "black",
                     binwidth = (max(mylist$ylist)-min(mylist$ylist))/10) +
      ggtitle("Histogram of Y-Variable") +
      scale_x_continuous("Y-Variable", 
                         breaks = round(seq(min(mylist$ylist), max(mylist$ylist), 
                                            (max(mylist$ylist)-min(mylist$ylist))/10)), 0) +
      scale_y_continuous("Proportion per Y") +
      theme_bw() + 
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank() )+
      theme(panel.border= element_blank())+
      theme(axis.line.x = element_line(color="black", size = 1),
            axis.line.y = element_line(color="black", size = 1))
    avline <- geom_vline(xintercept = mean(mylist$ylist), col = "red", size = 2)
    sdline1 <- geom_vline(xintercept = (mean(mylist$ylist) - sd(mylist$ylist)), col = "blue", size = 2)
    sdline2 <- geom_vline(xintercept = (mean(mylist$ylist) + sd(mylist$ylist)), col = "blue", size = 2)
    p3 <- p2 + avline
    p4 <- p2 + avline + sdline1 + sdline2
    p5 <- p2 + sdline1 + sdline2
    if(input$Yave == TRUE & input$Ysd == FALSE) {
      p3
    } else if (input$Yave == TRUE & input$Ysd == TRUE) {
      p4
    } else if (input$Yave == FALSE & input$Ysd == TRUE) {
      p5
    } else {
      p2
    }
  })
  
  output$xmean <- renderText(paste("X-variable average =", round(mylist$mx)))
  output$xsd <- renderText(paste("X-variable SD =", round(mylist$sx)))
  output$ymean <- renderText(paste("Y-variable average =", round(mylist$my)))
  output$ysd <- renderText(paste("Y-variable SD =", round(mylist$sy)))
  output$r1 <- renderText(paste("correlation coefficient r =", round(cor(mylist$xlist, mylist$ylist),2)))
  output$rms <- renderText(paste("r.m.s. error =", round(mylist$rms, 2)))
  
}


shinyApp(ui = ui, server = server)