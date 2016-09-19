library(shiny)
library(ggplot2)

plaintheme <- theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  theme(panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

axistheme <- theme(plot.title = element_text(color = "black", face = "bold", size=28)) +
  theme(axis.title = element_text(color = "black", size = 20)) +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16))

ui <- fluidPage(title = "Estimating from Graphs",
                actionButton("NewPlot", "Make a New Plot"),
                navlistPanel(              
                  tabPanel(title = "Scatter Plots",
                           plotOutput("scattplot"),
                           checkboxInput("RegLine", label = "Draw the Regression Line", value = FALSE),
                           checkboxInput("RMSLine", label = "Show one and two RMS errors above and below the regression line", value = FALSE)
                  ),
                  tabPanel(title = "X-Variable Histogram",
                           plotOutput("x_hist"),
                           checkboxInput("Xave", label = "Show the average", value = FALSE),
                           checkboxInput("Xsd", label = "Show one SD above and below the average", value = FALSE),
                           checkboxInput("Xsd2", label = "Show two SDs above and below the average", value = FALSE)
                  ),
                  tabPanel(title = "Y-Variable Histogram",
                           plotOutput("y_hist"),
                           checkboxInput("Yave", label = "Show the average", value = FALSE),
                           checkboxInput("Ysd", label = "Show one SD above and below the average", value = FALSE),
                           checkboxInput("Ysd2", label = "Show two SDs above and below the average", value = FALSE)
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
    for (i in 1:100) {mylist$ylist[i] = mylist$b1*mylist$xlist[i]+mylist$b0+rnorm(1,0,20)}
    mylist$mx <- mean(mylist$xlist)
    mylist$sx <- sd(mylist$xlist)
    mylist$my <- mean(mylist$ylist)
    mylist$sy <- sd(mylist$ylist)
    mylist$cor <- cor(mylist$xlist, mylist$ylist)
    mylist$rms <- sqrt(1 - (mylist$cor)^2) * mylist$sy
    
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
      plaintheme +
      axistheme
    regline <- geom_abline(slope = mylist$b1, intercept = mylist$b0, col = "red")
    rmsline1a <- geom_abline(slope = mylist$b1, intercept = mylist$b0 + mylist$rms, col = "blue")
    rmsline1b <- geom_abline(slope = mylist$b1, intercept = mylist$b0 - mylist$rms, col = "blue")
    rmsline2a <- geom_abline(slope = mylist$b1, intercept = mylist$b0 +2* mylist$rms, col = "blue")
    rmsline2b <- geom_abline(slope = mylist$b1, intercept = mylist$b0 - 2*mylist$rms, col = "blue")
    if(input$RegLine == TRUE & input$RMSLine == TRUE) {
      p + regline + rmsline1a + rmsline1b + rmsline2a + rmsline2b
    } else if (input$RegLine == TRUE & input$RMSLine == FALSE) {
      p + regline
    } else if (input$RegLine == "FALSE" & input$RMSLine == TRUE) {
      p + rmsline1a + rmsline1b + rmsline2a + rmsline2b
    } else {p}
  })
  output$x_hist <- renderPlot({
    dat <- data.frame(mylist$xlist, mylist$ylist)
    p2<- ggplot(dat, aes(x = mylist.xlist)) +
      geom_histogram(aes(y = ..density..), fill = "gray81", col = "black",
                     binwidth = (max(mylist$xlist)-min(mylist$xlist))/10) +
      ggtitle("Histogram of X-Variable") +
      scale_x_continuous("X-Variable", 
                         breaks = round(seq(min(mylist$xlist), max(mylist$xlist), 
                                            (max(mylist$xlist)-min(mylist$xlist))/10)), 0) +
      scale_y_continuous("Proportion per X") +
      plaintheme +
      axistheme
    
    avline <- geom_vline(xintercept = mean(mylist$xlist), col = "red", size = 2)
    sdline1 <- geom_vline(xintercept = (mean(mylist$xlist) - sd(mylist$xlist)), col = "blue", size = 2)
    sdline2 <- geom_vline(xintercept = (mean(mylist$xlist) + sd(mylist$xlist)), col = "blue", size = 2)
    sdline3 <- geom_vline(xintercept = (mean(mylist$xlist) + 2*sd(mylist$xlist)), col = "seagreen", size = 2)
    sdline4 <- geom_vline(xintercept = (mean(mylist$xlist) - 2*sd(mylist$xlist)), col = "seagreen", size = 2)
    p3 <- p2 + avline +sdline1 + sdline2 + sdline3 + sdline4
    p4 <- p2 + avline + sdline1 + sdline2
    p5 <- p2 + avline + sdline3 + sdline4
    p6 <- p2 + avline
    p7 <- p2 + sdline1 + sdline2 + sdline3 + sdline4
    p8 <- p2 + sdline1 + sdline2
    p9 <- p2 + sdline3 + sdline4
    if(input$Xave == TRUE & input$Xsd == TRUE & input$Xsd2 == TRUE) {
      p3
    } else if (input$Xave == TRUE & input$Xsd == TRUE & input$Xsd2 == FALSE) {
      p4
    } else if (input$Xave == TRUE & input$Xsd == FALSE & input$Xsd2 == TRUE) {
      p5
    } else if (input$Xave == TRUE & input$Xsd == FALSE & input$Xsd2 == FALSE) {
      p6
    } else if (input$Xave == FALSE & input$Xsd == TRUE & input$Xsd2 == TRUE) {
      p7
    } else if (input$Xave == FALSE & input$Xsd == TRUE & input$Xsd2 == FALSE) {
      p8
    } else if (input$Xave == FALSE & input$Xsd == FALSE & input$Xsd2 == TRUE) {
      p9
    } else {
      p2
    }
  })
  output$y_hist <- renderPlot({
    dat <- data.frame(mylist$xlist, mylist$ylist)
    p2<- ggplot(dat, aes(x = mylist.ylist)) +
      geom_histogram(aes(y = ..density..), fill = "gray81", col = "black",
                     binwidth = (max(mylist$ylist)-min(mylist$ylist))/10) +
      ggtitle("Histogram of Y-Variable") +
      scale_x_continuous("Y-Variable", 
                         breaks = round(seq(min(mylist$ylist), max(mylist$ylist), 
                                            (max(mylist$ylist)-min(mylist$ylist))/10)), 0) +
      scale_y_continuous("Proportion per Y") +
      plaintheme +
      axistheme
    
    avline <- geom_vline(xintercept = mean(mylist$ylist), col = "red", size = 2)
    sdline1 <- geom_vline(xintercept = (mean(mylist$ylist) - sd(mylist$ylist)), col = "blue", size = 2)
    sdline2 <- geom_vline(xintercept = (mean(mylist$ylist) + sd(mylist$ylist)), col = "blue", size = 2)
    sdline3 <- geom_vline(xintercept = (mean(mylist$ylist) + 2*sd(mylist$ylist)), col = "seagreen", size = 2)
    sdline4 <- geom_vline(xintercept = (mean(mylist$ylist) - 2*sd(mylist$ylist)), col = "seagreen", size = 2)
    p3 <- p2 + avline +sdline1 + sdline2 + sdline3 + sdline4
    p4 <- p2 + avline + sdline1 + sdline2
    p5 <- p2 + avline + sdline3 + sdline4
    p6 <- p2 + avline
    p7 <- p2 + sdline1 + sdline2 + sdline3 + sdline4
    p8 <- p2 + sdline1 + sdline2
    p9 <- p2 + sdline3 + sdline4
    if(input$Yave == TRUE & input$Ysd == TRUE & input$Ysd2 == TRUE) {
      p3
    } else if (input$Yave == TRUE & input$Ysd == TRUE & input$Ysd2 == FALSE) {
      p4
    } else if (input$Yave == TRUE & input$Ysd == FALSE & input$Ysd2 == TRUE) {
      p5
    } else if (input$Yave == TRUE & input$Ysd == FALSE & input$Ysd2 == FALSE) {
      p6
    } else if (input$Yave == FALSE & input$Ysd == TRUE & input$Ysd2 == TRUE) {
      p7
    } else if (input$Yave == FALSE & input$Ysd == TRUE & input$Ysd2 == FALSE) {
      p8
    } else if (input$Yave == FALSE & input$Ysd == FALSE & input$Ysd2 == TRUE) {
      p9
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