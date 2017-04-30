library(shiny)
library(scales)

shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    if(input$distribution1=='normal'){
      x <- seq(input$mn,input$mx,length.out=101)
      y <- dnorm(x, input$meanN, input$sdevN)}
    if(input$distribution1=='uniform'){
      x <- seq(input$mn,input$mx,length.out=101)
      y <- dunif(x, input$minU, input$maxU)}
    if(input$distribution1=='poisson'){
      x <- seq(input$mn,input$mx,by=1)
      y <- dpois(x, input$rateP)}
    if(input$distribution1=='binomial'){
      x <- seq(input$mn,input$mx,by=1)
      y <- dbinom(x, input$sizeB, input$probB)}
    if(input$distribution1=='exponential'){
      x <- seq(input$mn,input$mx,length.out=101)
      y <- dexp(x, input$rateX)}
    if(input$distribution1=='student (t)'){
      x <- seq(input$mn,input$mx,length.out=101)
      y <- dt(x, input$dfT, input$ncenT)}
    if(input$distribution1=='chi-squared'){
      x <- seq(input$mn,input$mx,length.out=101)
      y <- dchisq(x, input$dfC, input$ncenC)}
    
    plot(x, y, type="l", col="red", lwd=2, ylim=c(0,input$my),
         ylab='probability density (x)')
    
    if(input$numdist > 1){
      if(input$distribution2=='normal'){
        x2 <- seq(input$mn,input$mx,length.out=101)
        y2 <- dnorm(x2, input$meanN2, input$sdevN2)}
      if(input$distribution2=='uniform'){
        x2 <- seq(input$mn,input$mx,length.out=101)
        y2 <- dunif(x2, input$minU2, input$maxU2)}
      if(input$distribution2=='poisson'){
        x2 <- seq(input$mn,input$mx,by=1)
        y2 <- dpois(x2, input$rateP2)}
      if(input$distribution2=='binomial'){
        x2 <- seq(input$mn,input$mx,by=1)
        y2 <- dbinom(x2, input$sizeB2, input$probB2)}
      if(input$distribution2=='exponential'){
        x2 <- seq(input$mn,input$mx,length.out=101)
        y2 <- dexp(x2, input$rateX2)}
      if(input$distribution2=='student (t)'){
        x2 <- seq(input$mn,input$mx,length.out=101)
        y2 <- dt(x2, input$dfT2, input$ncenT2)}
      if(input$distribution2=='chi-squared'){
        x2 <- seq(input$mn,input$mx,length.out=101)
        y2 <- dchisq(x2, input$dfC2, input$ncenC2)}
      
      lines(x2, y2, col="green", lwd=2)
    }
    if(input$numdist > 2){
      if(input$distribution3=='normal'){
        x3 <- seq(input$mn,input$mx,length.out=101)
        y3 <- dnorm(x3, input$meanN3, input$sdevN3)}
      if(input$distribution3=='uniform'){
        x3 <- seq(input$mn,input$mx,length.out=101)
        y3 <- dunif(x3, input$minU3, input$maxU3)}
      if(input$distribution3=='poisson'){
        x3 <- seq(input$mn,input$mx,by=1)
        y3 <- dpois(x3, input$rateP3)}
      if(input$distribution3=='binomial'){
        x3 <- seq(input$mn,input$mx,by=1)
        y3 <- dbinom(x3, input$sizeB3, input$probB3)}
      if(input$distribution3=='exponential'){
        x3 <- seq(input$mn,input$mx,length.out=101)
        y3 <- dexp(x3, input$rateX3)}
      if(input$distribution3=='student (t)'){
        x3 <- seq(input$mn,input$mx,length.out=101)
        y3 <- dt(x3, input$dfT3, input$ncenT3)}
      if(input$distribution3=='chi-squared'){
        x3 <- seq(input$mn,input$mx,length.out=101)
        y3 <- dchisq(x3, input$dfC3, input$ncenC3)}
      lines(x3, y3, type="l", col="blue", lwd=2)
      }
    
  })
  
})
