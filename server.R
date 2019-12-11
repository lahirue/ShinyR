#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
     recVal <- reactiveValues()
     recVal$res <-  numeric(0) #data.frame(xVal = NA)
     recVal$itr <- 0
     recVal$accProb <- 0
     recVal$xValue <- 0
     recVal$propVal <- 0
     recVal$rejected <- FALSE
    
    
    ar <- reactive({
        nIterations <- input$numeric
        
        if(recVal$itr < nIterations)
        {
            recVal$rejected <- TRUE
            while(recVal$rejected)
            {
               
                y <- runif(1) # g(y) ~ uniform(0,1)
                
                u <- runif(1)
                
                recVal$accProb <- dbeta(y,2,5)/2.4576
                
                recVal$propVal  <- y
                
                if(u < recVal$accProb) # c <- 2.4576 and g(y) = 1
                {
                    #points(y,u*2.4576, col="red")
                    recVal$res <- c(recVal$res,y)
                    recVal$xValue <- y
                    recVal$rejected <- FALSE 
                }                 
            }
            recVal$itr <- recVal$itr + 1
            
        }
        #res
    })
    
    
    arProp <- reactive(
        {
            y <- runif(1) # g(y) ~ uniform(0,1)
            
            u <- runif(1)
            
            recVal$accProb <- dbeta(y,2,5)/2.4576
            
            recVal$propVal  <- y
            
            if(u < recVal$accProb) # c <- 2.4576 and g(y) = 1
            {
                #points(y,u*2.4576, col="red")
                recVal$res <- c(recVal$res,y)
                recVal$xValue <- y
                recVal$rejected <- FALSE 
            } 
        }
    )
    
   
    
    
    # forward <- function()
    # {
    #     
    # }
    # 
    
    session<-reactiveValues()
    session$timer<-reactiveTimer(Inf)
    
    observeEvent(input$start,{
        session$timer<-reactiveTimer(300)
        observeEvent(session$timer(),{
            ar()
        })
    })
    
    
    output$tDistPlot <- renderPlot(
        {
            p <- qplot(x, geom = "blank",ylab = "f(x)",xlab = "x") + theme_classic()
            p <- p + geom_vline(xintercept = recVal$propVal, linetype="dotted", 
                              color = "blue", size=1.5)
            x <- seq(0,1,length.out = 100)
            stat <- stat_function(aes(x = x, y = ..y..), fun = dbeta, colour="red", n = 100,
                                  args = list(shape1 = 2, shape2 = 5))  
             
            return(p + stat )
        }
    )
    
    output$hist <- renderPlot({
        
        nIterations <- input$numeric
        
        # if(length(recVal$res)==0)
        # {
        #     p <- qplot(x, geom = "blank") + theme_classic()
        #     
        #     x <- seq(0,1,length.out = 100)
        #     stat <- stat_function(aes(x = x, y = ..y..), fun = dbeta, colour="red", n = 100,
        #                           args = list(shape1 = 2, shape2 = 5))   
        #     stat <- stat 
        #     return(p + stat)
        #     
        #     
        #    
        # 
        # }
        
        
        df <- data.frame(xVal = recVal$res)
        
        # x <- seq(0,1,length.out = length(recVal$res))
        # stat <- stat_function(aes(x = x, y = ..y..), fun = dbeta, colour="red", n = 100,
        #                       args = list(shape1 = 2, shape2 = 5)) 
        # 
        # 
        
        
        
        
        
        
        #p <- ggplot(df, aes(x=xVal)) + geom_histogram(aes(y=..density..),bins = 30) + theme_classic()
        p <- ggplot(df, aes(x=xVal)) + geom_histogram( bins = 30) + theme_classic()
        
        
        
        
        
        
        #p + stat  + 
        
          p #+ stat  
    })
    
    observeEvent(input$play,{
          ar()
    })
    
    observeEvent(input$stop,{
        session$timer<-reactiveTimer(Inf)
    })
    
    
    observeEvent(input$reset,{
        recVal$itr <- 0
        recVal$res <-  numeric(0)
    })
    
    output$Itr<-renderText({
        paste("Iteration - ",recVal$itr,"" )
    })
    
    output$accProb <-renderText({
        paste("Acceptance Probability - ", round(recVal$accProb,2),"" )
    }) 
    
    output$xVal <-renderText({
        paste("x - ", round(recVal$xValue,2),"" )
    }) 
    
        
    observeEvent(input$nextItr,{
        ar()
    })
})
