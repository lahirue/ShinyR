#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Acceptance -Rejection method"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("numeric",
                        "Number of iterations:",
                        min = 100,
                        max = 10000,
                        value = 500),
            actionButton("start","Start"),
            actionButton("stop","Stop"),
            actionButton("nextItr","Next Iteration"),
            
            actionButton("reset","Reset") 
            
            #uiOutput("resetbutton")
            
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            h4("Generating random numbers from Beta(2,5)"),
            #plotOutput("tDistPlot"),
            
            fluidRow(
                column(4,textOutput("Itr")),
                column(4,textOutput("xVal")),
                column(4,textOutput("accProb")) 
                
                
            ),
            
        
            
            fluidRow(
                
                column(6,
                       
                       plotOutput('tDistPlot')
                ),
                column(6,
                       plotOutput('hist')  
                )
                
            )
        )
    )
))
