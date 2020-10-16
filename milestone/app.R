#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(PPBDS.data)
library(ggplot2)

data <- PPBDS.data::qscores

view(data)
# User interface ----
ui <- fluidPage(
    titlePanel("Milestone #4 - Ang Sonam Sherpa"),
        mainPanel(
            
            
            tabsetPanel(
                tabPanel("About", 
                     h4("Project Update"),
                     p("I have not gotten too far on my project yet. I am still
                       mostly trying to decide what exactly I am going to do.
                       I realized it is rather hard to find a lot of data
                       concerning Nepal given its lack of technological
                       advancement. For the milestone due today, I have
                       collected important data from Wikipedia which will be
                       useful in the actual project later on."),
                     h4("Github repo"),
                     p("https://github.com/sherpaang/milestone4.git")
                     ),
            
                tabPanel("Bar Graph", plotOutput("ggplot")),
            
            tabPanel("Discussion",
                     titlePanel("Discussion Title"),
                     p("Tour of the modeling choices you made and 
              an explanation of why you made them"))
            
                    )
        )
)

?verbatimTextOutput

# Server logic ----
server <- function(input, output) {
    output$ggplot <- renderPlot({
        
        data %>%
            group_by(department) %>%
            summarize(total_department = sum(enrollment)) %>%
            select(department, total_department) %>%
            arrange(desc(total_department)) %>%
            head(10) %>%
            ggplot(aes(department, total_department)) +
            geom_col()
    
    })
    
}

# Run app ----
shinyApp(ui, server)

