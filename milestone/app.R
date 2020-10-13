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
    titlePanel("Visualization of Number of Enrolles in the top 10
               departments in Harvard"),
        mainPanel(
            tabsetPanel(
                tabPanel("Bar Graph", plotOutput("ggplot"))
                

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

