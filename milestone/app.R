
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(ggthemes)

# putting population data to pop. I changed all the column types to col_double
# initially because they were in integers.

pop <- read_csv("nepalindata.csv", col_type = cols(
    X1 = col_character(),
    `1952` = col_double(),
    `1961` = col_double(),
    `1971` = col_double(),
    `1981` = col_double(),
    `1991` = col_double(),
    `2001` = col_double(),
    `2011` = col_double()
    ))

# I chose the second row which had the population data I needed. I then used
# pivot longer to put all the years into a single column called year and all
# the population data to another single column. Finally, since the year column
# will now be in character, I changed the column time afterwards.

pop <- pop %>%
    slice(2) %>%
    pivot_longer(cols = c(`1952`, `1961`, `1971`, `1981`, `1991`, `2001`,
                          `2011`),
                 names_to = "year",
                 values_to = "population") %>%
    select(year, population) %>%
    mutate(year = as.numeric(year))

# I did the exact same thing I did for the population with this dataset as well.

hdi <- read_csv("nepalhdi.csv") %>%
    slice(2)

# I kept on getting an error where the data would just disappear when I assign
# each as numeric column types. So I decided to wrangle them as characters even
# though they were years; I later changed the whole year column to numeric
# using mutate.

hdi <- hdi %>%
    mutate(content = X1) %>%
    select(c(content, `1980`, `1985`, `1990`, `1995`, `2000`, `2001`,
             `2005`, `2006`, `2010`, `2011`, `2012`, `2013`,
             `2014`)) %>%
    pivot_longer(cols = c(`1980`, `1985`, `1990`, `1995`, `2000`, `2001`,
                          `2005`, `2006`, `2010`, `2011`, `2012`, `2013`,
                          `2014`),
                 names_to = "year",
                 values_to = "hdi") %>%
    mutate(year = as.numeric(year)) %>%
    mutate(hdi = as.numeric(hdi)) %>%
    select(year, hdi)

# User interface ----

ui <- fluidPage(
    titlePanel("Milestone #5 - Ang Sonam Sherpa"),
        mainPanel(
            
            tabsetPanel(
                
                tabPanel("About", 
                     
                     h4("Project Update"),
                     p("I am currently trying to figure out how I will extract
                       the data that I need from the IPUMS extract I downloaded.
                       The data is very confusing, and it seems like it will
                       require a lot of wrangling.
                       
                       Since I already had a graph in the previous milestone, I
                       just resubmitted the same one with a minor change. I
                       created a graph with both hdi and population for this
                       milestone."),
                     
                     h4("Github repo"),
                     p("https://github.com/sherpaang/milestone4.git")
                     ),
            
                tabPanel("Graphs",
                         "Population and HDI over time - Nepal",
                         plotOutput("data"))
            
                    )
        )
)

glimpse(hdi)

# Server logic ----
server <- function(input, output) {
    output$data <- renderPlot({
        
        p1 <- pop %>%
            ggplot(aes(year, population)) +
            geom_line(color = 'orange', size = 1.5) +
            ggtitle("Population over time") +
            scale_y_continuous(
                breaks = c(5000000, 10000000, 15000000, 20000000,
                           25000000, 30000000),
                label = c("5 million", "10 million", "15 million", "20 million",
                          "25 million", "30 million")) +
            scale_x_continuous(breaks = c(1950, 1965, 1980, 1995, 2010)) +
            theme_ipsum()
        
        # I found about this function online while looking for ways to create
        # two y axes. This seems like a far more efficient way of doing so.
            
        p2 <- hdi %>%
            ggplot(aes(year, hdi)) +
            geom_line(color = "blue",size = 1.5) +
            ggtitle("HDI over time") +
            theme_ipsum()
        
        # To display both charts side by side
        
        p1 + p2
        
    })     
}

# Run app ----
shinyApp(ui, server)

