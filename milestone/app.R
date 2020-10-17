
library(shiny)
library(tidyverse)
library(ggplot2)

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
    select(year, hdi)

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
                       concerning Nepal. For the milestone due today, I have
                       collected some data regarding the terend of population
                       growth and hdi in Nepal which should be helpful in my
                       final project."),
                     
                     h4("Github repo"),
                     p("https://github.com/sherpaang/milestone4.git")
                     ),
            
                tabPanel("Graphs",
                         "Population over time", plotOutput("pop"))
            
                    )
        )
)

glimpse(hdi)

# Server logic ----
server <- function(input, output) {
    output$pop <- renderPlot({
        
        pop %>%
            ggplot(aes(year, population)) +
            geom_col()
    })    
    
    
}

# Run app ----
shinyApp(ui, server)

