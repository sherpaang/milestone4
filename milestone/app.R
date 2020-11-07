
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(ggthemes)
library(janitor)

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


# putting the data about the trend of gender employment in the Nepali
# civil service for the last decade.

ncsapplicantstrend <- read.csv("ncstrend2007to2018.csv") %>%
    clean_names()


# data from the academic paper: Nepali women in Politics (page 83)

# constituent assembly refers to the legislative parliament formed to write
# the final constitution. This was later named the legislative parliament.

electionhistory <- tibble(
    year = c(1959, 1986, 1991, 1999, 2008, 2013),
    type = c("parliamentary", "parliamentary", "parliamentary",
             "parliamentary", "constituent assembly",
             "constituent assembly"),
    womencontesting = c(6, NA, NA, NA, NA, NA),
    totalelected = c(109, 140, 205, 205, 240, 240),
    womenelected = c(0, 3, 7, 23, 40, 10),
    localtotal = c(NA, NA, NA, 4146, NA, NA),
    localwomen = c(NA, NA, NA, 806, NA, NA)
)


# User interface ----

ui <- fluidPage(
    titlePanel("Milestone #6 - Ang Sonam Sherpa"),
        mainPanel(
            
            tabsetPanel(
                
                tabPanel("About", 
                     
                     h4("Project Update"),
                     p("I am still looking for datasets currently. I have been
                      much more successful this week in looking for data. I
                      still could not find a good way to extract data from a
                      pdf to a tibble. I decided then to convert the pdf to an
                      excel file initially online, and then make a new csv file
                      altogether - which worked out pretty well.
                       
                      For the next one, I will look more into finally deciding
                       what I want to do with the final project ultimately. I
                       will explore more datasets and see what is possible to be
                       done in the time remaining."),
                     
                     h4("Github repo"),
                     p("https://github.com/sherpaang/milestone4.git")
                     ),
            
                tabPanel("Graphs",
                         "Population and HDI over time - Nepal",
                         plotOutput("data")),
                
                tabPanel("Nepal Civil Service - new graph",
                        plotOutput("data2")),
                
                tabPanel("Nepal Civil Service - new graph",
                         plotOutput("data3"))
            
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
    
    output$data2 <- renderPlot({
        
    
      ncsapplicantstrend %>%
            ggplot(aes(fiscal_year, total)) +
            geom_col(fill = "purple") +
            scale_x_discrete(labels = c(2008:2019)) + 
            labs(title = "Total Applicants to NCS")
        
        
        })
    
    output$data3 <- renderPlot({
        
        ncsapplicantstrend %>%
            select(fiscal_year, female_percent, male_percent) %>%
            pivot_longer(cols = c(female_percent, male_percent),
                         names_to = "gender",
                         values_to = "percent") %>%
            group_by(fiscal_year, gender) %>%
            ggplot(aes(fiscal_year, percent, group = gender, color = gender)) +
            geom_line() +
            geom_point() +
            scale_x_discrete(labels = c(2008:2019)) +
            labs(title = "Trend of Applicants to NCS by Gender")
        
    })
 
               
}

# Run app ----
shinyApp(ui, server)

