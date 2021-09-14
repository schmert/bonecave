#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wpp2019)
library(tidyverse)

sel_codes = c('China'                    = 156,
              'United States of America' = 840,
              'India'                    = 356,
              'Egypt'                    = 818,
              'Ethiopia'                 = 231,
              'Nigeria'                  = 566)
                
sel_names = names(sel_codes)

# make a tibble with one row per selected country
# and list-columns that contain FEMALE population, mx, etc

data(popF)

tmp = popF %>% 
        filter(country_code %in% sel_codes) %>% 
        select(country_code, name, pop=`2020`) %>%
        nest(pop=c(pop) ) 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Population Momentum\nat Replacement-Level Fertility"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId  = 'country', 
                        label    = 'Select a Country',
                        choices  = sel_names,
                        selected = sel_names[1],
                        width    = '40%'),
            actionButton(inputId = 'minus5',
                         label   = '-5 years',
                         width   = '15%'),
            actionButton(inputId = 'plus5',
                         label   = '+5 years',
                         width   = '15%'),
            

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("PopPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observeEvent(input$minus5, {
        session$sendCustomMessage(type = 'testmessage',
                                  message = '-5')
    })
    
    observeEvent(input$plus5, {
        session$sendCustomMessage(type = 'testmessage',
                                  message = '+5')
    })
    
    
    output$PopPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- seq(0,100,5)

        fpop = tmp %>% 
                filter( name== input$country) %>% 
                unnest(cols=c(pop)) %>% 
                add_column(age = seq(0,100,5))
        
        # draw the histogram with the specified number of bins
        ggplot(data=fpop) +
             aes(x=age, y=pop) +
             geom_point() +
             geom_line() + 
             labs( title= paste(input$country,'Females'),
                   x = 'Five-Year Age Group',
                   y = "Population (1000s)",
                   caption = "Source: UN World Population Prospects, 2019") +
             scale_y_continuous(limits=range(0,fpop$pop)) +
             scale_x_continuous(breaks=seq(0,100,10)) +
             theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
