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
# and list-columns as follows
# country_code, name, TFR, NRR,
#    Nx (21x1), Lx (21x1), Fx (21x1), 
#    Sx (21 x 1, survival elements of Leslie matrix)
#    Gx (21 x 1, first row of Leslie matrix)

# prepare a small data frame with age distributions
# and Leslie multipliers, for the selected countries

data(popF, tfr, percentASFR, mxF)

tfr_info = tfr %>% 
             filter(country_code %in% sel_codes) %>% 
             select(country_code, name, tfr=`2015-2020`)

pop_info = popF %>% 
             filter(country_code %in% sel_codes) %>% 
             select(country_code, name, pop=`2020`) %>% 
             nest( Nx = c(pop))
    
Fx_info = percentASFR %>% 
            filter(country_code %in% sel_codes) %>% 
            select(country_code, name,age, pct=`2015-2020`) %>% 
            left_join(tfr_info) %>% 
            mutate( asfr = tfr * pct/500) %>% 
            select(country_code, name, asfr) %>% 
            nest( Fx = c(asfr))

# function to approx Lx for (0,5,10,...,100) from
# lx for (0,*1*,5,10,...,100)

calc_Lx = function(lx) {
    n   = length(lx)-1
    tmp = rep(NA, n)

    tmp[1]       = 1 * (lx[1] + lx[2])/2 + 
                   4 * (lx[2] + lx[3])/2          #  1L0 + 4L1
    tmp[2:(n-1)] = 5 * (lx[3:n] + lx[4:(n+1)])/2  #  5Lx
    tmp[n]       = 5 * (lx[n+1] + 0)/2            #  5L95
    return(tmp)
}

Lx_info = mxF %>% 
           filter(country_code %in% sel_codes) %>% 
           select(country_code, name,age, mx=`2015-2020`) %>% 
           group_by(country_code, name) %>% 
           mutate(hx = diff(c(age,Inf)) * mx,
                  Hx = cumsum(c(0,head(hx,-1))),
                  lx = exp(-Hx)) %>% 
           summarize( Lx  = calc_Lx(lx)) %>% 
           ungroup() %>% 
           nest( Lx = c(Lx))


# join the (list-column) datasets by country

sel_data = tfr_info %>% 
            inner_join(pop_info) %>% 
            inner_join(Fx_info) %>% 
            inner_join(Lx_info) %>% 
            as_tibble()

# calculate NRR and the Leslie Matrix elements



# tmp1 = popF %>% 
#         filter(country_code %in% sel_codes) %>% 
#         select(country_code, name, pop=`2020`) %>% 
#         nest( female_pop = c(pop))
# 
# 
# # actually want to calculate the Lx terms....
# tmp3 = mxF %>% 
#     filter(country_code %in% sel_codes) %>% 
#     select(country_code, name, age, mx=`2015-2020`) %>% 
#     nest(mort = c(age, mx))
# 
# tmp = tmp1 %>% 
#      left_join(tmp2) %>% 
#      left_join(tmp3)

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
            
            # actionButton(inputId = 'minus5',
            #              label   = '-5 years',
            #              width   = '15%'),
            # 
            # actionButton(inputId = 'plus5',
            #              label   = '+5 years',
            #              width   = '15%'),
            

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("PopPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # tx = ''
    # 
    # observeEvent(input$minus5, {
    #    tx = 'MINUS'
    # })
    # 
    # observeEvent(input$plus5, {
    #    tx = 'PLUS'
    # })
    # 
    
    output$PopPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- seq(0,100,5)

        fpop = sel_data %>% 
                filter( name== input$country) %>% 
                unnest(cols=c(Nx)) %>% 
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
