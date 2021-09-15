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
#    Kx (21 x 1, first row of Leslie matrix)

# prepare a small data frame with age distributions
# and Leslie multipliers, for the selected countries

data(popF, tfr, percentASFR, mxF)

# start a tibble w/ country_code, name, tfr

D = tfr %>% 
      filter(country_code %in% sel_codes) %>% 
      select(country_code, name, tfr=`2015-2020`)

# add a list column 
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

mx100_info = mxF %>% 
            filter(country_code %in% sel_codes,
                   age==100) %>% 
            select(country_code, name,m100=`2015-2020`) %>% 
            mutate(S100 = exp(-5*m100))


# join the (list-column) datasets by country

D = tfr_info %>% 
            inner_join(pop_info) %>% 
            inner_join(Fx_info) %>% 
            inner_join(Lx_info) %>% 
            inner_join(mx100_info) %>% 
            as_tibble()

# calculate NRR and the Leslie Matrix elements

calc_NRR = function(Lx_values,Fx_values) {
    # assumes that 4-10th age groups are 15-19... 45-49
    0.4886 * sum(unlist(Lx_values)[4:10] * 
                 unlist(Fx_values))
}

calc_Sx = function(Lx_values, S100) {
    L = unlist(Lx_values)
    tmp = tail(L,-1) / head(L,-1)   #S0...S95 
    return(c(tmp, S100))
}

calc_Kx = function(Lx_values, Fx_values) {
  LL = unlist(Lx_values)
  FF = c( 0,0,0, unlist(Fx_values), rep(0,11))
  # start with all zeroes
  tmp = 0*LL
  # age groups 10-14 [3rd] through 45-49 [10th] element will have non-zero values
  ix = 3:10
  tmp[ix] = 0.4886 * LL[1]/2 * (FF[ix] + FF[ix+1] * LL[ix+1]/LL[ix])
  return(tmp)
}

D$NRR = map2_dbl(D$Lx, D$Fx, calc_NRR)
D$Sx  = map2(D$Lx, D$S100, calc_Sx)
D$Kx  = map2(D$Lx, D$Fx, calc_Kx)

#===============================================================

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

        fpop = D %>% 
                filter( name== input$country) %>% 
                unnest(cols=c(Nx)) %>% 
                add_column(age = seq(0,100,5))
  
        tmp = D %>% 
            filter(name == input$country)    
        
        this_title = paste(toupper(tmp$name),
                           'Female Population\nTFR=',sprintf("%.2f",tmp$tfr), 
                           ', NRR=',sprintf("%.2f",tmp$NRR))
        
        # draw the histogram with the specified number of bins
        ggplot(data=fpop) +
             aes(x=age, y=pop) +
             geom_point() +
             geom_line() + 
             labs( title= this_title,
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
