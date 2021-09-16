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

# data prep ----
#................................................
# DATA PREP:
# select a few countries, grab relevant WPP
# data, and calculate the projected populations
# assuming an immediate change to replacement-
# level TFR in 2020
#................................................

sel_codes = c('Republic of Korea'        = 410,
              'Serbia'                   = 688,
              'Norway'                   = 578,
              'Turkey'                   = 792,
              'South Africa'             = 710,
              'Guatemala'                = 320,
              'Iraq'                     = 368,
              'China'                    = 156,
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

tfr_info = tfr %>% 
            filter(country_code %in% sel_codes) %>% 
            select(country_code, name, tfr=`2015-2020`)

pop_info = popF %>% 
             filter(country_code %in% sel_codes) %>% 
             select(country_code, name, pop=`2020`) %>% 
             group_by(country_code, name) %>% 
             summarize( Nx = list(pop)) 
    
Fx_info = percentASFR %>% 
            filter(country_code %in% sel_codes) %>% 
            select(country_code, name,age, pct=`2015-2020`) %>% 
            left_join(tfr_info) %>% 
            mutate( asfr = tfr * pct/500) %>% 
            select(country_code, name, asfr) %>% 
            group_by(country_code, name) %>% 
            summarize( Fx = list(c(0,0,0,asfr,rep(0,11)))) 

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
           summarize( Lx  = list(calc_Lx(lx)))  

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
     LL  = unlist(Lx_values)
     FF  = unlist(Fx_values)
    # assumes that 4-10th age groups are 15-19... 45-49
    0.4886 * sum(LL*FF)
}

calc_Sx = function(Lx_values, S100) {
    LL  = unlist(Lx_values)
    ix  = 1:20
    tmp = LL[ix+1]/LL[ix]   #S0...S95 
    return(c(tmp, S100))
}

calc_Kx = function(Lx_values, Fx_values) {
  LL = unlist(Lx_values)
  FF = unlist(Fx_values)
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

# calculate projection for population by age 0-4,5-9,...,95-99,100+
# in 2020, 2025, ..., 2120

calc_projection = function(Nx, Sx, Kx, NRR) {
  SS = unlist(Sx)      # survival multipliers
  KK = unlist(Kx)/NRR  # kid multipliers AT REPLACEMENT LEVEL
  NN = unlist(Nx)      # 2020 pop
  
  Proj = matrix(NA, 21, 31, 
                dimnames=list(seq(0,100,5), seq(2020,2170,5)))
  
  Proj[,'2020'] = NN
  
  for (y in seq(2025,2170,5)) {
    thisy = paste(y)
    lasty = paste(y-5)
    
    Proj[ 1, thisy] = sum( KK * Proj[,lasty])
    Proj[-1, thisy] = SS[1:20] * Proj[1:20,lasty]
    Proj[21, thisy] = Proj[21,thisy] + SS[21] * Proj[21,lasty]
  }
  
  return(Proj)
}

D$projection = pmap(select(D, Nx,Sx,Kx,NRR), calc_projection)

# shiny app ----
# .....................................................
#  set up inputs and outputs for Shiny app
# .....................................................


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
            
            sliderInput(inputId = 'year',
                        label = 'Select a year',
                        value = 2020,
                        min   = 2020,
                        max   = 2170,
                        sep   = '',
                        step  = 5,
                        round = FALSE,
                        animate = animationOptions(
                          loop=TRUE,
                          playButton = 'Project at Replacement Level Fertility',
                          pauseButton = NULL
                        ))
            
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

  
    output$PopPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        age    <- seq(0,100,5)

        # fpop is the matrix of projectsions: 
        # 21 ages 0,5,...100 by 
        # 21 years 2020, 2025, ..., 2120

        tmp = D %>% 
                filter(name == input$country)
        
        fpop = tmp$projection[[1]] 
        
        yr = as.character(input$year)
        
        this_title = paste(toupper(input$country),
                           'Female Population',yr,
                           '\nTFR in 2020=',sprintf("%.2f",tmp$tfr), 
                           '\nNRR in 2020=',sprintf("%.2f",tmp$NRR))
        
        pop_start = sum( fpop[,'2020'])
        pop_now   = sum( fpop[, yr])
        
        this_info = paste0('2020 Population = ',
                          sprintf("%.0f", pop_start),
                          '\n',yr,' Population = ',
                          sprintf("%.0f", pop_now),
                          '\n',
                          sprintf("%.0f",100*pop_now/pop_start),
                          '% of original population')

        # draw the histogram with the specified number of bins
        ggplot() +
             aes(x=age, y=fpop[,yr]) +
             geom_point() +
             geom_line() +
             geom_line(aes(x=age, y=fpop[,'2020']),
                       color='grey',size=4,alpha=.40) +
             labs( title= this_title,
                   x = 'Five-Year Age Group',
                   y = "Population (1000s)",
                   caption = "Source: UN World Population Prospects, 2019") +
             scale_y_continuous(limits=range(0,fpop)) +
             scale_x_continuous(breaks=seq(0,100,10)) +
             theme_bw() +
             geom_text( aes(x=12, y=0.30*fpop['0','2020'],
                        label=this_info), size=6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
