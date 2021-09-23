#--------------------------------------
# Carl Schmertmann
# 19 Sep 2021
#
# interactive display of 
#     log mortality rates
#     survival probabilities (lx)
#     death fractions (dx)
# for any given set of linear spline coefficients in the
# TOPALS relational mortality model
#
# As in the app for the Brass logit model, 
# the user can select one of three standard
# schedules from the HMD (all for both sexes combined): 
#    Sweden   1880 
#    Portugal 1950 
#    Japan    2000 
#--------------------------------------

library(shiny)
library(shinyjs)
library(tidyverse)
library(splines)
library(bslib)

# load and process example data ----

#............................................................
# df with 3 (smoothed standards): 
# Norway females 2010-2019, 
# Poland males 1960-1969,
# Taiwan both sexes 1990-1999
#............................................................


df = structure(list(age = 0:99, 
        `Norway Female 2010` = 
                   c(-6.1374, -8.5625, -9.3369, -9.3939, -9.492, 
                    -9.6903, -9.8729, -9.9491, -9.9287, -9.8464, 
                    -9.7329, -9.6024, -9.4646, -9.3245, -9.1667,
                    -8.9705, -8.7304, -8.5002, -8.3485, -8.321, 
                    -8.3704, -8.4266, -8.4358, -8.4100, -8.3774, 
                    -8.3583, -8.3407, -8.3043, -8.2365, -8.1558,
                    -8.0880, -8.0505,  -8.025, -7.9843, -7.9099,
                    -7.8185, -7.7355, -7.6779, -7.6289, -7.5637, 
                    -7.4651, -7.3475, -7.2333, -7.1383, -7.0528, 
                    -6.9607, -6.8504, -6.7296, -6.6105, -6.5021, 
                    -6.3997, -6.2955, -6.1841, -6.0711, -5.9648, 
                    -5.8712, -5.7872, -5.7075, -5.6271, -5.5420, 
                    -5.4486, -5.3453, -5.2379, -5.1345, -5.0406,
                    -4.9516, -4.8605, -4.7622, -4.6595, -4.5570, 
                    -4.4581, -4.3603, -4.2595, -4.1527, -4.0400,
                    -3.9227, -3.8015, -3.6769, -3.5493, -3.4192, 
                    -3.2874, -3.1552, -3.0232, -2.8911, -2.7579, 
                    -2.6229, -2.4849, -2.3427, -2.1965, -2.0508, 
                    -1.9117, -1.7834, -1.6630, -1.5464, -1.4302, 
                    -1.3155, -1.2046, -1.0993, -0.9998, -0.9060), 
        `Poland Male 1960` = 
                  c(-2.9870, -5.7235, -6.7299, -6.9225, -7.0550, 
                    -7.2305, -7.3893, -7.4868, -7.5383, -7.5740, 
                    -7.6147, -7.6439, -7.6356, -7.5697, -7.4483, 
                    -7.2797, -7.0759, -6.8648, -6.6784, -6.5411,
                    -6.4467, -6.3814, -6.3332, -6.2981, -6.2741, 
                    -6.2585, -6.2461, -6.2315, -6.2100, -6.1808, 
                    -6.1443, -6.1014, -6.0551, -6.0091, -5.9657, 
                    -5.9217, -5.8727, -5.8156, -5.7532, -5.6897, 
                    -5.6279, -5.5656, -5.4993, -5.4267, -5.3507, 
                    -5.2755, -5.2036, -5.1307, -5.0509, -4.9603, 
                    -4.8631, -4.7655, -4.6720, -4.5802, -4.4860, 
                    -4.3866, -4.2832, -4.1787, -4.0751, -3.9733, 
                    -3.8736, -3.7762, -3.6798, -3.5832, -3.4854, 
                    -3.3877, -3.2922, -3.2001, -3.1112, -3.0246, 
                    -2.9395, -2.8548, -2.7692, -2.6823, -2.5951, 
                    -2.5094, -2.4259, -2.3422, -2.2551, -2.1627, 
                    -2.0684, -1.9766, -1.8909, -1.8097, -1.7302, 
                    -1.6503, -1.5710, -1.4941, -1.4209, -1.3510, 
                    -1.2837, -1.2181, -1.1533, -1.0883, -1.0226, 
                    -0.9586, -0.8991, -0.8459, -0.7969, -0.7488), 
        `Taiwan Both 1990` = 
                  c(-5.0822, -6.7675, -7.4261, -7.6166, -7.7929, 
                    -7.9897, -8.1373, -8.1876, -8.1803, -8.1768, 
                    -8.2154, -8.2403, -8.1726, -7.9606, -7.6619, 
                    -7.3614, -7.1299, -6.9821, -6.9184, -6.9306, 
                    -6.9748, -6.9981, -6.9633, -6.8955, -6.835, 
                    -6.8112, -6.8075, -6.7961, -6.7567, -6.699, 
                    -6.6401, -6.5925, -6.5502, -6.5023, -6.4411, 
                    -6.3714, -6.3011, -6.2360, -6.1737, -6.1096, 
                    -6.0404, -5.9681, -5.8957, -5.8256, -5.756, 
                    -5.6846, -5.6092, -5.5307, -5.4501, -5.3688, 
                    -5.2878, -5.2079, -5.1297, -5.0522, -4.9738, 
                    -4.8934, -4.8109, -4.7264, -4.6403, -4.5524, 
                    -4.4630, -4.3723, -4.2811, -4.1906, -4.1013, 
                    -4.0124, -3.9222, -3.8299, -3.7357, -3.6405, 
                    -3.5449, -3.4478, -3.3480, -3.2449, -3.1407, 
                    -3.0384, -2.9394, -2.8401, -2.7354, -2.6221, 
                    -2.5058, -2.3937, -2.2915, -2.1969, -2.1059, 
                    -2.0152, -1.9247, -1.8351, -1.7472, -1.6629, 
                    -1.5843, -1.5122, -1.4422, -1.3685, -1.2873, 
                    -1.203,  -1.1222, -1.0496, -0.9834, -0.9203)), 
   class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -100L))

# define functions used in server ----
#............................................................

knot_positions = c(0,1,10,20,40,70)
B              = splines::bs( 0:99, knots=knot_positions, degree=1 )
K              = ncol(B) 

TOPALS = function(std, alpha) {
  
    logmx = df[[ std ]] + B %*% alpha
    
    # fix this on second pass...quick and dirty way
    # to reuse old Brass code is to have this function
    # output *lx* column
    
    mx = exp(logmx)
    Hx = head( cumsum( c(0,mx)), -1)
    lx = exp(-Hx) 
    dx = c( -diff(lx), tail(lx,1))
    return( tibble( age=0:99,mx, logmx, Hx, dx, lx) )
}

# functoin to make a side-by-side pair of sliders
sliderPair <- function(k1,k2,w=4) {
  if (is.na(k2)) {
  fluidRow(
    column(w,sliderInput(inputId=paste0('a',k1),
                label = paste0('α',k1),
                min = -1.0,
                max = +1.0,
                step = 0.10,
                value=0)),
    column(w, actionButton(inputId='reset',
                           label='Reset all α'))
        ) # fluidRow
  } else {
    fluidRow(
      column(w,sliderInput(inputId=paste0('a',k1),
                           label = paste0('α',k1),
                           min = -1.0,
                           max = +1.0,
                           step = 0.10,
                           value=0)),
      column(w,sliderInput(inputId=paste0('a',k2),
                           label = paste0('α',k2),
                           min = -1.0,
                           max = +1.0,
                           step = 0.10,
                           value=0))
    ) # fluidRow
  }

}    
    
# UI layout ----
# Define UI for application that draws a histogram
ui <- fluidPage(
   
  theme = bslib::bs_theme(
    bootswatch = "spacelab", 
    base_font = font_google('Roboto Mono') 
  ),
  
  useShinyjs(),
  
   # Application title
   titlePanel("TOPALS Relational Model"),
   
   
  
   sidebarLayout(

     sidebarPanel( 
       fluidRow(
         column(6, radioButtons(inputId='std_select',
                                label='Standard Schedule',
                                choices = tail( names(df),-1)),
                selected = names(df)[2]),
         column(6, radioButtons(inputId='plot_select',
                                label='Data to Plot',
                                choices = c('logmx','lx','dx'),
                                selected='logmx'))
       ),
       
          sliderPair(1,2,w=5),
          sliderPair(3,4,w=5),
          sliderPair(5,6,w=5),
          sliderPair(7,NA,w=5)

      ),  #sidebarPanel

      # Show a plot of log mortality
      mainPanel(
         plotOutput(outputId="main_plot", height=600)
      )
 
   ) # sidebarLayout
) # fluid

# Server actions ----
server <- function(input, output) {
   
  observeEvent(input$reset, {
    for (k in 1:K) { 
      shinyjs::reset(paste0('a',k))
    }  
  })
  
  output$main_plot <- renderPlot({

    alpha = c(input$a1,input$a2,input$a3,input$a4,
              input$a5,input$a6,input$a7)
    
    std   = input$std_select
    
    plot =  input$plot_select
    
    legend_text = c("Standard","TOPALS")
    
    # calculate log mortality and baseline
    transformed   = TOPALS(std,alpha)
    baseline      = TOPALS(std,rep(0,7))
    
    n              = rep(1,100)
    age_dx         = baseline$age + n/2
    

    if (plot == 'logmx') {
      
      alpha_info  = tibble(age = c(knot_positions,100), alpha=alpha)
      offset_info = tibble(age=0:100, y=c(as.numeric(B %*% alpha),NA))
    
      tmp = data.frame( age   = rep(baseline$age,2),
                        sched = rep(legend_text,c(100,100)),
                        logm  = c(baseline$logmx,transformed$logmx))
        
       G =  ggplot(data=tmp, aes(x=age, y=logm, 
                            color = sched, 
                            alpha = sched)) +
#          geom_point(size=2) +
          geom_line(lwd=3) +
          scale_alpha_manual(guide=FALSE,
                             values=c(0.8,0.5)) +
          scale_color_manual(values=c('darkgrey','red')) +
          scale_x_continuous(limits=c(0,100),
                             breaks=seq(0,100,10)) +
          scale_y_continuous(limits=c(-11,1)) +
          labs(title='Log Mortality Rates by Age',
               subtitle=paste('Standard Schedule =',std)) +
          theme_bw() +
          theme(legend.text = element_text(family='mono')) +
          theme(panel.grid.major = element_line(colour = "darkgrey"))
       
       # add knot positions
         
       G = G + 
         geom_text(data=alpha_info,aes(x=age,y=alpha,label=1:K), 
                    color='red', inherit.aes = FALSE,size=6) +
         geom_line(data=offset_info,aes(x=age,y=y), 
                   color='red', lwd=1, inherit.aes = FALSE) +
         geom_text(aes(x=50,y=0.95), label='Linear Spline Offsets', 
                   size=5, color='red')
       
    } else if (plot == 'lx') {
      
      df = data.frame( age= rep(baseline$age,2),
                       sched = rep(legend_text,c(100,100)),
                       lx = c(baseline$lx, transformed$lx))
      
      G =  ggplot(data=df, aes(x=age, y=lx, 
                          color = sched, 
                          alpha = sched)) +
#        geom_point(size=4) +
        geom_line(lwd=3) +
        scale_alpha_manual(guide=FALSE,
                           values=c(0.8,0.5)) +
        scale_color_manual(values=c('darkgrey','limegreen')) +
        scale_x_continuous(limits=c(0,99),
                           breaks=seq(0,99,10)) +
        scale_y_continuous(limits=c(0,1)) +
        labs(title='Survival Prob by Age',
             subtitle=paste('Standard Schedule =',std)) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))
      
            
    } else if (plot == 'dx') {  
      
      df = data.frame( age= age_dx,
                       sched = rep(legend_text,c(100,100)),
                       dx = c(baseline$dx, transformed$dx))
      
      G=  ggplot(data=df, aes(x=age, y=dx, 
                          color = sched, 
                          alpha = sched)) +
#        geom_point(size=4) +
        geom_line(lwd=3) +
        scale_alpha_manual(guide=FALSE,
                           values=c(0.8,0.5)) +
        scale_color_manual(values=c('darkgrey','purple')) +
        scale_x_continuous(limits=c(0,99),
                           breaks=seq(0,99,10)) +
        scale_y_continuous(limits=c(0,.14)) +
        labs(title=paste('Deaths by Age'),
             subtitle=paste('Standard Schedule =',std)) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))
      
      
      }  # dx 
    
    print(G)
    
   }) # renderPlot
   
} # server

# Run the application 
shinyApp(ui = ui, server = server)

