#--------------------------------------
# Carl Schmertmann
# TOPALS-fit
#
# main ideas
#   choice of 3 datasets
#   choice of 3 standards
#   TABS:
#      * click-sequence or animation of Newton-Raphson fits
#      * D vs D-hat data and plot in one
#      * regression output in tabular form (à la Stata)
#      
#--------------------------------------

library(shiny)
library(shinyjs)
library(tidyverse)
library(splines)
library(bslib)

# load example data ----
# retrieve example data and HMD-based standards
load('input_datasets.Rdata')

windowsFonts(CO = 'Consolas')

population = paste(data$name, data$sex, data$period)

# TOPALS fitting function ----

TOPALS_fit = function( N, D, std,
                       age_group_bounds   = 0:100,
                       knot_positions     = c(0,1,10,20,40,70), 
                       penalty_precision  = 2,
                       max_iter           = 50,
                       alpha_tol          = .00005,
                       details            = FALSE) {
  
  require(splines)
  
  ## single years of age from 0 to (A-1)
  A   = length(std)
  age = 0:(A-1)
  
  ## B is an AxK matrix. Each column is a linear B-spline basis function
  B      = bs( age, knots=knot_positions, degree=1 )
  K = ncol(B) 
  
  D1 = diff( diag(K), diff=1)
  P  = penalty_precision * crossprod(D1)
  
  ## number and width of age groups
  G     = length(age_group_bounds)-1   
  nages = diff(age_group_bounds)
  
  ## weighting matrix for mortality rates (assumes uniform
  ## distribution of single-year ages within groups)
  W = matrix(0, nrow=G, ncol=A, 
             dimnames=list(head(age_group_bounds,-1) , age))
  
  offset = 0
  for (g in 1:G) {
    W[g, offset + 1:nages[g]] = 1/nages[g]
    offset = offset + nages[g]
  }
  
  ## log lik function (without scaling constant)
  Lik = function(alpha) {
    M = W %*% exp( std + B %*% alpha)
    likelihood = sum(D * log(M) - N * M)
    return( likelihood )
  }
  
  ## penalty function
  Pen = function(alpha) {
    penalty    = -1/2 * t(alpha) %*% P %*% alpha
    return( penalty )
  }
  
  
  #------------------------------------------------
  # iteration function: 
  # next alpha vector as a function of current alpha
  #------------------------------------------------
  next_alpha = function(alpha) {
    mu = as.vector( exp( std + B %*% alpha))
    M  = as.vector( W %*% mu)
    
    Dhat = N * M
    
    X = W %*% diag(mu) %*% B
    A = diag(N/M)
    
    y = (D-Dhat)/N + X %*% alpha
    
    updated_alpha = solve( t(X) %*% A %*% X + P, t(X) %*% A %*% y)
    return(as.vector(updated_alpha))
  }
  
  ## main iteration:     
  amat     = matrix(NA, nrow=max_iter, ncol=K)
  amat[1,] = rep(0, K)
  
  niter = 0
  repeat {
    niter          = niter + 1  # eg, 1st iteration
    
    this_i         = niter + 1  # eg, 2nd row of amat produced by 1st iter
    last_i         = niter      
    amat[this_i,]  = next_alpha( amat[last_i,] )  # update
    change         = amat[this_i,] - amat[last_i,]
    
    converge = all( abs(change) < alpha_tol )
    overrun  = (niter == max_iter)
    
    if (converge | overrun) { break }
    
  } # repeat
  
  if (details | !converge | overrun) {
    if (!converge) print('did not converge')
    if (overrun) print('exceeded maximum number of iterations')
    
    mu    = as.vector( exp(std + B %*% amat[niter,]))
    M     = as.vector( W %*% mu )
    dhat  = N * M
    
    X     = W %*% diag(mu) %*% B
    A     = diag(N/M)
    
    covar = solve( t(X) %*% A %*% X + P)
    
    final_i     = niter+1  # which row of amat has the final estimates?
    final_alpha = amat[final_i,]
    
    return( list( niter             = niter,
                  alpha             = amat[1:final_i,],
                  final_alpha       = final_alpha,
                  D                 = D,
                  N                 = N,
                  age_group_bounds  = age_group_bounds,
                  knots             = knot_positions,
                  std               = std,
                  B                 = B,
                  logm              = std + B %*% amat[final_i,],
                  M                 = W %*% mu,
                  Dhat              = N * M,
                  covar             = covar,
                  Lik               = apply(amat[1:final_i,], 1, Lik),
                  Pen               = apply(amat[1:final_i,], 1, Pen),
                  converge          = converge, 
                  maxiter           = overrun))
  } else return( amat[niter,]) 
  
} # TOPALS_fit

# functions for displaying data and fits ----

make_name = function(i) {
  paste(data$name[i], data$sex[i], data$period[i])
} # make name

make_df = function(i) {
  sex    = data[i,] %>% pull(sex) %>% unlist()
  period = data[i,] %>% pull(period) %>% unlist()
  label  = data[i,] %>% pull(label) %>% unlist()
  
  N = data[i,] %>% pull(N) %>% unlist()
  D = data[i,] %>% pull(D) %>% unlist()
  L = data[i,] %>% pull(L) %>% unlist()
  H = data[i,] %>% pull(H) %>% unlist()
  
  tibble(label,L,H,N,D)  
}

show = function(fit, hue='red', ti='',subti='', true_logm=NA) {
  
  df_grouped = data.frame(
    L = head( fit$age_group_bounds, -1),
    U = tail( fit$age_group_bounds, -1),
    N = fit$N,
    D = fit$D
  ) %>%
    mutate(logmx_obs = log(D/N))
  
  df_single  = data.frame(
    age=  seq(fit$std) - .50,  # 0.5, 1.5, ...
    std = fit$std,
    logmx_fit  = fit$logm
  )
  
  # simulate 10th and 90th pointwise intervals at each age, and e0
  CH  = t( chol( fit$covar ))
  m   = fit$final_alpha
  sim = replicate(10000, {
    a=m + CH %*% rnorm(7)
    as.vector(fit$std + fit$B %*% a)
  })
  
  df_single$Q10 = apply(sim,1,quantile,prob=.10)
  df_single$Q90 = apply(sim,1,quantile,prob=.90)
  
  e0 = function(logmx) {
    mx = exp(logmx)
    px = exp(-mx)
    lx = c(1,cumprod(px))
    ex = sum( head(lx,-1) + tail(lx,-1))/2
    return(ex)
  }
  
  esim = apply(sim,2,e0)
  e10  = quantile(esim,prob=.10)
  e90  = quantile(esim,prob=.90)
  
  text_info = paste0(
    'Newton-Raphson Converged =  ',fit$converge,'\n',
    '# iterations             =  ',fit$niter,'\n',
    '(Unscaled) Log Lik       =  ',prettyNum(fit$Lik[1+fit$niter], digits=2),'\n',
    'Roughness Penalty        =  ',prettyNum(fit$Pen[1+fit$niter], digits=2),'\n\n',
    'Life Exp. 10%-90%ile CI  = [',prettyNum(e10,digits=3),'-',prettyNum(e90,digits=3),']'
  )
  
  
  
  mx_vals =  c(1,2,10,20,100,200,1000,2000,10000)
  
  this_plot =
    ggplot(data = df_single, aes(x=age,y=std)) +
    geom_line(aes(x=age,y=std), color='black', lwd=0.8) +
    geom_line(aes(x=age,y=logmx_fit), color=hue, lwd=1.5, alpha=.80) +
    geom_segment(data=df_grouped,aes(x=L,xend=U,
                                     y=logmx_obs,
                                     yend=logmx_obs),
                 color=hue,lwd=1, alpha=.90) +
    geom_ribbon(aes(x=age,ymin=Q10,ymax=Q90), fill=hue, alpha=.10) +
    labs(x='Age',y='Mortality Rate per 10,000 (Log Scale)',
         title=ti, subtitle = subti) +
    scale_x_continuous(breaks=c(0,1,seq(5,100,5)),minor_breaks = NULL) +
    scale_y_continuous(limits=range(c(-10,0,df_single$Q10,df_single$Q90)),
                       breaks=log(mx_vals /10000),
                       minor_breaks = NULL,
                       labels=paste(mx_vals)) +
    geom_text(x=55, y=log(3e-4), label=text_info, hjust=0, size=4,
              family='CO',face='bold') +
    theme_bw()
  
  if (!is.na(true_logm)) {
    this_plot = this_plot +
      geom_point(aes(x=0:99,y=true_logm), shape='+')
  } 
  
  print(this_plot)
} # show 

show_data = function(i, hue='red') {
  
  df = make_df(i) %>% 
        mutate(logmx_obs = log(D/N))
  
 print(df)
 
  mx_vals =  c(1,2,10,20,100,200,1000,2000,10000)
  
  
  ti    = df$label[1]
  
  subti = paste(prettyNum(round(sum(df$D)),big.mark=','),'deaths in',
                prettyNum(round(sum(df$N)),big.mark=','),'person-years') 
  
this_plot =
    ggplot(data = df, aes(x=L,y=logmx)) +
    geom_segment(data=df,aes(x=L,xend=H,y=logmx_obs,yend=logmx_obs),
                 color=hue,lwd=1.5, alpha=.90) +
    labs(x='Age',y='Mortality Rate per 10,000 (Log Scale)',
         title=ti, subtitle = subti) +
    scale_x_continuous(breaks=c(0,1,seq(5,100,5)),minor_breaks = NULL) +
    scale_y_continuous(limits=range(c(-10,0,df$logmx_obs[is.finite(df$logmx_obs)])),
                       breaks=log(mx_vals /10000),
                       minor_breaks = NULL,
                       labels=paste(mx_vals)) +
    theme_bw()
  
  print(this_plot)
  
} # show_data 

process_case = function(i) {
  
  N = data[i,] %>% pull(N) %>% unlist()
  D = data[i,] %>% pull(D) %>% unlist()
  L = data[i,] %>% pull(L) %>% unlist()
  H = data[i,] %>% pull(H) %>% unlist()
  
  true_logm = data[i,] %>% pull(logmx) %>% unlist()
  
  
  boundaries = c(L, tail(H,1))
  
  this_title    = paste(data[i,'name'], data[i,'sex'], data[i,'period'])
  this_subtitle = paste( prettyNum(round(sum(D)),big.mark = ','), 'deaths in', 
                         prettyNum(round(sum(N)),big.mark = ','), 'person-years' )
  
  if (data[i,'sex'] == 'male')   {std = male_std}
  if (data[i,'sex'] == 'female') {std = female_std}
  if (data[i,'sex'] == 'both')   {std = both_std}
  
  fit = TOPALS_fit(N, D, std,
                   age_group_bounds = boundaries,
                   detail=TRUE)
  
  print(fit)
  show(fit, 'blue', ti=this_title, subti=this_subtitle, true_logm)
}

# UI layout ----

ui <- fluidPage(
   
  theme = bslib::bs_theme(
    bootswatch = "united"
  ),
  
  useShinyjs(),
  
   # Application title
   titlePanel("Fitting the TOPALS Relational Model to Data"),
   
   
   sidebarLayout(

     sidebarPanel( 
       radioButtons(inputId  = 'dataset_number',
                    label    = 'Select Data',
                    choiceNames  = population,
                    choiceValues = seq(population))
       

      ),  #sidebarPanel

      # Show a plot of log mortality
      div( 
        tabsetPanel(
        type='pills',
        tabPanel(title='Data',
                 tableOutput(outputId = 'data_table')
                 ),
        tabPanel(title='Plot',
            plotOutput(outputId="data_plot", width='600px')
                 ), #Data Tab
        tabPanel(title='Fit'),
        tabPanel(title='Diagnostics')
        )
      ,class="span9")
 
   ) # sidebarLayout
) # fluid

# Server actions ----
server <- function(input, output) {
   

  output$data_plot <- renderPlot({
    show_data(input$dataset_number, hue='blue')
   }, width=500,height=500) # renderPlot


  output$data_table <- renderTable({
     make_df(input$dataset_number)
  },digits=0)    
} # server

# Run the application 
shinyApp(ui = ui, server = server)

