# Carl Schmertmann
# 31 Jan 2026
#.....................................................
# Original state-level files from USStateLifetables2022.zip 
# at https://doi.org/10.7910/DVN/19WYUX 
# 
# Original international files from Human Mortality
# Database www.mortality.org
#
# This program must be in the working directory,
# which contains the /Nations and /States subdirectories
# extracted from the USMDB .zip file,
# and the intl-2022.csv file from the HMD
# ....................................................

library('tidyverse')
library('scales')

# read or create mortality dataframe ----

state_abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT","DC", 
              "DE", "FL", "GA", 
              "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
              "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
              "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
              "UT", "VT", "VA", "WA", "WV", "WI", "WY")

already_processed = file.exists('mortality-2022.csv')

if (already_processed) {
  data = read_csv('mortality-2022.csv')
} else {

  
  data = tibble()
  
  for (this_abb in state_abb) {
    
   this_filename = paste0('./States/',this_abb,
                          '/',this_abb,"_",
                          'bltper_1x1.csv')
   
   tmp = read_csv(this_filename, show_col_types = FALSE) %>%
          mutate( Age = as.numeric(Age)) %>% 
          filter(Year == 2022, Age <= 100) %>% 
          select(pop=PopName,age=Age,lx) 
   
    data = bind_rows(data, tmp)
   
  }
  
  # add the equivalent calculations for the entire USA in 2022
  this_filename = './Nationals/USA/USA_bltper_1x1.csv'

  tmp = read_csv(this_filename, show_col_types = FALSE) %>%
    mutate( Age = as.numeric(Age)) %>% 
    filter(Year == 2022, Age <= 100) %>% 
    select(pop=PopName,age=Age,lx)
  
  data = bind_rows(data, tmp)
  
  # add the international HMD data (downloaded separately)
  
  tmp = read_csv('intl-2022.csv', skip=4)
  
  data = bind_rows(data, tmp)
  
  write_csv(data, file='mortality-2022.csv')

  
} 

# create plotting function ---- 

#...............................................

# non-survival plot with one particular state
# highlighted

plot_state = function(this_pop   = 'MA', 
                      sel_ages   = 5:40,
                      ref_pop    = c('USA'),
                      this_color = 'red',
                      add_title  = FALSE) {

  L = min(sel_ages)
  H = max(sel_ages)
  
  fname = paste0('Q',L,'-',this_pop,'.pdf')
  
  tmp = data %>% 
    filter(age %in% sel_ages,
           pop %in% c(state_abb, ref_pop)) %>% 
    mutate(focus = (pop == this_pop),
           ref   = (pop %in% ref_pop)) %>% 
    mutate(Q = if_else(age >= L, 1-lx/lx[age==L], NA),
           .by=pop) 
  
# base plot of all US states as grey lines
  
  G = ggplot(data=tmp) +
    aes(x=age,y=Q,group=pop) +
    geom_line(lwd=0.3, color='lightgrey') +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(limits=c(L,H+2)) +
    labs(x='Age',
         y='Probability of Death',
         caption=paste0('US Mortality Database',
                        '\nhttps://doi.org/10.7910/DVN/19WYUX',
                        '\n@cschmert'))

  if (add_title) {
    G = G +
      labs(title=paste0('Probability that a ',L,
                 '-yr-old dies before a given age'),
          subtitle='(each grey line is a different state)')
  }
  
  print(G)
  
# add a colored line for the focal state  
    
  G = G + 
    geom_line(data= . %>% filter(focus),
              lwd=1.2, color=this_color) +
    geom_text( data = . %>% filter(focus, age==H),
               aes(label=pop),
               nudge_x = 0.3, size=4, hjust=0,
               color=this_color) 
  
  print(G)

  # add additional black lines for the referenence pop(s)
  
  hues = c('black',gray(.20),'violet','orange')[seq(ref_pop)]
  
  G = G + 
    geom_line(data= . %>% filter(ref),
              lwd=1.2, aes(group=pop,color=pop) ) +
    geom_text( data = . %>% filter(ref, age==H),
               aes(label=pop,color=pop),
               nudge_x = 0.3, size=3, hjust=0) +
    scale_color_manual(values=hues) +
    guides(color='none')
  
  print(G)
  
  ggsave(plot=G,filename=fname, height=6, width=6)
  
} # plot_state

# create and arrange comparative plots ----

for (this_state in c('MA','MS','WV','NM')) {
  plot_state(this_state, 5:40, ref_pop = c('USA','France'), 'red')
  plot_state(this_state, 75:95, ref_pop = c('USA','France'), 'red')
}

