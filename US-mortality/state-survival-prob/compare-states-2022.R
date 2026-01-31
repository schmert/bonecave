# Carl Schmertmann
# 30 Jan 2026
#.....................................................
# Original files from USStateLifetables2022.zip 
# at https://doi.org/10.7910/DVN/19WYUX 
# 
# This program must be in the working directory,
# into which the /Nations and /States subdirectories
# from the .zip file must have already been extracted
# ....................................................

library('tidyverse')

already_processed = file.exists('US-2022.csv')

if (already_processed) {
  my_data = read_csv('US-2022.csv')
} else {

  state_abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT","DC", 
                "DE", "FL", "GA", 
                "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  
  my_data = tibble()
  
  for (this_abb in state_abb) {
    
   this_filename = paste0('./States/',this_abb,
                          '/',this_abb,"_",
                          'bltper_1x1.csv')
   
   tmp = read_csv(this_filename, show_col_types = FALSE) %>%
          mutate( Age = as.numeric(Age)) %>% 
          filter(Year == 2022, Age <= 100) %>% 
          select(state=PopName,Age,lx) 
   
    my_data = bind_rows(my_data, tmp)
   
  }
  
  # add the equivalent calculations for the entire USA in 2022
  this_filename = './Nationals/USA/USA_bltper_1x1.csv'

  tmp = read_csv(this_filename, show_col_types = FALSE) %>%
    mutate( Age = as.numeric(Age)) %>% 
    filter(Year == 2022, Age <= 100) %>% 
    select(state=PopName,Age,lx)
  
  my_data = bind_rows(my_data, tmp)
  
  write_csv(my_data, file='US-2022.csv')

} 

#...............................................

# non-survival plot with one particular state
# highlighted

plot_state = function(sel_state='MA', 
                      sel_ages=5:40,
                      this_color = 'red') {

  L = min(sel_ages)
  H = max(sel_ages)
  
  fname = paste0('Q',L,'-',sel_state,'.pdf')
  
  tmp = my_data %>% 
    filter(Age %in% sel_ages) %>% 
    mutate(focus = (state == sel_state),
           USA   = (state == 'USA')) %>% 
    mutate(Q = if_else(Age >= L, 1-lx/lx[Age==L], NA),
           .by=state) 
  
  
  G = ggplot(data=tmp) +
    aes(x=Age,y=Q,group=state) +
    geom_line(lwd=0.2, color='lightgrey') +
    geom_line(data= . %>% filter(focus),
              lwd=1.2, color=this_color) +
    geom_line(data= . %>% filter(USA),
              color='black',lwd=0.8) +
    geom_text( data = . %>% filter(focus, Age==H),
               aes(label=state),
               nudge_x = 0.3, size=3, hjust=0,
               color=this_color) +
    geom_text( data = . %>% filter(USA, Age==H),
               aes(label='US'),
               nudge_x = 0.3, size=3, hjust=0,
               color='black') +
    theme_bw() +
    labs(y='Prob. of Death',
         title=paste0('State-by-State Probability that a ',L,
                      '-yr-old dies \n before reaching various ages'),
         subtitle='at 2022 mortality rates',
         caption=paste0('US Mortality Database',
                        '\nhttps://doi.org/10.7910/DVN/19WYUX',
                        '\n@cschmert'))
  
  print(G)
  
  ggsave(plot=G,filename=fname, height=6, width=6)
  
} # plot_state

#.............................

plot_state('MA', 5:40, 'red')
plot_state('WV', 5:40, 'blue')

plot_state('FL', 50:90, 'orangered')


