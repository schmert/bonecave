# Carl Schmertmann
# 31 Jan 2026

library('tidyverse')
library('scales')
library('showtext')


this_font = 'Nunito'

font_add_google(name=this_font)
showtext_auto() 

theme_carl <- function () { 
  theme_bw(base_size=16, base_family=this_font) %+replace% 
    theme( plot.title       = element_text(size=20,hjust=0),
           plot.subtitle    = element_text(size=12,hjust=0,color=grey(.20)),
           plot.caption     = element_text(
                                  size=10, hjust=1, 
                                  color='darkgrey'),
           panel.grid       = element_line(color='lightgrey', 
                                  size=0.2),
           panel.grid.minor = element_blank(),
           panel.border     = element_blank(),
           axis.text        = element_text(size=12),
           axis.ticks       = element_blank()
    )
}


state_abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT","DC", 
              "DE", "FL", "GA", 
              "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
              "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
              "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
              "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# read mortality dataframe ----

  data = read_csv('mortality-2022.csv', 
                  show_col_types = FALSE)

# create plotting function ---- 

#...............................................

# non-survival plot with one particular state
# highlighted

plot_state = function(this_pop   = 'MA', 
                      sel_ages   = 5:40,
                      ref_pop    = c('USA'),
                      this_color = grey(.10),
                      add_title  = TRUE) {

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
    geom_line(data = . %>% filter(!ref),
              lwd=0.3, color='lightgrey') +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(limits=c(L,H+3)) +
    labs(x='Age',
         y='',
         caption=paste0('US Mortality Database',
                        '\nhttps://doi.org/10.7910/DVN/19WYUX',
                        '\n@cschmert')) +
    theme_carl()

  if (add_title) {
    G = G +
      labs(title=paste0('Probability that a ',L,
                 '-yr-old dies\nbefore a given age'),
          subtitle='[Each grey line is a different US state]')
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
  
  hues = c('dodgerblue','orangered','violet','black')[seq(ref_pop)]
  
  G = G + 
    geom_line(data= . %>% filter(ref),
              lwd=1.2, aes(group=pop,color=pop) ) +
    geom_text( data = . %>% filter(ref, age==H),
               aes(label=pop,color=pop),
               nudge_x = 0.3, size=3, hjust=0) +
    scale_color_manual(values=hues) +
    guides(color='none')
  
  print(G)
  
  ggsave(plot=G,filename=fname, height=6, width=5)
  
} # plot_state

# create and arrange comparative plots ----

for (this_state in c('MA','MS','WV','NM')) {
  plot_state(this_state,  5:50, ref_pop = c('USA','France','Spain','UK'))
  plot_state(this_state, 75:95, ref_pop = c('USA','France','Spain','UK'))
}

