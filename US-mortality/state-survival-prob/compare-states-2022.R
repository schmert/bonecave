# Carl Schmertmann
# 01 Feb 2026

# housekeeping and data setup ----

library('tidyverse')
library('scales')
library('showtext')

this_font = 'Rubik' #'Nunito'

font_add_google(name=this_font)
showtext_auto() 

theme_carl <- function () { 
  theme_bw(base_size=30, base_family=this_font) %+replace% 
    theme( plot.title       = element_text(size=60,hjust=0,
                                           lineheight=0.3),
           plot.subtitle    = element_text(size=12,hjust=0,color=grey(.20)),
           plot.caption     = element_text(
                                  size=25, hjust=1, 
                                  color=grey(.20),
                                  lineheight = 0.3),
           panel.grid       = element_line(color='grey', 
                                  size=0.1),
           panel.grid.minor = element_blank(),
           panel.border     = element_blank(),
           axis.text        = element_text(size=36),
           axis.ticks       = element_blank()
    )
}

state_info = read_csv('US-state-info.csv',
                      show_col_types = FALSE) %>% 
               filter(abb != 'PR')


# read mortality dataframe ----

  data = read_csv('mortality-2022.csv', 
                  show_col_types = FALSE)

#...............................................

# construct a plot ---- 

# non-survival plot with selected state(s)
# highlighted


# plot parameters 
  sel_pop    = c('MA','NM') 
  sel_ages   = 5:50
  ref_pop    = c('USA','France','Spain','UK')
  sel_color  = 'tomato'
  add_title  = TRUE

  L = min(sel_ages)
  H = max(sel_ages)
  
  fname = paste0('Q',L,'-',paste(sel_pop,collapse='-'),'.pdf')
  
# survival calculations for selected age range   
  tmp = data %>% 
    filter(age %in% sel_ages,
           pop %in% c(state_info$abb, ref_pop)) %>% 
    mutate(focus = (pop %in% sel_pop),
           ref   = (pop %in% ref_pop)) %>% 
    mutate(Q = if_else(age >= L, 1-lx/lx[age==L], NA),
           .by=pop) 

# calculate coords at which to add text
# about state lines
  
txt_x = quantile(sel_ages,.10)
txt_y = tmp %>% 
         filter(age==H) %>% 
         pull(Q) %>% 
         quantile(.80)
    
# base plot of all US states as grey lines
  
  G = ggplot(data=tmp) +
    aes(x=age,y=Q,group=pop) +
    geom_line(data = . %>% filter(!ref),
              lwd=0.3, color=grey(.50)) +
    geom_text(x=txt_x, y=txt_y,
              label=paste0('Each dark grey line represents',
                           ' a US state','\n',
                           'All calculations use 2022 period mortality rates'),
              color=grey(.50), size=12,hjust=0,
              lineheight=0.3) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(limits=c(L,H+4.5)) +
    labs(x='Age',
         y='',
         caption=paste0('US Mortality Database',
                        '\nhttps://doi.org/10.7910/DVN/19WYUX',
                        '\n@cschmert')) +
    theme_carl()

  
  if (add_title) {
    G = G +
      labs(title=paste0('Probability that a ',L,
                 '-yr-old dies\nbefore reaching a given age'))
  }
  
  
# add a colored line for the selected state  

  for (this_state in sel_pop) {    

  mini = tmp %>%
          left_join(state_info, by=c('pop'='abb')) %>% 
          filter(pop == this_state)  

  G = G + 
    geom_line(data= mini,
              lwd=0.8, color=sel_color) +
    geom_text( data =mini %>% filter(age==H),
               aes(label=name),fontface='bold',
               nudge_x = 0.45, size=8, hjust=0,
               color=sel_color) 
  }
  
  
# add additional lines for national pop(s)
  
  hues = c('dodgerblue','darkgreen','violet','#dc143c')[seq(ref_pop)]
  
  G = G + 
    geom_line(data= . %>% filter(ref),
              lwd=1.2, aes(group=pop,color=pop) ) +
    geom_text( data = . %>% filter(ref, age==H),
               aes(label=pop,color=pop),
               nudge_x = 0.45, size=10, hjust=0,
               fontface='bold') +
    scale_color_manual(values=hues) +
    guides(color='none')
  
  ggsave(filename = 'compare-states-5-50.png', 
         height=6, width=7, units='in',dpi=300)
  
