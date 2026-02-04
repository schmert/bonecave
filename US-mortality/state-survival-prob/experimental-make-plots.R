# Carl Schmertmann
# 01 Feb 2026

# housekeeping and data setup ----

library('tidyverse')
library('scales')
library('showtext')

this_font = 'Rubik' 

font_add_google(name=this_font)
showtext_auto() 

theme_carl <- function () { 
  theme_bw(base_size=30, base_family=this_font) %+replace% 
    theme( plot.title       = element_text(size=60,hjust=0,
                                           lineheight=0.3),
           plot.subtitle    = element_text(size=35,hjust=0,color=grey(.20)),
           plot.caption     = element_text(
                                  size=25, hjust=1, 
                                  color=grey(.20),
                                  lineheight = 0.3),
           panel.grid       = element_line(color=grey(.60), 
                                  size=0.1),
           panel.border     = element_blank(),
           axis.text        = element_text(size=36),
           axis.title       = element_text(size=40),
           axis.ticks       = element_blank(),
           plot.background  = element_rect(fill='white') 
    )
}

state_info = read_csv('US-state-info.csv',
                      show_col_types = FALSE) %>% 
               filter(abb != 'PR')


# read mortality dataframe ----

  data = read_csv('mortality-2022.csv', 
                  show_col_types = FALSE)

#...............................................

# construct a baseplot of non-survival  ---- 

# for a selected age range

# plot parameters 
  sel_ages   = 20:65 #5:50
  ref_pop    = c('USA','France','Spain','UK','Sweden')
  sel_color  = 'red' #'tomato'

  L = min(sel_ages)
  H = max(sel_ages)
  
# survival calculations for selected age range   
  tmp = data %>% 
    filter(age %in% sel_ages,
           pop %in% c(state_info$abb, ref_pop)) %>% 
    mutate(ref   = (pop %in% ref_pop)) %>% 
    mutate(Q = if_else(age >= L, 1-lx/lx[age==L], NA),
           .by=pop) 

# calculate coords at which to add text
# about state lines
  
txt_x = quantile(sel_ages,.25)
txt_y = tmp %>% 
         filter(age==H) %>% 
         pull(Q) %>% 
         quantile(.80)
    
# base plot of all US states as grey lines
  
  baseplot = ggplot(data=tmp) +
    aes(x=age,y=Q,group=pop) +
    geom_line(data = . %>% filter(!ref),
              lwd=0.2, color='#ffadad') +
    geom_text(x=txt_x, y=txt_y,
              label=paste0('Each red line represents',
                           ' a US state'),
              color='red', size=12,hjust=0,
              lineheight=0.3, fontface='bold') +
    scale_y_continuous(labels = scales::percent,
                       breaks=seq(0,.25,.05),
                       minor_breaks = NULL) +
    scale_x_continuous(limits=c(L,H+4.5),
                       breaks=seq(L+5,H,10)) +
    labs(x='Age',
         y='',
         caption=paste0('2022 mortality rates, both sexes combined\nUS Mortality Database',
                        '\nhttps://doi.org/10.7910/DVN/19WYUX',
                        '\n@cschmert')) +
    theme_carl() +
    labs(title=paste0('Chance that a ',L,
                 '-yr-old dies\nbefore reaching a given age'))

  # add additional lines for international comparison
  
  hues = c('dodgerblue','darkgreen','violet','#dc143c')[seq(ref_pop)]
  
  info = tribble(
    ~pop, ~hue, ~ynudge,
    'USA',    '#dc143c',     0,  
    'France', 'royalblue',   0,
    'Spain',  'darkgreen',   0,
    'UK',     'violet',     .04,
    'Sweden', 'orange',      0
  )
  
  intl = tmp %>% 
          filter(pop %in% info$pop) %>% 
          left_join(info, by='pop')
  
  baseplot = baseplot + 
    geom_line(data= intl,
              lwd=1.2, aes(group=pop, 
                           color=.data[['hue']])) +
    geom_text( data = intl %>% filter(age==H) ,
               aes(label=pop,color=.data[['hue']]),
               nudge_x = 0.45,
               nudge_y = info$ynudge,
               size=10, hjust=0,
               fontface='bold') +
#    scale_color_manual(values=info$hue) +
    guides(color='none')

    
# create a .png plot with selected states highlighted  

    #sel_pop    = c('MA','NM','AL','CA','OH','TX')
    sel_pop    = c('UT','KY','CA','OH','WV','MS')
    
    G2 = baseplot 
    
for (this_state in sel_pop) {    

  mini = tmp %>%
          left_join(state_info, by=c('pop'='abb')) %>% 
          filter(pop == this_state)  

  G2 = G2 + 
    geom_line(data= mini,
              lwd=0.35, color=sel_color) +
    geom_text( data =mini %>% filter(age==H),
               aes(label=name),fontface='bold',
               nudge_x = 0.45, size=8, hjust=0,
               color=sel_color) 
  }
  
  ggsave(plot=G2, filename = 'compare-states-20-65.png', 
         height=6, width=7, units='in',dpi=300)

  
if (FALSE) {    
# create a .pdf file with 51 plots, each highlighting
# a different state

  
  for (this_state in state_info$abb) {    
    
    print(this_state)
    
    mini = tmp %>%
      left_join(state_info, by=c('pop'='abb')) %>% 
      filter(pop == this_state)  
    
     G = baseplot + 
      geom_line(data= mini,
                lwd=0.40, color=sel_color) +
      geom_text( data =mini %>% filter(age==H),
                 aes(label=name),fontface='bold',
                 nudge_x = 0.45, size=8, hjust=0,
                 color=sel_color) 
     
     ggsave(plot=G, 
            filename = paste0('./single-state-plots/',
                   this_state,'-5-50.png'), 
            height=6, width=7, units='in',dpi=300)
     
  }
  } # if FALSE
  
  