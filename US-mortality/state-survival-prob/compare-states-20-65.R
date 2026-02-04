# Carl Schmertmann
# 03 Feb 2026

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
  sel_ages        = 20:65 
  state_color     = '#ffadad'
  sel_state_color = 'red' 

  L = min(sel_ages)
  H = max(sel_ages)
  
# survival calculations for selected age range   
  tmp = data %>% 
    filter(age %in% sel_ages) %>% 
    mutate(Q = if_else(age >= L, 1-lx/lx[age==L], NA),
           .by=pop) 

# calculate coords at which to add text
# about state lines
  
txt_x = quantile(sel_ages,.25)
txt_y = tmp %>% 
         filter(age==H) %>% 
         pull(Q) %>% 
         quantile(.80)
    
# base plot - each US state is a separate line
  states = tmp %>% filter(pop %in% state_info$abb)  

  baseplot = ggplot(data=states) +
    aes(x=age,y=Q,group=pop) +
    geom_line(lwd=0.2, color=state_color) +
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
         caption=paste0('2022 mortality rates, both sexes combined\n',
         'US Mortality Database & Human Mortality Database',
                        '\nhttps://doi.org/10.7910/DVN/19WYUX & mortality.org',
                        '\nCarl Schmertmann: @cschmert')) +
    theme_carl() +
    labs(title=paste0('Chance that a ',L,
                 '-yr-old dies\nbefore reaching a given age'))

  # add additional lines for international comparison
  
  info = tribble(
    ~pop,     ~hue,       ~ynudge,
    'USA',    '#dc143c',      0,
    'France', 'dodgerblue',   0,
    'Spain',  'royalblue',    0,
    'Sweden', 'darkblue',     0
  ) %>% mutate(pop = factor(pop, levels=pop))
  
  intl = tmp %>% 
          filter(pop %in% info$pop) %>% 
          mutate(pop = factor(pop, levels=info$pop)) %>% 
          left_join(info, by='pop')  
  
  baseplot = baseplot + 
    geom_line(data= intl,
              lwd=1.2, aes(group=pop, 
                           color=pop) ) +
    geom_text( data = intl %>% filter(age==H) ,
               aes(label=pop,color=pop),
               nudge_x = 0.45,
               nudge_y = info$ynudge,
               size=10, hjust=0,
               fontface='bold') +
    scale_color_manual(values=info$hue) +
    guides(color='none')

    
# create a .png plot with selected states highlighted  

    sel_pop    = c('UT','KY','CA','OH','WV','MS')
    
    G2 = baseplot 
    
for (this_state in sel_pop) {    

  mini = tmp %>%
          left_join(state_info, by=c('pop'='abb')) %>% 
          filter(pop == this_state)  

  G2 = G2 + 
    geom_line(data= mini,
              lwd=0.35, color=sel_state_color) +
    geom_text( data =mini %>% filter(age==H),
               aes(label=name),fontface='bold',
               nudge_x = 0.45, size=8, hjust=0,
               color=sel_state_color) 
  }


  ggsave(plot=G2, filename = 'compare-states-20-65.png', 
         height=6, width=7, units='in',dpi=300)

