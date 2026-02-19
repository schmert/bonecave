# Carl Schmertmann
# 19 Feb 2026

# housekeeping and data setup ----

library('tidyverse')
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
# add state abb,... then keep rates

  cancer_df = read_csv('lung-cancer-age-adj-death-rates.csv',
                  skip=9,n_max=53,
                  show_col_types = FALSE) %>% 
         inner_join(state_info,by='fips') %>% 
         select(state=name,abb,fips,
                cancer_rate=starts_with('Age-Adjusted Death Rate'))

# read cigarette tax dataframe ----
# add state abb,... then keep rates

  tax_df = read_csv('state-cigarette-excise-tax-rate-2024.csv',
                    show_col_types = FALSE) %>% 
           inner_join(state_info, join_by(Location==name)) %>% 
           select(state=Location,abb,fips,tax_rate=`Excise Tax Rate`)

  tax_df = read_csv('KFF-cigarette-tax-data.csv',
                    skip=6, n_max=51, 
                    show_col_types = FALSE) %>% 
            select(state=Location,tax=2) %>% 
            left_join(state_info, join_by(state==name))
  
  
mydata = cancer_df %>% 
  inner_join(tax_df, join_by(fips))


G = ggplot(data=mydata) +
  aes(x=tax, y=cancer_rate, label=factor(abb.x)) +
  geom_text(size=10, color='darkblue', fontface='bold') +
  geom_smooth(method='loess',span=1.2,se=FALSE, 
              col='royalblue',lwd=0.80) +
  theme_carl() +
  scale_y_continuous(limits=range(0,55),
                     expand = c(0,2),
                     breaks=seq(0,50,10),
                     minor_breaks = NULL) +
  scale_x_continuous(breaks=0:6, 
                     labels=paste0('$',0:6))

G = G +
  labs(x='State Cigarette Tax (per pack)',
       y='Lung Cancer Mortality [per 100,000]',
       title='State Cigarette Taxes & Lung Cancer',
       caption=paste0('\n2018-2022 Age-adjusted cancer rates',
                      ' & 2025 Cigarette Taxes',
                      '\n cancer.gov & Kaiser Family Foundation',
                      '\nhttps://tinyurl.com/cancer-dot-gov',
                      ' & https://tinyurl.com/kff-cig-tax',
                      '\n@Cschmert'))

ggsave(filename='compare-states-lung-cancer-cig-tax.png', 
       plot=G, height=6, width=6)


