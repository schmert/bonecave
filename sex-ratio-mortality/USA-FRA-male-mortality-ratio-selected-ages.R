library(HMDHFDplus)
library(tidyverse)
library(gganimate)
library(transformr)

# use your own HMD username and password below

USA = readHMDweb('USA', item='mltper_1x1',
                username = my_username,
                password = my_password) %>% 
       add_column(country='USA')

FRA = readHMDweb('FRATNP', item='mltper_1x1',
                 username = my_username,
                 password = my_password) %>% 
  add_column(country='FRA')

age_crosswalk = tibble(
  Age   = unique(USA$Age),
  Group = cut(Age, breaks=c(-Inf,0,1,10,20,30,40,50,60,70,80,90,Inf),
              right=FALSE)
)

D = rbind(USA,FRA) %>% 
     select(Year,Age,mx,dx,Lx,country) %>%
     left_join(age_crosswalk) %>% 
     group_by(country, Year,Group) %>% 
     summarize(mx = sum(dx)/sum(Lx)) %>% 
     pivot_wider(id_cols=Year:Group,
                 values_from = mx,
                 names_from = country) %>% 
     filter(is.finite(USA), is.finite(FRA), Year > 1945) %>% 
     mutate(ratio = USA/FRA) %>% 
     ungroup() %>% 
     arrange(Group, Year)

selected_age_groups = c('[0,1)','[10,20)', '[20,30)','[30,40)',
                        '[40,50)','[50,60)', '[60,70)',
                        '[70,80)', '[80,90)', '[90, Inf)')
selected_age_labels = c('Infants','10-19','20-29','30-39', '40-49',
'50-59','60-69','70-79','80-89', '90+' )

age_info = tibble(
  Group = selected_age_groups,
  group_name = selected_age_labels
)

tmp = D %>% 
       filter(Group %in% selected_age_groups ) %>% 
       group_by(Group) %>% 
       mutate(smooth_ratio = smooth.spline(x=unique(D$Year),
                                            y=ratio, df=10)$y) %>% 
       left_join(age_info)

ygroup = tmp %>% 
          summarize(y_last = smooth_ratio[Year==2018])

tmp = left_join(tmp, ygroup)

hues = rep(c('red','black','blue','orange','darkgreen'),2)

G = ggplot(data=tmp) +
  aes(x=Year, y=smooth_ratio, color=Group) +
  scale_y_log10(breaks=c(0.5, 0.75, 1, 1.5, 2),
                minor_breaks=NULL) +
  scale_x_continuous(limits=c(1942,2025)) +
  scale_color_manual(values=hues) +
  geom_vline(xintercept = seq(1950,2020,10),lty='dotted', lwd=0.5, color='black') +
  theme_bw() +
  theme(axis.text = element_text(face='bold', size=16),
        axis.title = element_text(face='bold', size=16),
        plot.title = element_text(face='bold', size=18),
        plot.subtitle = element_text(size=14),
        plot.title.position = 'plot') +
  guides(color=FALSE, group=FALSE) +
  geom_hline(yintercept = 1) +
  labs(caption='Source: Human Mortality Database',
       y     = 'USA/FRA mortality rate',
       title = 'USA/France Male Mortality Ratio 1946-2018',
       subtitle = 'for selected age groups') +
  geom_segment(x=1942, y=log10(1), xend=1942, yend=log10(1.5),
               arrow = arrow(length=unit(0.30,"cm"), 
                             ends="last", type = "open"),
               inherit.aes = FALSE) +
  geom_text(x=1942, y=log10(1.8), label='Higher US\nMortality',
            size=5, hjust=0, inherit.aes = FALSE) +
  geom_segment(x=1942, y=log10(1), xend=1942, yend=-log10(1.5),
               arrow = arrow(length=unit(0.30,"cm"), 
                             ends="last", type = "open"),
               inherit.aes = FALSE) +
  geom_text(x=1942, y=-log10(1.8), label='Higher French\nMortality',
            size=5, hjust=0, inherit.aes = FALSE) +
  geom_line(lwd=1.2) +
  geom_text(aes(x=2019, y=y_last, label=group_name),
            nudge_x =0.5, hjust=0, size=6) +
  coord_cartesian(clip='off') +
  transition_states(Group, transition_length = 1, 
                    state_length = 1, wrap = FALSE) 
  

A = animate(plot=G, duration=30)

anim_save(filename='USA-FRA-male-ratio-mortality.gif', animation=A)


