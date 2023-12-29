library(tidyverse)
library(HMDHFDplus)

# fill these in with your own credentials for
# mortality.org
#   un = xxxxx
#   pw = xxxxx

if (!exists(US)) {
  US = readHMDweb(CNTRY='USA', username=un,
                  password=pw,item='Exposures_lexis') %>%
        filter(!is.na(Cohort)) %>% 
        mutate(Birth_Group = pmax(1890,10*floor(Cohort / 10)),
               BB = cut(Cohort,breaks=c(-Inf,1945,1964,Inf),
                        labels=c('Older','BB','Younger'))) %>% 
        select(Year,Age,Cohort,Birth_Group, BB, Pop=Total) 
}

tmp = US %>% 
  mutate(Birth_Group = factor(Birth_Group)) %>% 
  group_by(Year,Birth_Group) %>% 
  summarize(Pop=sum(Pop)) %>% 
  group_by(Year) %>% 
  mutate(PopFrac = 100*prop.table(Pop),
         L = head(cumsum(c(0,PopFrac)),-1),
         H = cumsum(PopFrac)) %>% 
  filter(Year %in% 1960:2019)

tmpBB = US %>% 
  group_by(Year,BB) %>% 
  summarize(Pop=sum(Pop)) %>% 
  group_by(Year) %>% 
  mutate(PopFrac = 100*prop.table(Pop),
         L = head(cumsum(c(0,PopFrac)),-1),
         H = cumsum(PopFrac)) %>% 
  filter(Year %in% 1960:2019)

G = ggplot(data=tmp) +
  aes(x=Year, ymin = L, ymax=H,
      fill=Birth_Group) +
  geom_ribbon(color='white',lwd=0.4, alpha=.70) +
  theme_bw() +
  theme(axis.title = element_text(face='bold',size=16),
        axis.text  = element_text(face='bold',size=16),
        title      = element_text(face='bold',size=18)) +
  guides(fill='none') +
  scale_fill_viridis_d(option='turbo') +
  scale_y_continuous(breaks=seq(0,100,20), minor_breaks = NULL)

txt_df = tribble(
  ~name, ~Year, ~Pct,
  'pre-1900', 1961, 3,
  '1900s',    1965, 11,
  '1910s',    1968, 18,
  '1920s',    1971, 26,
  '1930s',    1976, 32,
  '1940s',    1980, 40,
  '1950s',    1985, 50,
  '1960s',    1990, 58,
  '1970s',    1996, 66,
  '1980s',    2000, 75, 
  '1990s',    2005, 83,
  '2000s',    2010, 90,
  '2010s',    2012.5, 97
)

G = G +
  geom_label(data=txt_df, aes(x=Year,y=Pct,label=name),
            size=5, hjust=0,inherit.aes = FALSE,
            fill='white', color='black',  fontface='bold') +
  labs(title='Fraction of US Population By Decade of Birth',
       caption='Source: Human Mortality Database\n@CSchmert',
       y='Cumulative Percent')

BBtxt_df = tribble(
  ~name, ~Year, ~Pct,
  'OLDER', 2010, 8,
  'BABY BOOM', 2010, 25,
  'YOUNGER', 2010, 50
)

G +
  geom_line(data=filter(tmpBB,BB=='BB'), aes(x=Year,y=L), lty='longdash',color='black',
            inherit.aes = FALSE)+
  geom_line(data=filter(tmpBB,BB=='BB'), aes(x=Year,y=H), lty='longdash',color='black',
            inherit.aes = FALSE) +
  geom_text(data=BBtxt_df, aes(x=Year,y=Pct,label=name),
            inherit.aes = FALSE, fontface='bold',size=6)


ggsave(filename='fraction-of-US-pop-by-decade-of-birth.png',
       height=8,width=8, units='in', dpi=300)

