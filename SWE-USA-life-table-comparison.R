
library(tidyverse)
library(HMDHFDplus)


# HMD credentials have to be already in memory:
# un = username, pw = password

# period life tables, both sexes, from HMD

USA = readHMDweb(CNTRY='USA', item='bltper_1x1', username=un, password=pw)
SWE = readHMDweb(CNTRY='SWE', item='bltper_1x1', username=un, password=pw)

# find the latest year for which we have life tables from both countries

latest_year = max( intersect( USA$Year, SWE$Year))

# get the 2 life tables for that latest year
# and arrange in a data frame

df_us = filter(USA, Year==latest_year) %>%
          select(Year, Age, dx) %>%
          add_column(Country='USA', .before=1)

df_sw = filter(SWE, Year==latest_year) %>%
          select(Year, Age, dx) %>%
          add_column(Country='SWE', .before=1)

df = bind_rows(df_us, df_sw) %>%
      group_by(Country) %>%
      mutate(cumulative_dx = 100* cumsum(dx)/sum(dx))

G = ggplot(data=df) +
  aes(x=Age, y=cumulative_dx, color=Country) +
  geom_line(lwd=1.2) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,100,10), minor_breaks=NULL,limits=c(0,100)) +
  scale_y_continuous(breaks=seq(0,100,25), minor_breaks = NULL, expand=c(.01,.01)) +
  labs(title='2021 Period Life Table Death Distribution',
       subtitle='USA and Sweden, both sexes combined',
       y = '% who will die before this age',
       caption='Source: Human Mortality Database https://mortality.org\n@CSchmert') +
  scale_color_manual(values=c('steelblue','red')) +
  guides(color='none')

print(G)

med_df = df %>%
          summarize(med = approx(x=cumulative_dx, y=Age, xout=50)$y) %>%
          mutate(txt = paste0(Country,'\n(50% die before age ',
                                 sprintf('%4.1f',med),')'))


G = G + 
     geom_point(data=med_df, aes(x=med,y=50),size=3) +
     geom_text(data=med_df, aes(x=med+c(+10,-12), y=50, label=txt, hjust=0.5),
               size=3.5, face='bold')

ggsave('SWE-USA-life-table-comparison.png', plot=G)
