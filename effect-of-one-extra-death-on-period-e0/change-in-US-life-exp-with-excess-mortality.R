#####################################################
# quick calculation of the impact of excess deaths
# on US both-sex e0,  if new deaths by age are
# proportional to observed deaths
# 
# 2019 data from HMD
#####################################################

library(tidyverse)
library(HMDHFDplus)

deaths = readHMDweb(CNTRY='USA', item='Deaths_1x1',
                    username=myUN, password = myPW) %>%
         filter(Year == 2019) %>%
         pull(Total)

exposure = readHMDweb(CNTRY='USA', item='Exposures_1x1',
                    username=myUN, password = myPW) %>%
         filter(Year == 2019) %>%
         pull(Total)

LT = readHMDweb(CNTRY='USA', item='bltper_1x1',
                username=myUN, password = myPW) %>% 
         filter(Year == 2019)

e0 = function(D,N) {
  mu = D/N
  H  = cumsum(c(0,mu))
  lx = exp(-H)
  sum( (tail(lx,-1) + head(lx,-1))/2)
}

e0_obs = e0(deaths,exposure)

mult = seq(1,1.2,.001)

new_e0 = sapply(mult, function(m) {
  new_deaths = m * deaths
  return( e0(new_deaths,exposure) )
})

excess = m * sum(deaths)



D = tibble(
  m      = mult,
  e0     = new_e0,
  excess = (m-1) * sum(deaths)
)

##################

ydiff = seq(.5,2,.5)
ymarks = e0_obs - ydiff

xmarks = approx( y=D$excess, x= D$e0, xout=ymarks)$y

xlabs = c('0.5 Years of Life Lost','1 Year', '1.5 Years','2 Years' )

G = ggplot(data=D) +
  aes(x=excess, y=e0) +
  geom_line(color='blue',size=1) +
  theme_bw() +
  theme(axis.text = element_text(size=11,face='bold'),
        axis.ticks.length = unit(.35,'cm')) +
  scale_x_continuous(breaks=seq(0,5e5,1e5),
                     labels=c('0','100,000','200,000','300,000','400,000','500,000'),
                     expand=c(0,0)) +
  labs(x='Excess Deaths',
       y='Period Life Expectancy',
       title='US Life Expectancy at different levels of excess mortality',
       subtitle='(excess deaths by age proportional to current deaths)',
       caption='Source: Human Mortality Database (2019 both sexes)') 

for (i in seq(ymarks))  {
  xx = xmarks[i]
  yy = ymarks[i]
  G = G + 
       geom_segment(x=xx,y=0,xend=xx,yend=yy, lty='dashed',size=0.4) +
       geom_segment(x=0,y=yy,xend=xx,yend=yy, lty='dashed',size=0.4) +
       geom_text(x=xx+5000,y=yy+.05,label=xlabs[i],
                  hjust=0, size=4)    
}

G

ggsave(filename='change-in-US-life-exp-with-excess-mortality.png',
       height=6, width=6)
