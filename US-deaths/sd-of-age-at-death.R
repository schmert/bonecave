library(HMDHFDplus)
library(tidyverse)
library(showtext)

font_add_google("Fira Sans", "Fira")
showtext_auto() 

k = 4

theme_carl <- function () { 
  theme_bw(base_size=k*13, base_family="Fira") %+replace% 
    theme(
      axis.text        = element_text(size=(k-1)*11, face='bold'),
      panel.grid.major = element_line(color=grey(.80),size=0.3),
      panel.grid.minor = element_line(color=grey(.95),size=0.1),
      strip.text       = element_text(size=k*13, face='bold'),
      plot.title       = element_text(size=k*28,hjust=0),
      plot.subtitle    = element_text(size=k*18,hjust=0),
      axis.title       = element_text(size=k*14)
    )
}

US = readHMDweb(CNTRY='USA', item='fltper_1x1',un,pw) %>%
       select(Year,Age,dx) %>%
       group_by(Year) %>%
       summarize( xbar  = weighted.mean(Age+0.5, w=dx),
                  xxbar = weighted.mean((Age+0.5)^2, w=dx),
                  sd = sqrt( xxbar - xbar^2))

ggplot(data=US) +
  aes(x=Year, y=xbar) +
  geom_line(color='orange',lwd=2) +
  theme_carl() +
  labs(y=expression(e[0]),
       title='US Women 1933-2017',
       subtitle='mean age at death',
       caption='Source: HMD')

ggsave(filename='US-female-e0.png',
       height=8, width=8, dpi=300)

ggplot(data=US) +
  aes(x=Year, y=sd) +
  geom_line(color='purple',lwd=2) +
  theme_carl() +
  labs(title='US Women 1933-2017',
       subtitle='SD of age at death',
       caption='Source: HMD')

ggsave(filename='US-female-sd.png',
       height=8, width=8, dpi=300)

