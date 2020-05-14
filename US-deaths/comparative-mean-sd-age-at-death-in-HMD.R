library(HMDHFDplus)
library(tidyverse)
library(showtext)

font_add_google("Fira Sans", "Fira")
showtext_auto() 

k = 3

theme_carl <- function () { 
  theme_bw(base_size=k*13, base_family="Fira") %+replace% 
    theme(
      axis.text        = element_text(size=(k-1)*11),
      panel.grid.major = element_line(color=grey(.80),size=0.3),
      panel.grid.minor = element_line(color=grey(.95),size=0.1),
      strip.text       = element_text(size=k*13, face='bold'),
      plot.title       = element_text(size=k*28,hjust=0),
      plot.subtitle    = element_text(size=k*18,hjust=0),
      axis.title       = element_text(size=k*14)
    )
}

country_list = getHMDcountries()

for (i in seq(country_list)) {

  this_country = country_list[i]
  
  print(this_country)
  
  tmp = readHMDweb(CNTRY=this_country, item='fltper_1x1',
                  my_username,my_password) %>%
         add_column(Country = this_country, .before='Year') %>%
         select(Country,Year,Age,dx) %>%
         group_by(Country,Year) %>%
         summarize( xbar  = weighted.mean(Age+0.5, w=dx),
                    xxbar = weighted.mean((Age+0.5)^2, w=dx),
                    sd = sqrt( xxbar - xbar^2)) %>%
         select(Country,Year,e0=xbar,sd)

    
  if (i==1) df = tmp        
  if (i>1)  df = rbind(df,tmp)        
  
}


for (j in 1:10) {
  
    sel = sample(country_list, 2)
    
    tmp = filter(df, Country %in% sel)
  
  ggplot(data=tmp) +
    aes(x=sd, y=e0, group=Country, color=Country) +
    geom_path(lwd=1, alpha=.80) +
    scale_x_continuous(limits=c(10,40)) +
    scale_y_continuous(limits=c(20,90)) +
    theme_carl() +
    labs(y=expression(e[0]),
         title='SD and Mean of age at death',
         subtitle='all HMD countries',
         caption='Source: HMD')
  
  ggsave(filename=paste0('lixo',j,'.png'),
         height=8, width=8, dpi=300)
}