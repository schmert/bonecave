library(tidyverse)
library(HMDHFDplus)
library(cowplot)

rm(list=ls())

# select data and download from HMD ----
# HMD username and password must be in session memory as 'un' and 'pw' 


country_list = getHMDcountries()

need_data = !exists('HMDdata')

if (need_data) {
    
    HMDdata = tibble()
    
    country_list = getHMDcountries()
    
    for (this_country in country_list) {
    
      tmp = readHMDweb(CNTRY=this_country,item='fltper_1x1', username=un, password = pw) %>% 
              select(Year, Age, mx, dx, lx, ex,qx) %>% 
              mutate(Country = this_country)
      
      HMDdata = bind_rows(HMDdata, tmp)
      print(this_country)
    }
    
    df = HMDdata %>% 
          group_by(Country,Year) %>% 
          summarize(e0=ex[1], 
                    avg_mort = weighted.mean(mx,dx),
                    avg_inv_mort = weighted.mean(1/mx,dx),
                    missing_mx = any(mx==0)) %>% 
          filter(!missing_mx)
}


ggplot(df) + 
  aes(x=e0, y=avg_mort, color=Country) + 
  geom_point()


age_plot = function(this_country,this_year) {
  tmp = HMDdata %>% 
         filter(Country==this_country, Year==this_year)
  
  dpeak    = filter(tmp, dx == max(dx))
  mnadir   = filter(tmp, mx == min(mx))
  
  G = ggplot(data=tmp) +
       geom_segment(aes(x=Age,y=0,xend=Age, yend=1/mx)) +
       geom_line(aes(x=Age,y=dx), color='red', lwd=1) +
       geom_text(data=dpeak, aes(x=Age,y=dx),label='Deaths',color='red', nudge_y = 200, hjust=0) +
       geom_text(data=mnadir, aes(x=Age,y=1/mx),label='Inverse Mort Rate',color='black', nudge_y = 200, hjust=0) +
       geom_vline(xintercept = tmp$ex[1], color='red', lwd=1, lty='dotted') +
       theme_bw() +
       theme(plot.title = element_text(hjust=0.8, color='red')) +
       labs(title = paste(this_country,'Females',this_year),
            y='',
            caption='Source: Human Mortality Database')
  
  print(G)
}

den_plot = function(this_country,this_year) {
  tmp = HMDdata %>% 
    filter(Country==this_country, Year==this_year)
  
  G = ggplot(data=tmp) +
    geom_text(aes(x=1/mx, y=dx, label=Age),size=2.5) +
    geom_line(aes(x=1/mx, y=dx), lwd=0.2) +
    scale_x_log10(limits=c(1,3000)) +
    geom_vline(xintercept=tmp$ex[1], lty='dotted', color='red',lwd=1) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.8, color='red')) +
    labs(title = paste(this_country,'Females',this_year),
         x='Inverse Mortality Rate (log scale)',
         y='Deaths',
         caption='Source: Human Mortality Database') +
    geom_text(aes(x=tmp$ex[1]*1.10, y=3000), 
              label=paste('e0 =',tmp$ex[1]), color='red',
              size=5, hjust=0)
  
  print(G)
}

G1 = age_plot('GBRTENW',1930)
G2 = den_plot('GBRTENW',1930)
G3 = age_plot('USA',2000)
G4 = den_plot('USA',2000)

plot_grid(G1,G2,G3,G4, labels=rep(c('By Age', 'By 1/mx'),2),
          ncol=2)


ggsave(filename='e0-and-inverse-mortality.png',
       height=10, width=10, units='in',dpi=300)


