# Lexis Presidents

library(tidyverse)

graphics.off()
windows(record=TRUE)

daynumber = function(x) {
      
    this_year = as.Date(x, '%B%e,%Y') %>%
                   strftime(format='%Y') %>%
                   as.numeric()
    this_day = as.Date(x, '%B%e,%Y') %>%
                strftime(format='%j') %>%
                as.numeric()
    
    leap = (this_year %% 4 == 0) & 
           !(this_year %in% c(1700,1800,1900))
    
    return( this_year + (this_day-1) / ifelse(leap,366,365) )
}

today = daynumber('Feb 22, 2019')

df = read.csv('USpresidents.csv') %>%
      mutate( birthday = daynumber(birthdate),
              deathday = daynumber(deathdate),
              living   = is.na(deathdate),
              lastday  = ifelse(living,today,deathday),
              lastage  = lastday-birthday) %>%
      select(name,contains('date'),
             birthday:lastage)

# reorder factor value of name for prettier plot
ord = order(df$birthday)
df$pres = factor(df$name, levels=df$name[ord])

# jiggle a few colliding birthdays to make the
# graph more legible

i = which(df$pres == 'JQAdams')
df$birthday[i] = df$birthday[i]-1.5
i = which(df$pres == 'AJackson')
df$birthday[i] = df$birthday[i]+1

i = which(df$pres == 'Lincoln')
df$birthday[i] = df$birthday[i]-0.5
i = which(df$pres == 'AJohnson')
df$birthday[i] = df$birthday[i]+0.5

i = which(df$pres == 'Grant')
df$birthday[i] = df$birthday[i]-0.5
i = which(df$pres == 'Hayes')
df$birthday[i] = df$birthday[i]+0.5

i = which(df$pres == 'Wilson')
df$birthday[i] = df$birthday[i]-1
i = which(df$pres == 'Taft')
df$birthday[i] = df$birthday[i]-0.5

i = which(df$pres == 'Nixon')
df$birthday[i] = df$birthday[i]-0.5

i = which(df$pres == 'GBush')
df$birthday[i] = df$birthday[i]-0.5
i = which(df$pres == 'Carter')
df$birthday[i] = df$birthday[i]+0.5

## jiggle the labels for Clinton,GWBush,Trump (all born
## within about 6 wks of each other in 1946)

df$txt = as.character(df$pres)
df$txt[df$pres == 'Clinton'] = 'Clinton-GWBush-Trump'
df$txt[df$pres == 'GWBush']  = ''
df$txt[df$pres == 'Trump']   = ''


##################################################

ggplot(data=df, aes(x=birthday, y=lastage,color=pres,alpha=living)) +
    geom_segment(aes(x=birthday, xend=birthday,
                 y=0, yend=lastage), lwd=1.5) +
    theme_bw() +
    scale_alpha_discrete( range=c(.50,.80), guide=FALSE) +
    scale_y_continuous(breaks=seq(0,100,20),
                       minor_breaks = seq(0,110,5),
                       lim=c(0,108),
                       sec.axis=dup_axis()) +
    scale_x_continuous(breaks=seq(1700,1980,20)) +
    scale_color_discrete(guide=FALSE) +
    geom_text( data=filter(df,!living),
               aes(x=birthday, y=lastage, label=txt),
               size=4, angle=90, hjust=0,nudge_y = 1,
               alpha=.80) +
    geom_text( data=filter(df,living),
               aes(x=birthday, y=lastage, label=txt),
               fontface='bold',size=5,
               angle=90, hjust=0,nudge_y = 1) +
    labs(x='Year of Birth', y='Age',
         title='US Presidential Lifespans',
         subtitle='(Clinton, GW Bush, and Trump were all born within 2 months of each other in Jun-Aug 1946)',
         caption='Slight adjustments to a few birthdays to improve legibility')



ggsave(file='USpresidents.png', width=15,
       height=8, units='in', dpi=300)

    


  