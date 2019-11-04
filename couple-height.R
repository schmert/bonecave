# couples heights from Philip Cohen 4 Nov 2019

library(tidyverse)
library(haven)

D = read_dta('couple-height.dta') %>%
      select(rinch,sinch) %>%
      group_by(rinch,sinch) %>%
      summarize(n=n())


this_caption = 'Source: PSID 2017 data provided by Philip Cohen at https://osf.io/w72pc/\n(all couples in this dataset are opposite sex)'

G1 =
  ggplot( data=D, 
          aes(x=rinch, y=sinch, size=n)) +
  geom_point(shape=16, color='orangered', alpha=1) +
  scale_size_continuous(range=c(0,12)) +
  geom_abline(intercept=0, slope=1, lty=2) +
  scale_y_continuous(breaks=58:77, limits=c(58,77)) +
  scale_x_continuous(breaks=58:77, limits=c(58,77)) +
  labs(title='Distribution of US Couples by Height',
       x='Male (in)', y='Female (in)',
       caption=this_caption) +
  guides(color=FALSE, shape=FALSE,size=FALSE) +
  theme_bw()


print(G1)  

ggsave(plot=G1, filename='couple-height.png',
         height=8,width=8,units='in',dpi=300)


## diffs

df = D %>%
         mutate(ht_diff = rinch-sinch) %>%
         group_by(ht_diff) %>%
         summarize(n=sum(n))

G2 = ggplot(df) +
      aes(x=ht_diff, y=n) +
      geom_bar(stat='identity', fill='orangered', color=NA) +
      scale_x_continuous(breaks=seq(-10, 19,1), limits = c(-10,19)) +
      geom_vline(xintercept=0,lty=2) +
      labs(title='Height Differences of US Couples\nBars=Observed, Line=Expected Under Random Matching',
           x='Male Height - Female Height (in)', y='# of Couples',
           caption=this_caption) +
      theme_bw()
  

# calculate expected counts under random matching of spouses
L = tapply( D$n, list(D$rinch,D$sinch),sum)
L[is.na(L)] = 0

P = prop.table(L)

Pstar = outer( rowSums(P), colSums(P), '*')

hm = as.numeric(rownames(L))
hf = as.numeric(colnames(L))



expected_count = tapply(Pstar, outer(hm,hf,'-'), sum) * sum(D$n)

tmp = data.frame(ht_diff = as.numeric(names(expected_count)),
                 n = expected_count)

G2 = G2 + geom_point(data=tmp) +
        geom_line(data=tmp)

ggsave(plot=G2, filename='couple-height-diffs.png',
       height=8,width=8,units='in',dpi=300)
