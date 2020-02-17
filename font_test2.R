rm(list=ls())
graphics.off()

library(tidyverse)
library(showtext)

font_add_google("Fira Sans", "Fira")
showtext_auto() 

theme_carl <- function () { 
  theme_bw(base_size=13, base_family="Fira") %+replace% 
    theme(
      axis.text        = element_text(size=11, face='bold'),
      panel.grid.major = element_line(color=grey(.80),size=0.3),
      panel.grid.minor = element_line(color=grey(.95),size=0.1),
      strip.text       = element_text(size=13, face='bold'),
      plot.title       = element_text(size=28,hjust=0),
      plot.subtitle    = element_text(size=18,hjust=0),
      axis.title       = element_text(size=14)
    )
}

# mini dataset
df = tibble(
       x = runif(100),
       y = 4 - 2*x + rnorm(100,0,.40)
     )

pdf(file='font_test2.pdf')

  G = ggplot(data=df) +
    aes(x=x, y=y) +
    geom_point(size=3, shape=15, color='purple', alpha=0.90) +
    labs(title    = 'Fake Dataset w/ Fira Theme',
         subtitle = 'Ipsum Lorem Caveat Emptor',
         caption  = 'Ipso facto e pluribus unum',
         x        = 'Age',
         y        = 'Height') +
    theme_carl()
  
  print(G)

dev.off()
