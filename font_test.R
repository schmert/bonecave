rm(list=ls())
graphics.off()

library(tidyverse)
library(showtext)

## downloads from http://fonts.google.com
## 1st argument is Google name, 2nd is local name
font_add_google("Nunito", "Nunito")
font_add_google("Roboto", "Roboto")
font_add_google("Itim", "Itim")
font_add_google("Source Code Pro", "Source_code")
font_add_google("Source Sans Pro", "Source_sans")
font_add_google('Monoton','Monoton')
font_add_google("Fira Sans", "Fira")
font_add_google("Pacifico", "Pacifico")


# use these fonts for all EXTERNAL devices
# i.e., you can use the fonts in pdf, png, jpeg, windows(), x11(),...
# but NOT in R studios plots pand

showtext_auto() 


# mini dataset
df = tibble(
       x = runif(100),
       y = 4 - 2*x + rnorm(100,0,.2)
     )

font_vals = c('Nunito','Roboto','Itim','Source_code',
              'Source_sans','Monoton','Fira','Pacifico')

pdf(file='font_test.pdf')

for (this_font in font_vals) {

  G = ggplot(data=df) +
    aes(x=x, y=y) +
    geom_point(size=1) +
    labs(title    = paste(this_font),
         subtitle = 'Ipsem Lorem Caveat Emptor',
         caption  = 'E pluribus unum') +
    theme_bw(base_family = this_font) +
    theme(plot.title    = element_text(size=28),
          plot.subtitle = element_text(size=18),
          axis.text     = element_text(size=12),
          axis.title    = element_text(size=14))
  
  print(G)
  
}

dev.off()
