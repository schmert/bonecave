
library(tidyverse)

# data from https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=DK-US

D = read_csv('donny-deutsch-data.csv', skip=2)

ggplot(data=D) +
  aes(x=year, )