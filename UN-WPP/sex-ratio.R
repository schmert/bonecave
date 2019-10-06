###############################################
# sex ratios (males per 100 females)
# from UN WPP 2019 data
#
# uses as input xlsx files downloaded from 
#
# https://population.un.org/wpp/Download/Files/3_Indicators%20(Special%20Aggregates)/EXCEL_FILES/1_EconomicTrading/Population/WPP2019_SA1_POP_F07_2_POPULATION_BY_AGE_MALE.XLSX
# https://population.un.org/wpp/Download/Files/3_Indicators%20(Special%20Aggregates)/EXCEL_FILES/1_EconomicTrading/Population/WPP2019_SA1_POP_F07_3_POPULATION_BY_AGE_FEMALE.XLSX
# on 6 Oct 2019
###############################################

library(tidyverse)
library(readxl)

cnames = c("Index", "Variant", "Type of agg", 
              "Notes", "code", "Type", "Parent code", "date", 
              paste0('N',5*0:20))

ctypes = c(Index = "numeric", Variant = "text", `Type of agg` = "text", 
              Notes = "numeric", `code` = "numeric", Type = "text", 
              `Parent code` = "numeric", `Reference date (as of 1 July)` = "numeric", 
              rep('numeric',21))


MM = read_xlsx(path='WPP2019_SA1_POP_F07_2_POPULATION_BY_AGE_MALE.XLSX',
               sheet=1, skip=17, col_names = cnames, col_types = ctypes,
               na=c('','...'))

FF = read_xlsx(path='WPP2019_SA1_POP_F07_3_POPULATION_BY_AGE_FEMALE.XLSX',
               sheet=1, skip=17,col_names = cnames, col_types = ctypes,
               na=c('','...'))

pop = inner_join(MM,FF, by=names(MM)[1:8], suffix=c('_m','_f'))

## read country code list
## from https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv
##  with one manual edit after download: changed Sudan code from 736 to 729 (UN uses 729)  
##  and one addition: Channel Islands (code 830)
country_codes = read_csv('country-and-continent-codes-list.csv') %>%
                   select(code=Country_Number,
                          name=Country_Name,
                          continent = Continent_Name) %>%
                   add_row(code=830, name='Channel Islands', continent='Europe')

## add continents (matched via 'code'), and only keep
## one observation per country per year (countries are
## included multiple times, under different groupings)

pop = left_join(pop, country_codes, by='code') %>%
       filter(Type=='Country/Area') %>%
       group_by(code,name,date) %>%
       slice(1) %>%
       ungroup()



## observed sex ratios by date and continent

mpf = pop %>%
       mutate( males   = rowSums(.[grep('_m', names(pop))]),
               females = rowSums(.[grep('_f', names(pop))])) %>%
       group_by(date,continent) %>%
       summarize(sex_ratio = 100* sum(males)/sum(females))


G = ggplot(data = mpf) +
           aes(x=date, y=sex_ratio, color=continent, group=continent) +
           geom_point(data=filter(mpf, date < 2020), shape='square',size=2) +
           geom_line(lwd=1.2) +
           geom_vline(xintercept = 2020, lty='dotted') +
           geom_hline(yintercept = 100) +
           labs(x='Year', y='Males per 100 Females',
                title='Observed & Forecast Sex Ratios\nUNSTANDARDIZED FOR AGE',
                caption='Source: UN World Population Prospects 2019, https://population.un.org/wpp/Download/Standard/Population',
                color='Continent') +
           theme_bw() +
           theme(legend.text=element_text(size=5),
                 legend.title=element_text(size=5)) +
           scale_y_continuous(limits=c(86,106), breaks=seq(90,105,5))


#####################################################
## standardized sex ratios by date and continent
#####################################################

## calculate a standard population age structure of women (2020 world total)

std_wt = filter(pop, date==2020) %>%
  select(ends_with('_f')) %>%
  summarize_all(.funs='sum') %>%
  unlist() %>%
  prop.table()

## calculate a standardized sex ratio for each country-year
mcols = grep('_m', names(pop))
fcols = grep('_f', names(pop))

std_mpf = pop %>% 
        select(continent,date, mcols, fcols) %>%
        group_by(continent,date) %>%
        summarize_all(.funs='sum') 

## standardize. This supposes that each location's female population has the 
## same age distribution as the 2020 world population, and averages
## the observeed M[age]/F[age] ratios using those weights
mcols = grep('_m', names(std_mpf))
fcols = grep('_f', names(std_mpf))

# add age-group-specific 100M/F ratios
for (g in seq(mcols)) {
  std_mpf[[paste0('ratio',5*(g-1))]] = 100 * pull(std_mpf, mcols[g])/pull(std_mpf,fcols[g])
}


rcols = grep('ratio', names(std_mpf))

std_mpf$std_sex_ratio = apply(as.matrix(std_mpf[,rcols]), 1, weighted.mean, w=std_wt)

std_mpf = std_mpf %>%
       select(continent,date, std_sex_ratio)

G_std = ggplot(data = std_mpf) +
          aes(x=date, y=std_sex_ratio, color=continent, group=continent) +
          geom_point(data=filter(std_mpf, date < 2020), shape='square',size=2) +
          geom_line(lwd=1.2) +
          geom_vline(xintercept = 2020, lty='dotted') +
          geom_hline(yintercept = 100) +
          labs(x='Year', y='Males per 100 Females',
               title='Observed & Forecast Sex Ratios\nSTANDARDIZED FOR AGE',
               caption='Source: UN World Population Prospcts 2019, https://population.un.org/wpp/Download/Standard/Population',
               color='Continent') +
          theme_bw() +
          theme(legend.text=element_text(size=5),
                legend.title=element_text(size=5)) +
          scale_y_continuous(limits=c(86,106), breaks=seq(90,105,5))


library(cowplot)

plot_grid(G, G_std, nrow=1)

ggsave(file='sex-ratio.png', width=11, height=8.5, units='in', dpi=300)

  

