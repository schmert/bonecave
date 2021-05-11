# https://www.ers.usda.gov/webdocs/DataFiles/48685/fluidmilk.xlsx?v=2518.2
#
library(tidyverse)

# in millions of pounds of milk per year
US_fluid_beverage_milk_sales =
tribble(
~year, ~whole, ~reduced, ~lowfat, ~skim,  
1975,	36188,	8726	,2742	,2480
,1976,	35241,	9556	,2875	,2524
,1977,	34036,	10423,	3003,	2617,
1978,	33235,	11017,	3233,	2543,
1979,	32480,	11762,	3281,	2604,
1980,	31253,	12435,	3483,	2636,
1981,	30397,	13088,	3574,	2583,
1982,	29350,	13501,	3537,	2449,
1983,	28871,	14183,	3455,	2474,
1984,	28204,	15143,	3382,	2726,
1985,	27760,	16309,	3503,	3009,
1986,	26446,	17232,	3924,	3236,
1987,	25644,	17600,	3790,	3406,
1988,	24691,	18462,	3752,	3979,
1989,	22823,	19553,	4191,	5006,
1990,	21333,	19565,	4944,	5702,
1991,	20769,	19829,	5210,	6000,
1992,	20196,	19851,	5374,	6357,
1993,	19460,	19528,	5297,	6844,
1994,	19223,	19326,	5409,	7414,
1995,	18662,	18442,	5760,	8359,
1996,	18698,	18194,	5817,	8871,
1997,	18413,	17732,	5977,	9139,
1998,	18147,	17333,	6113,	9203,
1999,	18467,	17489,	6082,	8985,
2000,	18698,	17418,	6276,	8347,
2001,	18283,	17365,	6303,	8143,
2002,	18267,	17409,	6350,	8090,
2003,	18101,	17419,	6233,	7829,
2004,	17513,	17523,	6268,	7795,
2005,	16922,	17834,	6452,	8027,
2006,	16535,	18023,	6555,	8113,
2007,	15937,	18257,	6746,	8223,
2008,	15331,	18553,	6888,	8275,
2009,	15124,	18871,	7091,	8216,
2010,	14414,	19118,	7482,	8385,
2011,	14043,	18966,	7648,	8216,
2012,	13854,	18705,	7756,	7739,
2013,	13786,	18480,	7503,	7092,
2014,	13849,	17860,	7358,	6317,
2015,	14451,	16759,	7719,	5635,
2016,	15242,	16556,	7305,	5013,
2017,	15621,	16100,	6806,	4410,
2018,	15901,	15665,	6444,	3965,
2019,	16119,	15281,	6068,	3541
)

# from https://fred.stlouisfed.org/series/POP
# in thousands of people
US_pop = 
  tribble(
~year, ~pop,
1952, 157553,
1953, 160184,
1954, 163026,
1955, 165931,
1956, 168903,
1957, 171984,
1958, 174882,
1959, 177830,
1960, 180671,
1961, 183691,
1962, 186538,
1963, 189242,
1964, 191889,
1965, 194303,
1966, 196560,
1967, 198712,
1968, 200706,
1969, 202677,
1970, 205052,
1971, 207661,
1972, 209896,
1973, 211909,
1974, 213854,
1975, 215973,
1976, 218035,
1977, 220239,
1978, 222585,
1979, 225055,
1980, 227726,
1981, 229966,
1982, 232188,
1983, 234307,
1984, 236348,
1985, 238466,
1986, 240651,
1987, 242804,
1988, 245021,
1989, 247342,
1990, 250132,
1991, 253493,
1992, 256894,
1993, 260255,
1994, 263436,
1995, 266557,
1996, 269667,
1997, 272912,
1998, 276115,
1999, 279295,
2000, 282385,
2001, 285309,
2002, 288105,
2003, 290820,
2004, 293463,
2005, 296186,
2006, 298996,
2007, 302004,
2008, 304798,
2009, 307439,
2010, 309741.279,
2011, 311973.914,
2012, 314167.558,
2013, 316294.766,
2014, 318576.955,
2015, 320870.703,
2016, 323161.011,
2017, 325206.03,
2018, 326923.976,
2019, 328475.998,
2020, 330113.98
) %>% 
  mutate(pop_millions = pop/1000)

D = left_join(US_fluid_beverage_milk_sales, US_pop) %>% 
      mutate( whole_pc   = whole/pop_millions,
              reduced_pc = reduced/pop_millions,
              lowfat_pc  = lowfat/pop_millions,
              skim_pc    = skim/pop_millions,
              total_pc   = whole_pc+reduced_pc+lowfat_pc+skim_pc) %>% 
      pivot_longer(cols=contains('_pc'), names_to = 'categ', values_to = 'value') %>% 
      select(year,categ,value)



ggplot(data=D) +
  aes(x=year, y=value, group=categ, color=categ) +
  geom_line(size=1.5) +
  labs(x='Year',y='Pounds/Year',
       title='US beverage milk consumption',
       subtitle='Pounds per person per year',
       caption='Source:https://www.ers.usda.gov/webdocs/DataFiles/48685/fluidmilk.xlsx?v=2518.2
\nhttps://fred.stlouisfed.org/series/POP') +
  geom_text(aes(x=1985, y=220, label='Total'), color='purple') +
  geom_text(aes(x=1985, y=135, label='Whole'), color='orange') +
  geom_text(aes(x=1985, y= 45, label='Reduced (2%)'), color='blue') +
  geom_text(aes(x=2000, y= 10, label='Lowfat (1%)'), color='red') +
  geom_text(aes(x=1998, y= 45, label='Skim'), color='darkgreen') +
  theme_bw() +
  theme(axis.text = element_text(face='bold',size=12),
        axis.title
        =element_text(face='bold',size=12),
        plot.caption =element_text(size=7) ) +
  scale_color_manual(values=
    c('red','blue','darkgreen','purple','orange')) +
  guides(color=FALSE)
  

ggsave('US_milk.png', dpi=300)
