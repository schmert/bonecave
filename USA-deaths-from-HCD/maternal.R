library(tidyverse)
library(showtext)

## 1st argument is Google name, 2nd is local name
font_add_google("Fira Sans", "Fira")

# use these fonts for all EXTERNAL devices
# i.e., you can use the fonts in pdf, png, jpeg, windows(), x11(),...
# but NOT in R studio Plots panel

showtext_auto() 

age_group = tibble(
  age = c(0,1,seq(from=5,to=85,by=5)),
  agroup = factor(c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
            "70-74", "75-79", "80-84", "85+"))
)

# the HCD user agreement asks that we not share copies of data
# so you'll have to grab this .csv file for yourself from
# causesofdeath.org 
#
# cause 89 is "complications of pregnancy, childbirth, and 
#   puerperium", sex=2 is females

D = read_csv(file='USA_m_interm_idr.csv') %>%
      filter(cause==89, sex==2) %>%
      select(country:m85p) %>%
      pivot_longer(cols=m0:m85p,
                   names_to = 'vname',
                   values_to = 'rate') %>%
      transform(age=c(0,1,seq(5,85,5))) %>%
      left_join(age_group) %>%
      filter(age > 10, age < 50)

txt_df = D %>%
           filter(year == max(year)) %>%
           select(year,agroup,rate)

ggplot(data=D) +
  aes(x=year, y=rate, group=agroup, color=agroup) +
  geom_line(lwd=1.5) +
  theme_bw(base_family = 'Fira') +
  theme(title=element_text(size=60),
        axis.title=element_text(size=44),
        axis.text =element_text(size=44)) +
  guides(color=FALSE) +
  scale_y_continuous(limits=range(0,D$rate)) +
  geom_text(data=txt_df, aes(label=agroup),size=14, 
            nudge_x = 0.5, nudge_y = c(0,1,0,0,0,-1,0)) +
  labs(x='Year',
       y='Rate per Million',
       title='Deaths from Pregnancy and Childbirth Complications',
       subtitle = 'USA Women 1999-2013, per million',
       caption='Source: Human Cause-of-Death Database, https://www.causesofdeath.org')

ggsave('USA-maternal.png', height=11, width=8.5)
