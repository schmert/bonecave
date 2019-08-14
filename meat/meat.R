M = read.csv('meat.csv') %>%
      filter(TIME==2019, MEASURE=='KG_CAP') %>%
      group_by(LOCATION) %>%
      summarize(KG_CAP = sum(Value))

URL = 'https://tinyurl.com/y4l4mxbr'



ggplot(data=M, aes(y=KG_CAP, x=reorder(LOCATION,KG_CAP))) +
  geom_bar(stat='identity', width=.50, color='royalblue', fill='royalblue') +
  coord_flip() +
  labs(title='Per capita meat consumption (est. 2019)',
       caption=paste('Source: OECD data from', URL),
       y='Annual kg meat per capita',
       x='Country') +
  theme_bw() +
  theme(axis.text=element_text(face='bold', size=13))

ggsave(file='meat.png', height=8.5, width=11, units='in', dpi=300)