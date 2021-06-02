hue = rainbow(9)[3:7]

ggplot(data=filter(D, Cohort %in% 1920:2000)) +
  theme_bw() +
  scale_y_continuous(limits=range(0,1.8),
                     breaks=seq(0,1.8,0.5),
                     expand=c(0,0)) +
  scale_x_continuous(breaks=seq(1920,1995,5),
                     minor_breaks = 1910:2000,
                     expand=c(0,0)) +
  geom_line(size=1.5,aes(x=Cohort, y=CFR-CFR40), color=hue[1]) +
  geom_line(size=1.5,aes(x=Cohort, y=CFR40-CFR35), color=hue[2]) +
  geom_line(size=1.5,aes(x=Cohort, y=CFR35-CFR30), color=hue[3]) +
  geom_line(size=1.5,aes(x=Cohort, y=CFR30-CFR25), color=hue[4]) + 
  geom_line(size=1.5,aes(x=Cohort, y=CFR25), color=hue[5]) +
  geom_vline(xintercept = seq(1920,2000,10),size=0.2) +
  labs(title='USA: Average # of Children Born in Different Age Ranges',
       subtitle='by Woman\'s Year of Birth',
       x = 'Year of Birth',
       y = 'Average # of Children',
       caption = 'Source: Human Fertility Database\nhumanfertility.org 29 May 2021') +
  geom_text(x=1995,y=0.7, label='Ages 13-24', color=hue[5]) +
  geom_text(x=1991,y=0.5, label='25-29', color=hue[4]) +
  geom_text(x=1982,y=0.4, label='30-34', color=hue[3]) +
  geom_text(x=1975,y=0.15, label='35-39', color=hue[2]) + 
  geom_text(x=1968,y=0.05, label='40+', color=hue[1]) 
                          
