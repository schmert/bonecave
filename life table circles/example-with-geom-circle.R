library(tweenr) # Available on CRAN
library(ggforce) # Install from thomasp85/ggforce
library(gganimate) # Install from dgrtwo/gganimate
set.seed(2)
x <- sample(9,20, prob=c(1,2,3,4,5,4,3,2,1), replace=T)
df <- data.frame(x = x, y = 15)
dfs <- list(df)
for(i in seq_len(nrow(df))) {
  dftemp <- tail(dfs, 1)
  dftemp[[1]]$y[i] <- sum(dftemp[[1]]$x[seq_len(i)] == dftemp[[1]]$x[i])
  dfs <- append(dfs, dftemp)
}
dfs <- append(dfs, dfs[rep(length(dfs), 3)])
dft <- tween_states(dfs, 10, 1, 'cubic-in', 200)
dft$y <- dft$y - 0.5
dft <- dft[dft$y != 14.5, ]
dft$type <- 'Animate'
dfh <- data.frame(x=x, type = 'Histogram')
p <- ggplot(dft) + 
  geom_circle(aes(x0=x, y0=y, r=0.5, frame = .frame), n=20, fill = 'steelblue') + 
  geom_histogram(aes(x=x), data = dfh, fill = 'forestgreen', color = 'black', binwidth = 1) + 
  coord_fixed(ylim = c(0, 13.5)) + 
  theme_bw() + 
  facet_grid(.~type)
#animation::ani.options(interval = 1/20)
gg_animate(p, 'hist_ex.gif', title_frame = FALSE)
