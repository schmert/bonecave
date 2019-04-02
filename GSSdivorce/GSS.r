#==========================================
# first run the canned R program 
# from GSSDataExplorer.norc.org
#==========================================

library(foreign)
  read.dct <- function(dct, labels.included = "yes") {
      temp <- readLines(dct)
      temp <- temp[grepl("_column", temp)]
      switch(labels.included,
             yes = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
                 classes <- c("numeric", "character", "character", "numeric", "character")
                 N <- 5
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
             },
             no = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
                 classes <- c("numeric", "character", "character", "numeric")
                 N <- 4
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
             })
      temp_metadata <- setNames(lapply(1:N, function(x) {
          out <- gsub(pattern, paste("\\", x, sep = ""), temp)
          out <- gsub("^\\s+|\\s+$", "", out)
          out <- gsub('\"', "", out, fixed = TRUE)
          class(out) <- classes[x] ; out }), NAMES)
      temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
      temp_metadata
  }

  read.dat <- function(dat, metadata_var, labels.included = "yes") {
      read.fwf(dat, widths = metadata_var[["ColWidth"]], col.names = metadata_var[["ColName"]])
  }


GSS_metadata <- read.dct("GSS.dct")
GSS_ascii <- read.dat("GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii

#==========================================
# this analysis: cohort & attitudes about
# divorce law 
# divlaw
#   1 = should be easier
#   2 = should be more difficult
#   3 = should stay the same
#   8 = don't know
#   9 = no answer
#   0 = not applicable
#==========================================
library(tidyverse)

myData = GSS %>%
           filter(DIVLAW %in% 1:3,
                  COHORT < 9999) %>%
           group_by(COHORT) %>%
           mutate(easier = sum(DIVLAW==1)/n() )

theme_carl <- function () { 
  theme_bw(base_size=13) %+replace% 
    theme(
      axis.text = element_text(size=11, face='bold'),
      panel.grid.major = element_line(color=grey(.70)),
      panel.grid.minor = element_line(color=grey(.85)),
      strip.text = element_text(size=13, face='bold')
    )
}


png('GSS-divorce-easier.png',
      height=8.5, width=11, units='in',
      res=300)

G = ggplot(data=myData, 
       aes(x=COHORT+.5, y=100*easier)) +
    geom_point(size=4, color='royalblue', alpha=.70) +
    geom_smooth(color='royalblue') +
    theme_carl() +
    scale_x_continuous(breaks=seq(1920,2000,10),
                       minor_breaks = seq(1925,1995,10)) +
    labs(x='Year of Birth',y='Percent',
         title='% of GSS Respondents saying Divorce should be Easier',
         caption='Source: GSS Explorer, 2014-2018 data')
      
print(G)

dev.off()
