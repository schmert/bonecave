
library(read.dbc)

url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DNAC2016.dbc"

download.file(url, mode='wb',destfile = "DNAC2016.dbc")

AC <- read.dbc("DNAC2016.dbc")

head(AC)

str(AC)


