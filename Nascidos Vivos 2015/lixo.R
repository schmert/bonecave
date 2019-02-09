# Don't run!
# The following code will download data from the "Declarations of Death" database for
# the Brazilian state of Parana, year 2013. Source: DATASUS / Brazilian Ministry of Health

library(read.dbc)

url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/DNAC2016.dbc"
download.file(url, mode='wb',destfile = "DNAC2016.dbc")
AC <- read.dbc("DNAC2016.dbc")
head(AC)
str(AC)


