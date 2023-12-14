library(tidyverse)
library(HMDHFDplus)

countries = getHFDcountries()$CNTRY

stub = 'https://www.humanfertility.org/File/GetDocument/Docs/'

for (this_country in countries) {
  print(this_country)
  
  try({
    url     = paste0(stub,this_country,'/',this_country,
                   '_indb-map.pdf')
  save_as = paste0(this_country,'inputs.pdf') 
  download.file(url, destfile = save_as, mode='wb')
  })
}
