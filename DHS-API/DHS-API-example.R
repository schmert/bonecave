####################################################
# This is a variant of the R example 
# from DHS website 
# https://api.dhsprogram.com/#/samples-r.cfm
#
# TFR estimates for all DHS surveys
####################################################

require(RJSONIO)
library(tidyverse)

# Import DHS Indicator data for TFR for each survey

json_file <- fromJSON("https://api.dhsprogram.com/rest/dhs/data/FE_FRTR_W_TFR?perpage=500")

# Unlist the JSON file entries
json_data <- lapply(json_file$Data, function(x) { unlist(x) })

# Convert JSON input to a data frame
APIdata <- as.data.frame(do.call("rbind", json_data),
                         stringsAsFactors=FALSE) %>%
           mutate(SurveyYear = as.numeric(SurveyYear), TFR=as.numeric(Value)) %>%
           select(CountryName, SurveyYear, TFR, DataId, SurveyId)

head(APIdata,20)

#=============================================
#  make a PDF file with plots of this data
#=============================================

pdf( file='DHS-API-example.pdf', height=8.5, width=11)

  print( 
    ggplot( data=APIdata, aes(x=SurveyYear, y=TFR, group=CountryName, color=CountryName)) +
    geom_line() +
    geom_point() +
    lims(x=range(APIdata$SurveyYear),
         y=range(1,APIdata$TFR)) +
    labs(title='All DHS Countries: Kind of a Mess',
         caption='Source: API at https://api.dhsprogram.com') +
    theme_bw()  
  )
  
  SelectedLatAm = filter( APIdata,
                          CountryName %in% 
                  c('Bolivia','Brazil','Colombia',
                    'Dominican Republic','Guatemala',
                    'Nicaragua','Peru'))
  
  print( 
    ggplot( data=SelectedLatAm, aes(x=SurveyYear, y=TFR, group=CountryName, color=CountryName)) +
      geom_line(lwd=2) +
      geom_point(size=4) +
      lims(x=range(APIdata$SurveyYear),
           y=range(1,SelectedLatAm$TFR)) +
      labs(title='DHS TFR for selected Latin American & Caribb countries',
           caption='Source: API at https://api.dhsprogram.com') +
      theme_bw()  
  )
  
  for (this_country in unique(APIdata$CountryName)) {
    df = filter(APIdata, CountryName==this_country)
    print(
      ggplot( data=df, aes(x=SurveyYear, y=TFR)) +
        geom_line(lwd=2, color='royalblue') +
        geom_point(size=4,color='royalblue') +
        lims(x=range(APIdata$SurveyYear),
             y=range(1,APIdata$TFR)) +
        labs(title=this_country,
             caption='Source: API at https://api.dhsprogram.com') +
        theme_bw()           
    )
  }

dev.off()
