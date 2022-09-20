Using @Censusreporter metadata with {tidycensus}
================
Carl Schmertmann
20 Sep 2022

## Main Point

@Censusreporter has precomputed .csv files with all of the metadata in
the ACS table lists and table shells. These files are available on
Github, and they provide a way to search the lists of Census API
variables from within R.

Here I present a simple example in which we search for tables related to
the recent fertility of women 15–49.

## Load {tidycensus} and {tidyverse}

``` r
suppressPackageStartupMessages( library(tidycensus) )
```

    ## Warning: package 'tidycensus' was built under R version 4.2.1

``` r
suppressPackageStartupMessages( library(tidyverse) )
```

## Read precomputed metadata from @censusreporter on Github

In this case we’ll read ACS 2020 5-year tables and columns/variables

``` r
stub = 'https://raw.githubusercontent.com/censusreporter/census-table-metadata/master/precomputed/'

tab_info_url = paste0(stub, 'acs2020_5yr/census_table_metadata.csv')
col_info_url = paste0(stub, 'acs2020_5yr/census_column_metadata.csv')

tab_info = read_csv(tab_info_url)
```

    ## Rows: 1140 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): table_id, table_title, simple_table_title, subject_area, universe, ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
col_info = read_csv(col_info_url)
```

    ## Rows: 28002 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): table_id, column_id, column_title, parent_column_id
    ## dbl (2): line_number, indent
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Pick a keyword and search for corresponding tables

We’ll try “Fertility”

``` r
keyword = 'Fertility'

has_keyword = str_which(tab_info$subject_area, keyword)

candidate_tables = tab_info[has_keyword,] %>% 
                      select(table_id, simple_table_title)

candidate_tables %>% 
  print(n=9999)
```

    ## # A tibble: 17 × 2
    ##    table_id simple_table_title                                                  
    ##    <chr>    <chr>                                                               
    ##  1 B13002   Women Who Had a Birth by Marital Status and Age                     
    ##  2 B13002A  Women Who Had a Birth by Marital Status (White Alone)               
    ##  3 B13002B  Women Who Had a Birth by Marital Status (Black or African American …
    ##  4 B13002C  Women Who Had a Birth by Marital Status (American Indian and Alaska…
    ##  5 B13002D  Women Who Had a Birth by Marital Status (Asian Alone)               
    ##  6 B13002E  Women Who Had a Birth by Marital Status (Native Hawaiian and Other …
    ##  7 B13002F  Women Who Had a Birth by Marital Status (Some Other Race Alone)     
    ##  8 B13002G  Women Who Had a Birth by Marital Status (Two or More Races)         
    ##  9 B13002H  Women Who Had a Birth by Marital Status (White Alone, Not Hispanic …
    ## 10 B13002I  Women Who Had a Birth by Marital Status (Hispanic or Latino)        
    ## 11 B13004   Women Who Had a Birth by Presence of Spouse or Unmarried Partner    
    ## 12 B13008   Women Who Had a Birth by Marital Status and Nativity                
    ## 13 B13010   Women Who Had a Birth by Marital Status and Poverty Status          
    ## 14 B13012   Women Who Had a Birth by Marital Status and Labor Force Status      
    ## 15 B13014   Women Who Had a Birth by Marital Status and Educational Attainment  
    ## 16 B13015   Women Who Had a Birth by Marital Status and Receipt of Public Assis…
    ## 17 B13016   Women Who Had a Birth by Age

## Pick a candidate table (or two, or …) to examine in detail

Two of these tables look promising, so look at the more detailed
metadata.

``` r
selected_tabs = c('B13002','B13016')

col_info %>% 
  filter( table_id %in% selected_tabs) %>% 
  left_join(tab_info) %>% 
  select(table_id, line_number, column_title) %>% 
  print(n=9999)
```

    ## Joining, by = "table_id"

    ## # A tibble: 36 × 3
    ##    table_id line_number column_title                                         
    ##    <chr>          <dbl> <chr>                                                
    ##  1 B13002             1 Total:                                               
    ##  2 B13002             2 Women who had a birth in the past 12 months:         
    ##  3 B13002             3 Now married (including separated and spouse absent): 
    ##  4 B13002             4 15 to 19 years old                                   
    ##  5 B13002             5 20 to 34 years old                                   
    ##  6 B13002             6 35 to 50 years old                                   
    ##  7 B13002             7 Unmarried (never married, widowed, and divorced):    
    ##  8 B13002             8 15 to 19 years old                                   
    ##  9 B13002             9 20 to 34 years old                                   
    ## 10 B13002            10 35 to 50 years old                                   
    ## 11 B13002            11 Women who did not have a birth in the past 12 months:
    ## 12 B13002            12 Now married (including separated and spouse absent): 
    ## 13 B13002            13 15 to 19 years old                                   
    ## 14 B13002            14 20 to 34 years old                                   
    ## 15 B13002            15 35 to 50 years old                                   
    ## 16 B13002            16 Unmarried (never married, widowed, and divorced):    
    ## 17 B13002            17 15 to 19 years old                                   
    ## 18 B13002            18 20 to 34 years old                                   
    ## 19 B13002            19 35 to 50 years old                                   
    ## 20 B13016             1 Total:                                               
    ## 21 B13016             2 Women who had a birth in the past 12 months:         
    ## 22 B13016             3 15 to 19 years old                                   
    ## 23 B13016             4 20 to 24 years old                                   
    ## 24 B13016             5 25 to 29 years old                                   
    ## 25 B13016             6 30 to 34 years old                                   
    ## 26 B13016             7 35 to 39 years old                                   
    ## 27 B13016             8 40 to 44 years old                                   
    ## 28 B13016             9 45 to 50 years old                                   
    ## 29 B13016            10 Women who did not have a birth in the past 12 months:
    ## 30 B13016            11 15 to 19 years old                                   
    ## 31 B13016            12 20 to 24 years old                                   
    ## 32 B13016            13 25 to 29 years old                                   
    ## 33 B13016            14 30 to 34 years old                                   
    ## 34 B13016            15 35 to 39 years old                                   
    ## 35 B13016            16 40 to 44 years old                                   
    ## 36 B13016            17 45 to 50 years old

That’s it. We can do all of the searching from within R, thanks to the
heroic efforts of @censusreporter to build and publish the metadata
files in an easily-readble format.

At this point we can download the data of interest, *including* variable
descriptions in addition to the (often obscure) Census codes.

## Download a table and automatically join variable descriptions

Say we decided we want the simpler table, B13106, for counties in
Missouri. We can use *tidycensus::get_acs* and then join the metadata to
make file with variable descriptions:

``` r
# make a small dataframe with metadata for this table 

mytab      = 'B13016'
mytab_info = col_info %>% 
              filter(table_id == mytab) %>% 
              select(table_id, column_id, column_title)

print( mytab_info, n=999)
```

    ## # A tibble: 17 × 3
    ##    table_id column_id column_title                                         
    ##    <chr>    <chr>     <chr>                                                
    ##  1 B13016   B13016001 Total:                                               
    ##  2 B13016   B13016002 Women who had a birth in the past 12 months:         
    ##  3 B13016   B13016003 15 to 19 years old                                   
    ##  4 B13016   B13016004 20 to 24 years old                                   
    ##  5 B13016   B13016005 25 to 29 years old                                   
    ##  6 B13016   B13016006 30 to 34 years old                                   
    ##  7 B13016   B13016007 35 to 39 years old                                   
    ##  8 B13016   B13016008 40 to 44 years old                                   
    ##  9 B13016   B13016009 45 to 50 years old                                   
    ## 10 B13016   B13016010 Women who did not have a birth in the past 12 months:
    ## 11 B13016   B13016011 15 to 19 years old                                   
    ## 12 B13016   B13016012 20 to 24 years old                                   
    ## 13 B13016   B13016013 25 to 29 years old                                   
    ## 14 B13016   B13016014 30 to 34 years old                                   
    ## 15 B13016   B13016015 35 to 39 years old                                   
    ## 16 B13016   B13016016 40 to 44 years old                                   
    ## 17 B13016   B13016017 45 to 50 years old

``` r
# read the ACS data with {tidycensus}

MO = get_acs( geography='county', state='MO', table=mytab)
```

    ## Getting data from the 2016-2020 5-year ACS

    ## Loading ACS5 variables for 2020 from table B13016. To cache this dataset for faster access to ACS tables in the future, run this function with `cache_table = TRUE`. You only need to do this once per ACS dataset.

``` r
# Join the table data to the ACS data (this requires tweaking the tidycensus
# variable name to make a new column called column_id that matches the name
# and underbar-less format of the metadata). I'll also simplify county names.

MO = MO %>% 
      mutate( column_id = str_replace(variable,'_',''),
              NAME      = str_replace(NAME,' County, Missouri','')) %>% 
      left_join(mytab_info, by='column_id')


print(MO, n=50)  
```

    ## # A tibble: 1,955 × 8
    ##    GEOID NAME     variable   estimate   moe column_id table_id column_title     
    ##    <chr> <chr>    <chr>         <dbl> <dbl> <chr>     <chr>    <chr>            
    ##  1 29001 Adair    B13016_001     7307   154 B13016001 B13016   Total:           
    ##  2 29001 Adair    B13016_002      247    79 B13016002 B13016   Women who had a …
    ##  3 29001 Adair    B13016_003        2     5 B13016003 B13016   15 to 19 years o…
    ##  4 29001 Adair    B13016_004       57    31 B13016004 B13016   20 to 24 years o…
    ##  5 29001 Adair    B13016_005       72    40 B13016005 B13016   25 to 29 years o…
    ##  6 29001 Adair    B13016_006       90    54 B13016006 B13016   30 to 34 years o…
    ##  7 29001 Adair    B13016_007       26    28 B13016007 B13016   35 to 39 years o…
    ##  8 29001 Adair    B13016_008        0    22 B13016008 B13016   40 to 44 years o…
    ##  9 29001 Adair    B13016_009        0    22 B13016009 B13016   45 to 50 years o…
    ## 10 29001 Adair    B13016_010     7060   169 B13016010 B13016   Women who did no…
    ## 11 29001 Adair    B13016_011     1536    56 B13016011 B13016   15 to 19 years o…
    ## 12 29001 Adair    B13016_012     2631   150 B13016012 B13016   20 to 24 years o…
    ## 13 29001 Adair    B13016_013      616    67 B13016013 B13016   25 to 29 years o…
    ## 14 29001 Adair    B13016_014      639    97 B13016014 B13016   30 to 34 years o…
    ## 15 29001 Adair    B13016_015      442    85 B13016015 B13016   35 to 39 years o…
    ## 16 29001 Adair    B13016_016      546    86 B13016016 B13016   40 to 44 years o…
    ## 17 29001 Adair    B13016_017      650    60 B13016017 B13016   45 to 50 years o…
    ## 18 29003 Andrew   B13016_001     3651    98 B13016001 B13016   Total:           
    ## 19 29003 Andrew   B13016_002      107    60 B13016002 B13016   Women who had a …
    ## 20 29003 Andrew   B13016_003        1     2 B13016003 B13016   15 to 19 years o…
    ## 21 29003 Andrew   B13016_004       18    25 B13016004 B13016   20 to 24 years o…
    ## 22 29003 Andrew   B13016_005       35    30 B13016005 B13016   25 to 29 years o…
    ## 23 29003 Andrew   B13016_006       19    20 B13016006 B13016   30 to 34 years o…
    ## 24 29003 Andrew   B13016_007       17    15 B13016007 B13016   35 to 39 years o…
    ## 25 29003 Andrew   B13016_008        0    19 B13016008 B13016   40 to 44 years o…
    ## 26 29003 Andrew   B13016_009       17    28 B13016009 B13016   45 to 50 years o…
    ## 27 29003 Andrew   B13016_010     3544   119 B13016010 B13016   Women who did no…
    ## 28 29003 Andrew   B13016_011      520    49 B13016011 B13016   15 to 19 years o…
    ## 29 29003 Andrew   B13016_012      379    25 B13016012 B13016   20 to 24 years o…
    ## 30 29003 Andrew   B13016_013      442    20 B13016013 B13016   25 to 29 years o…
    ## 31 29003 Andrew   B13016_014      491    44 B13016014 B13016   30 to 34 years o…
    ## 32 29003 Andrew   B13016_015      427   121 B13016015 B13016   35 to 39 years o…
    ## 33 29003 Andrew   B13016_016      598   122 B13016016 B13016   40 to 44 years o…
    ## 34 29003 Andrew   B13016_017      687    84 B13016017 B13016   45 to 50 years o…
    ## 35 29005 Atchison B13016_001      959    31 B13016001 B13016   Total:           
    ## 36 29005 Atchison B13016_002       52    24 B13016002 B13016   Women who had a …
    ## 37 29005 Atchison B13016_003        0    17 B13016003 B13016   15 to 19 years o…
    ## 38 29005 Atchison B13016_004        5     7 B13016004 B13016   20 to 24 years o…
    ## 39 29005 Atchison B13016_005       28    18 B13016005 B13016   25 to 29 years o…
    ## 40 29005 Atchison B13016_006       13    15 B13016006 B13016   30 to 34 years o…
    ## 41 29005 Atchison B13016_007        5     9 B13016007 B13016   35 to 39 years o…
    ## 42 29005 Atchison B13016_008        0    17 B13016008 B13016   40 to 44 years o…
    ## 43 29005 Atchison B13016_009        1     2 B13016009 B13016   45 to 50 years o…
    ## 44 29005 Atchison B13016_010      907    38 B13016010 B13016   Women who did no…
    ## 45 29005 Atchison B13016_011      132    17 B13016011 B13016   15 to 19 years o…
    ## 46 29005 Atchison B13016_012      113    12 B13016012 B13016   20 to 24 years o…
    ## 47 29005 Atchison B13016_013      114    17 B13016013 B13016   25 to 29 years o…
    ## 48 29005 Atchison B13016_014      110    16 B13016014 B13016   30 to 34 years o…
    ## 49 29005 Atchison B13016_015      150    28 B13016015 B13016   35 to 39 years o…
    ## 50 29005 Atchison B13016_016      103    29 B13016016 B13016   40 to 44 years o…
    ## # … with 1,905 more rows

Thanks, @censusreporter!
