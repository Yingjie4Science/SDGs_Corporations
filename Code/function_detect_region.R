



## this script aim to detect country or region names in a sentence or statement

## load name database
file <- paste0('./Data/data_TextMining/database/', 'country_region_names.RData')
load(file)


detect_region <- function(data, text_column) {
  
  
  ## add a "region" column to the dataframe
  data$region <- ''
  
  ## loop and detect each country/region name 
  for (i in 1:nrow(country_region_names)){    
    region_i <- country_region_names$name[i]  ## get the region name
    
    ## if the number of character of a region name is less than 4, we need to add word boundary to the word
    region_i_enhanced <- ifelse(nchar(region_i) < 4,
                                paste0('\\b', region_i, '\\b'),
                                region_i)
    
    ## if the number of character of a region name is less than 4,, we cannot ignore case
    case_to_choose    <- ifelse(nchar(region_i) < 4, 
                                0,   
                                1)
    
    # print(region_i)
    print(region_i_enhanced)
    # print(case_to_choose)
    
    data <-  data %>%
      as.data.frame() %>%
      ## at the sentence level - detect if a subject country or region is mentioned ----------------
      dplyr::mutate(
        region = ifelse(grepl(pattern = region_i_enhanced, 
                              x = !!sym(text_column),   ## using column names as function arguments, see https://stackoverflow.com/questions/48062213/dplyr-using-column-names-as-function-arguments
                              ignore.case = case_to_choose, perl = T), 
                        paste0(region, ',', region_i),  ## If detected, add the region name to the cell
                        paste0(region, '')))  %>%       ## If not, add nothing
      as.data.frame()
  }
  
  return(data)

}