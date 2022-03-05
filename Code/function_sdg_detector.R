
## ######################################################################## ##
## This function will be used to **detect SDGs** in a sentence.             ##
## ######################################################################## ##


# THIS CODE IS TO IDENTIFY MATCHING ROWS, BUT IT DOES NOT EXTRACT SENTENCES
# in the original dataframe, if it matches, then in a new column, say yes; if not, say no.

sdg_detector <- function(df) {
  
  code <- df %>% 
    dplyr::mutate(#match   = 0,      
      sdgs    = '', ## for later use, to append data to this column
      n_total = 0,  ## count the accumulated mentions
      sdgs_n  = '') ## ## combine together SDG names and the number of mentions
  
  for (i in 1:nrow(SDG_keys)){                           ## loop each SDG indicators
    sdg_i_str <- SDG_keys$SDG_id[i] %>% as.character()   ## get the SDG id name
    sdg_i_obj <- SDG_keys$SDG_keywords[i]                ## get the corresponding SDG search term list
    
    print(sdg_i_str)
    # print(sdg_i_obj)
    
    code <-  code %>%
      as.data.frame() %>%
      
      ## at the sentence level - count once if goals/targets are mentioned -------------------------
      dplyr::mutate(
        match = ifelse(
          grepl(pattern = sdg_i_obj, x = statement, ignore.case = T, perl = T), 1, 0))  %>% ## yes-1 or no-0 if they match
      dplyr::mutate(
        sdgs  = ifelse(match > 0, paste0(sdgs, ',', sdg_i_str), sdgs)) %>%
      dplyr::select(-match) %>% ## remove this column 
      
      ## at the sentence level - count the times of all the mentions -------------------------------
      dplyr::mutate(
        n       = str_count(string = statement, regex(pattern = sdg_i_obj, ignore_case = T)),
        n_total = n_total + n,
        sdgs_n  = ifelse(n > 0, paste0(sdgs_n, ',', sdg_i_str, '-', n), sdgs_n)
      ) %>%
        dplyr::select(-n) %>%   ## remove this column 
        as.data.frame()
  }
  
  
  ### sort from most SDG hits to least (or, none)
  coded <- code %>% arrange(desc(nchar(sdgs)), id)
  
  return(coded)
  
}
