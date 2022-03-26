


## 2.4. Format results

func_format <- function(coded) {

  ### --> pull out the column with coded results
  # coded_sdgs <- dplyr::pull(coded, sdgs)   ## the same as below one
  coded_sdgs <- as.vector(unlist(coded['sdgs'])) %>%
    paste(sep = " ", collapse = " ")
  # coded_sdgs


  ### --> format it as a DF
  coded_sdgs <- unlist(strsplit(coded_sdgs, split = "\\,")) %>% trimws();

  coded_sdgs_df <- as.data.frame(table(coded_sdgs)) %>%
    dplyr::filter(coded_sdgs != '') %>%
    separate(col = coded_sdgs, into = c('goal', 'target_id'), sep = '_', remove = F) %>%
    dplyr::mutate(goal_id   = as.numeric(gsub("\\D", "", goal)),
                  target_id = gsub('^.{3}', "", coded_sdgs)) %>%
    arrange(goal_id, target_id) %>%
    dplyr::select(1:2, goal_id, everything()) %>%
    dplyr::rename(n_statements = Freq)



  ### --> The above data only presents what were detected, but some SDGs without match won't be shown.
  ### --> Force to list all 17 SDGs and 169 Targets


  ### --> merge data
  coded_sdgs_df_format <- merge(coded_sdgs_df, goals_df,
                                by.x = c('goal', 'coded_sdgs'), by.y = c('goalname', 'target_id_un'), all = T) %>%
    dplyr::mutate(goal = factor(goal, levels = goals_ls)) %>%
    arrange(goal, id) %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::mutate(coded_sdgs = gsub('general', 'g', coded_sdgs),
                  coded_sdgs = factor(coded_sdgs, levels = unique(coded_sdgs)),
                  coded_sdgs = reorder(coded_sdgs, id)) %>%
    as.data.frame()

  # str(coded_sdgs_df_format)
  return(coded_sdgs_df_format)

}


