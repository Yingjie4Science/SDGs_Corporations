


### UN Goal and Target list -----------------------------------------------------
library(reshape2)
library(dplyr)
library(tidyr)

goals_ls <- paste('SDG', seq(1,17), sep = ''); goals_ls
goals_df <- data.frame(goal = goals_ls)


ls_un <- read.csv('./Data/data_raw/_ls_un_goal_target.csv', stringsAsFactors = F) %>%
  dplyr::filter(!is.na(GoalID)) %>%
  tidyr::separate(Targets, c('target_id_un', 'target_desc_un'), sep = ' ', extra = 'merge', remove = T)
# str(ls_un)

ls_un_id <- ls_un %>%
  dplyr::select(GoalID, target_id_un) %>%
  tidyr::separate(target_id_un, c('target_id_un1', 'target_id_un2'), sep = '\\.', remove = F) %>%
  dplyr::select(-target_id_un1) %>%
  dplyr::mutate(goalname = paste0('SDG', GoalID),
                goalname = factor(goalname, levels = goals_ls))
# str(ls_un_id)


