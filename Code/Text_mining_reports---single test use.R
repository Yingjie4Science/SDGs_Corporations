




txt <- "149 4 In 2018, includes sales of non-strategic fixed assets in the United States, Spain and Mexico for $371, $158 and $125, respectively"
test <- data.frame(statement = txt)

sdg_i_obj <- SDG13_a
sdg_i_str <- 'SDG13_a'
sdgs <- ''

sdg_i_obj <- "^(?=.*(?:climat|warming|'extreme weather'|temperature|heat|melt|SDG 13|goal 13|target 13|indicator 13|^(?=.*(?:sea))(?=.*(?:level))(?=.*(?:ris)).+|kyoto protocol|convention|mitigation))(?=.*(?:financ|fund|assist|help|aid|invest|boost|bolster|cash|compensation|donor|donat|enhanc|expenditure|grant|loan|money|official flow|support|subsid|stimulate|strengthen|transfer|uphold|dollar|mobili|implement|operat)).+|United Nations Framework Convention on Climate Change|Green Climate Fund|G\\.C\\.F\\."

code <-  test %>%
  as.data.frame() %>%
  
  ## at the sentence level - count once --------------------------------------------------------
dplyr::mutate(
  match = ifelse(
    grepl(pattern = sdg_i_obj, x = statement, ignore.case = T, perl = T), 1, 0))  %>% ## yes-1 or no-0 if they match
  dplyr::mutate(sdgs = ifelse(match > 0, paste0(sdgs, ',', sdg_i_str), sdgs)) %>%
  
  ## at the sentence level - count all matches -------------------------------------------------
dplyr::mutate(
  n       = str_count(string = statement, regex(pattern = sdg_i_obj, ignore_case = T)))

