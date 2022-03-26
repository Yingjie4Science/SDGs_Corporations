
library(dplyr)
library(stringr)

### Custom function
pdf2text <- function(filename){
 
  
  
  ## read and clean the pdf ------------------------------------------------------------ #
  txt <- tabulizer::extract_text(file = filename) %>%
    iconv("UTF8", "ASCII", "") %>%                 # change encoding
    paste(sep = " ") %>%
    stringr::str_replace_all("\\.com", "") %>%     # avoid matching Comoros (iso3 code: COM)
    stringr::str_replace_all(fixed("\n"), " ") %>% # erase new lines
    stringr::str_replace_all(fixed("\r"), " ") %>% # erase carriage return
    stringr::str_replace_all(fixed("\t"), " ") %>% # erase tabs
    stringr::str_replace_all(fixed("\b"), " ") %>% # erase backspace
    stringr::str_replace_all(fixed("\""), " ") %>% # erase "
    paste(sep = " ", collapse = " ") %>%
    stringr::str_squish() %>%                      # remove leading & trailing whitespace, and repeated whitespace inside a string
    stringr::str_replace_all("- ", "")             # fix incomplete words continuing to next line

  
  
  ## text to DF ------------------------------------------------------------------------ #
  ### separate the large character chunk to short sentences by
  ###    '.' (period), ';' (semicolon), ':' (colon), and '/ ' (slash with space)
  ### --> used before 2022/02/06
  # txt_sentence <- unlist(strsplit(txt, split = "\\. |\\; |\\/ |\\: ")) %>% trimws() ## without ',' (comma) 
  
  ### --> edit on 2022/02/06
  px <- paste(".", ";", "/", ":", "?", "!", sep = "\\s|\\"); px
  p0 <- paste0("\\", px); p0
  ### --- split if there is a space between two numbers; e.g., "2012 3.1" --> "2012", "3.1"
  p1 <- '(?<=\\d)\\s\\d'   ## best, only keep the number before that whitespace
  ### --- split if "." followed by a non-number character, e.g., "of a primary school.During 2018" (space is accidentally missing)
  p2 <- '\\.(?=\\D)'
  pat <- paste(
    paste0('(', p1, ')'),
    # paste0('(', p2, ')'), 
    paste0('(', p0, ')'),  
    sep = "|")
  txt_sentence <- unlist(strsplit(txt, split = pat, perl=T)) %>% trimws()
  
  
  txt_sentence_df <- data.frame(statement = txt_sentence) %>%
    dplyr::mutate(id = row.names(.) %>% as.numeric(),
                  statement = as.character(statement)) %>%
    as.data.frame()

  return(txt_sentence_df)
}




### change encoding is needed to fix the encoding problems of punctuation in a sentence --> 'This plan 
###   is designed to fortify our company<U+9225><U+6A9A> position as a leading global building materials company.'

# ## --> get company name
# company_name <- str_split(string = basename(filename), pattern = "--", n = 2) %>% unlist()
# 
# ## --> save as local TXT file
# TXTname <- paste0(filename, company_name[1], '.txt'); print(TXTname)
# writeLines(txt, TXTname)

# ## --> save as r data
# fname <- paste0(dirname(filename), '/pdf2txt/', company_name[1], '.RData'); print(fname)
# save(txt, file = fname)
# 



# ## --> save as r data
# fname <- paste0(dirname(filename), '/pdf2txt2df/',  company_name[1], '.RData'); print(fname)
# save(txt_sentence_df, file = fname)


## re-load data to R
# fname <- paste0(dirname(filename), '/pdf2txt2df/',  company_name[1], '.RData');
# load(fname)


