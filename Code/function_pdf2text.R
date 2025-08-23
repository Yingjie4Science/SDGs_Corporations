

## ##################################################################################### #
## Install and load required packages 
## ##################################################################################### #

#' Note: 
#' The package `tabulizer` (starting from version 0.2.2) has been renamed as `tabulapdf` (starting from version 1.0.5) since 2024-04-10
#' See details in the NEWS.md file:
#' https://github.com/ropensci/tabulapdf/commit/581cad83014dc083dcb97dfda390ae2f77e6873d#diff-51920e95310ebfbc1ae31709f3b95f89afffbf4f1a6e38e8b2b406e2fb6197eaR3
#' https://github.com/ropensci/tabulapdf/blob/03cabea1c4cd5fec818a9539115d773c5cb4ff0b/NEWS.md#changes-to-tabulapdf-105

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}


pkg <- "tabulapdf"

if (!requireNamespace(pkg, quietly = TRUE)) {

  # on 64-bit Windows
  remotes::install_github(c("ropensci/tabulapdf"), INSTALL_opts = "--no-multiarch")
  
  # # elsewhere, e.g., on Linux or macOS
  # remotes::install_github(c("ropensci/tabulapdf"))
}


library(dplyr)
library(stringr)

packageVersion('tabulapdf')      # 1.0.5.5
packageVersion('rJava')          # 1.0.6


##' Increase Java Memory for R (tabulizer/jars)
##' This gives Java 8GB of memory (adjust to 8g, 16g, etc. if you have lots of RAM).
options(java.parameters = "-Xmx64g")



# -------------------------------------------------------------------------------------- #
# Function to count words with less than 3 letters
count_short_words <- function(x) {
  sum(nchar(x) < 3)
}


## ##################################################################################### #
## Use self-defined `sep` to splits the text into sentences
## ##################################################################################### #
pdf2text <- function(pdf){

  ## read and clean the pdf ---------------------- #
  txt <- extract_text(file = pdf) %>%
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

  
  
  ## text to DF ---------------------------------- #
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
                  statement = as.character(statement),
                  nchr = nchar(statement)) %>%
    as.data.frame()

  return(txt_sentence_df)
}






## ##################################################################################### #
## Use `tokenize` to splits the text into sentences
## ##################################################################################### #

library(tokenizers)
library(stringi)

pdf2text_tokenize <- function(pdf) {
  
  # === 1. Read the PDF and extract text ===
  raw_text <- extract_text(file = pdf) %>%
    # paste(collapse = " ") %>%                                # Combine pages first before processing
    paste(collapse = "\n") %>%                                 # Preserve newlines
    iconv("UTF-8", "ASCII", sub = "") %>%                      # Convert encoding, remove non-ASCII
    stringi::stri_enc_toutf8() 
  
  
  # --- Common base cleaning ---
  base_clean <- raw_text %>%
    # str_replace_all("\r\n|\r", "\n") %>%            # Normalize newlines first
    str_replace_all("http[s]?://\\S+", "") %>%                 # Remove URLs
    str_replace_all("\\s*www\\.\\S+", "") %>%                  # Remove www. URLs not caught by http
    str_replace_all("\\.{2,}", ".") %>%                        # Replace multiple dots with one
    str_replace_all("(-\\s){2,}", "- ") %>%                    # Replace repeated "- " with a single "- "
    str_replace_all("[—–]", " ") %>%                           # Normalize dashes to space
    str_replace_all("[‘’]", "'") %>%                           # Normalize single curly quotes
    str_replace_all("[“”]", "\"") %>%                          # Normalize double curly quotes
    str_replace_all("\u00A0", " ") %>%              # Replace non-breaking space with regular space
    str_replace_all("\u2022", "- ") %>%             # Replace bullet points (example)
    str_replace_all("\u2013|\u2014", "-") %>%       # More dash normalization
    # str_replace_all("\\b(e\\.g|i\\.e|etc)\\.", "\\1")            # Temporarily strip period from abbreviations
    str_replace_all("\\b(e\\.g|i\\.e|etc)\\.(?=(\\s|$))", "\\1") # Temporarily strip period from abbreviations
    
    
  
  # --- Step 1: Paragraph tokenization (preserve \n structure) ---------------------------
  paragraph_clean <- base_clean %>%
    str_replace_all("[^[:print:]\n\r]", " ") %>%  # Allow line breaks to stay
    str_replace_all("[\t\b]+", " ") %>%           # Replace tabs and backspaces (and sequences of them) with a single space. # Using + to handle multiple occurrences.
    str_replace_all("[ ]+", " ") %>%              # Ensure multiple spaces are condensed to one (but not touching \n)
    str_squish()
  
  
  # # Tokenize paragraphs and create DataFrame  
  # paragraphs <- tokenizers::tokenize_paragraphs(paragraph_clean, simplify = TRUE)
  # paragraph_df <- tibble(paragraph = paragraphs) %>%
  #   mutate(nchr = nchar(paragraph))
  ##' --> After several attempts, this method failed to properly tokenize paragraphs and typically returned only a single paragraph.
  ##' As a workaround, we chose to group a fixed number of adjacent sentences to approximate paragraph structure for contextual analysis.
  
  

  
  # --- Step 2: Sentence tokenization (after flatten \n) ---------------------------------
  sentence_clean <- base_clean %>%
    # str_replace_all("-\\s+", "") %>%                         # Remove hyphen-line breaks
    str_replace_all("\\s+([.,!?;:])", "\\1") %>%               # Remove space before punctuation
    str_replace_all('[\n\r\t\b\"]', " ") %>%                   # remove new lines, carriage return, tabs, backspace, and double quotes
    # stringr::str_replace_all(fixed("\n"), " ") %>% # erase new lines
    # stringr::str_replace_all(fixed("\r"), " ") %>% # erase carriage return
    # stringr::str_replace_all(fixed("\t"), " ") %>% # erase tabs
    # stringr::str_replace_all(fixed("\b"), " ") %>% # erase backspace
    # stringr::str_replace_all(fixed("\""), " ") %>% # erase "
    
    
    str_replace_all("[^[:print:]]", " ") %>%                 # Remove non-printable characters
    # paste(., collapse = " ") %>%
    stringr::str_squish()
  
  # === 2. Split text into sentences ===
  sentences <- tokenize_sentences(sentence_clean)[[1]]
  
  # === 3. Convert to DataFrame with character count ===
  sentence_df <- tibble(sentence = sentences) %>%
    dplyr::mutate(nchr = nchar(sentence)) %>%
    dplyr::rename(statement = sentence) 
  
  
  # === 3.1 Add word count and short word count columns ===
  
  # Tokenize statements once for efficiency
  tokens_list <- tokenize_words(sentence_df$statement)
  
  # Add total word count
  sentence_df$word_count <- sapply(tokens_list, length)
  
  # Add short word count (words with fewer than 3 characters)
  sentence_df$short_word_count <- sapply(tokens_list, count_short_words)
  
  
  # === 4. Save to CSV ===
  # write.csv(txt_sentence_df2, "sentences.csv", row.names = FALSE)
  
  
  # === 5. Return both ===
  # return(list(sentences = sentence_df, paragraphs = paragraph_df))
  return(sentence_df)

}


##'
##'
##'

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

  
# ------------------------------------------------------- #

