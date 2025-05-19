

# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), force = TRUE)

library(dplyr)
library(stringr)
library(tabulizer)
library(tabulizerjars)

packageVersion('tabulizer')
packageVersion('tabulizerjars')
packageVersion('rJava')


##' Increase Java Memory for R (tabulizer/jars)
##' This gives Java 8GB of memory (adjust to 8g, 16g, etc. if you have lots of RAM).
options(java.parameters = "-Xmx8g")

### Custom function
pdf2text <- function(pdf){
 
  
  
  ## read and clean the pdf ------------------------------------------------------------ #
  txt <- tabulizer::extract_text(file = pdf) %>%
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
                  statement = as.character(statement),
                  nchr = nchar(statement)) %>%
    as.data.frame()

  return(txt_sentence_df)
}





## ##################################################################################### #
## Use `tokenize` to splits the text into sentences [TBD]
## ##################################################################################### #

library(tabulizer)
library(tokenizers)
library(stringi)

pdf2text_tokenize <- function(pdf) {
  
  # === 1. Read the PDF and extract text ===
  raw_text <- tabulizer::extract_text(file = pdf) %>%
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

