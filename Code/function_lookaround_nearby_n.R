




# (?:...)  the non-grouping parentheses; e.g., to match "grey" or "gray", one can use "gr(?:e|a)y"
# (?=...)  positive look-ahead assertion
# (?<=...) positive look-behind assertion
# \\s      Space, tab
# \\S      Not space
# \\w      a-z, A-Z, 0–9, _
# {,n}     Matches at most n times

condition1 <- "\\s"                 ## a space after the 1st word    
condition2 <- "(?:\\w+\\s)"         ## a word character with a space after it

lookaround_nearby_n <- function(word_ls1, word_ls2, n, exclude = "", third_AND_string = "") {
  
  #'@title Look around
  #'@description Look around to match pattern in a sentence
  #'@author Yingjie Li
  #'
  #'@param word_ls1 is a string, which includes a list of words connected by "|" that indicates 'OR'
  #'@param word_ls2 is a string, which includes a list of words connected by "|" that indicates 'OR'
  #'@param n        is a number, indicates the number of words to look around
  #'@param exclude  is a vector, including a list of words to be excluded from match
  #'@param third_AND_string similar to word_ls1 or word_ls2, it is a string that includes a list of words connected by "|" that indicates 'OR'
  
  pat1 <- paste0(
    paste0('(?:', word_ls1, ')'),  ## the 1st word list
    condition1,                    ## a space after the 1st word 
    condition2,                    ## other strings after the space (`condition2`)
    "{0,", n, "}",                 ## Matches `condition2` at most n times  
    "(?=(?:", word_ls2, "))"       ## the 2nd word list
    ); 
  
  pat2 <- paste0(
    paste0('(?:', word_ls2, ')'),
    condition1,                 
    condition2,     
    "{0,", n, "}",         
    "(?=(?:", word_ls1, "))")     
  
  ## format the 'pat_exclude' ---------------------------------------------------------- #
  nchar_exclude <- paste(exclude, sep = "", collapse = "")
  nchar_exclude <- nchar(nchar_exclude)
  ## words to be excluded, default is ''
  if (nchar_exclude < 1) {
    pat_exclude <- exclude
  } else {
    # pat_exclude <- paste0("(?!.*", paste(exclude, collapse = ")(?!.*"), ")") 
    pat_exclude <- paste0("^(?!.*(?:", paste(exclude, collapse = "|"), ")).*") ## more concise and efficient
  }
  
  
  ## format the 3rd `AND` PATTERN ------------------------------------------------------ #
  if (nchar(third_AND_string) < 1) {
    pat_and <- third_AND_string
  } else {
    pat_and <- paste0('^(?=.*(?:', third_AND_string, ')).+')
  }
  
  
  
  ## combine `pat_exclude` and `pat_and` ----------------------------------------------- #
  pat_exclude_and <- paste0(pat_exclude, pat_and)
  if( nchar(pat_exclude)<1 ) {
    pat_exclude_and <- pat_exclude_and
  } else {
    pat_exclude_and <- gsub('\\.\\*\\^', '', pat_exclude_and); ## remove the ".*^" between `pat_exclude` and `pat_and`
    # pat_exclude_and 
  }
  
  ## assemble the pattern -------------------------------------------------------------- # 
  pat <- paste0(
    pat_exclude_and,          
    '(',                      ## put "pat1|pat2" in a pair of large brackets
    '(', pat1, ')', '|', 
    '(', pat2, ')', ')');  
  
  return(pat)
}



## Test
# install("docstring") ## https://github.com/dasonk/docstring
library(docstring)
docstring(lookaround_nearby_n)











### -----------------------------------------------------------------------------------###
###                                                                                    ###
### A new function that can combine a pair of `lookaround`, and a `AND` condition      ###
###                                                                                    ###
### -----------------------------------------------------------------------------------###

lookaround_nearby_n_plus1AND <- function(word_ls1, word_ls2, n, third_AND_string = "", exclude = "") {
  
  px <- lookaround_nearby_n(word_ls1, word_ls2, n, exclude);
  
  aANDstring  <- paste0('^(?=.*(?:', third_AND_string, ')).+')
  
  pat <- paste(aANDstring, px, sep = "")
  
  return(pat)
}

