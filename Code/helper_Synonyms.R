

library(tidyverse)
w  <- 'girl'
w2 <- 'help'


ocean_ls = "marine|maritime|ocean|sea|coast|tidal|aquatic|coral"


gsub('\\|', ',', ocean_ls)
unlist()
 
w <- strsplit(x = ocean_ls, split = '\\|') %>% unlist(); w

## syn ---------------------------------------------------------------------------------------------
# syn is a zero dependency R package that lists synonyms and antonyms.
# https://github.com/njtierney/syn

# library(syn)
# syn(word = w, n_words = 10)
# syns(words = w, n_words = 10)




## wordnet -----------------------------------------------------------------------------------------
# The wordnet package provides a R interface to the WordNet lexical database of English.
# WordNet® is a large lexical database of English. Nouns, verbs, adjectives and adverbs are grouped 
  # into sets of cognitive synonyms (synsets), each expressing a distinct concept. 
  # Synsets are interlinked by means of conceptual-semantic and lexical relations.
# https://wordnet.princeton.edu/

library("wordnet")
# setDict("C:/Program Files (x86)/WordNet/2.1/dict")
# Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1")
# getDict()



filter <- getTermFilter(type = "ExactMatchFilter", word = w, ignoreCase = TRUE)
terms <- getIndexTerms(pos = "NOUN", 1, filter)
getSynonyms(indexterm = terms[[1]])



# In addition there is the high-level function synonyms() omitting special parameter settings.
w  <- 'girl'
synonyms(word = w, pos = "NOUN")
synonyms(word = w, pos = "VERB")



wnet <- data.frame(
  "synset_offset" = c(02370954,02371120,02371337),
  "ss_type" = c("VERB","VERB","VERB"),
  "word" = c("fill", "depute", "substitute")
)


syn_list  <- apply(X = wnet, MARGIN = 1, FUN = function(row){synonyms(word = row["word"], pos = row["ss_type"])})


wnet$synonyms <- sapply(syn_list, paste,collapse=", ")


wnet$synset <- mapply(synonyms, as.character(wnet$word), as.character(wnet$ss_type))
