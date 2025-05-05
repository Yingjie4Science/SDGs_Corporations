

fname <- paste0('./Data/data_TextMining/database/', 'ls_com_top20.RData'); fname
load(fname)


ls_select <- ls_com_top20
ls_select %>% sort()


### select a few companies as input
# ls_select <- c(
#   # "Legrand", 
#   "Grupo Financiero Banorte", 
#   "Access Bank", 
#   # "Bayer", 
#   "Sanofi",
#   # "Pfizer", 
#   # "Toyota Motor", 
#   "Novartis")


### 
ls1 <- c()

for (i in ls_select) {
  
  test <- grep(pattern = i, x = ls)
  # cat(test, i, '\n')
  
  print(ls[test])
  
  ls1 <- c(ls1, ls[test])
}


ls1


ls <- ls1
