


## set work dir
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
dir_root <- dirname(dir)
setwd(dir_root)
getwd()



dir.raw     <- paste0(dir_root, '/data/data_raw/')

dir.tm      <- paste0(dir_root, '/data/data_tm/')
dir.reports <- paste0(dir.tm,'annual_reports/') ## PDFs are stored here to save space
dir.results <- paste0(dir_root, '/data/data_results/')
dir.figures <- paste0(dir_root, '/figures/')