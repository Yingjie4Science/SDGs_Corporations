# Dir and packages -----------------------------------------------------------------------

### To clear your environment 
remove(list = ls())

## set work dir
getwd()


# load packages
# List of required packages
pkgs <- c("dplyr", "tidyr", "stringr", "parallel", "doParallel", "foreach")

# Install any that are not yet installed
installed <- pkgs %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(pkgs[!installed])
}

# Load all packages
lapply(pkgs, require, character.only = TRUE)



## directories
dir_root  <- '/scratch/users/yingjiel/sdg_corp/'
dir_input <- paste0(dir_root, 'DF_tokenize/')
dir_output<- paste0(dir_root, 'DF_tokenize_coded/')


## Load SDG search terms
f <- paste0(dir_root, 'SDG_keys.RData'); 
load(f) ## --> SDG_keys_drc, SDG_keys_indrc, SDG_keys


## load the function
source('./function_sdg_detector.R') ## `sdg_detector()`



# 2. Detect SDGs  ------------------------------------------------------------------------

## ################################### #
## 2.1. Load cleaned reports -----
## ################################### #

## load the `txt to df` data
ls <- list.files(path = dir_input, pattern = '*.RData$', ignore.case = T, full.names = T); #ls
cat('There are', ls %>% length(), 'corporate reports were converted.\n') ## 352



  
## ################################### #
## 2.2. To detect 
## ################################### #


### - run parallel - SDGDetector
# Check available cores (logical = TRUE gives total threads, FALSE gives physical cores)
total_cores <- parallel::detectCores(logical = TRUE)
physical_cores <- parallel::detectCores(logical = FALSE)
cat("Total threads:", total_cores, "\nPhysical cores:", physical_cores, "\n")

# Use one less than the total number of cores to avoid freezing your system
n_cores_to_use <- max(1, total_cores - 1)

# Limit to 48 or 64, even if you have 128+ cores
n_cores_to_use <- min(64, n_cores_to_use)

# Set up cluster and register for parallel backend
cl <- makeCluster(n_cores_to_use)
doParallel::registerDoParallel(cl)
cat("Registered", n_cores_to_use, "cores for parallel processing.\n")


## get the total number of data files
n <- length(ls); n

## run parallel
system.time({
  foreach (i=1:n, .packages = c("dplyr", "stringr")) %dopar% {
    f <- ls[i]; #f
    ## --> get company name
    company_name <- gsub(".RData", "", basename(f)); 
    message(sprintf("[Task %d/%d] Processing: %s", i, n, company_name))
    
    df <- get(load(f))
    
    coded <- sdg_detector(df = df)

    fname <- paste0(dir_output, company_name, '_coded.RData'); #fname
    save(coded, file = fname)
    
    ## --> To clear up the memory for next loop
    # gc()
  }
})


## When you're done, clean up the cluster
stopImplicitCluster()



