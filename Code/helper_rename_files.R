


getwd()





mydir <- paste0(dir.reports, 'pdf_not_coded/')
mydir <- paste0(dir.reports, 'pdf_coded_human/')
setwd(mydir)

# List files ending with '_2022_coded.RData'
files <- list.files(path = mydir, pattern = "\\.pdf$", ignore.case = TRUE)

# Preview original file names
print("Original filenames:")
print(files)

# Define a renaming function
rename_file <- function(filename) {
  # Remove portion after first '2022' and before '_coded'
  new_name <- sub("--", "_2018_", filename)
  file.rename(filename, new_name)
  return(new_name)
}

# Apply the renaming function
new_files <- sapply(files, rename_file)

# Preview renamed files
print("Renamed files:")
print(new_files)


















mydir <- paste0(dir.reports, 'pdf_coded_human/DF_tokenize_coded/')
setwd(mydir)

# List files ending with '_2022_coded.RData'
files <- list.files(path = mydir, pattern = "_2022_coded\\.RData$")

# Preview original file names
print("Original filenames:")
print(files)

# Define a renaming function
rename_file <- function(filename) {
  # Remove portion after first '2022' and before '_coded'
  new_name <- sub("^(.*?2022).*(_coded\\.RData)$", "\\1\\2", filename)
  file.rename(filename, new_name)
  return(new_name)
}

# Apply the renaming function
new_files <- sapply(files, rename_file)

# Preview renamed files
print("Renamed files:")
print(new_files)
