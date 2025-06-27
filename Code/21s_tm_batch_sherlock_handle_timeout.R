

# Set your folder paths
path_A <- "Z:/sdg_corp/DF_tokenize"
path_B <- "Z:/sdg_corp/DF_tokenize_coded"
path_C <- file.path(path_A, "folderC")

# Create destination folder if it doesn't exist
if (!dir.exists(path_C)) dir.create(path_C)

# List files (filenames only, not full path)
files_A <- list.files(path_A)
files_B <- list.files(path_B)

# Remove '_coded' from B file names (before extension)
remove_coded <- function(fname) sub("_coded(?=\\.[^\\.]+$)", "", fname, perl = TRUE)
files_B_base <- sapply(files_B, remove_coded)

# Find overlaps: which files in A match the base names in B?
overlap_files <- files_A[basename(files_A) %in% files_B_base]

cat(length(overlap_files), "overlapping files will be copied from A to C\n")


# Move overlapped files from A to C
for (f in overlap_files) {
  from_path <- file.path(path_A, f)
  to_path <- file.path(path_C, f)
  
  # file.copy(from_path, to_path, overwrite = TRUE)
  file.rename(from_path, to_path)  # Move file (not just copy)
}

cat("Move complete! Overlapping files are now in:", path_C, "\n")
