
## ######################################################################## ##
## This function will be used to **detect SDGs** in a sentence.             ##
## ######################################################################## ##


# THIS CODE IS TO IDENTIFY MATCHING ROWS, BUT IT DOES NOT EXTRACT SENTENCES
# in the original dataframe, if it matches, then in a new column, say yes; if not, say no.

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
library(stringr)
library(dplyr)


sdg_detector <- function(df) {
  
  code <- df %>% 
    dplyr::mutate(#match   = 0,      
      sdgs    = '', ## for later use, to append data to this column
      n_total = 0,  ## count the accumulated mentions
      sdgs_n  = '') ## combine together SDG names and the number of mentions
  
  for (i in 1:nrow(SDG_keys)){                           ## loop each SDG indicators
    sdg_i_str <- SDG_keys$SDG_id[i] %>% as.character()   ## get the SDG id name
    sdg_i_obj <- SDG_keys$SDG_keywords[i]                ## get the corresponding SDG search term list
    
    print(sdg_i_str)
    # print(sdg_i_obj)
    
    code <-  code %>%
      as.data.frame() %>%
      
      ## at the sentence level - count once if goals/targets are mentioned -------------------------
      ###' option 1/ grepl() ran into a regular expression that's too complex or matched a very long string, 
      ###'    causing the PCRE engine to exceed its internal matching limit
      ###'    yes-1 or no-0 if they match
      # dplyr::mutate(
      #   match = ifelse(grepl(pattern = sdg_i_obj, x = statement, ignore.case = T, perl = T), 1, 0))  %>% 
      
      ###' option 2/ stringr::str_detect() uses stringi, which avoids common PCRE issues like match limit exceeded
      dplyr::mutate(
        match = ifelse(stringr::str_detect(statement, regex(sdg_i_obj, ignore_case = TRUE)), 1, 0) )%>% 
      
      dplyr::mutate(
        sdgs  = ifelse(match > 0, paste0(sdgs, ',', sdg_i_str), sdgs)) %>%
        
      
      dplyr::select(-match) %>% ## remove this column 
      
      ## at the sentence level - count the times of all the mentions -------------------------------
      dplyr::mutate(
        n       = str_count(string = statement, regex(pattern = sdg_i_obj, ignore_case = T)),
        n_total = n_total + n,
        sdgs_n  = ifelse(n > 0, paste0(sdgs_n, ',', sdg_i_str, '-', n), sdgs_n)
      ) %>%
        dplyr::select(-n) %>%   ## remove this column 
        as.data.frame()
  }
  
  
  ### sort from most SDG hits to least (or, none)
  coded <- code %>% arrange(desc(nchar(sdgs)), id)
  
  return(coded)
  
}






sdg_detector_gpt <- function(df) {
  require(dplyr)
  require(stringr)
  
  # Preallocate columns once
  df <- df %>%
    mutate(
      sdgs    = "",     # Accumulate matched SDG IDs
      n_total = 0,      # Accumulate match counts
      sdgs_n  = ""      # SDG_id-count pairs
    )
  
  # Loop over each SDG row
  for (i in seq_len(nrow(SDG_keys))) {
    sdg_i_str <- as.character(SDG_keys$SDG_id[i])
    sdg_i_obj <- SDG_keys$SDG_keywords[i]
    
    message("Processing: ", sdg_i_str)
    
    # Perform detection and counting once per iteration
    match_detected <- str_detect(df$statement, regex(sdg_i_obj, ignore_case = TRUE))
    match_count    <- str_count(df$statement, regex(sdg_i_obj, ignore_case = TRUE))
    
    # Update only rows with matches
    df <- df %>%
      mutate(
        sdgs    = if_else(match_detected, paste0(sdgs, ",", sdg_i_str), sdgs),
        n_total = n_total + match_count,
        sdgs_n  = if_else(match_count > 0,
                          paste0(sdgs_n, ",", sdg_i_str, "-", match_count),
                          sdgs_n)
      )
  }
  
  # Final formatting and sorting
  df %>%
    mutate(sdgs = str_remove(sdgs, "^,"),        # Clean leading commas
           sdgs_n = str_remove(sdgs_n, "^,")) %>%
    arrange(desc(nchar(sdgs)), id)
}





sdg_detector_deepseek <- function(df) {
  # Precompute regex patterns and IDs
  sdg_ids <- as.character(SDG_keys$SDG_id)
  patterns <- SDG_keys$SDG_keywords
  m <- nrow(SDG_keys)
  n <- nrow(df)
  
  # Precompile all regex patterns
  regex_list <- lapply(patterns, function(p) regex(p, ignore_case = TRUE))
  
  # Initialize storage matrices
  matches <- matrix(FALSE, nrow = n, ncol = m)
  counts <- matrix(0L, nrow = n, ncol = m)
  
  # Populate matches and counts matrices
  for (i in 1:m) {
    message("Processing ", sdg_ids[i])
    matches[, i] <- stringr::str_detect(df$statement, regex_list[[i]])
    counts[, i] <- stringr::str_count(df$statement, regex_list[[i]])
  }
  
  # Create contribution matrices
  sdg_contrib <- matrix("", nrow = n, ncol = m)
  sdg_n_contrib <- matrix("", nrow = n, ncol = m)
  
  for (i in 1:m) {
    sdg_contrib[, i] <- ifelse(matches[, i], paste0(",", sdg_ids[i]), "")
    cnt <- counts[, i]
    sdg_n_contrib[, i] <- ifelse(cnt > 0, paste0(",", sdg_ids[i], "-", cnt), "")
  }
  
  # Combine results
  result <- df %>%
    dplyr::mutate(
      sdgs = apply(sdg_contrib, 1, paste0, collapse = ""),
      n_total = rowSums(counts),
      sdgs_n = apply(sdg_n_contrib, 1, paste0, collapse = ""),
      .before = 1
    ) %>%
    dplyr::arrange(desc(nchar(sdgs)), id)
  
  return(result)
}




library(dplyr)
library(stringr)
library(rlang) # For sym() if needed for dynamic column names, not strictly here

# Make sure SDG_keys$SDG_id is character
# SDG_keys$SDG_id <- as.character(SDG_keys$SDG_id)

sdg_detector_gemini25pro <- function(df, SDG_keys) {
  
  if (nrow(df) == 0) {
    # Handle empty input dataframe
    df <- df %>%
      dplyr::mutate(
        sdgs    = character(0),
        n_total = integer(0),
        sdgs_n  = character(0)
      )
    return(df %>% arrange(id)) # Assuming 'id' column exists for sorting
  }
  
  if (nrow(SDG_keys) == 0) {
    # Handle empty SDG_keys: no SDGs to detect
    df <- df %>%
      dplyr::mutate(
        sdgs    = "",
        n_total = 0,
        sdgs_n  = ""
      )
    return(df %>% arrange(id)) # Assuming 'id' column exists for sorting
  }
  
  # Ensure SDG_id is character
  SDG_keys$SDG_id <- as.character(SDG_keys$SDG_id)
  
  # 1. Pre-compile regex patterns
  #    This is minor but good practice. stringr often caches internally,
  #    but explicit compilation can be clearer.
  compiled_patterns <- lapply(SDG_keys$SDG_keywords, function(p) {
    regex(p, ignore_case = TRUE)
  })
  
  # 2. For each SDG, get matches and counts for all statements
  #    This will result in a list of logical vectors (for matches)
  #    and a list of integer vectors (for counts).
  
  # Use a progress bar if SDG_keys is large (optional)
  pb <- progress::progress_bar$new(
    format = "  Processing SDG patterns [:bar] :percent eta: :eta",
    total = nrow(SDG_keys), clear = FALSE, width = 60
  )
  
  all_matches <- vector("list", length(compiled_patterns))
  all_counts <- vector("list", length(compiled_patterns))
  
  for (i in 1:length(compiled_patterns)) {
    pattern <- compiled_patterns[[i]]
    # pb$tick() # Increment progress bar
    
    # str_detect returns a logical vector of the same length as df$statement
    all_matches[[i]] <- stringr::str_detect(df$statement, pattern)
    
    # str_count returns an integer vector
    all_counts[[i]] <- stringr::str_count(df$statement, pattern)
    
    # Optional: print progress
    # print(paste("Processed pattern for:", SDG_keys$SDG_id[i]))
  }
  
  # Convert lists of vectors to matrices: rows are statements, columns are SDGs
  # This makes row-wise operations easier later.
  matches_matrix <- do.call(cbind, all_matches) # Logical matrix
  counts_matrix  <- do.call(cbind, all_counts)   # Integer matrix
  
  # Ensure column names are set for easier debugging and explicit mapping if needed
  colnames(matches_matrix) <- SDG_keys$SDG_id
  colnames(counts_matrix)  <- SDG_keys$SDG_id
  
  # 3. Aggregate results for each statement (row)
  
  # Calculate 'sdgs' column: comma-separated list of matching SDG IDs
  df$sdgs <- apply(matches_matrix, 1, function(row_matches) {
    # row_matches is a logical vector for one statement, across all SDGs
    matching_sdg_ids <- SDG_keys$SDG_id[row_matches]
    if (length(matching_sdg_ids) > 0) {
      paste(matching_sdg_ids, collapse = ",")
    } else {
      ""
    }
  })
  
  # Calculate 'n_total' column: sum of all counts for a statement
  df$n_total <- rowSums(counts_matrix, na.rm = TRUE) # na.rm just in case, though str_count shouldn't produce NAs
  
  # Calculate 'sdgs_n' column: comma-separated list of "SDG_ID-count"
  df$sdgs_n <- apply(counts_matrix, 1, function(row_counts) {
    # row_counts is an integer vector for one statement, across all SDGs
    sdg_n_parts <- character(0)
    for (j in 1:length(row_counts)) {
      if (row_counts[j] > 0) {
        sdg_n_parts <- c(sdg_n_parts, paste0(SDG_keys$SDG_id[j], "-", row_counts[j]))
      }
    }
    if (length(sdg_n_parts) > 0) {
      paste(sdg_n_parts, collapse = ",")
    } else {
      ""
    }
  })
  
  # 4. Sort the dataframe
  #    Assuming 'id' column exists in the original df for tie-breaking.
  #    If not, remove 'id' from arrange or replace with another column.
  if ("id" %in% names(df)) {
    coded <- df %>% dplyr::arrange(desc(nchar(sdgs)), id)
  } else {
    coded <- df %>% dplyr::arrange(desc(nchar(sdgs))) # Or some other default sort
  }
  
  return(coded)
}
