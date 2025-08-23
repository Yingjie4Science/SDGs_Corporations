

# 3) Add salted duplicates per coder (~duplicate_rate of their unique items)
# ---------------------------------------------------------------------------
inject_duplicates <- function(df_one_coder, dup_rate = 0.10) {
  # Keep only originals first
  base_rows <- df_one_coder %>%
    dplyr::filter(is_duplicate == 0L) %>%
    dplyr::arrange(statement_id) %>%               # or any stable order
    dplyr::mutate(position_hint = dplyr::row_number())
  
  n_unique <- nrow(base_rows)
  if (n_unique == 0L) return(df_one_coder)
  
  n_dups <- max(1, floor(n_unique * dup_rate))
  # Pick which statements to duplicate
  to_dup_ids <- sample(base_rows$statement_id, size = min(n_dups, n_unique), replace = FALSE)
  
  # Build duplicates *from* base_rows so we inherit position_hint
  to_dup <- base_rows %>%
    dplyr::filter(statement_id %in% to_dup_ids) %>%
    dplyr::mutate(
      is_duplicate = 1L,
      duplicate_group_id = paste0("DUP_", coder_id, "_", statement_id),
      # push duplicates roughly to the second half of the queue
      position_hint = position_hint + ceiling(n_unique / 2)
    )
  
  # Combine and finalize order
  out <- dplyr::bind_rows(base_rows, to_dup) %>%
    dplyr::arrange(position_hint) %>%
    dplyr::select(-position_hint)
  
  out
}