
library(openxlsx)

# Helper: estimate widths per column for openxlsx
auto_fit_widths <- function(df, min_width = 12, max_width = 60, quant = 0.95) {
  est_width <- function(vec, header, is_num) {
    if (is_num) {
      # keep numeric columns tidy; include header length
      w <- max(nchar(header), 10)
    } else {
      ch <- nchar(as.character(vec %||% ""))
      # robust to NAs; use 95th percentile to avoid super long outliers
      w <- max(nchar(header), as.integer(quantile(ch[ch > 0], probs = quant, na.rm = TRUE)) + 2L)
      if (!is.finite(w)) w <- nchar(header)
    }
    w <- max(min_width, min(w, max_width))
    return(w)
  }
  
  widths <- purrr::map_int(seq_along(df), function(j) {
    col <- df[[j]]
    header <- names(df)[j]
    is_num <- is.numeric(col) || is.integer(col) || is.logical(col)
    est_width(col, header, is_num)
  })
  widths
}


