
library(ggplot2)

func_ggsave_nature <- function(
    plot = last_plot(), 
    path,
    column = 1, # 1 for "single", 2 for "double"
    height_mm = 150,  # adjust per plot, ≤170 mm
    dpi = 300, 
    device = 'png' # c("pdf","tiff","png","svg")
) {
  
  # Validate column (numeric, not character)
  if (!column %in% c(1, 2)) {
    stop("column must be 1 (single) or 2 (double)")
  }
  
  # Validate device
  device <- match.arg(device, choices = c("pdf", "tiff", "png", "svg"))
  
  width_mm <- if (column == 1) 90 else 180
  ggsave(filename = path, 
         plot   = plot,
         width  = width_mm/25.4,
         height = height_mm/25.4,
         units  = "in",
         dpi    = dpi, 
         device = device)
}