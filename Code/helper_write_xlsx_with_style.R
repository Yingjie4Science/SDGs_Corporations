

library(openxlsx)
library(dplyr)
library(purrr)
library(stringr)
library(here)

# 3) Values allowed for engagement (include NA option, used in data validation)
engagement_allowed <- c("0 - NA", "1 - Symbolic", "2 - Strategic", "3 - Substantive")

# If you don't already have %||%
`%||%` <- function(x, y) if (is.null(x)) y else x

# 1) Recode to coder-friendly labels (numeric + short text)
per_coder_queues_export <- per_coder_queues_clean %>%
  mutate(
    engagement_llm = case_when(
      engagement_llm == "Symbolic Alignment" ~ "1 - Symbolic",
      engagement_llm == "Strategic Commitment" ~ "2 - Strategic",
      engagement_llm == "Substantive Implementation" ~ "3 - Substantive",
      TRUE ~ NA_character_
    )
  )

# (Optional) If you have a human entry column, standardize its name:
# per_coder_queues_export <- per_coder_queues_export %>%
#   mutate(engagement_coder = engagement_coder %||% NA_character_)

# 2) Helper: auto-fit widths

func_i <- here('code', 'function_xlsx_auto_fit_widths.R')
source(func_i)


# 4) Export per coder with legend, validation, styling
per_coder_queues_export %>%
  group_by(coder_id) %>%
  group_walk(~ {
    dat <- .x
    
    # Ensure a human entry column exists for validation (create if missing)
    if (!"engagement_coder" %in% names(dat)) {
      dat$engagement_coder <- NA_character_
    }
    
    # # Order columns (put coder-friendly LLM and human entry side-by-side)
    # preferred_order <- intersect(
    #   c("statement_id", "statement",
    #     "targets_llm", "engagement_llm", # recoded
    #     "engagement_coder",                  # to be filled by coder
    #     "coder_id"),
    #   names(dat)
    # )
    # dat <- dplyr::select(dat, all_of(preferred_order), dplyr::everything())
    
    # Workbook
    wb <- createWorkbook()
    addWorksheet(wb, "Queue")
    addWorksheet(wb, "Legend")
    
    # Write data
    writeData(wb, "Queue", dat)
    writeData(wb, "Legend",
              data.frame(
                Code = engagement_allowed,
                Meaning = c(
                  "No SDG-related engagement / Not Applicable",
                  "Mentions SDG without plan/action",
                  "Clear plan/commitment; not yet implemented",
                  "Implemented action with measurable results"
                ),
                stringsAsFactors = FALSE
              ),
              headerStyle = createStyle(textDecoration = "bold"))
    
    
    
    # Data validation: restrict human entry to the 3 options -----------------------------
    # Find the engagement_coder column index
    idx_human <- which(names(dat) == "engagement_coder")
    dv_range  <- "'Legend'!$A$2:$A$5"  # cells where we placed allowed values
    # Apply validation using the named range
    if (length(idx_human) == 1) {
      dataValidation(
        wb, "Queue",
        cols = idx_human,
        rows = 2:(nrow(dat) + 1),
        type = "list",
        value = dv_range,      # reference the range directly
        allowBlank = TRUE
      )
    }
    
    
    # Styles -----------------------------------------------------------------------------
    # Wrap text for all cells; bold header
    style_wrap   <- createStyle(wrapText = TRUE, valign = "top")
    style_header <- createStyle(textDecoration = "bold", halign = "left", valign = "top", border = "Bottom")
    
    
    setColWidths(wb, "Queue", cols = 1:ncol(dat), widths = 25) # Set same width for all columns
    # Auto-fit widths
    widths <- auto_fit_widths(dat, min_width = 12, max_width = 50, quant = 0.95)
    setColWidths(wb, "Queue", cols = 1:ncol(dat), widths = widths)
    setColWidths(wb, "Queue", cols = 1, widths = 12)  # specifically change the width of `statement_id`  
    
    
    # Apply styles
    addStyle(wb, "Queue", style_header, rows = 1, cols = 1:ncol(dat), gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Queue", style_wrap, rows = 1:(nrow(dat) + 1), cols = 1:ncol(dat), gridExpand = TRUE, stack = TRUE)
    
    # Freeze header
    freezePane(wb, "Queue", firstActiveRow = 2, firstActiveCol = 1)
    
    
    # Save
    saveWorkbook(wb, file.path(dir.assignments, paste0(.y$coder_id[1], xlsx_postfix)), overwrite = TRUE)
  })

