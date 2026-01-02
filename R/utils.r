#' Short internal functions
#'
#' Turns parameters into a list
#' An alias for list in pipe processing.
#' Paul J. Gordijn
#' @noRd
#' 
#' 
#' @export
# internal function to read metadata from docx tables
read_word_tables <- function(
  dp = NULL # document path
) {
  "." <- "cell_id" <- "col_ids" <- "col_span" <- "content_type" <- "row_id" <-
    "row_span" <- "text" <- "V1" <- "V2" <- "V3" <- NULL
  doc <- officer::read_docx(dp)

  # Get all table rows
  tbls <- officer::docx_summary(doc) |> data.table::setDT()
  tbls <- tbls[content_type %ilike% "table"]
  #tbls[text %ilike% 'consis']
  #tbls[doc_index %in% 110:116]
  # Split by table_id
  tables_list <- split(tbls, by = "table_index", keep.by = FALSE)

  cleaned_tables <- lapply(tables_list, function(tbl) {
    # Convert to data.table
    tbl <- data.table(tbl)
    tbl <- tbl[, col_span := as.integer(col_span)][,
      .(row_id, row_span, cell_id, col_span, text)
    ]

    # Expand horizontal spans
    tbl[, col_ids := lapply(seq_len(.N), function(i) {
      cell_id[i]:(cell_id[i] + col_span[i] - 1)
    })]

    # Expand vertical spans properly
    tbl_expanded <- tbl[rep(seq_len(.N), times = row_span)]

    # Expand col_ids and text
    dt_expanded <- tbl_expanded[, .(
      col_id = unlist(col_ids),
      text = rep(text, lengths(col_ids))
    ), by = row_id]

    # Reshape to wide table
    dt_wide <- dcast(dt_expanded, row_id ~ col_id,
      value.var = "text", fill = NA,
      fun.aggregate = function(x) paste(x, collapse = " ")
    )
    dt_wide <- dt_wide[, row_id := NULL]
    names(dt_wide) <- paste0("V", names(dt_wide))
    dt_wide <- dt_wide[, V1 := gsub(pattern = "\t", replacement = "", trimws(V1))][,
      V1 := gsub(pattern = " {2,}", replacement = " ", V1)
    ][, .(V1, V3)]
    data.table::setnames(dt_wide, old = c("V1", "V2"), new = c("field", "value"))
    dt_wide
  })
  cleaned_tables
}