extract_table <- function(tbl_vctr = character(),
                          merge_empty = TRUE,
                          orientation = c("horizontal", "vertical")) {
  boundaries <- .locate_boundaries(tbl_vctr)
  mtrx <- .extract_substrings_at_boundaries(tbl_vctr, boundaries)

  orientation <- match.arg(orientation)
  if (orientation == "vertical") {
    mtrx <- t(mtrx)
  }

  if (merge_empty) {
    mtrx <- .merge_empty(mtrx)
  }

  mtrx <- .row_to_names(mtrx)

  return(as.data.frame(mtrx))
}

.locate_boundaries <- function(tbl_vctr) {
  non_spaces <- unlist(stringi::stri_locate_all_regex(tbl_vctr, "\\S"))
  nchar(tbl_vctr)

  # Include the spot past the end so it merges if there spaces at the end of
  # everything.
  empty_locations <- setdiff(seq_len(max(non_spaces) + 1L), non_spaces)

  consecutive <- c(
    FALSE, # First location is never consecutive
    empty_locations[-1] == (empty_locations[-length(empty_locations)] + 1L)
  )
  return(empty_locations[!consecutive])
}

.extract_substrings_at_boundaries <- function(tbl_vctr, boundaries) {
  n_pieces <- length(boundaries) - 1L
  sub_map <- matrix(
    c(
      boundaries[-length(boundaries)],
      boundaries[-1] - 1L
    ),
    nrow = n_pieces,
    dimnames = list(NULL, c("start", "end"))
  )

  cols <- stringi::stri_sub_all(tbl_vctr, sub_map)

  return(
    matrix(
      stringi::stri_trim_both(unlist(cols)),
      nrow = length(tbl_vctr),
      ncol = n_pieces,
      byrow = TRUE
    )
  )
}

.merge_empty <- function(tbl_matrix) {
  tbl_matrix <- .merge_empty_rows(tbl_matrix)
  tbl_matrix <- .merge_empty_cols(tbl_matrix)
  # while (any(tbl_matrix == "")) {
  #   tbl_matrix <- t(tbl_matrix)
  #   tbl_matrix <- .merge_empty_rows(tbl_matrix)
  # }
  return(tbl_matrix)
}

.merge_empty_rows <- function(tbl_matrix) {
  rows_to_check <- seq_len(nrow(tbl_matrix))[-1]
  for (i in rev(rows_to_check)) {
    if (any(tbl_matrix[i, ] == "")) {
      tbl_matrix[i - 1L, ] <- stringi::stri_trim_both(
        paste(tbl_matrix[i - 1, ], tbl_matrix[i, ])
      )
      tbl_matrix <- tbl_matrix[-i, ]
    }
  }
  return(tbl_matrix)
}
.merge_empty_cols <- function(tbl_matrix) {
  tbl_matrix <- t(tbl_matrix)
  rows_to_check <- seq_len(nrow(tbl_matrix))[-1]
  for (i in rev(rows_to_check)) {
    if (any(tbl_matrix[i, ] == "")) {
      tbl_matrix[i - 1L, ] <- stringi::stri_trim_both(
        paste(tbl_matrix[i - 1, ], tbl_matrix[i, ])
      )
      tbl_matrix <- tbl_matrix[-i, ]
    }
  }
  return(t(tbl_matrix))
}

.row_to_names <- function(mtrx, names_from = 1L) {
  colnames(mtrx) <- mtrx[names_from, ]
  mtrx <- mtrx[-names_from, , drop = FALSE]
  return(mtrx)
}
