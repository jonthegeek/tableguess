extract_table <- function(tbl_vctr = character()) {
  # TODO: Eventually make that input a class so I know the table has already
  # been found and split into separate lines.
  data_map <- stringr::str_locate_all(
    tbl_vctr,
    "[^[:space:]]+([[:space:]]{0,2}[^[:space:]]+)*(?=[:space:]|$)"
  ) |>
    purrr::map(
      as.data.frame
    ) |>
    purrr::list_rbind() |>
    dplyr::distinct()

  map_vctr_start <- min(c(data_map$start, 1L))
  # Include the spot past the end so it merges if there spaces at the end of
  # everything.
  map_vctr_end <- max(data_map$end) + 1L
  # TODO: Validate those.
  map_vctr <- map_vctr_start:map_vctr_end
  empty_locations <- purrr::reduce2(
    data_map$start,
    data_map$end,
    \(map_vctr, start, end) {
      # TODO: Validate that they're all valid.
      this_vctr <- start:end
      setdiff(map_vctr, this_vctr)
    },
    .init = map_vctr
  )
  lagged <- dplyr::lag(empty_locations)
  consecutive <- !is.na(lagged) & empty_locations == lagged + 1L
  empty_locations <- empty_locations[!consecutive]
  # Select from empty_locations[[1]] to empty_locations[[2-1]], etc.
  cols <- purrr::map(
    seq_len(length(empty_locations) - 1),
    \(i) {
      stringr::str_sub(
        tbl_vctr,
        start = empty_locations[[i]],
        end = empty_locations[[i + 1]] - 1
      ) |>
        stringr::str_trim()
    }
  )

  tbl_mtrx <- matrix(
    unlist(cols),
    nrow = length(tbl_vctr),
    ncol = length(cols)
  )
  colnames(tbl_mtrx) <- tbl_mtrx[1, ]
  tbl_mtrx <- tbl_mtrx[-1, ]
  empty <- tbl_mtrx == ""

  # Row-by-row, if any are empty, merge that row to the row above.
  for (i in seq_len(nrow(tbl_mtrx) - 1) + 1L) {
    if (any(empty[i, ])) {
      tbl_mtrx[i - 1, ] <- paste(tbl_mtrx[i - 1, ], tbl_mtrx[i, ]) |>
        stringr::str_trim()
      tbl_mtrx[i, ] <- ""
    }
  }

  better_mtrx <- matrix(
    tbl_mtrx[tbl_mtrx != ""],
    ncol = ncol(tbl_mtrx),
    dimnames = list(NULL, colnames(tbl_mtrx))
  )

  return(as.data.frame(better_mtrx))
}
