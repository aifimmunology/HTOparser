#' Fuzzy Filtering for HTO tables
#'
#' Filters a data.table based on a specific column with imperfect matches to a reference set of values.
#' This is useful for filtering a table of hash oligo counts where there may be off-by-one errors in the
#' hash sequences.
#'
#' @param dt The data.table to filter. Must have columns count, cell_barcode, and hto_barcode.
#' @param match_column A character object matching the column name of the data.table to use for filtering.
#' @param match_values A character vector of values to match
#' @param max_distance The maximum number of substitutions allowed between values in match_column and any value
#' in match_values.
#'
#' @return A data.table with the same columns as dt. Non-matching values will be removed, and rows matching the same
#' value will be merged, with counts summed.
#' @export
#'
fuzzy_filtering <- function(dt,
                            match_column = "hto_barcode",
                            match_values,
                            max_distance = 1) {

  assertthat::assert_that("data.table" %in% class(dt))
  assertthat::assert_that(all(c("count","cell_barcode","hto_barcode") %in% colnames(dt)))
  assertthat::assert_that(class(match_column) == "character")
  assertthat::assert_that(length(match_column) == 1)
  assertthat::assert_that(class(max_distance) == "numeric")
  assertthat::assert_that(length(max_distance) == 1)

  # find matches using agrep
  # map_dfr will run this for every entry in match_values
  # and rbind the resulting data.frame for us
  matches <- purrr::map_dfr(match_values,
                     function(y) {
                       agrep_res <- agrep(y,
                                          dt[[match_column]],
                                          max.distance = list(substitution = max_distance,
                                                              insertions = 0,
                                                              deletions = 0),
                                          costs = list(substitutions = 1))
                       # data.frame will throw an error if
                       # length(agrep_res) == 0
                       if(length(agrep_res) > 0) {
                         data.frame(hto = y,
                                    row_match = agrep_res)
                       }

                     })

  # filter for matching rows and correct the hto_barcode column
  dt <- dt[matches$row_match,]
  dt$hto_barcode <- matches$hto

  # sum counts for corrected htos
  dt <- dt[, sum(count),
           by = list(cell_barcode,
                     hto_barcode)]
  # fix column order and names
  dt <- dt[, c(3,1,2)]
  names(dt)[1] <- "count"

  dt
}

#' Convert an HTO barcode table to a sparse matrix of hto_barcode x cell_barcode
#'
#' @param dt The data.table of barcode counting results. Must have columns count, cell_barcode, and hto_barcode.
#' @param valid_htos A character vector of valid HTO barcode sequences.
#'
#' @return A dgCMatrix with cell barcodes as columns and valid HTO sequences as rows.
#' @export
#'
#' @seealso fuzzy_filtering
#'
barcode_table_to_matrix <- function(dt,
                                    valid_htos) {

  assertthat::assert_that("data.table" %in% class(dt))
  assertthat::assert_that(all(c("count","cell_barcode","hto_barcode") %in% colnames(dt)))
  assertthat::assert_that(class(valid_htos) == "character")

  # Sort the hto table by cell barcode and hto barcode
  # This will make it much easier to translate to a sparse, as rows will be
  # grouped by cell barcode (column), and sorted by hto barcode (row)
  dt <- dt[order(cell_barcode, hto_barcode),]

  mat_cols <- unique(dt$cell_barcode)
  mat_rows <- valid_htos

  mat <- Matrix::sparseMatrix(x = dt$count,
                              i = match(dt$hto_barcode, mat_rows),
                              p = c(0, cumsum(table(dt$cell_barcode))),
                              dims = c(length(mat_rows), length(mat_cols)),
                              dimnames = list(mat_rows, mat_cols))

  # Make sure the output row order matches the input valid_htos
  mat <- mat[valid_htos, ]

  mat
}
