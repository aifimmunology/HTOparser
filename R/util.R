#' Write a stderr message with a leading date/time stamp
#'
#' @param x a character object with the message to display
#'
#' @return no return
#' @export
stm <- function(x) {
  assertthat::assert_that(class(x) == "character")
  assertthat::assert_that(length(x) == 1)

  write(paste0("[",Sys.time(),"] ",x), stderr())
}

#' Read a CITE-seq-Count .mtx directory as a standard R matrix
#'
#' @param csc_dir a directory containing matrix.mtx, barcodes.tsv, and features.tsv. gzipped versions will also work.
#'
#' @return a matrix
#' @export
read_csc_mtx <- function(csc_dir) {
  mtx_file <- list.files(csc_dir, pattern = "matrix.mtx")
  bc_file <- list.files(csc_dir, pattern = "barcodes.tsv")
  feat_file <- list.files(csc_dir, pattern = "features.tsv")

  mat <- Matrix::readMM(file.path(csc_dir,mtx_file))
  mat <- as(mat, "matrix")
  rownames(mat) <- data.table::fread(file.path(csc_dir, feat_file), header = FALSE)[[1]]
  colnames(mat) <- data.table::fread(file.path(csc_dir, bc_file), header = FALSE)[[1]]

  mat
}

#' Simple function to check for matrix or dgCMatrix classes for assertions
#'
#' @param x an object to check for matrix or dgCMatrix classes
#'
#' @return a logical value
#'
check_matrix <- function(x) {
  res <- FALSE
  x_classes <- class(x)
  if("matrix" %in% x_classes) {
    res <- TRUE
  }
  if("dgCMatrix" %in% x_classes) {
    res <- TRUE
  }
  res
}
