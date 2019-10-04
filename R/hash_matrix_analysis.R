#' Select a cutoff for hash counts using automatic, k-means based splitting
#'
#' @param x a numeric vector of hash count values
#' @param min_cut a numeric value for the minimum number of counts to consider. Default is 10.
#' @param seed a value to use as a random seed for k-means clustering. Default is 3030.
#'
#' @return a numeric value to use as a cutoff. If no values are above min_cut, returns max(x), otherwise returns max(x) for non-passing values.
#' @export
#'
select_hash_cutoff <- function(x,
                               min_cut = 10,
                               seed = 3030) {
  assertthat::assert_that(class(x) == "numeric")
  assertthat::assert_that(class(min_cut) == "numeric")
  assertthat::assert_that(length(min_cut) == 1)
  assertthat::assert_that(class(seed) == "numeric")
  assertthat::assert_that(length(seed) == 1)

  res <- rep(0, length(x))

  x_gt_cut <- x[x > min_cut]

  if(length(x_gt_cut) > 2) {
    set.seed(seed)
    km <- kmeans(log10(x_gt_cut), centers = 2)
    cl <- km$cluster
    high_cl <- which(km$centers == max(km$centers))

    res[x > min_cut][cl == high_cl] <- 1
  }

  if(sum(res) == 0) {
    return(max(x))
  } else {
    return(max(x[res == 0]))
  }

}

#' Binarize a vector of hash counts based on a cutoff value
#'
#' @param x a numeric vector of hash count values
#' @param cutoff a numeric value to use as a cutoff
#'
#' @return a binary vector with value 1 for all values in x > cutoff
#' @export
#'
binarize_hash <- function(x,
                          cutoff) {

  assertthat::assert_that(class(x) == "numeric")
  assertthat::assert_that(class(cutoff) == "numeric")
  assertthat::assert_that(length(cutoff) == 1)

  res <- rep(0, length(x))

  res[x > cutoff] <- 1

  return(res)
}

binarize_hash_matrix <- function(mat) {

}

binary_matrix_hash_results <- function(x) {
  results <- apply(x, 2,
                   function(binary_hash_scores) {
                     hash_sum <- sum(binary_hash_scores)
                     if(hash_sum == 0) {
                       category <- "no_hash"
                       sequence <- NA
                     } else if(hash_sum == 1) {
                       category <- "singlet"
                       sequence <- rownames(x)[binary_hash_scores == 1]
                     } else if(hash_sum == 2) {
                       category <- "doublet"
                       sequence <- paste(rownames(x)[binary_hash_scores == 1], collapse = ";")
                     } else if(hash_sum > 2) {
                       category <- "multiplet"
                       sequence <- paste(rownames(x)[binary_hash_scores == 1], collapse = ";")
                     }
                     data.frame(hash_category = category,
                                hash_sequence = sequence)
                   })
  results <- do.call("rbind",
                     results)
  results <- cbind(data.frame(cell_barcode = colnames(x)),
                   results)
  return(results)
}
