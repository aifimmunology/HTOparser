context("hash_matrix_analysis")
library(HTOparser)

# load test data
test_hto_table <- fread(system.file("testdata/CITE-seq_count_14hash_matrix.csv.gz",
                                    package = "HTOparser"))
test_hto_mat <- as(as.matrix(test_hto_table[1:14,-1]), "dgCMatrix")

test_hto_key <- fread(system.file("testdata/CITE-seq_count_tags.csv",
                                  package = "HTOparser"),
                      header = FALSE)

rownames(test_hto_mat) <- test_hto_key$V1[match(test_hto_table[[1]][1:14], test_hto_key$V2)]

test_that(
  "select_hash_cutoff selects a cutoff to split HTO values",
  {
    cutoff <- select_hash_cutoff(test_hto_mat[1,],
                                 min_cut = 10)

    expect_true(class(cutoff) == "numeric")
    expect_length(cutoff, 1)
  }
)

test_that(
  "binarize_hash converts a vector of values to binary values based on a cutoff",
  {
    binarized_result <- binarize_hash(test_hto_mat[10,],
                                      cutoff = 172)

    expect_true(all(unique(binarized_result) %in% c(0,1)))
    expect_length(binarized_result, ncol(test_hto_mat))
    expect_true(class(binarized_result) == "numeric")
    expect_equal(sum(binarized_result), sum(test_hto_mat[10,] > 172))
  }
)

test_that(
  "add_missing_hto_rows adds additional empty rows to a matrix for missing hto barcodes",
  {
    valid_htos <- totalseq_a_human()$hto_barcode

    missing_htos <- setdiff(valid_htos, rownames(test_hto_mat))

    mat_result <- add_missing_hto_rows(mat = test_hto_mat,
                                       valid_htos = valid_htos)

    expect_true(class(mat_result) == "dgCMatrix")
    expect_equal(nrow(mat_result), length(valid_htos))
    expect_equal(ncol(mat_result), ncol(test_hto_mat))
    expect_identical(rownames(mat_result), valid_htos)
    expect_equal(sum(mat_result[missing_htos,]), 0)
  }
)

test_that(
  "binarize_hash_matrix converts a matrix of values to a binary matrix with automatic cutoffs",
  {
    binarized_results <- binarize_hash_matrix(test_hto_mat)

    expect_true(class(binarized_results) == "list")
    expect_length(binarized_results, 2)
    expect_equal(names(binarized_results), c("bmat", "bsummary"))

    expect_true(class(binarized_results$bmat) == "dgCMatrix")
    expect_equal(ncol(binarized_results$bmat), ncol(test_hto_mat))
    expect_equal(nrow(binarized_results$bmat), nrow(test_hto_mat))
    expect_identical(rownames(binarized_results$bmat), rownames(test_hto_mat))
    expect_identical(colnames(binarized_results$bmat), colnames(test_hto_mat))
    expect_true(all(unique(binarized_results$bmat@x) %in% c(0,1)))

    expect_true(class(binarized_results$bsummary) == "data.frame")
    expect_equal(nrow(binarized_results$bsummary), nrow(test_hto_mat))
    expect_identical(binarized_results$bsummary$hto_barcode, rownames(test_hto_mat))
    expect_identical(names(binarized_results$bsummary),
                     c("hto_barcode","cutoff","n_pos","n_neg","n_below_threshold","frac_pos","frac_neg","frac_below_threshold"))
    expect_true(sum(binarized_results$bsummary$frac_pos > 0.04) == 12)
  }
)

test_that(
  "binarize_hash_matrix converts a matrix of values to a binary matrix using reference htos",
  {
    valid_htos <- totalseq_a_human()$hto_barcode
    binarized_results <- binarize_hash_matrix(test_hto_mat,
                                              valid_htos = valid_htos,
                                              expect_equal_loading = TRUE)

    expect_true(class(binarized_results) == "list")
    expect_length(binarized_results, 2)
    expect_equal(names(binarized_results), c("bmat", "bsummary"))

    expect_true(class(binarized_results$bmat) == "dgCMatrix")
    expect_equal(ncol(binarized_results$bmat), ncol(test_hto_mat))
    expect_equal(nrow(binarized_results$bmat), length(valid_htos))
    expect_identical(rownames(binarized_results$bmat), valid_htos)
    expect_identical(colnames(binarized_results$bmat), colnames(test_hto_mat))
    expect_true(all(unique(binarized_results$bmat@x) %in% c(0,1)))

    expect_true(class(binarized_results$bsummary) == "data.frame")
    expect_equal(nrow(binarized_results$bsummary), length(valid_htos))
    expect_identical(binarized_results$bsummary$hto_barcode, valid_htos)
    expect_identical(names(binarized_results$bsummary),
                     c("hto_barcode","cutoff","n_pos","n_neg","n_below_threshold",
                       "frac_pos","frac_neg","frac_below_threshold"))
    expect_true(sum(binarized_results$bsummary$frac_pos > 0.04) == 12)
  }
)

test_that(
  "binarize_hash_matrix over-represents a trimodal result if expect_equal_loading = FALSE",
  {
    valid_htos <- totalseq_a_human()$hto_barcode
    binarized_results <- binarize_hash_matrix(test_hto_mat,
                                              valid_htos = valid_htos,
                                              expect_equal_loading = FALSE)

    expect_true(class(binarized_results) == "list")
    expect_length(binarized_results, 2)
    expect_equal(names(binarized_results), c("bmat", "bsummary"))

    expect_true(class(binarized_results$bmat) == "dgCMatrix")
    expect_equal(ncol(binarized_results$bmat), ncol(test_hto_mat))
    expect_equal(nrow(binarized_results$bmat), length(valid_htos))
    expect_identical(rownames(binarized_results$bmat), valid_htos)
    expect_identical(colnames(binarized_results$bmat), colnames(test_hto_mat))
    expect_true(all(unique(binarized_results$bmat@x) %in% c(0,1)))

    expect_true(class(binarized_results$bsummary) == "data.frame")
    expect_equal(nrow(binarized_results$bsummary), length(valid_htos))
    expect_identical(binarized_results$bsummary$hto_barcode, valid_htos)
    expect_identical(names(binarized_results$bsummary),
                     c("hto_barcode","cutoff","n_pos","n_neg","n_below_threshold",
                       "frac_pos","frac_neg","frac_below_threshold"))

    expect_true(max(binarized_results$bsummary$n_pos) > 2e4)
  }
)

test_that(
  "categorize_binary_hash_matrix converts a binary matrix to categorical results",
  {
    binarized_matrix <- binarize_hash_matrix(test_hto_mat)$bmat

    hash_categories <- categorize_binary_hash_matrix(binarized_matrix)

    expect_true(class(hash_categories) == "list")
    expect_length(hash_categories, 2)

    expect_true(class(hash_categories$hash_category_table) == "data.frame")
    expect_equal(nrow(hash_categories$hash_category_table), ncol(test_hto_mat))
    expect_equal(length(hash_categories$hash_category_table), 3)
    expect_identical(names(hash_categories$hash_category_table),
                     c("cell_barcode","hto_category","hto_barcode"))

    expect_true(class(hash_categories$hash_summary) == "data.frame")
    expect_true(nrow(hash_categories$hash_summary) == 5)
    expect_identical(hash_categories$hash_summary$hto_category,
                     c("no_hash","singlet","doublet","multiplet","missing"))
  }
)


test_that(
  "make_singlet_summary reduces a category table to a summary of singlets for each hto barcode",
  {
    binarized_matrix <- binarize_hash_matrix(test_hto_mat)$bmat

    hash_categories <- categorize_binary_hash_matrix(binarized_matrix)

    valid_htos <- totalseq_a_human()$hto_barcode

    singlet_summary <- make_singlet_summary(hash_category_table = hash_categories$hash_category_table,
                                            valid_htos = valid_htos)


    expect_true(class(singlet_summary) == "data.frame")
    expect_equal(nrow(singlet_summary), length(valid_htos))
    expect_equal(length(singlet_summary), 3)
    expect_identical(names(singlet_summary),
                     c("hto_barcode","n_singlets","frac_singlets"))
    expect_true(sum(singlet_summary$n_singlets > 0) == 13)
  }
)
