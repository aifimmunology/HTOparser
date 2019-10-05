context("hash_matrix_analysis")
library(HTOparser)
library(data.table)

# load test data
test_hto_table <- fread(system.file("testdata/CITE-seq_count_matrix.csv.gz",
                                    package = "HTOparser"))
test_hto_mat <- as(as.matrix(test_hto_table[,-1]), "dgCMatrix")

test_hto_key <- fread(system.file("testdata/CITE-seq_count_tags.csv",
                                  package = "HTOparser"),
                      header = FALSE)

rownames(test_hto_mat) <- test_hto_key$V1[match(test_hto_table[[1]], test_hto_key$V2)]

test_that(
  "select_hash_cutoff selects a cutoff to split HTO values",
  {

  }
)


test_that(
  "binarize_hash converts a vector of values to binary values based on a cutoff",
  {

  }
)


test_that(
  "binarize_hash_matrix converts a matrix of values to a binary matrix with automatic cutoffs",
  {

  }
)


test_that(
  "binarize_hash_matrix converts a matrix of values to a binary matrix with manual cutoffs",
  {

  }
)


test_that(
  "categorize_binary_hash_matrix converts a binary matrix to categorical results",
  {

  }
)


