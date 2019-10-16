context("totalseq_references")
library(HTOparser)

test_that(
  "totalseq_a_human loads human reference data.",
  {
    human_ref <- totalseq_a_human()

    expect_true("data.table" %in% class(human_ref))
    expect_equal(nrow(human_ref), 14)
  }
)

test_that(
  "totalseq_a_mouse loads mouse reference data.",
  {
    mouse_ref <- totalseq_a_mouse()

    expect_true("data.table" %in% class(mouse_ref))
    expect_equal(nrow(mouse_ref), 15)
  }
)

test_that(
  "totalseq_a_biotin loads biotin reference data.",
  {
    biotin_ref <- totalseq_a_biotin()

    expect_true("data.table" %in% class(biotin_ref))
    expect_equal(nrow(biotin_ref), 9)
  }
)
