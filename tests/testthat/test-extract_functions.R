

test_that("Limits (Start and end) are correct", {
  # Load example data
  file<- testthat::test_path("example", "sample.fasta")
  sample_data<-Biostrings::readDNAStringSet(file,format="fasta")
  expect_identical(extract_seq(sample_data, 4, 6), 10)
  expect_identical(my_good_sum(6, 4), 10)
})
