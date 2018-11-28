source('libraries.R')
invisible(sapply(list.files("R", full.names = TRUE), source))
testthat::test_dir("tests/testthat")
