source('libraries.R')
invisible(sapply(list.files("R", full.names = TRUE), source))
testthat::auto_test("R","tests/testthat")
