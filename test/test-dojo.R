source("../src/dojo.R")

context("dojo")

test_that("encrypted 1 character string returns itself", {
    expect_equal(
        encrypt(text = "g", 1),
        "g"
    )
})


test_that("encrypted 2 character string returns itself reversed after one iteration", {
    expect_equal(
        encrypt(text = "ga", 1),
        "ag"
    )
})

test_that("abc encrpyted once returns bac", {
    expect_equal(
        encrypt(text = "abc", 1),
        "bac"
    )    
})


test_that("longer encrpyted text is correct", {
    expect_equal(
        encrypt(text = "This is a test!", 1),
        "hsi  etTi sats!"
    )    
})


test_that("encryption is repeated by given times", {
    expect_equal(
        encrypt(text = "This is a test!", 3),
        " Tah itse sits!"
    )    
})
