context("dojo")

test_that("if code consists of single line, no reformat needed", {
    expect_equal(
        reformat("alma"),
        "alma"
    )
})

test_that("if second line needs an extra space, one space inserted", {
    expect_equal(
        reformat(c("korte = korte", 
                   "alma = alma")),
        c("korte = korte", 
          "alma  = alma")
    )
})

test_that("if second line needs extra spaces, correct number of spaces inserted", {
    expect_equal(
        reformat(c("korte = korte", 
                   "a = alma")),
        c("korte = korte", 
          "a     = alma")
    )
})

test_that("if first line needs extra spaces, correct number of spaces inserted", {
    expect_equal(
        reformat(c("a = alma",
                   "korte = korte")),
        c("a     = alma",
          "korte = korte")
    )
})

test_that("if text consist of multiple lines containing =, spaces are inserted to all lines", {
    expect_equal(
        reformat(c("a = alma",
                   "korte = korte",
                   "be = ki")),
        c("a     = alma",
          "korte = korte",
          "be    = ki")
    )
})

test_that("if text consist of multiple lines, not all containing =, spaces are inserted to the correct lines", {
    expect_equal(
        reformat(c("a = alma",
                   "korte = korte",
                   "be ki")),
        c("a     = alma",
          "korte = korte",
          "be ki")
    )
})
