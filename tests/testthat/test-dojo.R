context("dojo")

test_that("empty molecule has no atoms", {
    expect_equal(
        parseMolecule(""),
        integer(0)
    )
})

test_that("can parse one atom", {
    expect_equal(
        parseMolecule("H"),
        c(H = 1)
    )
})

test_that("can parse two single letter atoms", {
    expect_equal(
        parseMolecule("CO"),
        c(C = 1, O = 1)
    )  
})

test_that("can parse 2-letter single atom", {
    expect_equal(
        parseMolecule("HCl"),
        c(H = 1, Cl = 1)
    )
})

test_that("can parse atoms with numbers", {
    expect_equal(
        parseMolecule("H2"),
        c(H = 2)
    )
})
