context("dojo")

testInvalidInput <- function(input) {
    test_that(paste0("no valid substring in ", input), {
        expect_equal(
            findLongestPart(input),
            "no valid substring"
        )
    })
}

testValidInput <- function(input, start, end) {
    test_that(paste0("longest valid substring is found in ", input), {
        expect_equal(
            findLongestPart(input),
            glue(
                "longest valid substring is between characters ",
                "{start}, {end}"
            )
        )
    })
}

testInvalidInput("")

testValidInput("()", 1, 2)

testInvalidInput("(")

testValidInput("(test)", 1, 6)

testInvalidInput(")(")

testValidInput("()()", 1, 4)
