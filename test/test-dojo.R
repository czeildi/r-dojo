source("../src/dojo.R")

context("dojo")

test_cases <- list(
    list(1, "I"),
    list(2, "II"),
    list(5, "V"),
    list(4, "IV"),
    list(6, "VI")
)

purrr::walk(test_cases, ~{
    test_that(glue::glue("{.x[[1]]} in roman is {.x[[2]]}"), {
        expect_equal(to_roman(.x[[1]]), .x[[2]])
    })    
})
