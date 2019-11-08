context("dojo")

test_that("no match for letter sequency from empty wordlist", {
    expect_equal(
        get_matching_words(pressed_keys = "2", wordlist = character(0)),
        character(0)
    )
})

test_that("exactly one match is returned for one letter provided which is in the wordlist", {
    expect_equal(
        get_matching_words(pressed_keys = "2", wordlist = c("a")),
        c("a")
    )
})

test_that("no match is returned for one letter provided which is not in the wordlist", {
    expect_equal(
        get_matching_words(pressed_keys = "2", wordlist = c("b")),
        character(0)
    )
})

test_that("no match is returned when the pressed key is not a, but should match", {
    expect_equal(
        get_matching_words(pressed_keys = "22", wordlist = c("a")),
        character(0)
        )
})

describe("each letter is matched based on the corresponding keys pressed", {
    keys <- c("2", "22", "222", "3", "33", "333", "4", "44", "444",
              "5", "55", "555", "6", "66", "666", "7", "77", "777", "7777",
              "8", "88", "888", "9", "99", "999", "9999")
    purrr::walk2(letters, keys, ~{
        letter <<- .x
        pressed_keys <<- .y
        it(glue::glue("{letter} is matched if {pressed_keys} is pressed"), {
            expect_equal(
                get_matching_words(pressed_keys, wordlist = letter),
                letter
            )
        })
    })
})

describe("multiple letters are entered", {
    it("returns exact match if two different letters are pressed", {
        expect_equal(
            get_matching_words(pressed_keys = "22 33", wordlist = c("be")),
            c("be")
        )
    })
    
    it("returns all words starting with the entered letters", {
        expect_equal(
            get_matching_words(pressed_keys = "2",
                               wordlist = c("alma", "auto", "banan")),
            c("alma", "auto")
        )
    })
})

