library("stringr")
library("purrr")

KEY_PRESSES <- c(
    "2", "22", "222", "3", "33", "333", "4", "44", "444", "5", "55", "555",
    "6", "66", "666", "7", "77", "777", "7777", "8", "88", "888", "9", "99", "999", "9999"
)

KEYS_TO_LETTERS <- letters
names(KEYS_TO_LETTERS) <- KEY_PRESSES

get_matching_words <- function(pressed_keys, wordlist) {
    word <- pressed_keys %>% 
        split_pressed_keys_to_list() %>% 
        key_list_to_word()
    
    keep(wordlist, ~startsWith(.x, prefix = word))
}

split_pressed_keys_to_list <- function(pressed_keys) {
    strsplit(pressed_keys, " ")[[1]]
}

key_list_to_word <- function(key_list) {
    map(key_list, ~KEYS_TO_LETTERS[[.x]]) %>% paste(collapse = "")
}
