library("stringr")

encrypt <- function(text, times) {
    once_encrypted <- encrypt_once(text)
    if (times == 1) {
        return(once_encrypted)
    }
    encrypt(once_encrypted, times - 1)
}

encrypt_once <- function(text){
    encrypted_text <- purrr::map(
        c(0, 1),
        function(remainder) {
            subset_text_by_remainders(text, remainder)
        }
    ) %>% 
        unlist() %>% 
        paste(collapse = "")
}

subset_text_by_remainders <- function(text, remainder){
    split_text_to_characters(text) %>% 
        .[(1:length(.)) %% 2 == remainder] %>% 
        paste(collapse = "")
}


split_text_to_characters <- function (text) {
    str_split(text, "")[[1]]
}
