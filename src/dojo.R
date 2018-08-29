library("stringr")
library("purrr")
library("dplyr")
library("magrittr")
library("glue")
library("dplyr")

look_and_say <- function(input, n) {
    df <- data.frame("digits" = split(input))
    df %>% 
        group_by(digits) %>% 
        summarise(n = n()) %>%
        mutate(pairs = str_c(n, digits)) %>% 
        pull(pairs) %>%
        collapse()
}

split <- function(input) {
    str_split(input, '')[[1]]
}

collapse <- . %>% glue_collapse() %>% as.character()
