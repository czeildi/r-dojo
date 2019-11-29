library("stringr")
library("magrittr")
parseMolecule <- function(molecule){
    if (nchar(molecule) == 0) {
        return(integer(0))
    }
    
    molecule %>% 
        splitMoleculeString() %>% 
        lapply(function(atom_group) {
            atom_name <- gsub("[0-9]+", "", atom_group)
            createKeyValuePair(atom_name, getNumAtoms(atom_group))
        }) %>% 
        unlist()
}

splitMoleculeString <- function(molecule){
    molecule %>%
        gsub("([A-Z])","#\\1", .) %>%
        strsplit(., "#") %>%
        .[[1]] %>% 
        .[2:length(.)]
}

getNumAtoms <- function(atomgroup) {
    atomgroup %>% 
        str_extract("[0-9]+") %>% 
        as.numeric() %>% 
        ifelse(is.na(.), 1, .)
}

createKeyValuePair <- function(key, value) {
    result <- c(value)
    names(result) <- key
    result
}
