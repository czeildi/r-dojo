library("stringr")

waiting_time <- function(buyers, num_cachier, selected_person) {
    ordered_buyers <- buyers %>%
        .[order(arrival)]
    
    finish_time_of_previous_person <- 0
    for(row_id in seq_len(nrow(ordered_buyers))) {
        start_time_of_current_person <- max(finish_time_of_previous_person, ordered_buyers[row_id, arrival])
        finish_time_of_current_person = start_time_of_current_person + ordered_buyers[row_id, ]$service_length
        ordered_buyers[row_id, finish_time := finish_time_of_current_person]
        finish_time_of_previous_person <- finish_time_of_current_person
    }
    ordered_buyers %>% 
        .[, waiting_time := finish_time - arrival] %>% 
        .[name == selected_person, ]%>% 
        .[["waiting_time"]]
}
