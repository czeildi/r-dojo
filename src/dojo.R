library("stringr")

lastDigitOfPower <- function(base, exponent){
    exponent <- getSmallestExponentWithSameDigit(base, exponent)
    
    last_digit_of_base = base %% 10
    (last_digit_of_base ^ exponent) %% 10
}

getSmallestExponentWithSameDigit <- function(base, exponent) {
    if (exponent > 10) {
        if(base %in% c(1, 5, 6)){
            exponent <- 1
        }
        else if(base %in% c(4, 9)){
            exponent <- exponent %% 2
        }
        
    }
    exponent
}
