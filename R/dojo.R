library("stringr")
weekdayOfDate <- function(year, month, day) {
    a_sunday <- c("year" = 2018, "month" = 6, "day" = 3)
    num_days_from_a_sunday <- numDaysBetween(year, month, day, a_sunday)
    nameOfDayWithIndex(num_days_from_a_sunday %% 7)
}

numDaysBetween <- function(year, month, day, reference_date) {
    year_distance <- getYearDistanceInDays(year, reference_date[["year"]])
    month_distance <- getMonthDistanceInDays(month, reference_date[["month"]])
    day_distance <- getDayDistanceInDays(day, reference_date[["day"]])
    year_distance + month_distance + day_distance
}

getYearDistanceInDays <- function(year, reference_year) {
    year_distance <- 365 * (year - reference_year)
    leap_days <- countLeapYearsInInterval(year, reference_year)
    year_distance + sign(year - reference_year) * leap_days
}

countLeapYearsInInterval <- function(year, reference_year) {
    years <- min(year, reference_year) : (max(year, reference_year) - 1)
    leap_years <- years[years %% 4 == 0 & (years %% 100 != 0 | years %% 400 == 0)]
    length(leap_years)
}

getMonthDistanceInDays <- function(month, reference_month) {
    sign(month - reference_month) * totalDaysBetweenMonths(month, reference_month)
}

totalDaysBetweenMonths <- function(month, reference_month) {
    month_lengths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    first_month_index <- min(month, reference_month)
    last_month_index <- max(month, reference_month) - 1
    sum(month_lengths[first_month_index : last_month_index])
}

getDayDistanceInDays <- function(day, reference_day) {
    day - reference_day
}

nameOfDayWithIndex <- function(index_in_week) {
    weekday_names <- c(
        "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
        "Saturday", "Sunday"
    )
    weekday_names[index_in_week]
}

# today
stopifnot(weekdayOfDate(2018, 6, 7) == "Thursday")

# this week
stopifnot(weekdayOfDate(2018, 6, 4) == "Monday")
stopifnot(weekdayOfDate(2018, 6, 5) == "Tuesday")
stopifnot(weekdayOfDate(2018, 6, 6) == "Wednesday")
stopifnot(weekdayOfDate(2018, 6, 8) == "Friday")
stopifnot(weekdayOfDate(2018, 6, 9) == "Saturday")
stopifnot(weekdayOfDate(2018, 6, 10) == "Sunday")

# this month
stopifnot(weekdayOfDate(2018, 6, 1) == "Friday")
stopifnot(weekdayOfDate(2018, 6, 30) == "Saturday")

# this summer
stopifnot(weekdayOfDate(2018, 7, 1) == "Sunday")
stopifnot(weekdayOfDate(2018, 7, 18) == "Wednesday")

# this year
stopifnot(weekdayOfDate(2018, 12, 31) == "Monday")
stopifnot(weekdayOfDate(2018, 1, 1) == "Monday")
stopifnot(weekdayOfDate(2018, 12, 1) == "Saturday")
stopifnot(weekdayOfDate(2018, 1, 6) == "Saturday")

# next year
stopifnot(weekdayOfDate(2019, 1, 1) == "Tuesday")

# simple leap year
stopifnot(weekdayOfDate(2023, 1, 1) == "Sunday")

# years divisible by 100 are not leap years
stopifnot(weekdayOfDate(2123, 1, 1) == "Friday")

# years divisible by 400 are leap years
stopifnot(weekdayOfDate(1975, 1, 1) == "Wednesday")

# handle if you are within leap year
