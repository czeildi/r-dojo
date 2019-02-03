context("dojo")

test_that("waiting time is returned for 1 person and 1 cachier", {
    buyers <- data.table(name="person01", arrival=5, service_length=2)
    expect_equal(waiting_time(buyers, num_cachier=1, selected_person="person01"), 2)
})

test_that("waiting time is returned for 1 other person and 1 cachier", {
    buyers <- data.table(name="person01", arrival=5, service_length=3)
    expect_equal(waiting_time(buyers, num_cachier=1, selected_person="person01"), 3)
})

test_that("waiting time is returned in case of 2 persons and 1 cachier", {
    buyers <- data.table(tribble(
        ~name, ~arrival, ~service_length,
        "person01",   5, 3,
        "bela02",     2, 4
    ))
    expect_equal(waiting_time(buyers, num_cachier=1, selected_person = "bela02"), 4)
})

test_that("waiting time is returned for 2 persons and 1 cachier if actual waiting happens", {
    buyers <- data.table(tribble(
        ~name, ~arrival, ~service_length,
        "person01",   5, 3,
        "bela02",     2, 4
    ))
    expect_equal(
        waiting_time(buyers, num_cachier=1, selected_person = "person01"),
        4
    )
})
