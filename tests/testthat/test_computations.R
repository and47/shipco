context("bulk distance calculation by ships")
#library(shinyShips)

simpleDataBite <- tibble::tribble(
                                  ~LAT,      ~LON,
                                    10,       20,
                                    11,       30,
                                    -5,       50
)

allDs <- calcDistances(simpleDataBite)

test_that(
  "For the first point (coordinates) distance is unknown (NA)", {
    expect_true(is.na(allDs[1]))
})

test_that(
  "Absurd input is not processed", {
    expect_error(calcDistances(-1000))
})

test_that(
  "Distance is correctly calculated", {
    expect_equal(tolerance = 1, allDs[-1],
                 c(1098908, 2839873))
})

context("bulk distance calculation by ships")
library(dplyr)
library(magrittr)


realDataBite <- tibble::tribble(
  ~FLAG,    ~LAT,     ~LON, ~SHIP_ID,              ~DATETIME,
  "MT", 54.76542, 18.99361,     2764, "2016-12-19T11:31:02Z",
  "MT", 54.75468, 18.98753,     2764, "2016-12-19T11:35:02Z",
  "MT", 54.76007, 18.99059,     2764, "2016-12-19T11:33:02Z",
  "MT", 54.74926, 18.98447,     2764, "2016-12-19T11:37:02Z",
  "MT", 54.74385,  18.9815,     2764, "2016-12-19T11:39:01Z",
  "FI", 54.40131, 18.67927,     6223, "2016-12-14T13:21:02Z",
  "FI", 54.40131, 18.67926,     6223, "2016-12-14T13:23:01Z",
  "FI", 54.40131, 18.67926,     6223, "2016-12-14T13:25:01Z",
  "FI", 54.40131, 18.67926,     6223, "2016-12-14T13:27:02Z"
)

distanceRecords <- realDataBite %>%
  mutate(DATETIME = lubridate::as_datetime(DATETIME)) %>% 
  arrange(DATETIME) %>% group_by(SHIP_ID) %>%
  group_modify( ~mutate(., METERSFROMLAST = calcDistances(select(., LAT, LON))))

test_that(
  "For the first point (coordinates) distance is unknown (NA)", {
    expect_true(all(is.na(distanceRecords[which(!duplicated(realDataBite$SHIP_ID)),
                                          "METERSFROMLAST"])))
  })

test_that(
  "Table has correct values in correct order (rows, cols, and filled distances)", {
    expect_equal(tolerance = 1, na.omit(distanceRecords[[6]]),
                 na.omit(
                   c(NA, 625.6487221, 630.6815798, 633.8606280, 631.0428341, 
                   NA, 0.6468281, 0.0000000, 0.0000000)))
  })
