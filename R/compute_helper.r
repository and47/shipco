#' Computes all distances in a tibble (between each two consecuitive points)
#'
#' @param coords two columns with time-sorted coordinates (lattitude & longitude); in other words, one row represents a point (two coordinates)
#'
#' @return A numeric (dbl) vector with distances in meters in the same order as provided input
#'
#' @examples
#'
#' require(readr)
#' require(dplyr)
#' require(magrittr)
#' require(lubridate)
#' 
#' shipData <- read_csv('extdata/ships.csv')
#'
#' shipData %<>% mutate(DATETIME = lubridate::as_datetime(DATETIME)) %>% 
#'  arrange(DATETIME) %>% group_by(SHIP_ID) %>%
#'  group_modify(~mutate(., METERSFROMLAST = calcDistances(select(., LAT, LON))))
#'
#' shipData %>% select(LON, LAT, METERSFROMLAST) 
#'
#' @export
#'
calcDistances <- function(coords) {
  stopifnot(ncol(coords) > 1)  # at least two dimensions
  stopifnot(all(range(coords[,1], na.rm = TRUE) %inrange% c(-90,90)))  # degrees
  stopifnot(all(range(coords[,2], na.rm = TRUE) %inrange% c(-180,180)))
  
  p = pi / 180
  
  lat <-  coords[[1]]  # long <- coords[[2]]
  
  lat_chgs <-  c(NA, diff(coords[[1]]))  # need to preceed diff with NA 
  long_chgs <- c(NA, diff(coords[[2]]))  # (for compatibility with lag)
  
  a = 0.5 - cos(lat_chgs * p) / 2 +
    cos(lat*p) * cos(lag(lat)*p) * (1 - cos(long_chgs*p)) / 2
  
  12742 * 1000 * asin(sqrt(a))  # Earth's diameter, meters
}


#' From processed data retrieve info about a given ship by name
#'
#' Implementation: tidyverse/dplyr. 
#' @param shipName A character vector of length = 1, with a single case-sensitive ship name from provided \code{dataset}.
#' @param allShips A tibble with a variable (column) named \code{SHIPNAME} from \code{dataset}, based on which other info is retrieved. \code{DATETIME} column is required to ensure retrieval of the latest info about a ship.
#'
#' @return A character vector with 3 info bits: ship's ID, flag, and port.
#'
#' @examples
#'
#' require(readr)
#' require(dplyr)
#' shipData <- read_csv('shinyShips4io/extdata/.csv')  #  not needed for the included dataset
#' shipData$DATETIME <- lubridate::as_datetime(shipData$DATETIME))
#' getShipLatestInfo("VIKING", shipData)
#'
#' @export
#'
getShipLatestInfo <- function(shipName, allShips) {
  if (nchar(shipName) <= 1) return("")
  allShips %>% filter(SHIPNAME == shipName) %>% arrange(DATETIME) %>% 
    last() %>% select(SHIP_ID, FLAG, PORT) %>% as.character()
}
