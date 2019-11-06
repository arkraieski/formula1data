

#' Get a dataframe of all lap times for a specific Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{aus_laps_2018 <- getLapsByRace(2018, 1)}
getLapsByRace <- function(year, race){
  url <- paste0("https://ergast.com/api/f1/", year, "/", race, "/laps.json?limit=2000")
  laps <- GET(url)
  laps <- fromJSON(content(laps, as = "text"))$MRData$RaceTable$Races$Laps[[1]]
  laps <- unnest(laps, .data$Timings) %>%
    mutate(lap = as.integer(.data$number), position = as.integer(.data$position)) %>%
    select(-.data$number)

  laps$time <- ms(laps$time)
  laps$seconds <- 60*minute(laps$time) + second(laps$time)
  laps
}

#' Get a dataframe of a specific driver's lap times in a Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @param driverId an 'Ergast' driverId, usually the driver's last name in all lowercase
#' @examples \donttest{vettel_laps_aus_2011 <- getDriverLaps(2011, 1, driverId = "vettel")}
getDriverLaps <- function(year, race, driverId){
  url <- paste0("https://ergast.com/api/f1/", year, "/", race, "/drivers/", driverId, "/laps.json?limit=1000")
  laps <- GET(url)
  laps <- fromJSON(content(laps, as = "text"))$MRData$RaceTable$Races$Laps[[1]]
  laps <- unnest(laps, .data$Timings) %>%
    mutate(lap = as.integer(.data$number), position = as.integer(.data$position)) %>%
    select(-.data$number)

  laps$time <- ms(laps$time)
  laps$seconds <- 60*minute(laps$time) + second(laps$time)
  laps
}

#' Get a dataframe of pit stops in a Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{aus_pitstops_2018 <- getPitStopsByRace(2018, 1)}
getPitStopsByRace <- function(year, race){
  url <- paste0("http://ergast.com/api/f1/", year, "/", race, "/pitstops.json?limit=75")
  pitstops <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$PitStops[[1]]
  pitstops <- pitstops %>%
    mutate(time = hms(.data$time),
           duration = as.numeric(.data$duration)) %>%
    mutate_at(c("lap", "stop"), as.integer)
  pitstops
}


# this function results in a dataframe with nested dataframe columns for Driver and Constructor
# I am leaving the dataframe like this so that the user can clean and keep/discard data according to their needs

#' Get a dataframe of race results for a Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{aus_results_2018 <- getRaceResults(2018, 1)}
getRaceResults <- function(year, race){
  url <- paste0("https://ergast.com/api/f1/", year, "/", race, "/results.json?limit=50")
  results <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$Results[[1]]

  # convert appropriate columns to integer
  int_vars <- c("number", "position", "points", "grid")
  results <- results %>%
    mutate_at(int_vars, .funs = as.integer)
  results
}

#' Get a dataframe of qualifying results for a Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{aus_qualy_2018 <- getQualifyingResults(2018, 1)}
getQualifyingResults <- function(year, race){
  url <- paste0("http://ergast.com/api/f1/", year, "/", race, "/qualifying.json?limit=100")
  qualy <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$Qualifying[[1]]

  # get rid of nested driver column, replace with driverId
  qualy$Driver <- qualy$Driver$driverId
  # get rid of nested constructor column
  qualy$Constructor <- qualy$Constructor$constructorId

  # fix names in columns that came from nested data
  qualy <- qualy %>% rename(driverId = .data$Driver,
                            constructorId = .data$Constructor)

  char_ms_to_seconds <- function(x){
    time <- ms(x)
    seconds <- 60 * minute(time) + second(time)
    seconds}

  qualy <- qualy %>%
    select(-.data$number) %>%
    mutate(position = as.integer(.data$position),
           Q1 = char_ms_to_seconds(.data$Q1),
           Q2 = char_ms_to_seconds(.data$Q2),
           Q3 = char_ms_to_seconds(.data$Q3))
  qualy
}


#' Get a dataframe of Formula 1 World Championship final standings for a season
#' @param year a four digit integer
#' @param type constructor or driver
#' @examples
#' \donttest{driver_standings_2012 <- getFinalF1Standings(2012, type = "driver")
#' constructor_standings_2012 <- getFinalF1Standings(2012, type = "constructor")}
getFinalF1Standings <- function(year, type = "driver"){
  if(type == "driver"){
    url <- paste0("http://ergast.com/api/f1/", year,"/driverStandings.json?limit=100")
    standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$DriverStandings[[1]]
    # extract driverId from nested df column
    standings$driverId <- standings$Driver$driverId
    # extract constructorId from nested df column
    standings$constructorId <- standings$Constructors$constructorId

    # remove nested df columns
    standings <- standings %>% select(-.data$Driver, -.data$Constructors, -.data$positionText) %>%
      mutate_at(c("position", "points", "wins"), .funs = as.integer) # convert appropriate columns to type integer
    standings
  }
  else if(type == "constructor"){
    url <- paste0("http://ergast.com/api/f1/", year,"/constructorStandings.json?limit=100")
    standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$ConstructorStandings[[1]]

    # extract constructorId from nested df column
    standings$constructorId <- standings$Constructor$constructorId

    standings <- standings %>%
      select(-.data$Constructor, -.data$positionText) %>%
      mutate_at(c("position", "points", "wins"), .funs = as.integer)
    standings

    }
 else {
   stop("type must be either 'driver' or 'constructor'", call. = FALSE)
 }

}

#' Get a dataframe of Formula 1 World Championship standings after a specific race
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @param type constructor or driver
#' @examples
#' \donttest{round2_driver_standings_2012 <- getF1StandingsAfterRace(2012, 2, type = "driver")}
getF1StandingsAfterRace <- function(year, race, type = "driver"){
  if(type == "driver"){
    url <- paste0("http://ergast.com/api/f1/", year,"/", race, "/driverStandings.json?limit=100")
    standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$DriverStandings[[1]]
    # extract driverId from nested df column
    standings$driverId <- standings$Driver$driverId
    # extract constructorId from nested df column
    standings$constructorId <- standings$Constructors$constructorId

    # remove nested df columns
    standings <- standings %>% select(-.data$Driver, -.data$Constructors, -.data$positionText) %>%
      mutate_at(c("position", "points", "wins"), .funs = as.integer) # convert appropriate columns to type integer
    standings
  }
  else if(type == "constructor"){
    url <- paste0("http://ergast.com/api/f1/", year, "/", race, "/constructorStandings.json?limit=100")
    standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$ConstructorStandings[[1]]

    # extract constructorId from nested df column
    standings$constructorId <- standings$Constructor$constructorId

    standings <- standings %>%
      select(-.data$Constructor, -.data$positionText) %>%
      mutate_at(c("position", "points", "wins"), .funs = as.integer)
    standings

  }
  else {
    stop("type must be either 'driver' or 'constructor'", call. = FALSE)
  }

}
