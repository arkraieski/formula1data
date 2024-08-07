#' Get a data.frame of all lap times for a specific Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{aus_laps_2018 <- getLapsByRace(2018, 1)}
getLapsByRace <- function(year, race){
  url1 <- paste0("https://ergast.com/api/f1/", year, "/", race, "/laps.json")
  laps1 <- GET(url1)
  stop_for_status(laps1)
  laps1 <- fromJSON(content(laps1, as = "text"))
  limit <- as.integer(laps1$MRData$total) - 30 # this is used to determine how many records to request in second api call
  laps1 <- laps1$MRData$RaceTable$Races$Laps[[1]]


  url2 <- paste0("https://ergast.com/api/f1/", year, "/", race, "/laps.json?offset=30&limit=", limit)
  laps2 <- GET(url2)
  stop_for_status(laps2)
  laps2 <- fromJSON(content(laps2, as = "text"))$MRData$RaceTable$Races$Laps[[1]]

  laps <- bind_rows(laps1, laps2) %>% unnest(.data$Timings) %>%
    mutate(lap = as.integer(.data$number), position = as.integer(.data$position)) %>%
    select(-.data$number)

  laps$time <- ms(laps$time)
  laps$seconds <- 60*minute(laps$time) + second(laps$time)
  laps
}

#' Get a data.frame of a specific driver's lap times in a Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @param driverId string containing an 'Ergast' driverId, usually the driver's last name in all lowercase
#' @examples \donttest{vettel_laps_aus_2011 <- getDriverLaps(2011, 1, driverId = "vettel")}
getDriverLaps <- function(year, race, driverId){
  url1 <- paste0("https://ergast.com/api/f1/", year, "/", race, "/drivers/", driverId, "/laps.json")
  laps1 <- GET(url1)
  stop_for_status(laps1)
  laps1 <- fromJSON(content(laps1, as = "text"))
  limit <- as.integer(laps1$MRData$total) - 30
  laps1 <- laps1$MRData$RaceTable$Races$Laps[[1]]

  url2 <-paste0("https://ergast.com/api/f1/", year, "/", race, "/drivers/", driverId, "/laps.json?offset=30&limit=", limit)
  laps2 <- GET(url2)
  stop_for_status(laps2)
  laps2 <- fromJSON(content(laps2, as = "text"))$MRData$RaceTable$Races$Laps[[1]]

  laps <- bind_rows(laps1, laps2) %>%
    unnest(.data$Timings) %>%
    mutate(lap = as.integer(.data$number), position = as.integer(.data$position)) %>%
    select(-.data$number)

  laps$time <- ms(laps$time)
  laps$seconds <- 60*minute(laps$time) + second(laps$time)
  laps
}

#' Get a data.frame of pit stops in a Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{aus_pitstops_2018 <- getPitStopsByRace(2018, 1)}
getPitStopsByRace <- function(year, race){
  url <- paste0("http://ergast.com/api/f1/", year, "/", race, "/pitstops.json?limit=100")
  pitstops <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$PitStops[[1]]
  pitstops <- pitstops %>%
    mutate(time = hms(.data$time),
           duration = as.numeric(.data$duration)) %>%
    mutate_at(c("lap", "stop"), as.integer)
  pitstops
}



#' Get a data.frame of race results for a Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{aus_results_2018 <- getRaceResults(2018, 1)}
getRaceResults <- function(year, race){
  url <- paste0("https://ergast.com/api/f1/", year, "/", race, "/results.json?limit=50")
  response <- GET(url)
  stop_for_status(response)
  results <- fromJSON(content(response, as = "text"))$MRData$RaceTable$Races$Results[[1]]

  # convert appropriate columns to integer
  int_vars <- c("number", "position", "points", "grid")
  results <- results %>%
    mutate_at(int_vars, .funs = as.integer)

  results$driverId <- results$Driver$driverId
  results$constructorId <- results$Constructor$constructorId

  results
}


#' Get a data.frame of results for an F1 Sprint Qualifying session
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{
#' # Sprint Qualifying results for the 2021 British Grand Prix
#' uk_sprint32 <- getSprintResults(2021, 10)
#' }
getSprintResults <- function(year, race){
  url <- paste0("https://ergast.com/api/f1/", year, "/", race, "/sprint.json")
  response <- GET(url)
  stop_for_status(response)
  sprint <- fromJSON(content(response, as = "text"))$MRData$RaceTable$Races$SprintResult[[1]]
  sprint

  # extract driverId from nested df column
  sprint$driverId <- sprint$Driver$driverId
  sprint <- sprint %>% select(-.data$Driver)
  # extract constructorId from nested df column
  sprint$constructorId <- sprint$Constructor$constructorId
  sprint <- sprint %>% select(-.data$Constructor)

  int_vars <- c("number", "position", "points", "grid", "laps")

  sprint <- sprint %>% mutate_at(int_vars, as.integer)

  # parse finishing time from "millis" column in "Time" nested df
  sprint$time <- ms(format( as.POSIXct(Sys.Date()) + as.integer(sprint$Time$millis)/1000, "%M:%S")) # note capitalization
  sprint$millis <- sprint$Time$millis
  sprint <- sprint %>% select(-.data$Time)

  sprint$fastestLap <- sprint$FastestLap$lap
  sprint$fastestLapTime <- ms(sprint$FastestLap$Time$time)
  sprint <- sprint %>% select(-.data$FastestLap)

  sprint
}

#' Get a data.frame of qualifying results for a Formula 1 Grand Prix
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @examples \donttest{aus_qualy_2018 <- getQualifyingResults(2018, 1)}
getQualifyingResults <- function(year, race){
  url <- paste0("http://ergast.com/api/f1/", year, "/", race, "/qualifying.json?limit=100")
  response <- GET(url)
  stop_for_status(response)
  qualy <- fromJSON(content(response, as = "text"))$MRData$RaceTable$Races$Qualifying[[1]]

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

  qualy <- fillMissingQualifyingColumns(qualy) # for years before q2, q3, etc. were introduced
  qualy <- qualy %>%
    select(-.data$number) %>%
    mutate(position = as.integer(.data$position),
           Q1 = char_ms_to_seconds(.data$Q1),
           Q2 = char_ms_to_seconds(.data$Q2),
           Q3 = char_ms_to_seconds(.data$Q3))
  qualy
}


#' Get a data.frame of Formula 1 World Championship final standings for a season
#' @param year a four digit integer
#' @param type constructor or driver
#' @examples
#' \donttest{driver_standings_2012 <- getFinalF1Standings(2012, type = "driver")
#' constructor_standings_2012 <- getFinalF1Standings(2012, type = "constructor")}
getFinalF1Standings <- function(year, type = "driver"){
  if(type == "driver"){
    url <- paste0("http://ergast.com/api/f1/", year,"/driverStandings.json?limit=100")
    response <- GET(url)
    stop_for_status(response)
    standings <- fromJSON(content(response, as = "text"))$MRData$StandingsTable$StandingsLists$DriverStandings[[1]]
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

#' Get a data.frame of Formula 1 World Championship standings after a specific race
#' @param year a four digit integer
#' @param race a 1 or 2 digit integer indicating which round of the season
#' @param type constructor or driver
#' @examples
#' \donttest{round2_driver_standings_2012 <- getF1StandingsAfterRace(2012, 2, type = "driver")}
getF1StandingsAfterRace <- function(year, race, type = "driver"){
  if(type == "driver"){
    url <- paste0("http://ergast.com/api/f1/", year,"/", race, "/driverStandings.json?limit=100")
    response <- GET(url)
    stop_for_status(response)
    standings <- fromJSON(content(response, as = "text"))$MRData$StandingsTable$StandingsLists$DriverStandings[[1]]
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

#' Get a data.frame of the Formula 1 schedule for a given year
#' @param year a four digit integer or "current"
#' @examples \donttest{
#' schedule_2019 <- getF1Schedule(2020)
#' }
getF1Schedule <- function(year){
  url <- paste0("https://ergast.com/api/f1/", year, ".json")
  response <- GET(url)
  stop_for_status(response)
  sched <- fromJSON(content(response, as = "text"))$MRData$RaceTable$Races
  sched <- sched %>%
    mutate_at(c("season", "round"), as.integer) %>%
    mutate(time = gsub("z", "", .data$time),
           datetime = ymd_hms(paste(.data$date, .data$time)),
           date = ymd(.data$date)) %>%
    select(-.data$time)
  sched

}

#' Get a data.frame of Formula 1 qualifying results for a specific constructor for a given year
#' @param year a four digit integer
#' @param constructor a string containing an 'Ergast' constructorId, usually the constructor's name in all lowercase
#' @examples \donttest{mercedes_qualy_2018 <- getConstructorQualifying(2018, "mercedes")}
getConstructorQualifying <- function(year, constructor){

  ergast_url <- "https://ergast.com/api/f1/"
  request_url <- paste0(ergast_url,
                        year,
                        "/constructors/",
                        constructor,
                        "/qualifying.json?limit=60") # hopefully limit is enough for a while

  response <- GET(request_url)
  stop_for_status(response)


  qualy_data <- fromJSON(content(response, as = "text"))$MRData$RaceTable$Races

  qualy_data <- qualy_data %>%
    unnest(.data$QualifyingResults,
           names_repair = "universal")

  char_ms_to_seconds <- function(x){
    time <- ms(x)
    seconds <- 60 * minute(time) + second(time)
    seconds
  }

  qualy_data <- fillMissingQualifyingColumns(qualy_data) # for years before q2, q3, etc. were introduced
  qualy_data <- qualy_data %>%
    mutate_at(c("season", "round"), as.integer) %>%
    mutate_at(c("Q1", "Q2", "Q3"), char_ms_to_seconds)

  qualy_data$Driver <- qualy_data$Driver$driverId



  qualy_data
}

#' Detect when a data.frame doesn't have Q2 and Q3 Columns and fill them with NA
#' @param df a data.frame
#' @NoRd
fillMissingQualifyingColumns <- function(df){
  if(!"Q2" %in% colnames(df)){
    df$Q2 <- NA
  }
  if(!"Q3" %in% colnames(df)){
    df$Q3 <- NA
  }
  df
}
