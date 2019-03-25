# cmd check throws a note because of use of nonstandard evaluation, I think this can be ingnored



getLapsByRace <- function(year, race){
  url <- paste0("https://ergast.com/api/f1/", year, "/", race, "/laps.json?limit=2000")
  laps <- GET(url)
  laps <- fromJSON(content(laps, as = "text"))$MRData$RaceTable$Races$Laps[[1]]
  laps <- unnest(laps, Timings) %>%
    mutate(lap = as.integer(number), position = as.integer(position)) %>%
    select(-number)

  laps$time <- ms(laps$time)
  laps$seconds <- 60*minute(laps$time) + second(laps$time)
  laps
}

getDriverLaps <- function(year, race, driverId){
  url <- paste0("https://ergast.com/api/f1/", year, "/", race, "/drivers/", driverId, "/laps.json?limit=2000")
  laps <- GET(url)
  laps <- fromJSON(content(laps, as = "text"))$MRData$RaceTable$Races$Laps[[1]]
  laps <- unnest(laps, Timings) %>%
    mutate(lap = as.integer(number), position = as.integer(position)) %>%
    select(-number)

  laps$time <- ms(laps$time)
  laps$seconds <- 60*minute(laps$time) + second(laps$time)
  laps
}


getPitStopsByRace <- function(year, race){
  url <- paste0("http://ergast.com/api/f1/", year, "/", race, "/pitstops.json?limit=100")
  pitstops <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$PitStops[[1]]
  pitstops <- pitstops %>%
    mutate(time = hms(time),
           duration = as.numeric(duration))
  pitstops
}


# this function results in a dataframe with nested dataframe columns for Driver and Constructor
# I am leaving the dataframe like this so that the user can clean and keep/discard data according to their needs
getRaceResults <- function(year, race){
  url <- paste0("https://ergast.com/api/f1/", year, "/", race, "/results.json$limit=50")
  results <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$Results[[1]]

  # convert appropriate columns to integer
  int_vars <- c("number", "position", "points", "grid")
  results <- results %>%
    mutate_at(int_vars, .funs = as.integer)
  results
}


getQualifyingResults <- function(year, race){
  url <- paste0("http://ergast.com/api/f1/", year, "/", race, "/qualifying.json$limit=100")
  qualy <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$Qualifying[[1]]

  # get rid of nested driver column, replace with driverId
  qualy$Driver <- qualy$Driver$driverId
  # get rid of nested constructor column
  qualy$Constructor <- qualy$Constructor$constructorId

  # fix names in columns that came from nested data
  qualy <- qualy %>% rename(driverId = Driver,
                            constructorId = Constructor)

  char_ms_to_seconds <- function(x){
    time <- ms(x)
    seconds <- 60 * minute(time) + second(time)
    seconds}

  qualy <- qualy %>%
    select(-number) %>%
    mutate(position = as.integer(position),
           Q1 = char_ms_to_seconds(Q1),
           Q2 = char_ms_to_seconds(Q2),
           Q3 = char_ms_to_seconds(Q3))
  qualy
}



getFinalF1Standings <- function(year, type = "driver"){
  if(type == "driver"){
    url <- paste0("http://ergast.com/api/f1/", year,"/driverStandings.json?limit=100")
    standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$DriverStandings[[1]]
    # extract driverId from nested df column
    standings$driverId <- standings$Driver$driverId
    # extract constructorId from nested df column
    standings$constructorId <- standings$Constructors$constructorId

    # remove nested df columns
    standings <- standings %>% select(-Driver, -Constructors, -positionText) %>%
      mutate_at(c("position", "points", "wins"), .funs = as.integer) # convert appropriate columns to type integer
    standings
  }
  else if(type == "constructor"){
    url <- paste0("http://ergast.com/api/f1/", year,"/constructorStandings.json?limit=100")
    standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$ConstructorStandings[[1]]

    # extract constructorId from nested df column
    standings$constructorId <- standings$Constructor$constructorId

    standings <- standings %>%
      select(-Constructor, -positionText) %>%
      mutate_at(c("position", "points", "wins"), .funs = as.integer)
    standings

    }
 else {
   stop("type must be either 'driver' or 'constructor'", call. = FALSE)
 }

}

getF1StandingsAfterRace <- function(year, race, type = "driver"){
  if(type == "driver"){
    url <- paste0("http://ergast.com/api/f1/", year,"/", race, "/driverStandings.json?limit=100")
    standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$DriverStandings[[1]]
    # extract driverId from nested df column
    standings$driverId <- standings$Driver$driverId
    # extract constructorId from nested df column
    standings$constructorId <- standings$Constructors$constructorId

    # remove nested df columns
    standings <- standings %>% select(-Driver, -Constructors, -positionText) %>%
      mutate_at(c("position", "points", "wins"), .funs = as.integer) # convert appropriate columns to type integer
    standings
  }
  else if(type == "constructor"){
    url <- paste0("http://ergast.com/api/f1/", year, "/", race, "/constructorStandings.json?limit=100")
    standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$ConstructorStandings[[1]]

    # extract constructorId from nested df column
    standings$constructorId <- standings$Constructor$constructorId

    standings <- standings %>%
      select(-Constructor, -positionText) %>%
      mutate_at(c("position", "points", "wins"), .funs = as.integer)
    standings

  }
  else {
    stop("type must be either 'driver' or 'constructor'", call. = FALSE)
  }

}
