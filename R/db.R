#' Download 'Ergast' CSV Database Tables
#' This function downloads the 'Ergast' database as a set of CSV files and unzips them into a local directory
#' @param destfile a character string with the name of the directory in which the files are saved
#' @details The CSV files downloaded by this function have column headers and are UTF-8 encoded. Each file contains a database table.
downloadErgastCSV <- function(destfile = paste0(getwd(), "/f1db_csv")){
  zipdest <- paste0(getwd(), "/f1db_csv.zip")
  download.file("http://ergast.com/downloads/f1db_csv.zip", destfile = zipdest)
  csv_dir <- destfile
  unzip(zipdest, exdir = csv_dir)
  unlink(zipdest)
}


#' Create an F1 database
#' Creates a local 'DuckDB' or 'SQLite' database using the latest 'Ergast' data and establish a connection.
#' @param csv_dir either NULL or the name of a directory containing csv files from Ergast.
#' If NULL, the files will be downloaded and placed in a directory within the working directory named "/f1db_csv"
#' @param rm_csv logical indicating whether the csv directory should be deleted after initializing the database
#' @param type "duckdb" or "sqlite"
#' @details \code{createF1db()} creates a local 'DuckDB' or 'SQLite' database using csv files downloaded from Ergast.
#' The database will be located in a file 'f1_db.duckdb' 'f1_db.sqlite' within the working directory.
#'
#' Databases created with this function can be interacted with using functions from the 'DBI' Package. You can also use the convenience function \code{\link{F1dbConnect}} to reconnect to a database created by \code{createF1db}
#' @return an object of class \code{\link[duckdb:duckdb_connection-class]{duckdb_connection}} or \code{\link[RSQLite:SQLiteConnection-class]{SQLiteConnection}}
#' @examples \donttest{
#' library(DBI)
#' con <- createF1db()
#' dbListTables(con)
#' dbDisconnect(con)
#' }
createF1db <- function(csv_dir = NULL, rm_csv = FALSE, type = "duckdb"){
   if(file.exists(paste0(getwd(), "/f1_db.", type))){
     stop("Database file already exists", call. = FALSE)
   }

  if(is.null(csv_dir)){
    downloadErgastCSV()
    csv_dir <- paste0(getwd(), "/f1db_csv")
  }

  if(type == "sqlite"){

    con <- dbConnect(RSQLite::SQLite(), "f1_db.sqlite")
    tryCatch({
      # function to write dataframes to the SQLite db and remove the objects from the
      # environment of create_F1_db()
      write_table <- function(con = con, tbl, env = caller_env(n = 1)){
        name <- deparse(substitute(tbl))
        dbWriteTable(con, name, value = tbl)
        rm(list = name, envir = env)
      }


      circuits <- read.csv(paste0(csv_dir, "/circuits.csv"),
                           colClasses = c("integer", rep("character", 4), rep("numeric", 2), "integer", "character"),
                           na.strings = "\\N",
                           fileEncoding = "UTF-8")
      write_table(con, tbl = circuits)
      constructor_results <- read.csv(paste0(csv_dir, "/constructor_results.csv"),
                                      colClasses = c(rep("integer", 3), "numeric", "character"))
      write_table(con, tbl = constructor_results)
      constructor_standings <- read.csv(paste0(csv_dir, "/constructor_standings.csv"),
                                        colClasses = c(rep("integer", 3), "numeric", "integer", "character", "integer"))
      write_table(con, tbl = constructor_standings)
      constructors <- read.csv(paste0(csv_dir, "/constructors.csv"),
                               colClasses = c("integer", rep("character", 4)))
      write_table(con, tbl = constructors)
      driver_standings <- read.csv(paste0(csv_dir, "/driver_standings.csv"),
                                   colClasses = c(rep("integer", 3), "numeric", "integer", "character", "integer"))
      write_table(con, tbl = driver_standings)
      drivers <- read.csv(paste0(csv_dir, "/drivers.csv"),
                          colClasses = c("integer", "character", "integer", rep("character", 3), "Date", rep("character", 2)),
                          na.strings = "\\N",
                          fileEncoding = "UTF-8")
      write_table(con, tbl = drivers)
      lap_times <- read.csv(paste0(csv_dir, "/lap_times.csv"),
                            colClasses = c(rep("integer", 4), "character", "integer"))
      write_table(con, tbl = lap_times)
      pit_stops <- read.csv(paste0(csv_dir, "/pit_stops.csv"),
                            colClasses = c(rep("integer", 4), "character", "character", "integer"))
      write_table(con, tbl = pit_stops)
      qualifying <- read.csv(paste0(csv_dir, "/qualifying.csv"),
                             colClasses = c(rep("integer", 6), rep("character", 3)),
                             na.strings = "\\N")
      write_table(con, tbl = qualifying)
      races <- read.csv(paste0(csv_dir, "/races.csv"),
                        colClasses = c(rep("integer", 4), "character", "character", "character", "character"))
      write_table(con, tbl = races)
      results <- read.csv(paste0(csv_dir, "/results.csv"),
                          colClasses = c(rep("integer", 7), "character", "integer", "numeric", "integer",
                                         "character", rep("integer", 3), rep("character", 2), "integer"),
                          na.strings = "\\N")
      write_table(con, tbl = results)
      seasons <- read.csv(paste0(csv_dir, "/seasons.csv"),
                          colClasses = c("integer", "character"))
      write_table(con, tbl = seasons)
      status <- read.csv(paste0(csv_dir, "/status.csv"),
                         colClasses = c("integer", "character"))
      write_table(con, tbl = status)
    },
    error = function(e) {
      dbDisconnect(con)
      unlink("f1_db.sqlite")
      stop(e)
    }
    )
  } else if(type == "duckdb"){
    con <- dbConnect(duckdb::duckdb(), "f1_db.duckdb")

    csv_names <- list.files(paste0(getwd(), "/f1db_csv"))

    types_by_table <- list(circuits = c("integer", rep("character", 4), rep("numeric", 2), "integer", "character"),
                  constructor_results = c(rep("integer", 3), "numeric", "character"),
                  constructor_standings = c(rep("integer", 3), "numeric", "integer", "character", "integer"),
                  constructors = c("integer", rep("character", 4)),
                  driver_standings = c(rep("integer", 3), "numeric", "integer", "character", "integer"),
                  drivers = c("integer", "character", "integer", rep("character", 3), "Date", rep("character", 2)),
                  lap_times = c(rep("integer", 4), "character", "integer"),
                  pit_stops = c(rep("integer", 4), "character", "character", "integer"),
                  qualifying =  c(rep("integer", 6), rep("character", 3)),
                  races =  c(rep("integer", 4), "character", "Date", "character", "character"),
                  results = c(rep("integer", 7), "character", "integer", "numeric", "integer",
                              "character", rep("integer", 3), rep("character", 2), "integer"),
                  seasons = c("integer", "character"),
                  status = c("integer", "character"))

    tryCatch(
      {
        purrr::walk2(csv_names, types_by_table, function(x, y){duckdb_read_csv(con,
                                                       name = gsub(".csv", "", x),
                                                       files = paste0(csv_dir, "/", x),
                                                       na.strings = "\\N",
                                                       fileEncoding = "UTF-8",
                                                       colClasses= y)})
      },

      error = function(e){
        dbDisconnect(con)
        unlink("f1_db.duckdb.wai")
        unlink("f1_db.duckdb")
        stop(e)
      }

    )

  } else {
    stop("type must be either duckdb or sqlite")
  }

  if(rm_csv){
    unlink(csv_dir)
  }
  con


}

#' Connect to an existing F1 database
#' Establishes a connection to a 'DuckDB' or 'SQLite' database previously created by \code{\link{createF1db}}.
#' @param file path to the database file
#' @return an object of class \code{\link[duckdb:duckdb_connection-class]{duckdb_connection}} or \code{\link[RSQLite:SQLiteConnection-class]{SQLiteConnection}}
#' @examples \donttest{
#' # a file "f1_db.duckdb" already exists in the working directory
#' con <- F1dbConnect()
#' dbListFields(con)
#' circuits <- dbReadTable(con, "circuits")
#' }
F1dbConnect <- function(file = "f1_db.duckdb"){
  if(!file.exists(file)){
    stop(paste0("Database file ", file, " not found"), call. = FALSE)
  }
  if(grepl(".sqlite", file, ignore.case = TRUE)){
    dbConnect(RSQLite::SQLite(), file)
  } else if(grepl(".duckdb", file, ignore.case = TRUE)){
    dbConnect(duckdb::duckdb(), file)
  } else {

    stop("file does not have extension '.duckdb' or '.sqlite'", call. = FALSE)
  }

}
