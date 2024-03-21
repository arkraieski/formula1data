.onAttach <- function(libname, pkgname){
  packageStartupMessage("Notice from 'formula1data': The Ergast API is deprecated will be discontinued at year's end after the 2024 season. Please save any essential dataframes for future use (type '?saveRDS' for help with this). Please see this R package's github repo for issues/discussion regarding future plans and potential alternative data sources. Thank you.")
}
