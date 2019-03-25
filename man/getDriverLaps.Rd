\name{getDriverLaps}
\alias{getDriverLaps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a dataframe of a single driver's laptimes for a race.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getDriverLaps(year, race, driverId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
4 digit year of the season the race took place
}
  \item{race}{
numeric indicating which round of the season (the first race of a season is 1)
}
  \item{driverId}{
Ergast driverId of the driver, this is usually the drivers' last name in lowercase
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (year, race, driverId)
{
    url <- paste0("https://ergast.com/api/f1/", year, "/", race,
        "/drivers/", driverId, "/laps.json?limit=2000")
    laps <- GET(url)
    laps <- fromJSON(content(laps, as = "text"))$MRData$RaceTable$Races$Laps[[1]]
    laps <- unnest(laps, Timings) \%>\% mutate(lap = as.integer(number),
        position = as.integer(position)) \%>\% select(-number)
    laps$time <- ms(laps$time)
    laps$seconds <- 60 * minute(laps$time) + second(laps$time)
    laps
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
