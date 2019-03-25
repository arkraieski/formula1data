\name{getF1StandingsAfterRace}
\alias{getF1StandingsAfterRace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a dataframe of Formula 1 World Championship standings after a specific race
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getF1StandingsAfterRace(year, race, type = "driver")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
4 digit year of the season the race took place
}
  \item{race}{
numeric indicating which round of the season (the first race of a season is 1)
}
  \item{type}{
"constructor" or "driver"
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
function (year, race, type = "driver")
{
    if (type == "driver") {
        url <- paste0("http://ergast.com/api/f1/", year, "/",
            race, "/driverStandings.json?limit=100")
        standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$DriverStandings[[1]]
        standings$driverId <- standings$Driver$driverId
        standings$constructorId <- standings$Constructors$constructorId
        standings <- standings \%>\% select(-Driver, -Constructors,
            -positionText) \%>\% mutate_at(c("position", "points",
            "wins"), .funs = as.integer)
        standings
    }
    else if (type == "constructor") {
        url <- paste0("http://ergast.com/api/f1/", year, "/",
            race, "/constructorStandings.json?limit=100")
        standings <- fromJSON(content(GET(url), as = "text"))$MRData$StandingsTable$StandingsLists$ConstructorStandings[[1]]
        standings$constructorId <- standings$Constructor$constructorId
        standings <- standings \%>\% select(-Constructor, -positionText) \%>\%
            mutate_at(c("position", "points", "wins"), .funs = as.integer)
        standings
    }
    else {
        stop("type must be either 'driver' or 'constructor'",
            call. = FALSE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
