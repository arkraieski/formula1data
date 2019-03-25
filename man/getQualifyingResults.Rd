\name{getQualifyingResults}
\alias{getQualifyingResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gets a dataframe of qualifying results for a F1 Grand Prix
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getQualifyingResults(year, race)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
4 digit year of the season the race took place
}
  \item{race}{
numeric indicating which round of the season (the first race of a season is 1)
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
function (year, race)
{
    url <- paste0("http://ergast.com/api/f1/", year, "/", race,
        "/qualifying.json$limit=100")
    qualy <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$Qualifying[[1]]
    qualy$Driver <- qualy$Driver$driverId
    qualy$Constructor <- qualy$Constructor$constructorId
    qualy <- qualy \%>\% rename(driverId = Driver, constructorId = Constructor)
    char_ms_to_seconds <- function(x) {
        time <- ms(x)
        seconds <- 60 * minute(time) + second(time)
        seconds
    }
    qualy <- qualy \%>\% select(-number) \%>\% mutate(position = as.integer(position),
        Q1 = char_ms_to_seconds(Q1), Q2 = char_ms_to_seconds(Q2),
        Q3 = char_ms_to_seconds(Q3))
    qualy
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
