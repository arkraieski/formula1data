\name{getRaceResults}
\alias{getRaceResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a Dataframe of Results for an F1 Grand Prix
}
\description{
This function returns a dataframe of results for the specified Grand Prix. The dataframe has some nested columns that are themselves dataframes. I have left those as is so the user can extract the information needed for their analysis.
}
\usage{
getRaceResults(year, race)
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
    url <- paste0("https://ergast.com/api/f1/", year, "/", race,
        "/results.json$limit=50")
    results <- fromJSON(content(GET(url), as = "text"))$MRData$RaceTable$Races$Results[[1]]
    int_vars <- c("number", "position", "points", "grid")
    results <- results \%>\% mutate_at(int_vars, .funs = as.integer)
    results
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
