# formula1data
R package to import Formula 1 data using the ['jolpica'](https://github.com/jolpica/jolpica-f1) API (formerly the deprecated Ergast API was used)

Start your engines! This package aims to provide a simple, consistent syntax to query the API and get clean data.frames of Formula 1 World Championship data. It includes functions to import lap times, pit stops, qualifying results, race results, and world championship standings. The package also includes functions to create a full local copy of Ergast's database in 'SQLite' using the 'RSQLite' package. 

I have had a ton of fun playing with these datasets, so I wanted to make it easy for novice and advanced R users alike to get started analyzing Formula 1. I hope you will have as much fun with this data as I have had!

The package can be installed in R as follows:

    install.packages("devtools")
    library(devtools)
    install_github("arkraieski/formula1data")
    
## Transition to jolpica-f1 API

As a part of the transition to the new API, users should keep in mind the [rate limits](https://github.com/jolpica/jolpica-f1/blob/main/docs/rate_limits.md). This package should keep you under the burst limit with any given function call (most function calls make multiple requests). The jolpica-f1 team also plans to implement a token system and decrease the unauthenticated rate limit, so please be on the lookout for updates and don't be afraid to open issues as necessary.

The new API uses a new database schema, and I will support some sort of local database option when dumps become available. Building duckDB and SQLite databases with the last versions of the Ergast CSVs will continue to be supported for backwards compatibility, but that dataset is no longer updated.

## Disclaimer

This package is unofficial and is not associated in any way with the Formula 1 companies. F1, FORMULA ONE, FORMULA 1, FIA FORMULA ONE WORLD CHAMPIONSHIP, GRAND PRIX and related marks are trade marks of Formula One Licensing B.V. Any use of these or related marks in this package and its documentation is intended to inform users of the software's purpose rather than to brand it or imply an association with Formula 1. I am just a passionate fan!
