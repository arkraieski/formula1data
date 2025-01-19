# formula1data
R package to import Formula 1 data using the ['jolpica'](https://github.com/jolpica/jolpica-f1) API (formerly the deprecated Ergast API was used)

Start your engines! This package aims to provide a simple, consistent syntax to query the API and get clean data.frames of Formula 1 World Championship data. It includes functions to import lap times, pit stops, qualifying results, race results, and world championship standings. The package also includes functions to create a full local copy of Ergast's database in 'SQLite' using the 'RSQLite' package. 

I have had a ton of fun playing with these datasets, so I wanted to make it easy for novice and advanced R users alike to get started analyzing Formula 1. I hope you will have as much fun with this data as I have had!

The package can be installed in R as follows:

    install.packages("devtools")
    library(devtools)
    install_github("arkraieski/formula1data")


This package is unofficial and is not associated in any way with the Formula 1 companies. F1, FORMULA ONE, FORMULA 1, FIA FORMULA ONE WORLD CHAMPIONSHIP, GRAND PRIX and related marks are trade marks of Formula One Licensing B.V. Any use of these or related marks in this package and its documentation is intended to inform users of the software's purpose rather than to brand it or imply an association with Formula 1. I am just a passionate fan!
