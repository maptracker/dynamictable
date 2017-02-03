## dynamictable - Generate interactive HTML tables from R objects

This package will create an interactive HTML table from R matrices and
data.frames. Tables can be sorted and filtered by interacting with
column headers. The result is generated as a single HTML file with no
external dependencies, to aid in file management and reproducible
research. Tables are intentionally non-paginated; For paginated HTML
tables, consider the `DT` package.

The package is designed for simple and rapid use
(`dynamictable(myObject)`), but also supports extensive customization
options. See `?dynamictable`, or run the "hyperCustomizedTable" demo
to see most of the options applied.

### Installation instructions

```R
## Install from CRAN
## ... 3-Feb-2017 ... submitting today, will update when available

## //OR// Directly from github.com
## If you do not have devtools, run: install.packages("devtools")
library("devtools")
install_github("maptracker/dynamictable")
```

### Examples

```R
## Quick table generation

dynamictable(mtcars)

## Using the "*" wildcard to apply a setting (here a gradient) to all columns

dynamictable(state.x77, options = list("*"=list(gradient=TRUE)))

## A highly customized view of mtcars to illustrate most options:

demo("hyperCustomizedTable", package="dynamictable", ask=FALSE)

## Stress-test with 1000 rows. The performance is not stellar - tens
## of thoushands of rows are passable in Chrome, but awkward in Firefox.
## For larger datasets a paginated browser (eg DT) is likely more
## appropriate

demo("largeDataSets", package="dynamictable", ask=FALSE)

## Some simple factorized data sets:

dynamictable(texteditorplatforms)
dynamictable(typesystemcomparison)

## Factor-rich table, with min.level set to consolidate uncommon levels:

dynamictable(vampiretraits, min.level=2)
```
