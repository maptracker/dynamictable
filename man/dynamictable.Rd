% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dynamictable.R
\name{dynamictable}
\alias{dynamictable}
\title{Dynamic Table}
\usage{
dynamictable(data, cols = NA, options = list(), params = list(),
  file = NA, header = "", title = NA, footer = "", sortby = "",
  show.rownames = TRUE, facet.title = TRUE, show.attr = TRUE,
  auto.url = TRUE, auto.title = TRUE, auto.factor = TRUE,
  min.level = NA, factor.colors = "hash", style = character(),
  favicon = NA,
  numeric.style = "text-align: right; font-family: monospace;",
  display.url = TRUE)
}
\arguments{
\item{data}{The only required argument, an R object to be
displayed, typically a data.frame, matrix or array. Vectors
will be shown as a table with a single column.}

\item{cols}{The columns to display. If \code{NA} (Default) then all
columns in \code{data} will be shown (with some exceptions -
see \code{auto.url} and \code{auto.title}). Alternatively a
character vector of column names can be provided, to show only
a subset of columns, or to change their order.}

\item{options}{List of configuration options, keyed by the column
names of \code{x}, sub-keyed by parameter name. See
\code{Options} below for more details}

\item{params}{A fallback to \code{options}, useful for providing
default values. It is keyed first by parameter name, then
column. If a parameter is defined in both \code{options} and
\code{params}, the \code{options} value will be used}

\item{file}{File path for output. If not provided, a temporary path
will be used}

\item{header}{HTML to display at top of document. If it begins with
'<' it will be presumed to contain HTML markup and will be used
as-is, otherwise it will be escaped}

\item{title}{Text to define the HTML <title> block (displayed on
the browser's window frame). If \code{NA} will default to
\code{header}}

\item{footer}{Text to add at the end of the document, after the
table. Like \code{header}, will be escaped unless the first
character is '<'}

\item{sortby}{Column for initial sort of displayed data}

\item{show.rownames}{When \code{TRUE} (default) the first column
will be rownames, if set}

\item{facet.title}{If TRUE (default) then any column titles set
with the 'coltitle' option will also be shown as descriptive
text above the faceting buttons.}

\item{show.attr}{When \code{TRUE} (default) then some object
attributes will be shown at the end of the table. Attributes
will be listed only if the value is atomic, and if the key is
not 'reserved' (names, dims, etc)}

\item{auto.url}{If \code{TRUE} (default), will look for data/URL
column pairs, such that a column 'foo URL' (or 'foo link') will
be hidden and instead used as a hyperlink for column 'foo',
provided that 'foo' exists}

\item{auto.title}{Like \code{auto.url}, but if \code{TRUE}
(default) will supress display of 'foo title' and use it for
mouse-over title text on column 'foo' instead. Also matches
'foo description'}

\item{auto.factor}{If \code{TRUE} (default), then any
\code{is.factor()} columns will have faceting widgets set for
them. These are buttons for each factor level that toggle row
visibility on and off}

\item{min.level}{Default \code{NA}, a minimum factor level
count. Can be set here for all factors, or applied to
individual columns, see the 'factor' section for more
information.}

\item{factor.colors}{Default 'hash', controls auto-generated colors
assigned to factor levels. The value can also be set for each
column, see the 'factor' section for more information.}

\item{style}{Character vector of CSS styles to add to document}

\item{favicon}{If \code{NA} (default) a default favicon will be
assigned to the document. If \code{NULL}, no favicon will be
set. Otherwise should be an appropriate URL or an HTML data
block.}

\item{numeric.style}{Default 'text-align: right; font-family:
monospace;'. CSS styling of table columns is awkward, it is
easiest to have this as an explicit parameter with explicit CSS
code, rather than trying to set a particular class.}

\item{display.url}{If \code{TRUE} (default), \code{browseURL()}
will be called to display the newly-created output}
}
\value{
The file path of the generated HTML document
}
\description{
Light-weight, non-paginated HTML table with basic sorting,
filtering and automated markup functionality
}
\details{
dynamictable was built with the following design goals:

\itemize{

  \item Ease of use - A table can be built by passing a single
        object, with reasonable defaults already set

  \item Configurable - Default settings can be changed with nested
        list options

  \item Single output file - The generated file is entirely self-contained

  \item Lightweight HTML - Most dynamic operations are CSS-driven,
        with a minimum amount of simple JavaScript

  \item Compact HTML - The resultant table is designed to pack a
        high amount of information into a given space

  \item No pagination - While this will be seen as a limitation by
        some users, it is an explicit design choice

  \item Basic functionality - Sortable columns, per-column filters
        (factor faceting, numeric and text filters), collapsable
        text, automatic and custom gradient generation, hyperlinks,
        mouse-over text

}
}
\section{Column Names}{


Column names have loose matching - an attempt will first be made
with an exact, case-sensitive search; If that fails a
case-insensitive attempt will be made.

In addition, the special token '*' will match ALL columns

Finally, if row names have been added, that column can be
configured with the 'dtRowNames' column name.
}

\section{Options}{


The \code{options} parameter should be a list, with column names
corresponding to the column names of \code{data}. You need only
include those columns you wish to configure. Each column should
itself be a list of specific parameters to set. Most of those
parameters will have simple values, but a few can optionally be
provided as lists themselves, so for example:

  dynamictable(mtcars, options=list(cyl=list(factor=TRUE),
     hp=list(gradient=list( colors=c("yellow","purple")))))

The call is requesting that "cyl" be treated as a factor, an option
that could be configured with a sub-list, but passing \code{TRUE}
will cause default values to be applied. The use of a sub-list to
define the 'gradient' option for column "hp" allows finer-grained
configuration.
}

\section{Recognized Options}{


\itemize{

\item{"name"} - Provide an alternative column name

\item{"url"} - The name of another column that should serve as a
    source of URLs for this one.

    \itemize{

    \item Alternatively, the value can be a \code{sprintf} format
    string, which will be recognized by the presence of "\%s",
    which will accept the cell content as a value

    }

\item{"title"} - The name of another column that should serve as a
    source of titles (text that appears on mouse hover over the
    CELL) for this one.

\item{"coltitle"} - Text that will appear when holding the mouse
    over the column HEADER

\item{"hide"} - If true, then the column will not be shown

\item{"byFactor"} - Normally factor columns have styling applied to
    themselves. The byFactor option allows a column to be styled by
    the levels from another column

\item{"class"} - Allows custom classing of cells.

    \itemize{

    \item If a column name is provided, then the values of that
    column will be used as class names. The high-level function
    parameter "style" should include relevant CSS definitions for
    this to have a visible effect. Class names will be transformed
    to meet W3C specifications. This will strip out 'illegal'
    characters, and will prefix 'DT-' to any values that do not
    start with '-?[_a-zA-Z]'. So a column with a value of '4.3@C'
    will have a class of 'DT-4.3C'

    \item If an R function is provided, that function will be
    applied to each cell content and the returned value used as a
    class name

    \item Otherwise, if a static value is provided, that value will
    be applied to all cells in the column.

    }

\item{"truncate"} - An integer value constraining column width. A
    cell will show at most this number of characters. If the value
    is exceeded, a clickable ellipses will be shown for the missing
    text; Clicking on the ellipses will toggle visibility. This
    option is ignored for numeric columns.

\item{"spacemap"} - A list of characters that should be shown as
    spaces. Useful for long identifiers that use underscores
    preventing text wrapping (eg MSigDB).

\item{"sprintf"} - If non-null will be used as a sprintf string to
    format values in the cells. Original values will still be used
    for sorting. Can be used on any column, but user is responsible
    for using relevant formatting tokens.

\item{"signif"} - If an integer between 1-22, will restrict the
    column to that number of significant digits. Original values
    will still be used for sorting. Does not affect non-numeric
    columns, and will be ignored if sprintf is set.

\item{"fold"} - If non-null will merely affix the &times; character entity
    (a slightly fancier "x") to the end of the column. Will not
    alter underlying values

\item{"percent"} - If non-null will merely affix a "\%" character to the
    end of the column. Will not alter underlying values

\item{"gradient"} - Indicates that column cells should have a background
    color applied according to their value. The values will be
    broken into bins and assigned a CSS class for each bin.

    \itemize{

    \item "min" - Defines the smallest bin value. Defaults to
    \code{min()}

    \item "max" - Defines the largest bin value. Defaults to
    \code{max()}

    \item "bins" - Specifies the number of bins to use

    \item "step" - If "bins" was not specified, then step can
    specify the bin size. If neither are specified, then 15 bins
    will be used.

    \item "binVals" - An explicit list of bin values, will override
    "min", "max", "bins" and "step". Useful for hand-specifying
    non-linear (eg logarithmic/exponential) gradient values.

    \item "colors" - The colors to use for generating the gradient,
    default are \code{c("#5555ff", "white", "red")}. These will be
    passed to \code{colorRampPalette()}

    \item "lowstyle" - a CSS style (not class name) to be used for
    values that fall below the minimum bin. If not provided, then
    the lowest color gradient will be used.

    \item "highstyle" - as above, but for values that exceed the maximum.

    }

\item{"factor"} - When non-null, will treat the column as a factor
    even if not formally in R. If the value is not a list, default
    factor options will be used. If a list, the following keys are
    recognized as configuration options:

    \itemize{

    \item "name" - defines the text that will be shown above the
    facetting buttons. If not defined, the "name" as defined for
    the column will be used

    \item "active" - a character vector listing levels that should be
    pre-selected when the page first loads If not set then all
    levels will be off, which is equivalent to all levels being on.

    \item "min.level" - An optional minimum level count. Levels
    with representation below this level will be grouped together
    in an "<Other>" category. This is useful for handling columns
    that are mostly controlled vocabullary, but include a few
    "noisy" levels, eg "Yes (except when it is raining)"

    \item "textFilter" - if \code{TRUE}, will include a free-text
    filter box in addition to facetting buttons.

    \item "color" - A flag to control how factor levels are colored

        \itemize{

        \item \code{NULL} - will default to the high-level option
        \code{factor.colors}, which is default "hash"

        \item \code{NA} - no coloring at all

        \item "rainbow", "heat.colors", "terrain.colors",
        "topo.colors", "cm.colors" - default R palette functions

        \item "hash" - will use the internal
        \code{colorizeString()} function to generate an RGB color
        from a hashed string of each factor level (name). This
        color is "portable" across different data sets, in that the
        same factor level (name) will always generate the same
        color. Nuances of hash coloring can be controlled by the
        "min", "max", "r", "g" and "b" factor sub-options

            \itemize{

                \item "min" - Sets the minimum intentisty value for
                hash colors (default 128 in \code{colorizeString})

                \item "max" - Sets the maximum intensity for hash
                colors (default 240) in \code{colorizeString}

                 \item "r" - Sets a fixed intensity for the hashed
                 red channel

                 \item "g" - Sets a fixed intensity for the hashed
                 green channel

                 \item "b" - Sets a fixed intensity for the hashed
                 blue channel
             }

        }

    }

}
}
\examples{

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

}
\author{
Charles A Tilford, \email{cran@agent.fastmail.com}
}
\seealso{
Library "DT", which implements jQuery DataTables. These are
paginated and are likley more appropriate for visualizing larger
(10,000+ rows) tables: <URL: http://rstudio.github.io/DT>
}
\keyword{dhtml,}
\keyword{html,}
\keyword{javascript,}
\keyword{table}

