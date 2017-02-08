# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
# Roxygen2 lists: https://stackoverflow.com/a/9278911

#' Dynamic Table
#'
#' Light-weight, non-paginated HTML table with basic sorting,
#' filtering and automated markup functionality
#'
#' @details
#'
#' dynamictable was built with the following design goals:
#' 
#' \itemize{
#' 
#'   \item Ease of use - A table can be built by passing a single
#'         object, with reasonable defaults already set
#' 
#'   \item Configurable - Default settings can be changed with nested
#'         list options
#' 
#'   \item Single output file - The generated file is entirely self-contained
#' 
#'   \item Lightweight HTML - Most dynamic operations are CSS-driven,
#'         with a minimum amount of simple JavaScript
#' 
#'   \item Compact HTML - The resultant table is designed to pack a
#'         high amount of information into a given space
#' 
#'   \item No pagination - While this will be seen as a limitation by
#'         some users, it is an explicit design choice
#' 
#'   \item Basic functionality - Sortable columns, per-column filters
#'         (factor faceting, numeric and text filters), collapsable
#'         text, automatic and custom gradient generation, hyperlinks,
#'         mouse-over text
#' 
#' }
#' 
#' @param data The only required argument, an R object to be
#'     displayed, typically a data.frame, matrix or array. Vectors
#'     will be shown as a table with a single column.
#' @param cols The columns to display. If \code{NA} (Default) then all
#'     columns in \code{data} will be shown (with some exceptions -
#'     see \code{auto.url} and \code{auto.title}). Alternatively a
#'     character vector of column names can be provided, to show only
#'     a subset of columns, or to change their order.
#' @param options List of configuration options, keyed by the column
#'     names of \code{x}, sub-keyed by parameter name. See
#'     \code{Options} below for more details
#' @param params A fallback to \code{options}, useful for providing
#'     default values. It is keyed first by parameter name, then
#'     column. If a parameter is defined in both \code{options} and
#'     \code{params}, the \code{options} value will be used
#' @param file File path for output. If not provided, a temporary path
#'     will be used
#' @param header HTML to display at top of document. If it begins with
#'     '<' it will be presumed to contain HTML markup and will be used
#'     as-is, otherwise it will be escaped
#' @param title Text to define the HTML <title> block (displayed on
#'     the browser's window frame). If \code{NA} will default to
#'     \code{header}
#' @param footer Text to add at the end of the document, after the
#'     table. Like \code{header}, will be escaped unless the first
#'     character is '<'
#' @param sortby Column for initial sort of displayed data
#' @param show.rownames When \code{TRUE} (default) the first column
#'     will be rownames, if set
#' @param show.attr When \code{TRUE} (default) then some object
#'     attributes will be shown at the end of the table. Attributes
#'     will be listed only if the value is atomic, and if the key is
#'     not 'reserved' (names, dims, etc)
#' @param auto.url If \code{TRUE} (default), will look for data/URL
#'     column pairs, such that a column 'foo URL' (or 'foo link') will
#'     be hidden and instead used as a hyperlink for column 'foo',
#'     provided that 'foo' exists
#' @param auto.title Like \code{auto.url}, but if \code{TRUE}
#'     (default) will supress display of 'foo title' and use it for
#'     mouse-over title text on column 'foo' instead. Also matches
#'     'foo description'
#' @param auto.factor If \code{TRUE} (default), then any
#'     \code{is.factor()} columns will have faceting widgets set for
#'     them. These are buttons for each factor level that toggle row
#'     visibility on and off
#' @param facet.title If TRUE (default) then any column titles set
#'     with the 'coltitle' option will also be shown as descriptive
#'     text above the faceting buttons.
#' @param factor.colors Default 'hash', controls auto-generated colors
#'     assigned to factor levels. The value can also be set for each
#'     column, see the 'factor' section for more information.
#' @param min.level Default \code{NA}, a minimum factor level
#'     count. Can be set here for all factors, or applied to
#'     individual columns, see the 'factor' section for more
#'     information.
#' @param display.url If \code{TRUE} (default), \code{browseURL()}
#'     will be called to display the newly-created output
#' @param style Character vector of CSS styles to add to document
#' @param favicon If \code{NA} (default) a default favicon will be
#'     assigned to the document. If \code{NULL}, no favicon will be
#'     set. Otherwise should be an appropriate URL or an HTML data
#'     block.
#' @param numeric.style Default 'text-align: right; font-family:
#'     monospace;'. CSS styling of table columns is awkward, it is
#'     easiest to have this as an explicit parameter with explicit CSS
#'     code, rather than trying to set a particular class.
#'
#' @section Column Names:
#'
#' Column names have loose matching - an attempt will first be made
#' with an exact, case-sensitive search; If that fails a
#' case-insensitive attempt will be made.
#'
#' In addition, the special token '*' will match ALL columns
#'
#' Finally, if row names have been added, that column can be
#' configured with the 'dtRowNames' column name.
#'
#' @section Options:
#' 
#' The \code{options} parameter should be a list, with column names
#' corresponding to the column names of \code{data}. You need only
#' include those columns you wish to configure. Each column should
#' itself be a list of specific parameters to set. Most of those
#' parameters will have simple values, but a few can optionally be
#' provided as lists themselves, so for example:
#'
#'   dynamictable(mtcars, options=list(cyl=list(factor=TRUE),
#'      hp=list(gradient=list( colors=c("yellow","purple")))))
#'
#' The call is requesting that "cyl" be treated as a factor, an option
#' that could be configured with a sub-list, but passing \code{TRUE}
#' will cause default values to be applied. The use of a sub-list to
#' define the 'gradient' option for column "hp" allows finer-grained
#' configuration.
#'
#' @section Recognized Options:
#'
#' \itemize{
#' 
#' \item{"name"} - Provide an alternative column name
#'
#' \item{"url"} - The name of another column that should serve as a
#'     source of URLs for this one.
#'
#'     \itemize{
#' 
#'     \item Alternatively, the value can be a \code{sprintf} format
#'     string, which will be recognized by the presence of "\%s",
#'     which will accept the cell content as a value
#' 
#'     }
#' 
#' \item{"title"} - The name of another column that should serve as a
#'     source of titles (text that appears on mouse hover over the
#'     CELL) for this one.
#' 
#' \item{"coltitle"} - Text that will appear when holding the mouse
#'     over the column HEADER
#' 
#' \item{"hide"} - If true, then the column will not be shown
#' 
#' \item{"byFactor"} - Normally factor columns have styling applied to
#'     themselves. The byFactor option allows a column to be styled by
#'     the levels from another column
#' 
#' \item{"class"} - Allows custom classing of cells.
#'
#'     \itemize{
#' 
#'     \item If a column name is provided, then the values of that
#'     column will be used as class names. The high-level function
#'     parameter "style" should include relevant CSS definitions for
#'     this to have a visible effect. Class names will be transformed
#'     to meet W3C specifications. This will strip out 'illegal'
#'     characters, and will prefix 'DT-' to any values that do not
#'     start with '-?[_a-zA-Z]'. So a column with a value of '4.3@C'
#'     will have a class of 'DT-4.3C'
#'
#'     \item If an R function is provided, that function will be
#'     applied to each cell content and the returned value used as a
#'     class name
#'
#'     \item Otherwise, if a static value is provided, that value will
#'     be applied to all cells in the column.
#'
#'     }
#' 
#' \item{"truncate"} - An integer value constraining column width. A
#'     cell will show at most this number of characters. If the value
#'     is exceeded, a clickable ellipses will be shown for the missing
#'     text; Clicking on the ellipses will toggle visibility.
#' 
#' \item{"spacemap"} - A list of characters that should be shown as
#'     spaces. Useful for long identifiers that use underscores
#'     preventing text wrapping (eg MSigDB).
#' 
#' \item{"percent"} - If non-null will merely affix a "\%" character to the
#'     end of the column. Will not alter underlying values
#' 
#' \item{"fold"} - If non-null will merely affix the &times; character entity
#'     (a slightly fancier "x") to the end of the column. Will not
#'     alter underlying values
#' 
#' \item{"gradient"} - Indicates that column cells should have a background
#'     color applied according to their value. The values will be
#'     broken into bins and assigned a CSS class for each bin.
#'
#'     \itemize{
#' 
#'     \item "min" - Defines the smallest bin value. Defaults to
#'     \code{min()}
#' 
#'     \item "max" - Defines the largest bin value. Defaults to
#'     \code{max()}
#'
#'     \item "bins" - Specifies the number of bins to use
#'
#'     \item "step" - If "bins" was not specified, then step can
#'     specify the bin size. If neither are specified, then 15 bins
#'     will be used.
#'
#'     \item "binVals" - An explicit list of bin values, will override
#'     "min", "max", "bins" and "step". Used for specifying non-linear
#'     gradient values.
#'
#'     \item "colors" - The colors to use for generating the gradient,
#'     default are \code{c("#5555ff", "white", "red")}. These will be
#'     passed to \code{colorRampPalette()}
#'
#'     \item "lowstyle" - a CSS style (not class name) to be used for
#'     values that fall below the minimum bin. If not provided, then
#'     the lowest color gradient will be used.
#'
#'     \item "highstyle" - as above, but for values that exceed the maximum.
#' 
#'     }
#' 
#' \item{"factor"} - When non-null, will treat the column as a factor
#'     even if not formally in R. If the value is not a list, default
#'     factor options will be used. If a list, the following keys are
#'     recognized as configuration options:
#'
#'     \itemize{
#' 
#'     \item "name" - defines the text that will be shown above the
#'     facetting buttons. If not defined, the "name" as defined for
#'     the column will be used
#'
#'     \item "active" - a character vector listing levels that should be
#'     pre-selected when the page first loads If not set then all
#'     levels will be off, which is equivalent to all levels being on.
#'
#'     \item "min.level" - An optional minimum level count. Levels
#'     with representation below this level will be grouped together
#'     in an "<Other>" category. This is useful for handling columns
#'     that are mostly controlled vocabullary, but include a few
#'     "noisy" levels, eg "Yes (except when it is raining)"
#'
#'     \item "textFilter" - if \code{TRUE}, will include a free-text
#'     filter box in addition to facetting buttons.
#'
#'     \item "color" - A flag to control how factor levels are colored
#'
#'         \itemize{
#' 
#'         \item \code{NULL} - will default to the high-level option
#'         \code{factor.colors}, which is default "hash"
#' 
#'         \item \code{NA} - no coloring at all
#' 
#'         \item "rainbow", "heat.colors", "terrain.colors",
#'         "topo.colors", "cm.colors" - default R palette functions
#' 
#'         \item "hash" - will use the internal
#'         \code{colorizeString()} function to generate an RGB color
#'         from a hashed string of each factor level (name). This
#'         color is "portable" across different data sets, in that the
#'         same factor level (name) will always generate the same
#'         color. Nuances of hash coloring can be controlled by the
#'         "min", "max", "r", "g" and "b" factor sub-options
#'
#'             \itemize{
#' 
#'                 \item "min" - Sets the minimum intentisty value for
#'                 hash colors (default 128 in \code{colorizeString})
#'
#'                 \item "max" - Sets the maximum intensity for hash
#'                 colors (default 240) in \code{colorizeString}
#'
#'                  \item "r" - Sets a fixed intensity for the hashed
#'                  red channel
#' 
#'                  \item "g" - Sets a fixed intensity for the hashed
#'                  green channel
#' 
#'                  \item "b" - Sets a fixed intensity for the hashed
#'                  blue channel
#'              }
#'
#'         }
#'
#'     }
#'
#' }
#'
#' @seealso
#'
#' Library "DT", which implements jQuery DataTables. These are
#' paginated and are likley more appropriate for visualizing larger
#' (10,000+ rows) tables: <URL: http://rstudio.github.io/DT>
#' 
#' @examples
#'
#' ## Quick table generation
#' 
#' dynamictable(mtcars)
#'
#' ## Using the "*" wildcard to apply a setting (here a gradient) to all columns
#' 
#' dynamictable(state.x77, options = list("*"=list(gradient=TRUE)))
#'
#' ## A highly customized view of mtcars to illustrate most options:
#' 
#' demo("hyperCustomizedTable", package="dynamictable", ask=FALSE)
#'
#' ## Stress-test with 1000 rows. The performance is not stellar - tens
#' ## of thoushands of rows are passable in Chrome, but awkward in Firefox.
#' ## For larger datasets a paginated browser (eg DT) is likely more
#' ## appropriate
#' 
#' demo("largeDataSets", package="dynamictable", ask=FALSE)
#'
#' ## Some simple factorized data sets:
#'
#' dynamictable(texteditorplatforms)
#' dynamictable(typesystemcomparison)
#' 
#' ## Factor-rich table, with min.level set to consolidate uncommon levels:
#' 
#' dynamictable(vampiretraits, min.level=2)
#'
#' @return The file path of the generated HTML document
#'
#' @author Charles A Tilford, \email{cran@agent.fastmail.com}
#'
#' @keywords html, dhtml, javascript, table
#'
#' @importFrom htmltools htmlEscape
#'
#' @export

dynamictable <- function (data,
    cols=NA, options=list(), params=list(),
    file=NA, header="", title=NA, footer="", sortby="", 
    show.rownames=TRUE, facet.title=TRUE, show.attr=TRUE,
    auto.url=TRUE, auto.title=TRUE, auto.factor=TRUE, min.level = NA,
    factor.colors='hash', style=character(), favicon=NA,
    numeric.style='text-align: right; font-family: monospace;',
    display.url=TRUE) {

    ## All we are doing here is wrapping up object creation and
    ## rendering in a single call. 
    DT <- DynamicTable(data, cols=cols, options=options, params=params,
             file=file, header=header, title=title, footer=footer,
             sortby=sortby, show.rownames=show.rownames, show.attr=show.attr,
             facet.title=facet.title, auto.url=auto.url,auto.title=auto.title,
             auto.factor=auto.factor, factor.colors=factor.colors,
             min.level=min.level,
             style=style, favicon=favicon, numeric.style=numeric.style)
    DT$prepareOutput()
    DT$generateOutput( display.url )
}

## Text value for NA, used for factor buttons
dtNaString    <- "[NA]"
dtOtherString <- "{Other}"

#' @title Dynamic Table Object
#'
#' @description
#'
#' This is the underlying ReferenceClass object supporting
#' \code{dynamictable}. While it could be used directly, it is
#' presumed that in most cases users will be calling the function
#' \code{dynamictable()}
#'
#' See \code{?dynamic} for a full parameter listing
#'
#' @field data The primary data, held as a data.frame (either as
#'     provided, or converted to one)
#' @field cols Character vector of columns to display
#' @field colInfo List holding supporting structures for building
#'     columns in the HTML table
#' @field autoCols Logical value, TRUE if columns were simple
#'     extracted from the supplied object, FALSE if user has specified
#'     particular columns to use.
#' @field userStyle Character vector holding any user-supplied CSS
#'     styles
#' @field style Character vector, programatically-generated styles
#' @field file File path for output
#' @field options A list holding both user-supplied and generated
#'     'primary' settings. See \code{?dynamic} for full details on
#'     options
#' @field params A list, similar to \code{options} - See
#'     \code{?dynamic} for full details
#' @field flags A list of assorted backend configuration flags
#' @field title The title (upper left browser window frame)
#' @field header Optional text to put above the table
#' @field footer Optional text for below the table
#' @field sortby Optional column for an initial sort
#'
#' @importFrom stats setNames
#' @importFrom methods new
#' 
#' @export

DynamicTable <-
    setRefClass("DynamicTable",
                fields = list(
                    data        = "data.frame", # The data to display
                    cols        = "character",  # The columns to show
                    colInfo     = "list",       # Column data structures
                    autoCols    = "logical",    # If cols was set automatically
                    userStyle   = "character",  # User-supplied CSS styles
                    style       = "character",  # Auto-generated styles
                    file        = "character",  # File path for output
                    options     = "list",       # Main options hash
                    params      = "list",       # Fallback options hash
                    flags       = "list",       # Hodge-podge of settings
                    title       = "character",  # Window frame title
                    sortby      = "character",  # Column to sort by
                    header      = "character",  # HTML at top of page
                    footer      = "character"   # HTML at bottom of page
                    )
                )

DynamicTable$methods(
    initialize =
        function(data, cols=NA, options=list(), params=list(),
                 file=NA, header="", title=NA, footer="", sortby="",
                 min.level=NA,
                 show.rownames=TRUE, facet.title=TRUE, show.attr=TRUE,
                 auto.url=TRUE, auto.title=TRUE, auto.factor=TRUE,
                 factor.colors='hash', style=character(), favicon=NA,
                 numeric.style='text-align: right; font-family: monospace;' ) {
            ## Do initial standardization of input
            options   <<- if (is.def(options)) { options } else { list() }
            params    <<- params
            userStyle <<- style
            autoCols  <<- FALSE
            colInfo   <<- list()
            sortby    <<- sortby
            flags     <<- list(
                show.rownames = show.rownames,
                auto.url      = auto.url,
                auto.title    = auto.title,
                auto.factor   = auto.factor,
                show.attr     = show.attr,
                factor.colors = factor.colors,
                favicon       = favicon,
                facet.title   = facet.title,
                min.level     = min.level,
                numeric.style = numeric.style
            )
            data    <<- .normalizeData(data, show.rownames)
            cols    <<- if (all(is.na(cols))) {
                            ## No columns were provided
                            autoCols <<- TRUE
                            colnames(.self$data)
                        } else {
                            cols[ !is.na(cols) ]
                        }
            setHeader(header)
            setFooter(footer)
            setTitle(title)
            .normalizeOptions()
            .processDerivatives()
            file    <<- if (is.na(file)) {
                            tempfile("DynamicTable-", fileext=".html" )
                        } else { file }
            callSuper()
        },
    
    .normalizeData = function(x, show.rownames=TRUE) {
        ## Try to turn the input into a data.frame
        atts  <- attributes(x)
        if (is.array(x)) {
            x <- as.data.frame(x)
            attr(x, "originalAttributes") <- atts
        } else if (!is.data.frame(x)) {
            ## If this is not a table-looking thing, make it so:
            x <- data.frame( Input = x )
            attr(x, "originalAttributes") <- atts
        }
        
        if (show.rownames) {
            rn <- rownames(x)
            if (length(rn) > 0) {
                ## There are a bunch of datasets with character rownames
                ## that are actually just integers ... Detect and change:
                asInts <- suppressWarnings(as.integer(rn))
                if (!any(is.na(asInts))) {
                    ## The row names are actually integers, set them
                    ## as such so they sort properly:
                    rn <- asInts
                }
                ## Insert the names as column 1:
                rnn  <- "dtRowNames"
                rndf <- data.frame(dtRowNames = rn, stringsAsFactors=FALSE)
                x <- cbind( rndf, x)
                attr(x, "originalAttributes") <- atts
                setDefaultOpt(rnn, "name", "") # No title in <tr>
                setDefaultOpt(rnn, "class", "header")
            }
        }
        if (is.def(sortby) && sortby != "") {
            ind <- matchCol(sortby, colnames(x))
            if (is.na(ind)) {
                warning("Could not sort by unknown column: ", sortby)
            } else {
                x <- x[ order(x[[ind]]), ]
                attr(x, "originalAttributes") <- atts
           }
        }
        x
    },

    .normalizeOptions = function() {
        ## Preliminary normalization of displayed columns
        ## Make sure all columns have at least a blank options set:
        for (col in union(colnames(data), cols)) {
            if (is.null(options[[ col ]])) options[[ col ]] <<- list()
            ## The source column from input:
            setOpt(col, 'src', matchCol(col))
            ## $name allows a different column name to be shown in the
            ## table header. If not set, use the column as-is
            setDefaultOpt(col, "name", col)
        }
    },

    .processDerivatives = function () {
        ## Scan of *all* columns, in case we can find derivative data
        ## in non-displayed ones. That is, column "Foo URL" may not
        ## have been chosen for display, but it should be identified
        ## as a source of URLs to build hyperlinks in column "Foo".
        doAutoFactor <- flags$auto.factor
        autoMatch    <- list(url   = "(.+) (?:url|link)",
                             title = "(.+) (?:title|description)")
        derivCols    <- names(autoMatch)
        for (col in colnames(data)) {
            i <- matchCol(col)
            if (!is.null(getOpt(col, 'factor')) ||
                (doAutoFactor && is.factor(data[[i]]))) {
                ## An explicit request to treat the column as a
                ## factor, or auto.factor is on and column is classed
                ## as one.
                .factorizeColumn(col)
            }
            ## Check to see if this is a derivative column of some sort
            for (deriv in derivCols) {
                ## Check the "auto.---" flag to see if auto
                ## derivitives were requested for this type
                if (!flags[[ paste("auto", deriv, sep='.') ]]) next
                isDeriv <- parenRE(autoMatch[[ deriv ]], col)
                if (is.na(isDeriv[1])) next # Not a derivative column
                ## This appears to be a derivative column for another...
                trgCol <- isDeriv[1]
                trgInd <- matchCol(trgCol, cols)
                ## Skip if the target column is not shown:
                if (is.na(trgInd)) next
                ## Ok, set derivative information on the target, unless it
                ## is already set:
                setDefaultOpt(col, deriv, col)
                setDefaultOpt(trgCol, deriv, col)
                ## If explicit columns were not provided, and if the hide
                ## state is not explicitly set, we should hide this col
                if (autoCols) setDefaultOpt(col, 'hide', TRUE)
            }
        }

    },

    setHeader = function (txt=NULL, asHTML=FALSE) {
        if (!is.null(txt)) header <<- txt
        if (asHTML) {
            .handleUserHTML(header)
        } else {
            header
        }
    },

    setFooter = function (txt=NULL, asHTML=FALSE) {
        if (!is.null(txt)) footer <<- txt
        if (asHTML) {
            .handleUserHTML(footer)
        } else {
            footer
        }
    },

    .handleUserHTML = function( txt, simpleTag='h2' ) {
        ## Try to do something smart with user-supplied HTML content
        if (is.na(txt)) {
            ""
        } else {
            if (grepl('^<', txt[1])) {
                ## If it appears the user is providing tags, use the
                ## content unescaped
                paste(txt, collapse="\n")
            } else {
                html <- paste(.hesc(txt), "\n")
                if (is.na(simpleTag)) {
                    ## Just use as raw text
                    html
                } else {
                    ## wrap in a tag
                    sprintf("<%s>%s</%s>", simpleTag, html, simpleTag)
                }
            }
        }
    },

    setTitle = function (txt=NULL, asHTML=FALSE) {
        ## Text that will be put in <title> tag of <head>
        if (is.def(txt)) {
            title <<- txt[1]
        } else if (length(txt) != 0 && is.na(txt)) {
            ## Use the header, if set
            head <- setHeader()[1]
            if (is.def(head) && head != "") {
                ## Set title to header, without HTML tags
                title <<- gsub("<[^>]+>",'', head[1])
            } else {
                title <<- "Dynamic Table"
            }
        }
        if (asHTML) {
            .aesc(title)
        } else {
            title
        }
    },

    ## Functions that don't do anything or return the input
    ## unaltered. Used in the hope (untested) that they will be faster
    ## than a logical test
    .nullFunc = function(ri, ci) NULL,       # For classes (no class)
    .selfFunc = function(x, ri=NA, ci=NA) x, # For link generation (no link)

    prepareOutput = function () {

        ## These are pre-computed column-specific data structures and
        ## callbacks that will be used while generating the HTML cells
        ## of the table. The list is keyed to output column
        ## index. That is, colInfo[[ 1 ]] holds information for the
        ## first OUTPUT column.

        colInfo <<- list(
            srcInds   = integer(),   # Source column index for output col
            srcName   = character(), # Source column name
            name      = character(), # Display name for column
            urlCB     = list(),      # Hyperlink building callback
            urlInds   = integer(),   # Source column for URL data
            urlFmt    = character(), # sprintf formats for basic URLs
            titInds   = integer(),   # Source for title text
            facCB     = list(),      # Factorization callbacks
            facInds   = integer(),   # Factor source columns
            facLU     = list(),      # Factor-to-CSS lookup vectors
            classCB   = list(),      # Custom CSS class callbacks
            fixCls    = character(), # Static CSS values for classCB
            refCls    = integer(),   # Get a CSS class from another column
            usrCls    = list(),      # User CSS class generating functions
            gradInfo  = list(),      # Gradient properties
            quart     = list(),      # Quartile boundaries for numeric cols
            truncLen  = integer(),   # Truncation length
            spaceMap  = list(),      # Chars that should be shown as spaces
            usedFact  = integer(),   # Output columns that were factorized
            filtHTML  = character(), # HTML factor filter widgets
            thRow     = character()  # <tr> Header row for HTML table
        )

        allStyles    <- character()
        ci           <- 0 # The output column index number
        colInfo$usedFac <<- integer() # Output columns that are factorized

        sortInd <- matchCol(sortby, cols)
        if (is.na(sortInd)) sortInd <- -1
        for (col in cols) {
            doHide <- getOpt(col, 'hide')
            if (!is.null(doHide) && doHide) next # Not showing this column
            ci <- ci + 1 ## Move on to the next output column
            colInfo$srcName[ci] <<- col
            srcInd <- colInfo$srcInds[ci] <<- getOpt(col, 'src') # input col
            colInfo$name[ci] <<- getOpt(col, 'name')
            ## If a title column is provided for the *cells*, note the
            ## column index:
            cTit  <- getOpt(col, 'title')
            cTind <- matchCol(cTit)
            colInfo$titInds[[ ci ]] <<- ifelse(is.null(cTind), NA, cTind)

            filtBox     <- ""
            hasTextFilt <- FALSE
            filtOn      <- FALSE
            ## Set factor class callbacks 
            colInfo$facCB[[ ci ]] <<- .self$.nullFunc
            facCol <- getOpt(col, 'byFactor')
            if (!is.null(facCol)) {
                facInd <- matchCol(facCol)
                if (!is.na(facInd)) {
                    ## Categorizing this column by an available factor
                    fOpts <- .factorizeColumn(facCol)
                    colInfo$usedFact <<- c(colInfo$usedFact, ci)
                    ## The input index to get the factor from:
                    colInfo$facInds[ ci ] <<- facInd
                    ## The function we will call to generate the class:
                    colInfo$facCB[[ ci ]] <<- .self$.factFunc
                    ## Lookup hash "input text" -> "CSS class name":
                    colInfo$facLU[[ ci ]] <<- fOpts$LU
                    if (filtBox == "") {
                        ## Build in-column filter widget for factor selection
                        if (any(fOpts$textFilter)) {
                            ## Also want a text filter
                            filtBox <- .textFilterWidget(ci)
                            hasTextFilt <- TRUE
                        }
                        filtBox <- paste(filtBox, .factorFilterWidget(ci,fOpts),
                                         sep='')
                        if (length(fOpts$activeInd) > 0) filtOn <- TRUE
                    }
                }
            }
            
            ## Hyperlinking callbacks:
            colInfo$urlCB[[ ci ]] <<- NA
            urlCol <- getOpt(col, 'url')
            if (!is.null(urlCol)) {
                urlInd <- matchCol(urlCol)
                if (!is.na(urlInd)) {
                    ## Take a hyperlink directly from another column
                    ## The input source columns:
                    colInfo$urlInds[ ci ] <<- urlInd
                    ## Callback for making the hyperlink:
                    colInfo$urlCB[[ ci ]] <<- .self$.linkFunc
                } else if (grepl('%s', urlCol)) {
                    ## This is a sprintf format, just stuff the cell value
                    ## into it
                    ## Store the sprintf format:
                    colInfo$urlFmt[ ci ] <<- urlCol
                    ## Callback for generating the hyperlink:
                    colInfo$urlCB[[ ci ]] <<- .self$.basicURL
                }
            }
            
            ## Table cell classing
            oCls <- getOpt(col, 'class')
            if (all(is.null(oCls))) {
                colInfo$classCB[[ ci ]] <<- .self$.nullFunc # No class
            } else if (is.function(oCls)) {
                ## User has provided their own callback
                ## Store the user's callback
                colInfo$usrCls[[ ci ]]  <<- oCls
                ## Wrapper function to manage user's callback:
                colInfo$classCB[[ ci ]] <<- .self$.classUser
            } else {
                clsInd <- matchCol(oCls)
                if (!is.na(clsInd)) {
                    ## This appears to be a column reference that is
                    ## supplying CSS class names
                    ## The source coulmn in the input data:
                    colInfo$refCls[ ci ]    <<- clsInd
                    ## Callback to wrap it up:
                    colInfo$classCB[[ ci ]] <<- .self$.classRef
                } else {
                    ## Static class, will be applied to every cell
                    colInfo$classCB[[ ci ]] <<- .self$.classFunc
                    colInfo$fixCls[ ci ] <<- oCls[1]
                }
            }
            
            ## Gradient classing
            gOpts <- .columnGradient(col)
            if (is.null(gOpts) || gOpts$ignore ) {
                ## No gradient on this column
                colInfo$gradInfo[[ ci ]] <<- NA
            } else {
                ## Note the information for callbacks:
                colInfo$gradInfo[[ ci ]] <<- gOpts
                ## Extend the stylesheet with any gradient-specific styles:
                allStyles <- c(allStyles, gOpts$styles)
            }
            

            ## Characters (eg '_') that should be shown as spaces
            sm <- getOpt(col, 'spacemap')
            colInfo$spaceMap[[ ci ]] <<- NA
            if (!is.null(sm)) {
                ## Make a regular expression of characters we want to turn
                ## into spaces.
                colInfo$spaceMap[[ ci ]] <<-
                    sprintf("(%s)", paste(sm, collapse='|'))
            }

            ## Flagged as a percentage value, add a percent sign. Will NOT
            ## multiply the displayed value by 100.
            ## https://stackoverflow.com/a/2741342
            if (!is.null(getOpt(col, 'percent'))) allStyles <- c(allStyles,
                sprintf("  td:nth-child(%d):after { content: '%%' }", ci ))
            ## Flagged as fold change, add a fancy "x" (&times;):
            ## Character entity escaping: https://stackoverflow.com/a/190412
            if (!is.null(getOpt(col, 'fold'))) allStyles <- c(allStyles,
                sprintf("  td:nth-child(%d):after { content: '\\D7' }", ci ))
            
            ## While we are cycling through output columns, let's
            ## build the header row here:
            th <- sprintf("<th onclick='doSort(event)' ind='%d' id='Col%d'",
                          ci,ci, .aesc(col))
            if (ci == sortInd) th <- c(th, " sorted='up'")
            cls <- character()
            colInfo$quart[[ ci ]] <<- NA
            if (is.numeric(data[[srcInd]])) {
                ## Note that the column is numeric to guide Javascript sorting:
                th <- paste(th, " isnum='1'")
                ## Add a column-level style rule
                ## Column styling: https://stackoverflow.com/a/33005330
                allStyles <- c(allStyles, sprintf("  td:nth-child(%d) { %s }",
                                          ci, flags$numeric.style))
                if (filtBox == "") {
                    ## Set the fitler box
                    ## Quantile levels
                    colInfo$quart[[ ci ]] <<- quantile(data[[ srcInd ]])
                    filtBox <- .numericFilterWidget(ci)
                    allStyles <- c(allStyles, sprintf(
                       "  tr[filt%d='yes'] { display: none ! important; }",ci))
                }
            }
            
            if (filtBox == "") {
                filtBox <- .textFilterWidget(ci)
                hasTextFilt <- TRUE
            }
            if (hasTextFilt) {
                allStyles <- c(allStyles, sprintf(
                       "  tr[filt%d='yes'] { display: none ! important; }",ci))
            }
            
            ## If a title is provided for the *column*, set it in the th row:
            hTit <- getOpt(col, 'coltitle')
            if (is.null(hTit)) hTit <- paste('Click to sort table by',col)
            th <- c(th, sprintf(" title='%s'", .aesc(hTit[1])))
            th <- c(th, .buildClass(cls),'>') # Close out <th> tag
            if (col == cols[1]) {
                ## This is the first column header, add a reset button
                ## and a "close all" button
                th <- c(th, "<img onclick='resetAll(event)' class='resetall' title='Reset all filters'>", "<img onclick='closeAll(event)' class='closeAll' title='Close all filter settings windows'>")
            }
            th <- c(th, .hesc(getOpt(col, 'name'))) # Column header
            if (!is.na(filtBox)) {
                ## Add filter pop-up widgets
                did <- sprintf("filtWidg%d", ci)
                th <- c(th, "<img class='fTog' onclick='fTog(event)' id='",did,
                        "' filtered='", if (filtOn) { 'yes' } else { 'no' },
                        "' title='Filter options'>",
                        "<div class='filtBox' draggable='true' title='Filter settings for column ",.aesc(col),"'>", filtBox,
                        "</div>")
            }
            
            ## Dynamic truncated text management:
            tLen <- getOpt(col, 'truncate')
            if (is.null(tLen) || is.na(tLen[1])) {
                ## No truncation requested
                colInfo$truncLen[[ ci ]] <<- NA
            } else {
                ## Validate setting as integer
                chk <- suppressWarnings(as.integer(tLen[1]))
                if (is.na(chk)) {
                    message("truncate option could not be converted into an integer")
                } else if (chk > 0) {
                    ## Valid integer, also non-zero. Do any cells
                    ## actually exceed the length?
                    numTrunc <- sum(nchar(data[[ srcInd ]]) > chk,
                                    na.rm = TRUE)
                    if (!is.na(numTrunc) && numTrunc > 0) {
                        ## at least one row has truncated text
                        th <- c(th, sprintf("<span ind='%d' onclick='truncCol(event)' class='tTog' title='Toggle %d truncated text rows in column'>&hellip;</span>", ci, numTrunc, if (numTrunc==1) { '' } else {'s'}))
                    } else {
                        ## Nothing will be truncated, do not include col widget
                        chk <- NA
                    }
                }
                colInfo$truncLen[[ ci ]] <<- chk
            }

            ## Stitch together all the HTML components:
            thCell <- paste(c(th, "</th>"), collapse="")
            
            colInfo$thRow <<- c(colInfo$thRow, thCell)
        }
        ## Assemble factor filters
        colInfo$numOut <<- ci
        allColNames <- colnames(data)
        srcFactors  <- sort(unique(allColNames[ colInfo$facInds[ colInfo$usedFact ] ]))
        for (col in srcFactors) {
            fOpts     <- .factorizeColumn(col)
            allStyles <- c(allStyles, sprintf("/* Factor styles for %s */",
                                              .aesc(col)), fOpts$styles)
            ##colInfo$filtHTML  <<- c(colInfo$filtHTML,
            ##                        fOpts$heading, fOpts$buttons)
        }
        style <<- allStyles
        ## What to return here?...

    },

    generateOutput = function (display.url=TRUE) {
        titHTML  <- setTitle(asHTML=TRUE)
        headHTML <- setHeader(asHTML=TRUE)
        footHTML <- setFooter(asHTML=TRUE)
        
        ## My browser has tabs. Lots of them. Having a favicon helps
        ## me find relevant ones in all the mess. The default is just
        ## a simple "DT"
        favlink <- ""
        if (!is.null(flags$favicon)) {
            ## https://en.wikipedia.org/wiki/Data_URI_scheme
            if (is.na(flags$favicon)) flags$favicon <<- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4QEaEjUB2Zx58gAAAGFJREFUOMtjvDqV5z8DJeDqVJ7///+f+f///5n/MLaYwZf/YgZfcIrD5K5O5fnPhMvgl+dvMIgbamCIIdMMDAwMTAwUApwGiBtqoNiECzDSIBAxAT5x2oUB3QwYDcTBEIgA0nC298mCHGAAAAAASUVORK5CYII="
            favlink <- 
                sprintf("<link rel='shortcut icon' href='%s'>", flags$favicon)
        }
        fh <- file(file, "w")

        ## Write the head boilerplate
        writeLines(sprintf("<!DOCTYPE html>
<html><head>
  <meta content='text/html;charset=utf-8' http-equiv='Content-Type'>
  <meta content='utf-8' http-equiv='encoding'>
  <title>%s</title>
%s
<script>
%s
</script>
<style>
%s
</style>
</head><body onload='load()'>
%s
<div id='wrapper'>
", titHTML[1], favlink[1], .jsCode(), .cssStyles(), headHTML[1]), fh)

        if (length(colInfo$filtHTML) > 0)
            writeLines(paste(c(colInfo$filtHTML,"<hr />"), collapse="\n"), fh)
        
        writeLines("<table class='tab'><tbody id='data'>\n",fh)
        
        writeLines(paste(c("<tr failed=''>",colInfo$thRow,"</tr>"),
                         collapse="  \n"), fh)
        numCols  <- colInfo$numOut
        blankRow <- character(numCols)
        cInds    <- seq_len(numCols)
        for (ri in seq_len(nrow(data))) {
            usingMethods(".factFunc")
            usingMethods(".buildCell")
            ## Extract any category classes for this row:
            cls <- vapply(colInfo$usedFact, function(ci) .factFunc(ri, ci), "")
            ## Build the <tr> with row-level class names:
            rowTag <- c("  <tr ", .buildClass(cls), ">")
            ## Build the set of <td>s for this row:
            row <- vapply(cInds, function(ci) .buildCell(ri, ci), "")
            ## Write to file
            writeLines(paste(c(rowTag, row, "</tr>\n"), collapse = ''), fh)
        }
        writeLines("</tbody></table>\n",fh)
        if (flags$show.attr) writeLines(attributeHTML(), fh)
        ## Check buttons for set filters
        writeLines("<script>setFactorFilters()</script>\n", fh)
        writeLines(footHTML, fh)

        writeLines("</div></body></html>", fh)
        close(fh)
        if (display.url) browseURL(paste("file://", file, sep=""))
        file
    },

    attributeHTML = function( ) {
        ## We may have had to re-factor the original data set. If so,
        ## attributes would have been held here:
        atts <- attr(data, "originalAttributes")
        ## Otherwise, use attributes "as-is":
        if (is.null(atts)) atts <- attributes(data)
        
        ignore <- c("dim", "names", "row.names")
        an <- names(atts)
        ul <- character()
        skipped <- character()
        for (nm in an) {
            av <- atts[[ nm ]]
            if (!is.atomic(av)) next
            if (nm %in% ignore) {
                skipped <- c(skipped, nm)
                next
            }
            ul <- c(ul,paste("<li>", "<b>",.hesc(nm),"</b>: ", sep=""))
            if (length(av) == 1) {
                ul <- c(ul, paste(.hesc(av), collapse='<br>'))
            } else {
                ul <- c(ul,"<ol>",sprintf("<li>%s", .hesc(av)),"</ol>")
            }
        }
        if (length(ul) != 0) {
            paste("<h4>Attributes</h4><ul>",
                  paste(ul, collapse="\n"), "</ul>", sep="\n")
        } else {
            ""
        }
    },


### Building pop-up filter containers


    .closeImage = function( ) {
        "<img src='foo.png' class='close' onclick='fClose(event)' title='Close filter settings'>"
    },

    .submitAndReset = function( ci ) {
        sar <- c("<br>",
                 "<button class='submit' onclick='submitFilt(this)' title='Apply the filter'>Submit</button>",
              "&nbsp;&diams;&nbsp;", .ResetButton( ci ))
        paste(sar,  sep='')
    },

    .ResetButton = function( ci ) {
        rb <- c("<button class='reset' onclick='resetFilt(this)' title='Remove all filters from this column'>Reset</button>")
        hTit <- getOpt(colInfo$srcName[ci], 'coltitle')
        if (!is.null(hTit)) {
            ## Add the column title (description) to the end of the
            ## filter widget
            rb <- c(rb, sprintf("<div class='desc'>%s</div>", .aesc(hTit[1])))
        }
        paste(rb,  sep='')
    },

    .fallbackName = function( ci ) {
        ## Get column name and HTML escape, falling back to 
        ## "Column #" if no name is defined
        nm <- colInfo$name[ci]
        if (!is.def(nm) || nm == "") nm <- paste("Column",ci)
        .hesc(nm)
    },

    .factorFilterWidget = function( ci, fOpts ) {
        head <- if (any(fOpts$textFilter)) {
            ## Do not include a header if a text filter is already present
            ""
        } else {
            name <- .fallbackName(ci) 
            paste(.closeImage(), name, "Levels:")
        }
        paste(c(head,sprintf("<div col='%d' ftype='fact'>",ci),fOpts$buttons,
                .ResetButton(ci), "</div>"), collapse="\n")
    },

    .textFilterWidget = function (ci, header=TRUE) {
        nm <- .fallbackName(ci)
        paste(c(.closeImage(),nm,"<br>",
                "<div col='",ci,"' ftype='txt'><input class='txtFilt' onchange='textfilt(this)' value='' reset='' title='Show only rows matching this text (regular expressions supported)'>",
                .submitAndReset(ci), "</div>"), collapse='')
    },

    .numericFilterWidget = function( ci  ) {
        srcInd <- colInfo$srcInds[ci]
        mn <- min(data[[srcInd]])
        mx <- max(data[[srcInd]])
        nm <- .fallbackName(ci)
        rv <- c(.closeImage(),nm," range:<br>")
        sf <- 5
        rv <- c(rv, signif(mn, sf))
        gInfo <- colInfo$gradInfo[[ ci ]]
        if (is.def(gInfo)) {
            ## A gradient has been defined, make little scale
            if (gInfo$min > mn) {
                ## A gradient minimum is set that's greater than actual
                sfMin <- signif(gInfo$min, sf)
                rv <- c(rv, sprintf("<span class='help %s' title='' tooltip='Under-range' numrow='%s' max='%s' onclick='intfilt(this)'>&rarr;</span>",
                                    gInfo$lowClass[1], gInfo$lowClassTxt[1], sfMin), sfMin)
            }
            rv <- c(rv, sprintf("<span class='help %s' title='' tooltip='%s'  numrow='%s' min='%s' max='%s' onclick='intfilt(this)'>&nbsp;</span>",
                                gInfo$css, 
                                .aesc(gInfo$intTxt), .aesc(gInfo$numRow),
                                gInfo$intMin, gInfo$intMax))
            if (gInfo$max < mx ) {
                ## The gradient maximum is less than actual
                sfMax <- signif(gInfo$max, sf)
                rv <- c(rv, sfMax,
                        sprintf("<span class='help %s' title='' tooltip='Over-range'  numrow='%s' min='%s' onclick='intfilt(this)'>&rarr;</span>", gInfo$highClass[1], gInfo$highClassTxt[1], sfMax))
            }
        } else {
            rv <- c(rv, '&rarr;')
        }
        rv <- c(rv, signif(mx, sf), "<br>")
        
        rv <- c(rv, sprintf("<div col='%d' ftype='num'><input class='numFilt' onchange='rangefilt(this)' value='%s' reset='%s' title='Minimum value to show'>&rarr;<input class='numFilt' onchange='rangefilt(this)' value='%s' reset='%s' title='Maximum value to show'>", ci, mn, mn, mx, mx) )
        if (!is.na(colInfo$quart[[ci]][1])) {
            ## Add quartile buttons
            qq <- c("Q1", "Q2", "Q3", "Q4")
            rv <- c(rv, "<br>", sprintf("<span class='help %s' title='' tooltip='Quartile'  numrow='%s' min='%s' max='%s' onclick='intfilt(this)'>%s</span>",
            qq, c("0-25%","25-50%","50-75%", "75-100%"),
            colInfo$quart[[ci]][1:4], colInfo$quart[[ci]][2:5], qq))
        }
        rv <- c(rv, .submitAndReset(ci), "</div>")
        paste(rv, collapse='')
    },

### Various callback functions applied to cells

    .buildCell = function(ri, ci) {
        ## MAIN HTML CELL GENERATION - this function takes a
        ## (rowIndex,colIndex) and builds the <td> object to add to
        ## the table.
        val   <- data[ ri, colInfo$srcInds[ci] ] # Value from input
        if (!is.def(val)) val <- ""
        ## Are there characters we should map to spaces?
        sm    <- colInfo$spaceMap[[ ci ]]
        if (!is.na(sm)) val <- gsub(sm, ' ', val)
        ## Should we dynamically truncate long cells?
        tLen  <- colInfo$truncLen[[ ci ]]
        trunc <- NULL
        rawV  <- FALSE
        if (!is.na(tLen) && tLen > 0 && nchar(val) > tLen) {
            ## Request to truncate long cells
            rgtVal <- substr(val, tLen+1, nchar(val))
            trunc <- sprintf("<span class='trunc'>%s</span><span onclick='tTog(this)' class='tTog' title='%s'>&hellip;</span>", .hesc(rgtVal), "toggle visibility of long text")
            val <- substr(val, 1, tLen)
            rawV <- TRUE
        }
        
        val  <- .hesc(val) # HTML escaped
        url  <- val
        ## Wrap column in hyperlink, if appropriate:
        if (is.function(colInfo$urlCB[[ci]])) {
            url  <- colInfo$urlCB[[ci]](val, ri, ci)
            rawV <- TRUE
        }
        ## Collect all the CSS class names together
        cls  <- c(colInfo$facCB[[ci]](ri, ci),
                  colInfo$classCB[[ ci ]](ri,ci),
                  .gradFunc(ri,ci))
        ## Is there title attribute information specified?
        tit <- data[ ri, colInfo$titInds[[ ci ]] ]
        if (is.null(tit) || is.na(tit)) {
            tit <- NULL
        } else {
            tit <- sprintf(" title='%s'", .aesc(tit))
            cls <- c(cls, "help") 
        }
        ## If we have marked up the cell, store original value for filtering
        if (!rawV) {
            rawV <- ""
        } else if (is.def(data[ ri, colInfo$srcInds[ci] ])) {
            rawV <- sprintf(" txt='%s'", .aesc(data[ri, colInfo$srcInds[ci]]))
        } else {
            rawV <- " txt=''"
        }
        ## Stitch all the parts together into a full <td> object
        paste(c("<td", .buildClass(cls), tit, rawV, ">", url, trunc, "</td>"),
              collapse='')
    },


    ## CSS class functions

    .classFunc = function(ri, ci) {
        ## Just returns a single, STATIC CSS class for every cell in a column
        colInfo$fixCls[ ci ]
    },
    
    .classRef = function(ri, ci) {
        ## Pull a CSS class DIRECTLY from another column. That is, the
        ## column contents are themselves CSS classes
        cls <- data[ri, colInfo$refCls[ ci ] ]
        ## Make sure it meets CSS specification
        ## https://stackoverflow.com/a/449000
        cls <- gsub('[^_a-zA-Z0-9-]', '', cls)
        ## Assure valid first characters. Essential when classing
        ## columns that are numeric:
        if (!grepl('-?[_a-zA-Z]', cls)) cls <- paste('DT-',cls,sep='')
    },

    .factFunc = function(ri, ci) {
        ## Returns a CSS class BASED ON the value in another
        ## column. That is, the factor levels each have a generated
        ## (by .factorizeColumn) CSS class
        facInd <- colInfo$facInds[ ci ] # The column holding the factor
        fac <- data[ri, facInd] # The value (level) of the factor
        fac <- if (is.na(fac)) { dtNaString } else { as.character(fac) }
        colInfo$facLU[[ci]][ fac ] # Map the value to a CSS class
    },

    .classUser = function(ri, ci) {
        ## A user-supplied FUNCTION is taking the value of the cell
        ## and dynamically generating a CSS class
        val <- data[ ri, colInfo$srcInds[ci] ] # The value of this cell
        colInfo$usrCls[[ ci ]]( val ) # $usrCls holds the user's callback
    },

    ## Hyperlink generation functions. The first argument (val) is
    ## the HTML cell content as generated so far. It may be altered
    ## from the original value (eg formatting)

    .basicURL = function(show, ri, ci) {
        ## Generate a URL based on a format
        val <- data[ ri, colInfo$srcInds[ci] ] # The raw value of this cell
        fmt <- colInfo$urlFmt[ci] # The sprintf format
        url <- sprintf(fmt, URLencode(val))
        sprintf("<a href='%s' target='_blank'>%s</a>", url, show[1])
    },

    .linkFunc = function(val, ri, ci) {
        ## The URL for this cell is explicitly held in another column
        url <- data[ ri, colInfo$urlInds[ci] ] # The URL cell
        if (is.null(url) || is.na(url) || url == "") {
            ## There is no URL, just return the value
            val
        } else {
            sprintf("<a href='%s' target='_blank'>%s</a>", url, val[1])
        }
    },

    ## Gradient CSS style selection

    .gradFunc = function(ri, ci) {
        ## Calculates a gradient CSS class, if available
        gInfo <- colInfo$gradInfo[[ ci ]]
        if (is.list( gInfo )) {
            val  <- as.numeric(data[ri,colInfo$srcInds[ci]]) # Value of the cell
            bins <- gInfo$binVals
            if (is.na(val)) {
                gInfo$naClass
            } else if (val < bins[1]) {
                ## Value under the gradient range
                gInfo$lowClass
            } else if (val > bins[length(bins)]) {
                ## Value over the gradient range
                gInfo$highClass
            } else {
                int <- findInterval(val, bins, all.inside = TRUE) # Interval
                gInfo$css[ int ] # CSS style for interval
            }
        } else {
            ## Gradient information was not available
            NULL
        }
    },

### Column option normalization for gradients and factors
    
    .columnGradient = function (col) {
        ## Normalize the 'gradient' option to a list, generate
        ## derivative data structures (CSS styles and breaks)
        opts    <- getOpt( col, 'gradient' )
        ## Normalize settings to list:
        if (is.null(opts)) return(opts)
        if (!is.list(opts)) opts <- list(normalized = FALSE)
        if (any(opts$normalized)) return (opts) # Only need to normalize once
        opts$normalized <- TRUE
        opts$ignore     <- FALSE
        srcInd <- getOpt(col, 'src')
        if (!is.numeric(data[[ srcInd ]])) {
            ## Column is non-numeric. User error, or a non-numeric column
            ## that is being considered because a '*' option requested
            ## gradients be applied to all columns
            opts$ignore <- TRUE
            setOpt(col, 'gradient', opts)
            return(opts)
        }
        if (is.null(opts$binVals) || is.na(opts$binVals[1])) {
            ## Automatically calculate a linear gradient
            ## Find min/max if not set:
            if (is.null(opts$min)) opts$min <- min( data[[ srcInd ]] )
            if (is.null(opts$max)) opts$max <- max( data[[ srcInd ]] )
            ## Calculate number of bins
            numBins <- ifelse(is.null(opts$bins),
                       ifelse(is.null(opts$step), 15,
                       (opts$max-opts$min)/opts$step[1]), opts$bins[1])
            opts$bins <- as.integer(numBins)
            ## Generate bins as linear sequence
            opts$binVals <- seq(from=opts$min, to=opts$max,
                                length.out= opts$bins + 1)
        } else {
            ## User has provided explicit bin values
            ## Calculate derivative values:
            opts$bins <- length(opts$binVals) - 1
            opts$min  <- min( opts$binVals )
            opts$max  <- max( opts$binVals )
        }
        numBins <- opts$bins
        binVals <- opts$binVals
        if (is.null(opts$colors)) opts$colors <- c("#5555ff", "white", "red")
        ## https://stackoverflow.com/a/13353264
        scale <- if (opts$colors[1] %in% .standardColors) {
                     ## This is a standard R color map
                     do.call(opts$colors[1], list(n=numBins, alpha=NULL))
                 } else {
                     colorRampPalette(opts$colors)( numBins )
                 }
        opts$scale <- scale
        ## How many rows fall in each bin? cut() and findInterval()
        ## are inconsistent in how they handle breaks, so since we
        ## will use findInterval later for classing data, we'll use it
        ## here to tabulate row counts
        allInts  <- table(findInterval(data[[ srcInd ]],binVals,
                                       all.inside=FALSE,rightmost.closed=TRUE))
        ## The table might have index "0" (below interval) and
        ## "binLen" (above interval). I'm also not sure that I can
        ## count on the interval levels to be ordered. So guarantee
        ## order (and empty counts) by referencing the table with
        ## numeric *names*       
        binCnts  <- allInts[ as.character(1:numBins) ]
        ## Set empty bins to zero
        binCnts  <- ifelse(is.na(binCnts), 0, binCnts)

        classes  <- character(numBins) # CSS class names
        intTxt   <- character(numBins) # Pop-up text for interval eg [1,3.2)
        rowTxt   <- character(numBins) # Pop-up text for affected rows
        intMin   <- numeric(numBins)   # Used to build DOM attribute filter
        intMax   <- numeric(numBins)
        styles   <- sprintf("/* Column %s gradient styles */", .hesc(col))

        for (i in seq_len(numBins)) {
            ## CSS class to use with this gradient level
            cls <- classes[i] <- sprintf("Grad%d-%d", srcInd, i)
            styles <- c(styles, sprintf(
                "  td.%s, span.%s { background-color: %s; }",
                cls, cls, scale[i]))
            numRow    <- binCnts[i]
            intTxt[i] <- sprintf("[ %s , %s %s",
                signif(binVals[i], 4), signif(binVals[i+1],4),
                ifelse(i == numBins,']',')'))
            rowTxt[i] <- sprintf("%d row%s", numRow, ifelse(numRow == 1,'','s'))
            intMin[i] <- binVals[i]
            intMax[i] <- binVals[i+1]
        }
        lhCnts <- setNames(c(allInts["0"],allInts[as.character(numBins+1)]),
                           c("low","high"))
        for (lh in c("low","high")) {
            ## Check low.style and high.style for custom settings
            sty   <- opts[[ paste(c(lh,"style"), collapse='.') ]]
            okey  <- paste(c(lh,"Class"), collapse='')
            lhNum <- lhCnts[lh]
            opts[[ paste(okey,'Txt',sep='') ]] <- sprintf(
                "%d row%s %s %s", lhNum, ifelse(lhNum == 1, '', 's'),
                ifelse(lh == 'low', '<', '>'),
                signif(ifelse(lh == 'low',binVals[1],binVals[numBins+1]), 5))
            if (is.null(sty)) {
                ## Default class: take the extreme bin
                opts[[okey]] <- ifelse(lh == "low", classes[1],
                                       classes[numBins])
            } else {
                cls <- opts[[okey]] <-  sprintf("%sCustClass-%d", lh, srcInd)
                styles <- c(styles, sprintf("  .%s { %s }", cls, sty))
            }
        }
        
        naCls <- opts$naClass <- sprintf("naClass-%d", srcInd)
        styles <- c(styles, sprintf("  .%s { %s }", naCls,
             if (is.def(opts$nastyle)) { opts$nastyle } else {
             "background-color: #fdf" }))
        opts$styles <- paste(styles, collapse="\n") # Style lines
        opts$intTxt <- intTxt
        opts$numRow <- rowTxt
        opts$intMin <- intMin 
        opts$intMax <- intMax
        opts$css    <- classes

        ## Update the higher-level dtOptions list:
        setOpt(col, 'gradient', opts)
        opts
    },

    .factorizeColumn = function (col) {
        ## Normalize the 'factor' options to a list, generate
        ## derivative data structures (HTML widgets and CSS styles)
        col  <- stndCol( col, names(data) )
        opts <- getOpt( col, 'factor' )
        ## Normalize settings to list:
        if (!is.list(opts)) opts <- list(normalized = FALSE)
        if (any(opts$normalized)) return (opts) # Only need to normalize once
        
        srcInd <- getOpt(col, 'src')
        if (is.null(srcInd)) return (opts)
        opts$normalized <- TRUE
        ## Set name to self if not set
        if (is.null(opts$name)) opts$name <- getOpt( col, 'name' )
        if (is.factor(data[[ srcInd ]])) {
            ## Maintain original level order
            lvls <- opts$levels <-levels(data[[ srcInd ]])
            ## Convert factor values to their names:
            data[[ srcInd ]] <<- as.character(data[[ srcInd ]])
        } else {
            ## Otherwise, order the unique values
            lvls <- opts$levels <-as.character(sort(unique(data[[ srcInd ]])))
        }
        ## Find all levels, construct look-ups and some HTML
        lvlCnt   <- table( data[[ srcInd ]] )
        ## Is there a minimum count set?
        minLvl   <- opts$min.level                       # Set at column level
        if (!is.def(minLvl)) minLvl <- flags$min.level   # Set globally
        if (!is.def(minLvl) || minLvl == 0) minLvl <- NA # ... nope.
        nlvl     <- length(lvls)
        fTok     <- opts$tok <- paste("Fac", srcInd, sep='')
        classes  <- character(nlvl)
        styles   <- character()
        ## Heading for in-line buttons
        html     <- c("<h3>", .hesc(opts$name))
        cTit     <- getOpt( col, 'coltitle' )
        if (is.def(cTit) && flags$facet.title)
            html <- c(html, " <span class='desc'>", .hesc(cTit),"</span>")
        opts$heading <- paste(c(html, ":</h3>"), sep='')
        buttons <- character()
        ## Check which level filters should start out active
        isActive <- integer()
        if (!is.null(opts$active)) {
            for (req in opts$active) {
                aInd <- matchCol(req, lvls)
                ## The requested filter matches a factor level
                if (!is.na(aInd)) isActive <- c(isActive, aInd)
            }
        }
        opts$activeInd <- isActive
        otherCnt <- 0
        othCls   <- sprintf("%s-Other", fTok)
        for (i in seq_len(nlvl)) {
            lvlNm <- lvls[i]
            numR  <- lvlCnt[lvlNm]
            if (!is.na(minLvl) && numR < minLvl) {
                ## Not enough rows to have its own button
                classes[i] <- othCls
                otherCnt   <- otherCnt + numR
                next
            }
            ## CSS class to use with this factor
            cls <- classes[i] <- sprintf("%s-%d", fTok, i)
            ## Style to hide the row when factor is not selected:
            styles <- c(styles, sprintf(
               "  .No%s tr.%s { display: none ! important; }",
               cls, cls))
            buttons <- c(buttons,
                         .factorButton(lvlNm, opts$name, cls, numR, fTok,
                                       ifelse(i %in% isActive, "yes", "no")))
        }
        cf <- opts$colors
        if (is.null(cf)) cf <- flags[['factor.colors']]
        if (!all(is.na(cf))) {
            ## User wants to assign colors to the factors
            if (cf[1] == 'hash') {
                ## Internal function that generates a color based on
                ## hashing the factor text. This has the advantage of
                ## generating an identical color for a given factor
                ## text in any set
                
                colOpts <- list( x = lvls )
                for (co in c("min","max","r","g","b")) {
                    ## Pull out dtOptions associated with the factor request
                    cv <- opts[[ co ]]
                    if (!is.null(cv)) colOpts[[ co ]] <- cv
                }
                cs <- do.call("colorizeString", colOpts)
            } else if (cf[1] %in% .standardColors) {
                ## Default R color pallete options
                cs <- do.call(cf, list(n = nlvl, alpha=NULL))
            } else {
                ## Presume this is a list of explicit colors
                cs <- cf
            }
            styles <- c(styles, sprintf("  td.%s, .button.%s { background-color: %s; }", classes, classes, cs))
        }

        if (otherCnt > 0) {
            ## Some levels got combined into "Other"
            styles <- c(styles, sprintf(
               "  .No%s tr.%s { display: none ! important; }", othCls, othCls),
               sprintf("  td.%s, .button.%s { background-color: %s; }",
                       othCls, othCls, "#ccc"))
            buttons <- c(buttons,
                         .factorButton(dtOtherString, opts$name, othCls,
                                       otherCnt, fTok,"no"))
        }
        numNA        <- sum(is.na(data[[ srcInd ]]))
        if (numNA > 0) {
            ## At least some values are NA
            cls <- sprintf("%s-NA", fTok)
            styles <- c(styles, sprintf(
               "  .No%s tr.%s { display: none ! important; }",
               cls, cls),
               sprintf("  td.%s, .button.%s { background-color: %s; }",
                       cls, cls, "white"))
            buttons <- c(buttons,
                         .factorButton(dtNaString, opts$name, cls,
                                       numNA, fTok, "no"))
            classes <- c(classes, cls)
            lvls    <- c(lvls, dtNaString)
        }

        opts$styles  <- paste(styles, collapse="\n") # Style lines
        opts$buttons <- buttons # Filter widgets
        opts$LU      <- setNames(classes, lvls)     # Lookup Level -> CSS class
        
        ## Update the higher-level dtOptions list:
        setOpt(col, 'factor', opts)
        ## Will categorize the column by itself:
        setDefaultOpt(col, 'byFactor', col)
        opts
    },

    .factorButton = function (lvlNm, colNm, cls, numR, fTok, picked) {
        ## Title text to help explain usage:
        tit  <- sprintf("Toggle visibility of %d row%s where %s='%s'",
                        numR, ifelse(numR == 1, "", "s"),
                        .aesc(colNm), .aesc(lvlNm))
        ## Button is actually a <div> with onclick
        paste(c("<div class='button ", cls, "' numrow='",numR,
                "' what='",fTok, "' picked='",picked, "' title='", .aesc(tit),
                "' onclick='factClick(event)'",">",.hesc(lvlNm),
                " <span class='cnt'>[",numR, "]</span></div>"), collapse='')
    },

### Column option getters and setters

    matchCol = function (col, allCols=NULL) {
        ## Match a string case-sensitive, then -insensitive if that
        ## failed, returning the matching index number of the target vector
        
        ## By default compare against the data object
        if (is.null(allCols)) allCols <- colnames(data)
        ifelse(col %in% allCols,
               match(col, allCols),
               match(tolower(col), tolower(allCols)))
    },
    
    stndCol = function (col, allCols) {
        ## Like matchCol, but returns the standardized column name,
        ## rather than the matching index
        
        ## By default compare against the data object
        if (is.null(allCols)) allCols <- colnames(data)
        cind <- matchCol(col, allCols)
        allCols[ cind ]
    },

    getOpt = function (col, key) {
        ## Get the option for a column, falling back to global options
        ## if not set. Use case-flexible matching for the column
        cInd <- matchCol(col, names(options))
        if (!is.null(options[[ cInd ]][[key]])) {
            ## Option defined on cIndumn
            options[[ col ]][[key]]
        } else if (!is.null(options[[ '*' ]][[key]])) {
            ## Option defined for the global wildcard
            options[[ '*' ]][[key]]
        } else {
            ## See if fallback settings are provided in the params structure
            bp <- params[[ key ]]
            if (is.null(bp)) {
                ## params did not provide any information for this key
                NULL
            } else {
                ## The key has a structure in params
                pInd <- matchCol(col, names(bp))
                if (!is.null(bp[[ pInd ]])) {
                    ## Yes, match to the column requsted
                    bp[[ pInd ]]
                } else if (!is.null(bp[[ '*' ]])) {
                    ## Yes, final fallback to wildcard column in parameters
                    bp[[ '*' ]]
                } else {
                    ## Even though the key was in params, the column was not
                    NULL
                }
            }
        }
    },

    setOpt = function (col, key, val) {
        ## Set an option
        options[[ col ]][[ key ]] <<- val
    },
    
    setDefaultOpt = function (col, key, val) {
        ## Set an option, but only if it is not set already
        if (is.null(options[[ col ]])) {
            options[[col]] <<- list()
            options[[col]][[key]] <<- val
        } else if (is.null(options[[ col ]][[ key ]])) {
            options[[col]][[key]] <<- val
        }
        val
    },
    
    xtndOpt = function (col, key, val) {
        ## Extend a value in an option
        options[[ col ]][[ key ]] <<- c(getOpt(col, key), val)
    },

    .readSupporting = function (x, what, impact) {
        ## Boilerplate JS and CSS files are kept in etc/. However,
        ## they will be pasted inline into the output - this is to
        ## assure that the output is a standalone result that won't be
        ## separated from its functional components.
        xf <- system.file("etc", x ,package="dynamictable")
        if (xf == "") {
            ## Failed to find the file! We will note this to alert the
            ## user in the output, since something's going to break.
            flags$etcErrs <<- c(flags$etcErrs, sprintf(
              "<b>%s</b>: %s", what, impact))
            ""
        } else {
            ## Just return the file as-is
            paste(readLines(xf), collapse = "\n")
        }
    },

    .cssStyles = function () {
         paste(c(.readSupporting("dynamictable.css", "CSS styling",
                                 "Will omit coloring and some interactivity"),
                 style, userStyle), collapse="\n")
    },

    .jsCode = function() {
        ## Supporting javascript code for interactivity
        rv <- .readSupporting("dynamictable.js", "Javascript functions",
                              "Will prevent the table from being interactive")
        if (!is.null(flags$etcErrs)) {
            ## Add warning to output of missing component(s)
            rv <- c(rv,  paste(c("
// Some assets could not be loaded!
window.addEventListener('load', function () {
  var err = document.createElement('div');
  err.innerHTML = \"<div class='err'>Some HTML components could not be loaded:<br>", paste(flags$etcErrs, collapse="<br>"),
"</div>\";
  document.body.insertBefore(err, document.body.firstChild)
});
"), collapse = ''))
        }
        paste(rv, collapse="\n")
    }
)

## Just some short aliases for escape functions
.hesc  <- function(x) htmltools::htmlEscape(x)
.aesc  <- function(x) htmltools::htmlEscape(x, attribute=TRUE)


.buildClass <- function (cls) {
    ## Combine class names into a single space-delimited class=''
    ## entry, or return NULL if no class names provided.
    if (is.null(cls)) {
        NULL
    } else {
        cls <- cls[ !is.na(cls) ]
        if (length(cls) > 0) {
            sprintf(" class='%s'", .aesc(paste(cls, collapse=' ')))
        } else {
            NULL
        }
    }
}

#' Parenthetical Regular Expression
#'
#' Simplifies parenthetical extraction of matches from regular expressions
#'
#' @param RegExp The regular expression, presumably with one or more
#'     parenthetical components
#' @param text The string to match. NOT vectorized
#'
#' @return If no matches found \code{NA}; Otherwise a character vector
#'     with one element per parenthetical capture
#'
#' @examples
#'
#' parenRE("(quick|lazy)\\b.+(fox|DOG)", "Quick brown fox")
#' ## Word boundary (\b) prevents matching to "quickest":
#' parenRE("(quick|lazy)\\b.+(fox|DOG)", "Quickest brown fox")
#'
#' @export
    
parenRE <- function(RegExp, text) {
    ## RegExp in R is not fully fleshed out. This implements a hack
    ## suggested in the internal documentation to allow recovery of
    ## text from multiple parenthetical captures
    m <- lapply(regmatches(text, gregexpr(RegExp, text, ignore.case = TRUE,
                                          perl = TRUE)),
                function(e) {
                    regmatches(e, regexec(RegExp, e, ignore.case = TRUE))
                })
    hitList <- m[[1]]
    ## Return NA if the match failed
    if (length(hitList) == 0) return(NA)
    ## Otherwise, extract out the character vector of the matches, and
    ## exclude the first entry
    hitList[[1]][-1]
    ## Initially written for my first pass at a file registry:
    ## https://github.com/maptracker/GettingAndCleaningData/blob/master/registryManager.R#L153-L166
}

.standardColors <- c("rainbow", "heat.colors", "terrain.colors",
                     "topo.colors", "cm.colors")

#' Colorize String
#'
#' Takes one or more strings as input, and generates a 24-bit
#' hexadecimal color based on hashing the string
#'
#' @param x The string(s) to be hashed
#' @param min Default 128. The minimum value (0-256) any RGB value can
#'     have
#' @param max Default 240. Maximum value an RGB value can have
#' @param as.html When TRUE (default) the returned value will be
#'     hexadecimal strings suitable (eg "#aa318f"). If false, a three
#'     column ("R","G","B") array will be returned, one row per input
#'     string.
#' @param r Explicit value to set for the red channel. Ignores "min"
#'     and "max". Combined with "g" and "b" allows some control over
#'     the hues generated during hashing.
#' @param g Explict green channel value
#' @param b Explicit blue channel value
#'
#' @examples
#'
#' colorizeString(c("quick fox","QUICK fox", "lazy dog"))
#' colorizeString(c("quick fox","QUICK fox", "lazy dog"), as.html=FALSE)
#'
#' @importFrom digest digest
#' @importFrom stats setNames
#' 
#' @export
    
colorizeString <- function (x, min=128, max=240, r=NA, g=NA, b=NA,
                            as.html=TRUE) {
    ## Hash the passed string, don't really care about algorithm:
    d   <- vapply(x, digest::digest, "")
    numStr <- length(x)
    ## Take the left-most six bytes of the hash as the three RGB values
    rgb <- matrix(c(strtoi(substr(d, 1, 2), 16L),
                    strtoi(substr(d, 3, 4), 16L),
                    strtoi(substr(d, 5, 6), 16L)), ncol=3,
                  dimnames = list(input=x, RGB = c("R","G","B")))
    ## Setting min prevents dark colors, max prevents light colors
    if (max > 255) max <- 255
    if (min < 0)   min <- 0
    rng <- max - min ## The intensity range left by min-max
    ## Rescale to the min-max range:
    rgb <- min + floor(rng * rgb / 255)
    ## If the user has specified particular RGB colors, use
    ## those. This can help constrain the pallet to a particular set
    ## of hues.
    if (!is.na(r)) rgb[,1] <- rep(r, numStr)
    if (!is.na(g)) rgb[,2] <- rep(g, numStr)
    if (!is.na(b)) rgb[,3] <- rep(b, numStr)
    ## Final sanity checks:
    rgb <- ifelse( rgb < 0, 0, ifelse(rgb > 255, 255, rgb))
    if (as.html) {
        ## 6 byte hex color
        setNames(sprintf("#%02X%02X%02X", rgb[,1], rgb[,2], rgb[,3]),x)
    } else {
        ## Array of values
        rgb
    }
}

#' Is Defined
#'
#' Convenience method, returns FALSE if the object is null, an
#' uninitialized RefClass field, a zero-length matrix or all NAs.
#'
#' @param x The object being tested
#'
#' @return TRUE or FALSE
#'
#' @export

## Return TRUE for defined objects:
is.def <- function(x) {
    if (is.null(x) || any(inherits(x, "uninitializedField"))) {
        FALSE
    } else if (any(is.object(x))) {
        TRUE
    } else {
        ## We will consider matrices "defined" as long as they have
        ## one row OR column. This is to allow certain cached results
        ## to pass after analysis, even if a crossproduct results in
        ## no content. RefClass fields defined as "matrix"
        ## auto-instantiate with 0x0 matrices.
        (is.matrix(x) && (nrow(x) != 0 || ncol(x) != 0)) || any(!is.na(x))
    }
}
