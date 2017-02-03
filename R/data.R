## Help information for dynamictable data files

## Using as a guide ggplot's roxygen:
##   https://github.com/tidyverse/ggplot2/blob/master/R/data.R
##   As suggested by: https://stackoverflow.com/a/21622673

#' Table comparing core traits of different representations of vampires
#'
#' Extracted from Wikipedia article
#'   "List of vampire traits in folklore and fiction". Intended to
#'   showcase factor filtering in dynamictable. This data set is
#'   mostly factorized, but includes several "atypical" entries for
#'   each column.
#'
#' @format A data.frame with 141 rows and 5 variables
#' \itemize{
#'   \item Skin.Colour: The complexion of the fiend
#'   \item Fangs: Are fangs present?
#'   \item Reflection: Does the villain appear in mirrors?
#'   \item Film: Can the monster be captured on film?
#'   \item Shadow: Does the creature cast a shadow?
#' }
#'
#' @references
#'
#' \url{https://en.wikipedia.org/wiki/List_of_vampire_traits_in_folklore_and_fiction#Appearance}
#'
#' @examples
#'
#' # The min.level setting can be used to group the atypical levels
#' # into a single facet in the filtering interface.
#'
#' dynamictable(vampiretraits, min.level=2)
"vampiretraits"

#' Compatibility grid of text editor software with major operating systems
#'
#' Extracted from Wikipedia article
#'   "Comparison of text editors". Intended to showcase factor
#'   filtering in dynamictable. The data are very regular with three
#'   levels ("Yes", "No", "Partial") plus \code{NA}.
#'
#' @format A data.frame with 74 rows and 6 variables representing
#'     operating systems: "Windows", "OS.X", "Linux", "BSD", "Unix"
#'     and "OpenVMS"
#'
#' @references
#'
#' \url{https://en.wikipedia.org/wiki/Comparison_of_text_editors#Cross-platform}
#'
#' @examples
#'
#' dynamictable(texteditorplatforms)
"texteditorplatforms"

#' Table highlighting the type systems used by various programming languages
#'
#' Extracted from Wikipedia article
#'   "Comparison of type systems". Intended to showcase factor
#'   filtering in dynamictable. The data consist of relatively clean
#'   factor levels.
#'
#' @format A data.frame with 41 rows and 4 variables
#' \itemize{
#'   \item static.dynamic: Are safety checks performed at compilation (static) or runtime (dynamic)?
#'   \item strong.weak: Is it easy to violate type constraints (weak) or will errors be generated (strong). Terminology is contentious here.
#'   \item safety: Another contentious category.
#'   \item nominative.structural: Is the system, nominative, structural, duck or some combination?
#' }
#'
#' @references
#'
#' \url{https://en.wikipedia.org/wiki/Comparison_of_type_systems}
#'
#' @examples
#'
#' dynamictable(typesystemcomparison)
"typesystemcomparison"

