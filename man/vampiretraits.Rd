% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{vampiretraits}
\alias{vampiretraits}
\title{Table comparing core traits of different representations of vampires}
\format{A data.frame with 141 rows and 5 variables
\itemize{
  \item Skin.Colour: The complexion of the fiend
  \item Fangs: Are fangs present?
  \item Reflection: Does the villain appear in mirrors?
  \item Film: Can the monster be captured on film?
  \item Shadow: Does the creature cast a shadow?
}}
\usage{
vampiretraits
}
\description{
Extracted from Wikipedia article
  "List of vampire traits in folklore and fiction". Intended to
  showcase factor filtering in dynamictable. This data set is
  mostly factorized, but includes several "atypical" entries for
  each column.
}
\examples{

# The min.level setting can be used to group the atypical levels
# into a single facet in the filtering interface.

dynamictable(vampiretraits, min.level=2)
}
\references{
\url{https://en.wikipedia.org/wiki/List_of_vampire_traits_in_folklore_and_fiction#Appearance}
}
\keyword{datasets}

