
locFile <- "inst/extdata/vampiretraits.tsv"
src     <- "https://en.wikipedia.org/wiki/List_of_vampire_traits_in_folklore_and_fiction#Appearance"
when    <- "2017 Jan 18"
lic     <- "Creative Commons Attribution-ShareAlike"

## Needed a LOT of clean up
x <- read.table(locFile, quote=c(), sep="\t", na=c("","?"),
                header=T, row.names=1)

attr(x,"License")   <- lic
attr(x,"Source")    <- src
attr(x,"Retrieved") <- when
attr(x,"Note")      <- "This table has been modified from the original to reduce the number of factor levels and remove nuanced text from some columns. As such, it should not be applied in situations where precise classification of vampires is required. Submitter is not responsible for exsanguination, or darkening of either wardrobe or music tastes."

rda <- gsub("\\.tsv$",".rda", locFile)
rda <- gsub("^inst\\/extdata","data", rda)
vampiretraits <- x
save(vampiretraits, file = rda)
message("RDA object saved to: ", rda)
