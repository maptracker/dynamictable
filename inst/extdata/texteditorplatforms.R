
locFile <- "inst/extdata/texteditorplatforms.tsv"
src     <- "https://en.wikipedia.org/wiki/Comparison_of_text_editors#Cross-platform"
when    <- "2017 Jan 17"
lic     <- "Creative Commons Attribution-ShareAlike"

x <- read.table(locFile, quote=c(), sep="\t", na=c("NA",""),
                header=T, row.names=1)

attr(x,"License")   <- lic
attr(x,"Source")    <- src
attr(x,"Retrieved") <- when

rda <- gsub("\\.tsv$",".rda", locFile)
rda <- gsub("^inst\\/extdata","data", rda)
texteditorplatforms <- x
save(texteditorplatforms, file = rda)
message("RDA object saved to: ", rda)
