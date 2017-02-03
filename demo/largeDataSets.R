library("dynamictable")

# Generate a moderately large amount of data to test performance of
# DHTML components

n      <- 1000 # Number of rows

## Some strings to play with:
states <- rownames(state.x77)        # 50 U.S. states
cant   <- rownames(swiss)            # 47 Swiss provences
cntrs  <- rownames(LifeCycleSavings) # 50 countries
carNm  <- rownames(mtcars)           # 32 car models
feeds  <- levels(chickwts$feed)      # 6 chicken feeds
isls   <- names(islands)             # 48 islands
colNm  <- colors()
colNm  <- colNm[ !grepl("\\d", colNm) ]    # ~128 color names

## Some MadLib-type bits:
start  <- c("The", "That", "A", "Purportedly")
r1     <- c("powered","rusted","clogged","terrified", "fascinated",
            "confused","depleted")
act    <- c("driven","raced","stolen","humiliated","sauntered","praised")
r2     <- c("before", "after", "while")
r3     <- c("rusting","retiring","swimming","vacationing","incorporating")

## Make a data frame
myDF   <- data.frame(
    dbid  = seq(1,n),
    Weight = runif(n, min = 0, max = 1000),
    Parrots = rbinom(n, 1000, 0.01),
    State = as.factor(sample(states, n, replace=TRUE)),
    Description = sprintf("%s %s %s %s by %s and %s from %s to %s %s %s in %s",
                          sample(start, n, replace=TRUE),
                          sample(colNm, n, replace=TRUE),
                          sample(carNm, n, replace=TRUE),
                          sample(r1,    n, replace=TRUE),
                          sample(feeds, n, replace=TRUE),
                          sample(act,   n, replace=TRUE),
                          sample(cant,  n, replace=TRUE),
                          sample(cntrs, n, replace=TRUE),
                          sample(r2,    n, replace=TRUE),
                          sample(r3,    n, replace=TRUE),
                          sample(isls,  n, replace=TRUE)),
    stringsAsFactors = FALSE
    )

Sys.time()
dynamictable(myDF, options = list(
    Description=list(truncate=30),
    Weight=list(gradient=TRUE)
    ),
    header=sprintf("<h3>dynamictable() stress test - table with %d rows</h3>", n[1]),
)
Sys.time()
