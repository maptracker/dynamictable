## Over-the-top customization of a dynamic table
library("dynamictable")
## We will stuff the R-code itself into a <pre> for the footer:
dtDemoFile <- system.file("demo", "hyperCustomizedTable.R",
                           package="dynamictable")
footerHTML <- if (dtDemoFile == "") {
  "<i>Your footer text here</i>" # Couldn't find the demo file...
} else {
    paste(c("<h4>This is the footer</h4>",
            "Code run to generate this page:",
            "<pre style='color:navy; background-color:#ffa'; padding: 1em;>",
            htmltools::htmlEscape(readLines(dtDemoFile)), "</pre>"),
          collapse= "\n")
}

## Some custom styles, will be added to the document stylesheet:
myStyles <-
    c("/* Re-styling of a default style (table header row) */",
      ".tab th { background-color: #afa; } ",
      "/* Custom classes for $carb - numeric values need 'DT-' prefix */",
      ".DT-1 { font-size: 0.2em; } .DT-2 { font-size: 0.4em; }",
      ".DT-3 { font-size: 0.6em; } .DT-4 { font-size: 0.8em; }",
      ".DT-6 { font-size: 1.2em; } .DT-8 { font-size: 1.6em; }",
      "/* Goofy classes to support detection of '7' in $wt */",
      ".hasSeven { background-color: lime; } ",
      ".hasSeven:after { content: ' Lucky!!'; }")

## Brief (honestly) description of the customizations used:
headerHTML <-"<h2>Excessive customization example for Basic HTML Table</h2>
This is the 'header' section. This tabular view of mtcars has the following
customizations, which are largely gratuitous but designed to illustrate options:
<pre>
* Set 'title' (browser window frame)
* Set 'header' (above table - this list of text)
* Set 'footer' (below table)
* Use 'sortby' to pre-sort by column $mpg
* Set a custom favorite icon (Wikimedia Tux icon)
* Set column 'options':
  * Hyperlink rownames (column added as $dtRowNames) with 'url' to image search
  * Truncate rownames to 15 characters, with '...' expand widgets
  * Customize 'gradient' for $mpg:
    * Explicit 'min'/'max' range (15-25), set 'bins' (7)
    * Custom 'colors' (red, orange, green)
    * Custom 'low.style' and 'high.style' (values outside min-max)
  * Factorize $cyl, change displayed name, set custom factor colors
  * Auto gradient for $disp
  * Auto gradient for $hp, using built-in rainbow() palette
  * Flagged $drat as 'fold', which appends an 'x' to the value
  * 'hide' the $qsec column
  * Auto factorization for both $vs and $am. Hash-based colorization assures
    that the same colors assigned to identically-named factor levels
  * Custom mouse-over column 'title' for $am and $wt headers
  * Use built-in terrain.colors() palette to factorize $gear, and pre-select
    the '3' and '5' facet buttons to start with only those levels showing
  * Column $wt sets 'class' to be a user-supplied function that flags any
    value containing a '7'
* Custom CSS styles to change &lt;th&gt; background color, a style for each
  value of $carb, and styles to add 'Lucky!!' to cells flagged by the custom
  class callback function for $wt
</pre>
"

## Finally, make the table:
dynamictable(mtcars,
  title="Overly-customized DT demonstration",
  header=headerHTML,
  footer=footerHTML,
  sortby="mpg",
  favicon="https://upload.wikimedia.org/wikipedia/commons/9/9d/Page_white_tux.png",
  options=list(
    dtRowNames=list(url="https://www.ixquick.com/do/search?q=1974 %s&cat=pics",
                     truncate=15),
    mpg=list(gradient=list(min=15, max=25, bins=7,
      low.style="background-color:black; color:white;",
      high.style="background-color:lime; color:red; font-weight:bold;",
      colors=c("red", "orange", "green"))),
    cyl=list(name="Cylinders", factor=list(colors=c("pink","peru","plum"))),
    disp=list(gradient=TRUE),
    hp=list(gradient=list(colors="rainbow")),
    drat=list(fold=TRUE),
    qsec=list(hide=TRUE),
    vs=list(factor=TRUE),
    am=list(factor=TRUE, coltitle="0 = automatic, 1 = manual"),
    gear=list(factor=list(colors="terrain.colors", active=c(3,5))),
    carb=list(class='carb'),
    wt=list(class=function(x) if (grepl('7', x)) { 'hasSeven' } else { '' },
            coltitle="If the weight has a '7', then the car is lucky!")
  ),
  style=myStyles)
