##-------------------------------------------
## Configure trellis graphical style

## http://www.magesblog.com/2013/04/how-to-change-alpha-value-of-colours-in.html
add.alpha <- function(col, alpha = 1){
    apply(sapply(col, col2rgb)/255, 2,
          function(x) rgb(x[1], x[2], x[3], alpha = alpha))
}

## Define Colors
mycol <- c(1, "#377EB8", "#E41A1C", "#4DAF4A",
           "#ff00ff", "#FF7F00", "#984EA3", "#FFFF33", "#808080")
myreg <- colorRampPalette(c(mycol[3],  "gray90", mycol[2]))(100)

## Trellis graphical style.
ps <- list(
    superpose.symbol = list(
        col = mycol, pch = 1,
        fill = add.alpha(mycol, alpha = 0.4)),
    box.rectangle = list(col = 1, fill = c("gray70")),
    box.umbrella = list(col = 1, lty = 1),
    box.dot = list(pch = "|"),
    dot.symbol = list(col = 1, pch = 19),
    dot.line = list(col = "gray50", lty = 3),
    plot.symbol = list(col = 1),
    plot.line = list(col = 1),
    plot.polygon = list(col = "gray95"),
    superpose.line = list(col = mycol, lty = 1),
    superpose.polygon = list(col = mycol),
    strip.background = list(col = c("gray90", "gray70")),
    regions = list(col = myreg)
    )

trellis.par.set(ps)

##-------------------------------------------
## Remove objects
## rm(list = ls())
