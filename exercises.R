## ---- include=FALSE------------------------------------------------------

##----------------------------------------------------------------------
## Reports
library(knitr)
library(xtable)
opts_chunk$set(
    warning = FALSE,
    message = FALSE,
    cache = FALSE,
    echo = FALSE,
    results = "hide",
    fig.width = 7,
    fig.height = 5,
    fig.align = "center",
    fig.pos = "H",
    out.width = "1\\textwidth",
    dev.args = list(
        family = "Palatino")
    )
options(
    digits = 3,
    xtable.comment = FALSE,
    xtable.caption.placement = "top",
    xtable.table.placement = "H"
)

##----------------------------------------------------------------------
## Load packages

library(labestData)         ## Datas
library(reshape2)           ## Manipule data
library(plyr)               ## Manipule data

## For graphics
library(lattice)
library(latticeExtra)
source("configs/setup.R")
cols <- trellis.par.get("superpose.line")$col


## ------------------------------------------------------------------------

##======================================================================
## Exercise chapter 4
##======================================================================

##----------------------------------------------------------------------
## Packages
library(car)

##----------------------------------------------------------------------
## Functions

## Modify print Manova function for exibe table tests
outtests <- car:::print.Anova.mlm
body(outtests)[[16]] <- quote(invisible(tests))
body(outtests)[[15]] <- NULL

## Modify print LinearHypothesis function for exibe table tests
outcontrasts <- car:::print.linearHypothesis.mlm
body(outcontrasts)[[21]] <- quote(invisible(tests))
body(outcontrasts)[[22]] <- NULL

##----------------------------------------------------------------------
## Load and organize data
## help(ManlyTb4.5, h = "html")
da <- transform(ManlyTb4.5, sexo = factor(sexo))
levels(da$grup) <- c("Modernos", "Pré-históricos",
                     "Chacais", "Cuons", "Indianos")
da$grup <- relevel(da$grup, ref = "Pré-históricos")
levels(da$sexo) <- c("Desconhecido", "Macho", "Fêmea")


## ----box-dist, fig.height=4.5, fig.width = 10, fig.cap="Box-plots das nove medidas de mandíbula para cada grupo canino (esquerda) e matriz de distâncias multivariadas entre os grupos caninos (direita)."----

##----------------------------------------------------------------------
## Descriptive analysis

## Boxplot data
da_long <- melt(da[,-11], id.vars = "grup")
codvars <- parse(text = paste0("X[", seq(ncol(da)-2), "]"))
xy1 <- bwplot(value ~ grup | variable,
              axis = axis.grid,
              data = da_long,
              as.table = TRUE,
              layout = c(3, 3),
              ylab = "",
              scales = list(x = list(rot = 30), y = "free"),
              strip = strip.custom(
                  factor.levels = codvars)
              )

## Means distance matrix visualization
da_means <- sapply(da[, -c(1, 11)], function(x) {
    tapply(x, da$grup, mean)
})
xy2 <- levelplot(as.matrix(dist(da_means)),
                 xlab = "",
                 ylab = "",
                 scales = list(x = list(rot = 30)),
                 panel = function(x, y, z, ...) {
                     ## print(as.list(...))
                     panel.levelplot(x, y, z, ...)
                     panel.text(x, y, round(z, 2))
                 })

## Visualization
print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ----stars, fig.height=3, fig.width=9, fig.cap="Gráficos-radar representando as médias de cada medida da mandíbula para cada grupo canino."----

## Star graph
stars(da_means,
      draw.segments = TRUE,
      nrow = 1,
      col.segments = cols,
      ## key.labels = codvars,
      ## key.loc = c(8.7, 3.2),
      flip.labels = FALSE,
      radius = FALSE,
      frame.plot = TRUE## ,
      ## mar = c(1, 1, 2, 1)
      )
par(xpd = TRUE)
## polygon(c(1, 1, 10, 10), c(6, 0.5, 0.5, 6))
legend("top", codvars, fill = cols,
       horiz = TRUE, bty = "n", inset = -0.1)


## ---- results="asis"-----------------------------------------------------

cap <- paste("Teste de Mardia para normalidade multivariada.",
             "Estatísticas de teste e respectivos níveis",
             "descritivos entre parênteses.")

## Multivariate normality by group
ass1 <- do.call(
    rbind, lapply(
               split(da[, -c(1, 11)], da$grup),
               function(x) {
                   res <- MVN::mardiaTest(x)
                   s1 <- formatC(res@chi.small.skew, 3, format = "f")
                   s2 <- formatC(res@p.value.small, 4, format = "f")
                   k1 <- formatC(res@z.kurtosis, 3, format = "f")
                   k2 <- formatC(res@p.value.kurt, 4, format = "f")
                   cbind("Assimetria" = paste(s1, "(", s2, ")"),
                         "Curtose" = paste(k1, "(", k2, ")"))
               }))
rownames(ass1) <- levels(da$grup)

## Equals variances and covariances matrices
ass2 <- biotools::boxM(da[, -c(1, 11)], da$grup)

## Latex table
print(xtable(ass1,
             caption = cap,
             label = "tab:assumptions"))


## ---- results="asis"-----------------------------------------------------

##----------------------------------------------------------------------
## Multivariate analysis of variance

model <- lm(cbind(compm, largmapm, largca, altmapm, comppm, largpm,
                  compptm, comppqp, largci) ~ grup, data = da)

## Global statistical tests
mtests <- c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
tab_global <- do.call(rbind, lapply(mtests, function(i) {
    x <- outtests(Anova(model, test.statistic = i))
    class(x) <- "data.frame"
    x
}))
rownames(tab_global) <- mtests

## Resultados em formato de tabela Latex
cap <- c("Tabela de análise de variância multivariada global (MANOVA)")
print(xtable(tab_global, digits = c(0, 0, 4, 4, 0, 2, -3),
             caption = cap,
             label = "tab:manova")
      )


## ------------------------------------------------------------------------

## Linear Hypothesis Tests (contrasts)
HM <- cbind(0, -1 * diag(4))
tab_contr <- do.call(rbind, lapply(seq(nrow(HM)), function(i) {
    x <- outcontrasts(
        linearHypothesis(model, hypothesis.matrix = HM[i, ]))
    class(x) <- "data.frame"
    x
}))
tab_contr$Statistic <- rep(mtests, nrow(HM))
tab_contr$Contrast <- do.call(
    "c", strsplit(paste(
             paste("\\underline{Pré-Históricos = ",
                   levels(da$grup)[-1]), "};;;;"), ";"))


## ---- results="asis"-----------------------------------------------------

cap <- c("Tabela de contrastes multivariados")
print(xtable(tab_contr[c(8, 7, 1:6)],
             digits = c(0, 0, 0, 0, 4, 4, 0, 2, -3),
             caption = cap,
             label = "tab:contrasts"),
      include.rownames = FALSE,
      sanitize.text.function = identity)


## ----perfis2, fig.height=5, fig.width=10---------------------------------

##----------------------------------------------------------------------
## Comparação das distribuições das 9 variáveis entre os cães
## tailandeses
subda_long <- droplevels(
    subset(da_long, grup %in% c("Pré-históricos", "Modernos")))

## Distribuições empíricas das variaveis
xy1 <- densityplot(~value | variable,
                   data = subda_long,
                   scales = "free",
                   groups = grup,
                   axis = axis.grid,
                   as.table = TRUE,
                   layout = c(3, 3),
                   auto.key = TRUE,
                   xlab = "",
                   strip = strip.custom(
                       factor.levels = codvars)
                   )

## Gráfico do perfil mediano
##   - Ordenando as variaveis
ordvar <- order(aggregate(value ~ variable, subda_long, mean)[["value"]])
subda_long$variable <- ordered(subda_long$variable,
                               levels =
                                   levels(da_long$variable)[ordvar])

gap <- 0.15; widt <- 0.2
xy2 <- bwplot(value ~ variable,
              data = subda_long,
              axis = axis.grid,
              xlab = "Variáveis",
              ylab = "Valores Observados",
              scales = list(x = list(labels = codvars[ordvar])),
              subset = grup == "Modernos",
              horizontal = FALSE,
              box.width = widt,
              fill = cols[1],
              col = cols[1],
              alpha = 0.5,
              panel = function(x, y, subscripts, ...) {
                  panel.xyplot(x = as.integer(x) - gap, y,
                               type = "a", ...)
                  panel.bwplot(x = as.integer(x) + gap, y = y, ...)
              }) +
    as.layer(
        bwplot(value ~ variable,
               data = subda_long,
               subset = grup == "Pré-históricos",
               horizontal = FALSE,
               box.width = widt,
               fill = cols[2],
               col = cols[2],
               alpha = 0.5,
               panel = function(x, y, subscripts, ...) {
                   panel.xyplot(x = as.integer(x) - gap, y,
                                type = "a", ...)
                   panel.bwplot(x = as.integer(x) - gap, y = y, ...)
               })
    )

## Visualization
print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ---- include=FALSE------------------------------------------------------

##======================================================================
## Exercise chapter 5
##======================================================================

##----------------------------------------------------------------------
## Packages
library(biotools)

##----------------------------------------------------------------------
## Functions

## Distance matrix for proportion data
pdist <- function(x, diag = FALSE, upper = FALSE) {
    n <- nrow(x)
    aux <- t(combn(n, 2))
    out <- apply(aux, 1, function(ind) {
        sum(abs(x[ind[1], ] - x[ind[2], ]))
    })
    attr(out, "class") <- "dist"
    attr(out, "Size") <- n
    attr(out, "Diag") <- diag
    attr(out, "Upper") <- upper
    attr(out, "method") <- "euclidean"
    return(out)
}

##----------------------------------------------------------------------
## Load and organize data
## help(ManlyTb1.3, h = "html")
da <- ManlyTb1.3

## Define ambiental and genetics variables
ambvars <- c("alt", "precip", "tempmax", "tempmin")
genvars <- c("dg0.4", "dg0.6", "dg0.8", "dg1", "dg1.16", "dg1.3")

da[, ambvars] <- scale(da[, ambvars])
da[, genvars] <- da[, genvars] / 100


## ----splom-ecdf, fig.height=5, fig.width=10, fig.cap="Gráfico de dispersão por pares entre as variáveis ambientais (esquerda). Proporções acumuladas de mobilidade gênica dos cinco diferentes tipos genéticos de Pgi."----

##----------------------------------------------------------------------
## Descriptive analysis
da <- cbind(da, expand.grid(pch = c(8, 19),
                            color = cols[1:8],
                            stringsAsFactors = FALSE))
pspace <- list(layout.heights = list(top.padding = 10))

## For ambiental variables
codvarsa <- parse(text = paste0("X[A", 1:length(ambvars), "]"))
xy1 <- splom(~da[, ambvars],
             groups = col,
             data = da,
             varnames = codvarsa,
             xlab = "",
             par.settings = list(
                 layout.heights = list(top.padding = 10)
             ),
             pscales = lapply(da[, ambvars], function(x) {
                 list(limits = extendrange(x, f = 0.2))
             }),
             diag.panel = function(x, ...){
                 yrng <- current.panel.limits()$ylim
                 d <- density(x, na.rm = TRUE)
                 d$y <- with(d, yrng[1] + 0.9 * diff(yrng) * y / max(y))
                 panel.polygon(
                     x = c(d$x, rev(d$x)),
                     y = c(d$y, rep(min(d$y), length(d$y))),
                     col = "gray80", border = "white")
                 diag.panel.splom(x, ...)
             },
             panel = function(x, y, groups, ...) {
                 panel.grid()
                 panel.points(x, y,
                              col = da$color,
                              pch = da$pch,
                              cex = 1.1, alpha = 0.8)
                 ## panel.text(x, y - 0.1, groups,
                 ##            cex = 0.6, col = "gray50")
             })

## For genetics variables (note they are percentage)
aux <- cbind(da[, c("col", "pch", "color")],
             as.data.frame(t(apply(da[, genvars], 1, cumsum))))

da_long <- melt(aux, id.vars = c("col", "pch", "color"))
codvarsg <- parse(text = paste0("X[G", 1:length(genvars), "]"))

xy2 <- xyplot(value ~ variable,
              groups = col,
              data = da_long,
              type = c("g", "l", "p"),
              xlab = "",
              ylab = "Proporções acumuladas de Pgi",
              scales = list(x = list(labels = codvarsg)),
              par.settings = list(
                 layout.heights = list(top.padding = 9)
             ),
              panel = function(x, y, groups, ...) {
                  panel.xyplot(x, y, groups, ...,
                               col = da$color, pch = da$pch)
              })

## Legend for graphics
key <- list(
    space = "top",
    column = 4,
    points = list(
        pch = da$pch,
        fill = "white",
        col = da$color),
    lines = list(col = da$color),
    text = list(as.character(da$col), cex = 0.8)
)

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)
draw.key(key = key, draw = TRUE,
         vp = grid::viewport(
             x = grid::unit(0.55, "npc"),
             y = grid::unit(0.92, "npc")))


## ----distaxg, fig.height=6, fig.width=12, fig.cap="Matrizes de distâncias considerando as variáveis ambientais (esquerda) e genéticas (direita)."----

##----------------------------------------------------------------------
## Calcule and visualize distances matrices

ambD <- dist(da[, ambvars])
genD <- pdist(da[, genvars])

## Build data frame for use subscripts in lattice
## daD <- rbind(
##     cbind(melt(as.matrix(ambD)), vars = "Ambiental"),
##     cbind(melt(as.matrix(genD)), vars = "Genética")
## )
xy1 <- levelplot(value ~ Var1 + Var2 | vars,
          data = cbind(melt(as.matrix(ambD)),
                       vars = "Ambiental"),
          ## data = daD,
          colorkey = FALSE,
          xlab = "",
          ylab = "",
          scales = list(
              at = 1:16, labels = da$col,
              x= list(rot = 90)
          ),
          par.settings = list(
              layout.widths = list(right.padding = -2)
          ),
          panel = function(x, y, z, ...) {
                     ## print(as.list(...))
                     panel.levelplot(x, y, z, ...)
                     panel.text(x, y, round(z, 1), cex = 0.8)
                 })

xy2 <- levelplot(value ~ Var1 + Var2 | vars,
          data = cbind(melt(as.matrix(genD)),
                       vars = "Genética"),
          ## data = daD,
          colorkey = FALSE,
          xlab = "",
          ylab = "",
          scales = list(
              at = 1:16, labels = da$col,
              x= list(rot = 90)
          ),
          par.settings = list(
            layout.widths = list(left.padding = -2)
          ),
          panel = function(x, y, z, ...) {
                     ## print(as.list(...))
                     panel.levelplot(x, y, z, ...)
                     panel.text(x, y, round(z, 1), cex = 0.8)
                 })

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## Mantel test for correlation between ambiental and genetics distances
N <- 1000
out <- mantelTest(ambD, genD, nperm = N, graph = FALSE)

est <- round(out$correlation, 3)
pval <- round(out$p.value, 4)
dest <- round(out$nullcor, 3)
q95 <- round(quantile(dest, 0.95), 4)


## ------------------------------------------------------------------------

##======================================================================
## Exercise chapter 6
##======================================================================

##----------------------------------------------------------------------
## Load and organize data
## help(ManlyTb6.6, h = "html")
da <- data.frame(scale(ManlyTb6.6))

##----------------------------------------------------------------------
## Compute principal components
out <- prcomp(da)
stddev <- out$sdev
loading <- out$rotation
scores <- out$x
propvar <- stddev^2 / sum(stddev^2)
pacum <- cumsum(propvar)
corr <- t(loading) * stddev


## ----biplot, fig.height=5, fig.width=10, fig.cap="Proporção acumulada da variância por cada componente com apresentação dos autovalores (superior à esquerda). Matriz de carregamentos, auto vetores, associados as 2 primeiras componentes (inferior à esquerda). Biplot (à direita), dispersão dos escores calculados com base nas 2 primeiras componentes e correlação das variáveis originais com as componentes."----

##----------------------------------------------------------------------
## Visualize analysis
varsname <- parse(text = paste("X[", seq(propvar), "]"))

## Alternative scree-plot (shows cumulative proportion of variance
## explained)

lambdas <- parse(text = paste0("lambda[", 1:6, "]~(",
                               round(stddev^2, 2), ")"))
xy1 <- xyplot(pacum ~ seq(propvar),
              pch = 19, type = c("l", "p"),
              xlab = "",
              ylab = "% variância explicada",
              xlim = c(0.5, 7),
              ylim = c(0.67, 1.03),
              scales = list(
                  x = list(at = seq(propvar),
                           labels = paste0("PC", seq(propvar)))
              ),
              panel = function(x, y, ...) {
                  panel.abline(h = seq(.7, 1, by = .05),
                               col = "lightgray", lty = 2)
                  panel.xyplot(x, y, ...)
                  panel.text(x + 0.45, y - 0.015, lambdas,
                             cex = 0.8)
              })

## Loadings of principal components
xy2 <- levelplot(loading[, 1:2],
                 xlab = "",
                 ylab = "",
                 scales = list(x = list(labels = varsname)),
                 aspect = "fill",
                 colorkey = FALSE,
                 ## colorkey = list(space = "bottom"),
                 at = seq(-1, 1, length.out = 20),
                 col.regions = colorRampPalette(
                     c(cols[3], "gray90", cols[2]))(100),
                 panel = function(x, y, z, ...) {
                     panel.levelplot(x, y, z, ...)
                     panel.text(x, y, round(z, 3), cex = 0.8)
                 })

##-------------------------------------------
## Biplot
limits <- c(-1, 1) * max(abs(scores[, 1:2])) * 1.1
xy3 <- xyplot(PC2 ~ PC1, data = as.data.frame(scores),
              scales = list(alternating = 1),
              xlab = "PC1",
              ylab = "PC2",
              xlim = limits,
              ylim = limits,
              par.settings = list(
                  layout.heights = list(top.padding = 4.5),
                  layout.widths = list(right.padding = 6.4)
              ),
              xscale.components = function(...) {
                  ans <- xscale.components.default(...)
                  ans$top <- ans$bottom
                  ans$top$ticks$tck <- 0
                  ans$bottom$ticks$tck <- 1
                  ans
              },
              yscale.components = function(...) {
                  ans <- yscale.components.default(...)
                  ans$right <- ans$left
                  ans$right$ticks$tck <- 0
                  ans$left$ticks$tck <- 1
                  ans
              },
              panel = function(x, y, ...) {
                  panel.abline(h = 0, v = 0, lty = 2,
                               col = "gray50")
                  panel.xyplot(x, y, alpha = 0.8,
                               pch = 19, cex = 1.1,
                               col = "gray50", ...)
                  panel.text(x + 0.1, y - 0.1, cex = 0.8,
                             rownames(da), ...)
              })
xy4 <- xyplot(-1:1 ~ -1:1,
              scales = list(
                  alternating = 2,
                  col = cols[2]
              ),
              xlab = "",
              ylab = "",
              xlab.top = list("Correlações com PC1", col = cols[2]),
              ylab.right = list("Correlações com PC2", col = cols[2]),
              par.settings = list(
                  layout.heights = list(bottom.padding = 4.5),
                  layout.widths = list(left.padding = 5.2)
              ),
              xscale.components = function(...) {
                  ans <- xscale.components.default(...)
                  ans$top <- ans$bottom
                  ans$top$ticks$tck <- 1
                  ans$bottom$ticks$tck <- 0
                  ans
              },
              yscale.components = function(...) {
                  ans <- yscale.components.default(...)
                  ans$right <- ans$left
                  ans$right$ticks$tck <- 1
                  ans$left$ticks$tck <- 0
                  ans
              },
              panel = function(x) {
                  panel.arrows(
                      x0 = 0, y0 = 0, col = cols[2],
                      x1 = corr[1, ], y1 = corr[2, ],
                      length = 0.1, angle = 20)
                  panel.text(x = corr[1, ] + 0.03,
                             y = corr[2, ] - 0.03,
                             varsname, col = cols[2])
              })


## Organize display
print(xy1, position = c(0.0, 0.4, 0.5, 1.0), more = TRUE)
print(xy2, position = c(0.0, 0.0, 0.5, 0.5), more = TRUE)
print(xy3, position = c(0.48, 0.0, 1.0, 1.0), more = TRUE)
print(xy4, position = c(0.48, 0.0, 1.0, 1.0), more = FALSE)


## ---- include=FALSE------------------------------------------------------

##======================================================================
## Exercise chapter 7
##======================================================================

##----------------------------------------------------------------------
## Packages
library(psych)
library(corrplot)

##----------------------------------------------------------------------
## Functions
outlierid <- function(dados, centerfun = median, n = 5, ...) {
    centro <- apply(dados, 2, centerfun, ...)
    dists  <- apply(dados, 1, function(x) sqrt(sum((x - centro)^2)))
    ident <- order(dists, decreasing = TRUE)[1:n]
    attr(ident, "dist") <- dists[ident]
    return(ident)
}

##----------------------------------------------------------------------
## Load and organize data
da <- ManlyTb6.7[, -c(1, 11)]
rownames(da) <- ManlyTb6.7[, 1]

## Correlation matrix
R <- cor(da)
colnames(R) <- rownames(R) <- paste0(":X[", seq(ncol(da)), "]")


## ----corr-dens, fig.height=5, fig.width=10, fig.cap="Representação da matriz de correlação dos dados (esquerda) e distribuições marginais empíricas das variáveis originais (direita)."----

##----------------------------------------------------------------------
## Descriptive data

## Build graphics
layout(mat = cbind(1, t(matrix(seq(2, l = 9), ncol = 3))),
       widths = c(11, 3, 3, 3)/20)
par(mai = c(0, 0, 0, 0))
col1 <- colorRampPalette(c("gray6", "gray40", "gray90",
                           "gray41", "gray5"))
corrplot.mixed(
    R,
    tl.col = "black",
    lower = "number", upper = "ellipse",
    number.cex = 1.1, tl.cex = 1.5, cl.cex = 1,
    upper.col = col1(20),
    lower.col = 1,
    mar = c(0, 0, 1, 0))
lapply(1:ncol(da), function(i) {
    par(mar = c(2, 1, 2, 1) + 0.1)
    d <- density(da[, i])
    plot(d$x, d$y, type = "n",
         xlab = "", ylab = "",
         main = "", axes = FALSE)
    axis(1)
    grid()
    box()
    polygon(x = c(d$x, rev(d$x)),
            y = c(d$y, rep(min(d$y), length(d$y))),
            col = "gray70", border = "white")
    mtext(parse(text = gsub(":", replace = "", colnames(R)[i])),
          line = -2)
})


## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## Modelling - Get the factors and statistics

model <- principal(da, nfactors = 4, rotate = 'varimax')

lambdas <- model$values
loadings <- model$loadings
comunal <- model$communality
espvar <- model$uniquenesses
matres <- model$residual
propvar <- lambdas^2 / sum(lambdas^2)
scores <- model$scores


## ---- results="asis"-----------------------------------------------------

##-------------------------------------------
## Buid model expression
longnames <- c("Carne vermelha", "Carne branca", "Ovos", "Leite",
               "Peixes", "Cereais", "Carboidratos",
               "Grãos nozes e sementes", "Frutas e Vegetais")

cargas <- unclass(loadings)
expr_model <- do.call(
    rbind, lapply(1:nrow(cargas), function(i){
        ci <- formatC(cargas[i, ], 3, format = "f")
        cond <- !grepl("^-", ci)
        ci[cond] <- paste0("+", ci[cond])
        paste0(paste0("(\\text{", longnames[i], "})\\quad",
                      "&X_", i, "="),
               paste0(ci, "\\,F_", 1:ncol(cargas),
                      collapse = " "), "\\,+\\epsilon_", i, "\\\\")
    }))

cat("\\begin{equation}",
    "\\label{eqn:factors}",
    "\\begin{split}",
    expr_model,
    "\\end{split}",
    "\\end{equation}",
    sep= "\n")


## ----fact-plot, fig.height=5, fig.width=10, fig.cap="Avaliação da qualidade de ajuste do modelo. Distribuição dos resíduos calculados pela diferença entre $R$ e $LL^t + \\Psi$ (esquerda superior) e representação da matriz de resíduos (esquerda superior) e escores dos países com base nos quatro fatores obtidos (direita)."----

##-------------------------------------------
## Goodness of fit (residual matriz R - LL' + psi)
## cor(da) - ((loadings[]) %*% t(loadings[]) + diag(espvar))

varnames <- parse(text = paste("X[", 1:ncol(da), "]"))
diag(matres) <- 0
xy1 <- bwplot(c(matres),
              xlab = "",
              ylab = "",
              axis = axis.grid,
              par.settings = list(
                  layout.heights = list(
                      top.padding = 0,
                      bottom.padding = 0
                  ),
                  layout.widths = list(
                      right.padding = 0,
                      left.padding = 3
                  )
              ))

xy2 <- levelplot(matres,
                 xlab = "",
                 ylab = "",
                 aspect = "fill",
                 scales = list(labels = varnames),
                 at = seq(-max(abs(matres)), max(abs(matres)),
                          length.out = 12),
                 col.regions = colorRampPalette(
                     c(cols[3], "gray95", cols[2]))(50),
                 panel = function(x, y, z, ...) {
                     panel.levelplot(x, y, z, ...)
                     panel.text(1:9, 1:9,
                                formatC(espvar, 2, format = "f"))
                 })

##-------------------------------------------
## Visualize the scores
fanames <- parse(text = paste0("F[", 1:ncol(loadings), "]"))

## ## 3D visualization
## cloud(PC3 ~ PC1 * PC2,
##       data = data.frame(scores),
##       screen = list(z = 30, x = -80, y = 5),
##       xlab = "Fator 1",
##       ylab = "Fator 2",
##       zlab = "Fator 3",
##       scales = list(arrows = FALSE),
##       cex = 1.2,
##       axis = axis.grid,
##       panel = function(x, y, z, ...) {
##           panel.cloud(x = x, y = y, z = z,
##                       pch = 19, fill = 0, ...)
##       })
xy3 <- splom(~scores,
             varnames = fanames,
             xlab = "",
             groups = rownames(da),
             ## par.settings = list(
             ##     layout.heights = list(top.padding = 10)
             ## ),
             pscales = apply(scores, 2, function(x) {
                 list(limits = extendrange(x, f = 0.2))
             }),
             diag.panel = function(x, ...){
                 yrng <- current.panel.limits()$ylim
                 d <- density(x, na.rm = TRUE)
                 d$y <- with(d, yrng[1] + 0.9 * diff(yrng) * y / max(y))
                 panel.polygon(
                     x = c(d$x, rev(d$x)),
                     y = c(d$y, rep(min(d$y), length(d$y))),
                     col = "gray70", alpha = 0.9, border = "white")
                 diag.panel.splom(x, ...)
             },
             panel = function(x, y, groups, ...) {
                 panel.grid()
                 panel.abline(h = 0, v = 0, lty = 2, lwd = 0.8)
                 panel.points(x, y, col = "gray50", pch = 19,
                              cex = 1.1, alpha = 0.8)
                 labs <- rep("", length(x))
                 ind <- outlierid(cbind(x, y), n = 3)
                 labs[ind] <- groups[ind]
                 panel.text(x, y - 0.1, labs, cex = 0.65)
             })

## Organize visualization
print(xy1, position = c(0.0, 0.70, 0.5, 1.00), more = TRUE)
print(xy2, position = c(0.0, 0.00, 0.5, 0.80), more = TRUE)
print(xy3, position = c(0.5, 0.05, 1.0, 0.97), more = FALSE)


## ---- results="asis"-----------------------------------------------------

##-------------------------------------------
## Build table of scores
cap <- c("Escores dos fatores rotacionados para 23 países europeus.")
tab <- scores
colnames(tab) <- paste("Fator", 1:ncol(scores))
print(xtable(tab, digits = c(0, 4, 4, 4, 4),
             caption = cap,
             align = "lcccc",
             label = "tab:scores-fat")
      )


## ------------------------------------------------------------------------

##======================================================================
## Exercise chapter 8
##======================================================================

##----------------------------------------------------------------------
## Load packages
library(MASS)

##----------------------------------------------------------------------
## Load and organize data
## help(ManlyTb4.5, h = "html")
da <- transform(ManlyTb4.5, sexo = factor(sexo))
levels(da$grup) <- c("Modernos", "Pré-históricos",
                     "Chacais", "Cuons", "Indianos")
da$grup <- relevel(da$grup, ref = "Pré-históricos")
levels(da$sexo) <- c("Desconhecido", "Macho", "Fêmea")

##----------------------------------------------------------------------
## Get the discriminant functions

model <- lda(grup ~ ., data = da[, -11])
pred <- predict(model, da)


## ----plot-discrim, fig.height=5, fig.width=10, fig.cap="Densidades empíricas estimadas para as quatro variáveis canônicas estratificando pelos 5 grupos caninos (esquerda) e gráfico de dispersão das duas primeiras componentes (direita). No gráfico de dispersão o preenchimento dos pontos representa o grupo real e o contorno o grupo predito."----

##----------------------------------------------------------------------
## Visualization of quality prediction
pred <- predict(model, da)
aux <- cbind(data.frame(pred$x), grup = da$grup)
aux <- melt(aux, id.vars = "grup")

## Visualizing the discriminants scores
xy1 <- densityplot(~value | variable,
                   group = grup,
                   axis = axis.grid,
                   xlab = "",
                   ylab = "Densidade",
                   scales = list(x = "free"),
                   layout = c(2, 2),
                   as.table = TRUE,
                   data = aux)

xy2 <- xyplot(LD2 ~ LD1,
              ## groups = da$grup,
              groups = pred$class,
              type = c("g", "p"),
              data = data.frame(pred$x[, 1:2]),
              auto.key = list(lines = TRUE),
              par.settings = list(superpose.symbol = list(pch = 19)),
              cex = 1.2, pch = 19,
              panel = function(x, y, groups, ...) {
                  panel.xyplot(x, y, groups, ...)
                  panel.points(x, y, col = cols[da$grup],
                               pch = 19, cex = 0.5)
              })

print(xy1, split = c(1, 1, 2, 1), more = TRUE)
print(xy2, split = c(2, 1, 2, 1), more = FALSE)


## ---- results="asis"-----------------------------------------------------

## Correlations between canonical variables and original variables
codvars <- paste0("$X_", 1:ncol(model$means), "$")
corrs <- apply(pred$x, 2, function(x) {
    sapply(da[, -c(1, 11)], function(y) cor(x, y))
})
rownames(corrs) <- codvars

cap <- paste("Correlações entre as medidas de mandíbula originais",
             "e as quatro variáveis canônicas.")
print(xtable(t(corrs),
             digits = rep(3, 10),
             caption = cap,
             align = "lccccccccc",
             label = "tab:corrs",
             ),
      sanitize.colnames.function = identity)


## ---- results="asis"-----------------------------------------------------

## Prediction error estimation via leave-one-out approach
pc_oov <- sapply(1:nrow(da), function(i) {
    model <- lda(grup ~ ., data = da[-i, -11])
    predict(model, da[i, ])$class
})

## Errors
errostab <- cbind(table(da$grup, pred$class),
                  table(da$grup, pc_oov))

print(xtable(errostab),
      only.contents = TRUE,
      include.colnames = FALSE)


## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## GLM Binomial model

## Fit models for each groups
subda <- droplevels(subset(da, sexo != "Desconhecido"))
subda <- split(subda[, -1], subda$grup)

models <- lapply(subda, function(data) {
    mnul <- glm(sexo ~ 1, data = data,
                family = "binomial")
    msat <- glm(sexo ~ ., data = data,
                family = "binomial")
    mfit <- step(mnul, direction = "both", trace = FALSE,
                 scope = list(lower = mnul, upper = msat))
    return(mfit)
})


## ---- results="asis"-----------------------------------------------------

## Get estimates and analysis of deviance table
out <- do.call(
    rbind,
    lapply(models, function(model) {
        cbind("Estimativa" = coef(model),
              anova(model, test = "Chisq"))
    }))

columsname <- do.call(
    rbind,
    lapply(names(models), function(m) {
        co <- coef(models[[m]])
        grp_name <- c(m, rep(" ", length(co)-1))
        par_name <- sapply(names(co), function(x) {
            if (x == "(Intercept)") {
                return("$\\beta_0$")
            } else {
                ind <- which(x == colnames(subda[[1]]))
                paste0("$\\beta_", ind, "$")
            }
        })
        cbind(grp_name, par_name)
    }))


tabglm <- cbind(columsname, out)
colnames(tabglm)[1:2] <- c("Grupo", "Parameter")

cap <- paste("Estimativas dos parâmetros estimados dos modelos para",
             "cada grupo e seus respectivos quadros de análise de",
             "deviance sequencial.")
print(xtable(tabglm, caption = cap,
             digits = c(0, 0, 0, 3, 1, 3, 1, 3, 4),
             align = c("ccccccccc"),
             label = "tab:glmbinom"),
      include.rownames = FALSE,
      sanitize.text.function = identity)


## ------------------------------------------------------------------------

##======================================================================
## Exercise chapter 9
##======================================================================

##----------------------------------------------------------------------
## Packages
library(dendextend)
library(magrittr)

##----------------------------------------------------------------------
## Load and organize data
## help(ManlyTb9.7, h = "html")
da <- ManlyTb9.7
colnames(da)[-1] <- paste("Lote", 1:ncol(da[, -1]))
da_long <- melt(da, id.vars = "esp")

da_spec <- as.matrix(da[, -1])
rownames(da_spec) <- da$esp
da_lote <- t(as.matrix(da[, -1]))
rownames(da_lote) <- colnames(da)[-1]


## ----boxplot9, fig.height=4.5, fig.width=11, fig.cap="Box-plots das medidas de abundância para as 25 espécies (esquerda) e para os 17 lotes (direita)."----

##----------------------------------------------------------------------
## Descriptive analysis
xy1 <- bwplot(value ~ esp, data = da_long,
              horizontal = FALSE,
              scales = list(x = list(rot = 45, cex = 0.8)),
              ylab = "",
              axis = axis.grid)
xy2 <- bwplot(value ~ variable, data = da_long,
              horizontal = FALSE,
              scales = list(x = list(rot = 45, cex = 0.8)),
              ylab = "",
              axis = axis.grid)

print(xy1, position = c(0.02, 0.0, 0.58, 1.0), more = TRUE)
print(xy2, position = c(0.55, 0.0, 1.0, 1.0), more = FALSE)


## ----dendro, fig.height=4, fig.width=10, fig.cap="Dendrograma obtidos da análise de agrupamento hierárquico pelo método de Ward. Agrupamento de espécies (esquerda) e de lotes (direita)."----

##----------------------------------------------------------------------
## Clustering analysis

##-------------------------------------------
## By Species

## Grouping 10% most common words
k_spec <- 3 ## Decisão não automatizada, para agrupamento herárquico

mdist_spec <- dist(scale(da_spec))
agrup_spec <- hclust(mdist_spec, method = "ward.D")

## Build and show the dendogram
## par(mfrow = c(1, 2))
layout(matrix(c(1, 2), ncol = 2), widths = c(0.6, 0.4))
par(mar = c(8, 3, 1, 1) + 0.1)
dendr_spec <- as.dendrogram(agrup_spec)
dendr_spec %>%
    set("labels_cex", 0.9) %>%
    color_branches(k = k_spec) %>%
    plot(horiz = FALSE)
rect.dendrogram(dendr_spec, k = k_spec, horiz = FALSE,
                lty = 2, border = "gray30")

##-------------------------------------------
## By Lotes

## Grouping 10% most common words
k_lote = 2 ## Decisão não automatizada, para agrupamento herárquico

mdist_lote <- dist(scale(da_lote))
agrup_lote <- hclust(mdist_lote, method = "ward.D")

## Build and show the dendogram
par(mar = c(8, 1, 1, 1) + 0.1)
dendr_lote <- as.dendrogram(agrup_lote)
dendr_lote %>%
    set("labels_cex", 0.9) %>%
    color_branches(k = k_lote) %>%
    plot(horiz = FALSE)
rect.dendrogram(dendr_lote, k = k_lote, horiz = FALSE,
                lty = 2, border = "gray30")


## ------------------------------------------------------------------------

##======================================================================
## Exercise Chaper 10
##======================================================================

##----------------------------------------------------------------------
## Packages
library(corrplot)
library(CCA)

##----------------------------------------------------------------------
## Functions
outlierid <- function(dados, centerfun = median, n = 5, ...) {
    centro <- apply(dados, 2, centerfun, ...)
    dists  <- apply(dados, 1, function(x) sqrt(sum((x - centro)^2)))
    ident <- order(dists, decreasing = TRUE)[1:n]
    attr(ident, "dist") <- dists[ident]
    return(ident)
}

## Barlett Test for all canonical correlations equals 0
ccbarlett_test <- function(out) {
    ## `out` is output of CCA::cc() function
    with(out, {
        n <- nrow(scores$xscores)
        p <- nrow(scores$corr.X.xscores)
        q <- nrow(scores$corr.Y.xscores)
        l <- cor^2
        ## Statistic and test
        est <- -(n - (1/2) * (p + q + 3)) * sum(log(1 - l))
        pval <- pchisq(est, p * q, lower.tail = FALSE)
        list("est" = est, "pval" = pval)
    })
}

##----------------------------------------------------------------------
## Load and organize data
## help(ManlyTb10.4, h = "html")
## help(ManlyTb1.5, h = "html")
## help(ManlyTb6.7, h = "html")

da <- ManlyTb10.4

## Separe the datas
indX <- 2:10
indY <- 11:19
X <- da[, indX]
Y <- da[, indY]


## ----corr18, fig.height=11, fig.width=11, fig.cap="Matriz de correlações amostrais entre todas as 18 variáveis mensuradas para 22 países europeus."----

##----------------------------------------------------------------------
## Descripte analysis
R <- cor(da[, c(indX, indY)])
colnames(R) <- rownames(R) <-
    c(paste0(":X[", seq(indX), "]"),
      paste0(":Y[", seq(indY), "]"))

col1 <- colorRampPalette(
    c("gray6", "gray40", "gray90", "gray41", "gray5"))
corrplot.mixed(
    R,
    tl.col = cols[2],
    lower = "number", upper = "ellipse",
    number.cex = 1, tl.cex = 1.5, cl.cex = 1,
    upper.col = col1(20),
    lower.col = 1,
    mar = c(0, 0, 1, 0))
par(xpd = TRUE)
rect(9.5, 9.5, 18.5, 18.5, border = cols[2], lwd = 2)
rect(0.5, 0.5, 9.5, 9.5, border = cols[2], lwd = 2)


## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## Get the canonical variables and the canonical correlations
out <- cc(X, Y)
test <- ccbarlett_test(out)

## Correlations
corUX <- out$scores$corr.X.xscores
corVY <- out$scores$corr.Y.yscores


## ---- results="asis"-----------------------------------------------------

## Coreelations between original variables
tabcor <- data.frame(paste0("$X_", 1:9, "$"), corUX[, 1:3],
                     paste0("$Y_", 1:9, "$"), corVY[, 1:3])
rownames(tabcor) <- NULL
colnames(tabcor) <- c("Variável", paste0("$U_", 1:3, "$"),
                      "Variável", paste0("$V_", 1:3, "$"))

cap <- paste("Correlações entre as variáveis canônicas e as",
             "variáveis originais")
print(xtable(tabcor, label = "tab:corcc",
             align = c("ccccccccc"),
             digits = 3,
             caption = cap),
      include.rownames = FALSE,
      sanitize.text.function = identity)


## ----cancor, fig.height=4, fig.width=12, fig.cap="Gráficos de dispersão entre os três primeiros pares de variáveis canônicas."----

##----------------------------------------------------------------------
## Visualize analysis
varnamesX <- parse(text = paste("X[", 1:ncol(X), "]"))
varnamesY <- parse(text = paste("Y[", 1:ncol(Y), "]"))

## Dispersion
xys <-
    lapply(1:3, function(i) {
        aux <- data.frame(U = out$scores$xscores[, i],
                          V = out$scores$yscores[, i])
        xlab <- parse(text = paste0(
                          "\"Variável canônica\"~U[", i, "]"))
        ylab <- parse(text = paste0(
                          "\"Variável canônica\"~V[", i, "]"))
        xyplot(V ~ U, data = aux,
               ## aspect = "iso",
               type = c("g", "p"),
               xlab = list(xlab, col = cols[2]),
               ylab = list(ylab, col = cols[3]),
               scales = list(
                   x = list(col = cols[2]),
                   y = list(col = cols[3])
               ),
               panel = function(x, y, ...) {
                   panel.xyplot(x, y, alpha = 0.8,
                                pch = 19, cex = 1.1,
                                col = "gray50", ...)
                   panel.abline(h = 0, v = 0, lty = 2,
                                col = "gray50")
                   ind <- outlierid(cbind(x, y), n = 4)
                   panel.text(x[ind], y[ind],
                              da$pais[ind],
                              cex = 0.75)
               })
    })

gridExtra::marrangeGrob(xys, ncol = 3, nrow = 1, top = "")


## ------------------------------------------------------------------------

##======================================================================
## Exercise Chaper 11
##======================================================================

##----------------------------------------------------------------------
## Packages
library(MASS)

##----------------------------------------------------------------------
## Functions
outlierid <- function(dados, centerfun = median, n = 5, ...) {
    centro <- apply(dados, 2, centerfun, ...)
    dists  <- apply(dados, 1, function(x) sqrt(sum((x - centro)^2)))
    ident <- order(dists, decreasing = TRUE)[1:n]
    attr(ident, "dist") <- dists[ident]
    return(ident)
}

##----------------------------------------------------------------------
## Load and organize data
## help(ManlyTb1.5, h = "html")

da <- ManlyTb1.5
da$pais[21] <- "Rep. Tcheca"
rownames(da) <- paste0(da$pais, " (", da$grup, ")")

D <- dist(da[, -(1:2)])


## ----dist-mult, fig.height=9.5, fig.width=9.5, fig.cap="Matriz de distância entre os países europeus considerando as porcentagens de trabalho empregada em cada setor industrial."----

##----------------------------------------------------------------------
## Descriptive analysis
levelplot(as.matrix(D),
          xlab = "",
          ylab = "",
          scales = list(x = list(rot = 40)),
          panel = function(x, y, z, ...) {
              ## print(as.list(...))
              panel.levelplot(x, y, z, ...)
              ## panel.text(x, y, round(z, 2), )
          })


## ----stress-plot, fig.height=4, fig.width=5, out.width="0.5\\textwidth", fig.cap = "Valor de STRESS para diferentes números de dimensões no escalonamento multidimensional não-métrico de Kruskal."----

##----------------------------------------------------------------------
## Choose number of dimensions

kseq <- 1:10
mds <- lapply(kseq, function(k) {
    isoMDS(D, k = k, trace = FALSE)
})
meds <- sapply(mds, function(x) x$stress)

xyplot(meds ~ kseq,
       xlab = "Número de dimensões",
       ylab = "STRESS",
       type = c("g", "o"),
       pch = 19,
       scales = list(x = list(at = kseq)))

kchoose <- 4


## ----dimen-plot, fig.height=6, fig.width=11, fig.cap="Representação dos países europeus nas 4 dimensões obtidas pela análise multidimensional não métrica de Krukal (esquerda) e distâncias observadas e distâncias obtidas das dimensões conforme configuração da análise."----

##----------------------------------------------------------------------
## Visualize dimensions
dimen <- mds[[kchoose]]$points
dnames <- parse(text = paste("D[", 1:ncol(dimen), "]"))
xy1 <- splom(~dimen,
             varnames = dnames,
             xlab = "",
             groups = rownames(da),
             diag.panel = function(x, ...){
                 yrng <- current.panel.limits()$ylim
                 d <- density(x, na.rm = TRUE)
                 d$y <- with(d, yrng[1] + 0.9 * diff(yrng) * y / max(y))
                 panel.polygon(
                     x = c(d$x, rev(d$x)),
                     y = c(d$y, rep(min(d$y), length(d$y))),
                     col = "gray70", alpha = 0.4, border = "white")
                 diag.panel.splom(x, ...)
             },
             panel = function(x, y, groups, ...) {
                 panel.grid()
                 panel.abline(h = 0, v = 0, lty = 2, lwd = 0.8)
                 panel.points(x, y, col = "gray50", pch = 19,
                              cex = 1, alpha = 0.8)
                 ## labs <- rep("", length(x))
                 ## ind <- outlierid(cbind(x, y), n = 3)
                 ## labs[ind] <- groups[ind]
                 panel.text(x, y - 0.1, da$grup, cex = 0.65)
             })

## Recover the distances
aux <- data.frame("dreal" = c(D), "dconf" = c(dist(dimen)))
xy2 <- xyplot(dreal ~ dconf,
              xlab = "Distância de configuração",
              ylab = "Distância observada",
              type = c("g", "p", "r"),
              data = aux)

print(xy1, position = c(0, 0, 0.5, 1), more = TRUE)
print(xy2, position = c(0.5, 0, 1, 1), more = FALSE)


