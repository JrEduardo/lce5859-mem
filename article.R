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
    xtable.table.placement = "ht",
    xtable.sanitize.text.function = identity
)

##----------------------------------------------------------------------
## Packages

library(magrittr)
library(SnowballC)
library(tm)
library(clusterSim)

## For graphics
library(wordcloud)
library(lattice)
library(latticeExtra)
source("configs/setup.R")
cols <- trellis.par.get("superpose.line")$col

##----------------------------------------------------------------------
## Functions
text2dtm <- function(text) {
    text %>%
        VectorSource %>%
        Corpus %>%
        tm_map(removeWords,
               c(stopwords("english"),
                 "can")) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace) %>%
        tm_map(removeNumbers) %>%
        tm_map(stemDocument) %>%
        DocumentTermMatrix
}


## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## Read and organize data

## Data long, each line is paper combine with topic (duplicate papers)
data_long <- read.table(
    file = "./data/kdd-papers.txt",
    header = TRUE,
    sep = ";",
    colClasses = c("factor", "character", "character")
)

## Data short, each line is unique paper
data_short <- data_long[!duplicated(data_long$title), -1]
data_short <- within(data_short, {
    ntopics = sapply(title, function(x) sum(x == data_long$title))
})

## Texts, using title and abstract texts
data_texts <- with(data_short, paste(title, abstract))

## Tranform to document-term matrix
dtm_texts <- text2dtm(data_texts)


## ----webscrap, out.width="0.8\\textwidth", results="asis", fig.cap="Sítio do SIGKDD2016 de onde foram extraídos os títulos, resumos e tópicos."----

## Web pages images
include_graphics("images/webscrap.pdf")


## ------------------------------------------------------------------------

##----------------------------------------------------------------------
## Descriptive analysis
mat_texts <- as.matrix(dtm_texts)
counts <- colSums(mat_texts)

## Frequency tables
freq_papers <- table(data_long$topic)
freq_topics <- table(paste(data_short$ntopics, "Tópico(s)"))


## ---- results="asis"-----------------------------------------------------

aux <- sort(freq_papers, decreasing = TRUE)
tab_freq <- data.frame("Tópico" = names(aux),
                       "Nº de artigos" = c(aux),
                       "Freq. absoluta" = c(aux)/sum(c(aux)),
                       "Freq. acumulada" = cumsum(c(aux)/sum(c(aux))),
                       check.names = FALSE,
                       stringsAsFactors = FALSE)

tab_freq <- rbind(tab_freq, c(NA, colSums(tab_freq[, -1])[-3], NA))
tab_freq[17, 1] <- "\\textbf{Total}"
rownames(tab_freq) <- c(1:nlevels(data_long$topic), "")

## Build latex table
cap <- c("Frequência de artigos em cada tópico definido no evento.")
print(xtable(tab_freq, digits = c(0, 0, 0, 3, 3),
             align = c("llccc"),
             caption = cap,
             label = "tab:freq"))


## ----wordcloud, out.width="0.7\\textwidth", fig.height=5, fig.width=12, fig.cap="Nuvem dos 5\\% termos mais frequentes nos textos extraídos dos resumos e títulos dos artigos apresentados no SIGKDD 2016."----

## Wordcloud
paleta <- brewer.pal(9, "Greys")[-(1:4)]
corte <- quantile(counts, probs = 0.95)
wnames <- names(counts)[counts > corte] %>%
    stringi::stri_trans_general(id = "latin-ascii")
cnames <- counts[counts > corte]
wordcloud(words = wnames,
          freq = cnames,
          min.freq = 1,
          random.order = FALSE,
          colors = paleta,
          family = "serif")


## ----pca, cache=TRUE-----------------------------------------------------

##----------------------------------------------------------------------
## Analysis

##-------------------------------------------
## Dimensionality reduction

## PCA decomposition
Vcor <- cov(mat_texts)
deco <- eigen(Vcor, symmetric = TRUE)
li <- deco$values
ei <- deco$vectors
pvar <- li/sum(li)

## Define the number of principal components
ni <- sum(cumsum(pvar) < 0.999)
scores <- mat_texts %*% ei[, 1:ni]


## ----pca-plot, fig.height=4.5, fig.width=11, fig.cap="Representação da matriz termo-documento de frequências com os 50 termos mais frequentes (à esquerda) e proporção acumulada da variância explicada pelo número de componentes considerados (à direita)"----

## Visualizing data matrix
aux <- mat_texts[, order(counts, decreasing = TRUE)[1:30]]
xy1 <- levelplot(aux,
                 aspect = "fill",
                 col.regions = colorRampPalette(
                     c("gray90",  "gray50", "gray20"))(100),
                 ylab = "Termos",
                 xlab = "Artigos",
                 colorkey = list(space = "top"),
                 scales = list(
                     x = list(rot = 90, cex = 0.7, labels = NULL)),
                 par.settings = list(
                     layout.heights = list(
                         axis.xlab.padding = -2,
                         key.axis.padding = -1))
                 )

txt <- parse(text = paste0("lambda[", ni, "]==", li[ni]))
xy2 <- xyplot(cumsum(pvar)[1:250] ~ seq(pvar)[1:250],
              pch = 19, type = c("g", "l", "p"),
              xlab = "Nº de componentes",
              ylab = "% variância explicada",
              scales = list(y = list(rot = 90)),
              par.settings = list(
                  layout.widths = list(ylab.axis.padding = 0)),
              panel = function(x, y, ...) {
                  panel.xyplot(x, y, ...)
                  panel.abline(h = 0.99, v = ni, lty = 2)
                  panel.points(x[ni], y[ni], col = cols[2], pch = 19)
                  panel.text(x[ni] - 5, y[ni] - 0.06,
                             txt, cex = 0.9, pos = 4)
              })

print(xy1, position = c(0, 0, 0.62, 1), more = TRUE)
print(xy2, position = c(0.6, 0, 1, 1), more = FALSE)


## ----kmeans-gap, cache=TRUE----------------------------------------------

##-------------------------------------------
## K-means clustering (choosen number of groups)

## Find groups
ks <- structure(2:15, names = paste0("k", 2:15))
lkms <- lapply(ks, function(k) {
    kmeans(x = scores, centers = k,
           iter.max = 50, nstart = 10)
})
meds <- sapply(lkms, function(x) {
    c("intra" = x$tot.withinss, "entre" = x$betweens)
})
meds <- cbind(t(meds), k = ks)

## Compute gap statistic
len <- length(lkms) - 1
mat <- matrix(nrow = len, ncol = 2)
for (u in 1:len) {
    cls <- cbind(lkms[[u]]$cluster, lkms[[u + 1]]$cluster)
    gap <- index.Gap(mat_texts, cls, B = 12, method = "k-means")
    mat[u, ] = do.call("c", gap)
}
colnames(mat) <- names(gap)
gaps <- cbind(k = 1:len + 1, mat)


## ----sqs-gap, fica.height=5, fig.width=11, fig.cap="Medidas de qualidade de agrupamento. Soma das distâncias euclidianas intra e entre clusters (esquerda) e diferenças de indíces Gap (direita)."----

## Illustrate statistics for choose the number of groups
xy1a <- xyplot(intra/1000 ~ k, pch = 19,
               type = "o", cex = 0.7,
               data = as.data.frame(meds),
               ylab = "SQ intra-clusters (em milhares)",
               scales = list(x = list(at = ks)))
xy1b <- xyplot(entre/1000 ~ k, pch = 19,
               type = "o", cex = 0.7,
               data = as.data.frame(meds),
               ylab = "SQ entre-clusters (em milhares)",
               scales = list(x = list(at = ks)))
xy1 <- doubleYScale(
    xy1a, xy1b, add.ylab2 = TRUE,
    text = c("Dentro dos clusters", "Entre os clusters"),
    title = "Soma de quadrados (SQ) das observações",
    cex.title = 1.1,
    points = TRUE,
    column = 2) +
    layer(panel.abline(v = ks, col = "gray80", lty = 2))
lab <- expression("Gap"*(k)-"Gap"*(k+1) +~s[k+1])
xy2 <- xyplot(diffu ~ k,
              data = as.data.frame(gaps),
              ylab = lab,
              type = c("p"),
              pch = 19,
              scales = list(y = list(rot = 90),
                            x = list(at = ks)),
              panel = function(x, y, subscripts, ... ) {
                  cols <- rep("gray60", length(y))
                  cols[y > 0] <- 1
                  panel.abline(v = ks, col = "gray80", lty = 2)
                  panel.xyplot(x, y, col = cols, ...)
                  panel.abline(h = 0, lty = 2, col = 1)
              })

print(xy1, position = c(0.0, 0.0, 0.52, 1.0), more = TRUE)
print(xy2, position = c(0.55, 0.0, 1.0, 0.93), more = FALSE)

## Choose k according to Tibishirani 2001
kchoosen <- with(as.data.frame(gaps), {
    min(k[diffu > 0])
})


## ------------------------------------------------------------------------

##-------------------------------------------
## Verify groups
agrup <- lkms[[kchoosen-1]]
data_agrup <- cbind(data_short, "group" = factor(agrup$cluster))

split_results <- lapply(levels(data_agrup$group), function(g) {
    da <- subset(data_agrup, group == g)
    data_texts <- with(da, paste(title, abstract))
    dtm_texts <- text2dtm(data_texts)
    mat_texts <- as.matrix(dtm_texts)
    counts <- colSums(mat_texts)
    list("narticles" = nrow(da),
         "dtm_texts" = dtm_texts,
         "mat_texts" = mat_texts,
         "counts" = counts)
})

narticles <- sapply(split_results, function(x) x$narticles)


## ----wordcloud-groups, fig.height=6, fig.width=12, fig.cap="Nuvem com os 5\\% termos mais frequentes em cada grupo formado pelo algoritmo k-means."----

## Wordclouds
par(mfrow = c(2, 3), mar = c(0, 0, 0, 0))
sapply(seq(unique(agrup$cluster)), function(i) {
    main <- paste0("Grupo ", i, " (", split_results[[i]]$narticles,
                   " artigos)")
    counts <- split_results[[i]]$counts
    paleta <- brewer.pal(9, "Greys")[-(1:4)]
    corte <- quantile(counts, probs = 0.95)
    wnames <- names(counts)[counts > corte] %>%
        stringi::stri_trans_general(id = "latin-ascii")
    cnames <- counts[counts > corte]
    wordcloud(words = wnames,
              freq = cnames,
              min.freq = 1,
              random.order = FALSE,
              colors = paleta,
              family = "serif")
    mtext(main, line = -1.5)
    box()
})


## ------------------------------------------------------------------------

## Random select 2 articles for each group
set.seed(1994)
index <- lapply(levels(data_agrup$group), function(i) {
    sample(which(data_agrup$group == i), 2)
})
random_da <- data_agrup[unlist(index), c("group", "title")]


## ---- results="asis"-----------------------------------------------------
print(xtable(random_da),
      include.colnames = FALSE,
      hline.after = NULL,
      only.contents = TRUE)


## ------------------------------------------------------------------------

## Dimensions
npa <- nrow(data_long)
nar <- nrow(data_short)
nto <- length(levels(data_long$topic))
nx <- nrow(mat_texts)
px <- ncol(mat_texts)
mx <- ncol(scores)


