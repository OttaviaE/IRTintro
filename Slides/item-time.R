## ----r setup, include=FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = T, 
                      warning = F, 
                      message = F, 
                      fig.align = "center", 
                      out.width = "90%", 
                      class.output="scroll-100")
hook_output <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(purl = knitr::hook_purl)

knitr::knit_hooks$set(output = function(x, options) {
  if (!is.null(n <- options$out.lines)) {
    x <- xfun::split_lines(x)
    if (length(x) > n) {
      # truncate the output
      x <- c(head(x, n), "....\n")
    }
    x <- paste(x, collapse = "\n")
  }
  hook_output(x, options)
})
library(TAM)
library(mokken)
library(lavaan)
library(difR)
library(sirt)
library(ggplot2)
library(tidyverse)
library(knitr)
set.seed(999)
N = 1000
b <- runif(5, -3,3)
a = c(runif(5, 0.4, 2))
true_theta = seq(-4, 4, length.out = N)
data <- sirt::sim.raschtype( true_theta, b=b, 
                             fixed.a = a)
true_thetaES = seq(-5, 2, length.out = N-200)
bES <- runif(8, -5,5)
aES = rep(1, 8)

dataES = sirt::sim.raschtype(true_thetaES, b = bES, fixed.a=aES)


IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}

i_info <- function(b, a=1,c=0, theta = seq(-5,5,length.out=1000)){
 
P <- NULL 
Q <- NULL
Ii <- NULL
for(i in 1:1000){
  P[i] <- 1/(1+ exp (-a*(theta[i] - b)))
  Q[i]= 1-P[i]
  Ii[i] =(a*Q[i]*(P[i]-c)^2)/(P[i]*((1-c)^2)) # (3PL)
   }
return(Ii)
}
# Function to get all item information
item_info <- function(b,a=1, c= 0){
item <- NULL
  for(i in 1:length(b)){
  item[[i]] <- i_info(b[i],a[i])
  }
return(item)
}

set.seed(999)

theta = seq(-4,4,length.out=1000)
library(xtable)



## ----r eval =T, echo = FALSE--------------------------------------------------
data = read.csv("data/itemClass.csv", header = T, sep = ",")
prop_item = data.frame(item = names(colMeans(data[, -c(1:2)])), 
           proportion = colMeans(data[, -c(1:2)]))

ggplot(prop_item, 
       aes(x = reorder(item, proportion), 
           y = proportion), color = item) + geom_bar(stat = "identity") + theme_light() + 
  ylab("Proportion correct") + ylim(0, 1) +
  theme(legend.position = "none", 
        axis.title = element_text(size = 26), 
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 22))


## ----r eval =FALSE, echo = TRUE-----------------------------------------------
#  data = read.csv("data/itemClass.csv", header = T, sep = ",")
#  prop_item = data.frame(item = names(colMeans(data[, -c(1:2)])),
#             proportion = colMeans(data[, -c(1:2)]))
#  
#  ggplot(prop_item,
#         aes(x = reorder(item, proportion),
#             y = proportion), color = item) + geom_bar(stat = "identity") + theme_light() +
#    ylab("Proportion correct") + ylim(0, 1) +
#    theme(legend.position = "none",
#          axis.title = element_text(size = 26),
#          axis.title.x = element_blank(),
#          axis.text = element_text(size = 22))

## ----r echo = FALSE-----------------------------------------------------------
m1pl = tam.mml(data[, grep("item", colnames(data))], verbose = F)
m2pl = tam.mml.2pl(data[, grep("item", colnames(data))], irtmodel = "2PL", verbose = F)
m3pl = tam.mml.3pl(data[, grep("item", colnames(data))], est.guess = grep("item", colnames(data)), verbose = F)
IRT.compareModels(m1pl, m2pl, m3pl) 

## ----r eval =FALSE, echo = TRUE-----------------------------------------------
#  m1pl = tam.mml(data[, grep("item", colnames(data))], verbose = F)
#  m2pl = tam.mml.2pl(data[, grep("item", colnames(data))], irtmodel = "2PL", verbose = F)
#  m3pl = tam.mml.3pl(data[, grep("item", colnames(data))], est.guess = grep("item", colnames(data)), verbose = F)
#  IRT.compareModels(m1pl, m2pl, m3pl)
#  

## ----r echo = FALSE-----------------------------------------------------------
fit_m1pl = tam.modelfit(m1pl, progress = F)

fit_m1pl$statlist %>% kable(align = "c", 
                            row.names = FALSE, 
                            digits = rep(2, 4))

## ----r eval =FALSE, echo = TRUE-----------------------------------------------
#  fit_m1pl$statlist

## ----r out.lines = 20---------------------------------------------------------
item_fit_1pl = IRT.itemfit(m1pl)
str(item_fit_1pl)

## ----r echo = FALSE-----------------------------------------------------------
item_fit_1pl$chisquare_stat



## ----r eval =FALSE, echo = TRUE-----------------------------------------------
#  item_fit_1pl$chisquare_stat

## ----r echo = FALSE-----------------------------------------------------------
item_fit_1pl$RMSD_summary %>% kable(aling = "c", digits = 2)
item_fit_1pl$RMSD %>% kable(aling = "c", digits = 2)

## ----r eval =FALSE, echo = TRUE-----------------------------------------------
#  item_fit_1pl$RMSD_summary %>% kable(aling = "c", digits = 2)
#  item_fit_1pl$RMSD %>% kable(aling = "c", digits = 2)

## ----r------------------------------------------------------------------------
b = c(-0.5616, -0.07)
a = c(1,1)


par(mar = c(5,7,4,2) + 0.1) 
plot(theta, IRT(theta, b = b[1], a = a[1]),
     cex.lab= 2.5, 
     cex.axis =1.5, cex.main = 3,
       xlab = expression(theta), ylab = "P(x = 1)",
       xlim = c(-4, 4), ylim = c(0, 1), 
     type = "l", lwd = 3, 
     col = "royalblue", main = "Item Charcteristic Curve (ICC)")
text(x= -1.5, y = 0.6, "M", col = "royalblue", cex = 2)
lines(theta, IRT(theta, b=b[2], 
                a = 1), 
      lty = 1, lwd=3, col = "magenta")
text(x= 1.5, y = 0.6, "F", col = "magenta", cex = 2)

## ----r------------------------------------------------------------------------
difficulty <- c(-1, 1)
par(mar = c(5,7,4,2) + 0.1) 
 theta <- theta <- seq(-7, 7, .001)
plot(theta, IRT(theta, b=difficulty[1]),
     cex.lab= 2, 
     cex.axis =1.5,
       xlab = expression(theta), ylab = expression(paste("P(", x[vi], " = 1)")),
       xlim = c(-5, 5), ylim = c(0, 1), 
     type = "l", lwd = 3, 
     col = "royalblue")

#item B
lines(theta, IRT(theta, b = difficulty[2]), 
      lty = 4, lwd=3, col = "magenta")

## ----r------------------------------------------------------------------------
difficulty <- c(1, 1)
par(mar = c(5,7,4,2) + 0.1) 
 theta <- theta <- seq(-7, 7, .001)
plot(theta, IRT(theta, b=difficulty[1], a = 1),
     cex.lab= 2, 
     cex.axis =1.5,
       xlab = expression(theta), ylab = expression(paste("P(", x[vi], " = 1)")),
       xlim = c(-5, 5), ylim = c(0, 1), 
     type = "l", lwd = 3, 
     col = "royalblue")

#item B
lines(theta, IRT(theta, b = difficulty[2], a = 1.5), 
      lty = 4, lwd=3, col = "magenta")

