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


