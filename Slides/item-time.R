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
# data = read.csv("data/itemClass.csv", header = T, sep = ",")
# prop_item = data.frame(item = names(colMeans(data[, -c(1:2)])),
#            proportion = colMeans(data[, -c(1:2)]))
# 
# ggplot(prop_item,
#        aes(x = reorder(item, proportion),
#            y = proportion), color = item) + geom_bar(stat = "identity") + theme_light() +
#   ylab("Proportion correct") + ylim(0, 1) +
#   theme(legend.position = "none",
#         axis.title = element_text(size = 26),
#         axis.title.x = element_blank(),
#         axis.text = element_text(size = 22))

## ----r echo = FALSE-----------------------------------------------------------
m1pl = tam.mml(data[, grep("item", colnames(data))], verbose = F)
m2pl = tam.mml.2pl(data[, grep("item", colnames(data))], irtmodel = "2PL", verbose = F)
m3pl = tam.mml.3pl(data[, grep("item", colnames(data))], est.guess = grep("item", colnames(data)), verbose = F)
IRT.compareModels(m1pl, m2pl, m3pl) 

## ----r eval =FALSE, echo = TRUE-----------------------------------------------
# m1pl = tam.mml(data[, grep("item", colnames(data))], verbose = F)
# m2pl = tam.mml.2pl(data[, grep("item", colnames(data))], irtmodel = "2PL", verbose = F)
# m3pl = tam.mml.3pl(data[, grep("item", colnames(data))], est.guess = grep("item", colnames(data)), verbose = F)
# IRT.compareModels(m1pl, m2pl, m3pl)
# 

## ----r echo = TRUE------------------------------------------------------------
fit_m1pl = tam.modelfit(m1pl, progress = F)
fit_m1pl$statlist 

## ----r out.lines = 20---------------------------------------------------------
item_fit_1pl = IRT.itemfit(m1pl)
str(item_fit_1pl)

## ----r echo = TRUE------------------------------------------------------------
item_fit_1pl$chisquare_stat



## ----r echo = TRUE------------------------------------------------------------
item_fit_1pl$RMSD_summary 
item_fit_1pl$RMSD 

## ----r------------------------------------------------------------------------
b = c(-0.5616, -0.07)
a = c(1,1)


par(mar = c(5,7,4,2) + 0.1) 
plot(theta, IRT(theta, b = b[1], a = a[1]),
     cex.lab= 2.5, 
     cex.axis =1.5, cex.main = 3,
       xlab = expression(theta), ylab = expression(paste("P(", x[ip], " = 1|", theta, ", ", b[i], ")")),
       xlim = c(-4, 4), ylim = c(0, 1), 
     type = "l", lwd = 3, 
     col = "royalblue")
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
       xlab = expression(theta), 
     ylab = expression(paste("P(", x[ip], " = 1|", theta, ", ", b[i], ")")),
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
       xlab = expression(theta), ylab = expression(paste("P(", x[ip], " = 1|", theta, ", ", b[i], ")")),
       xlim = c(-5, 5), ylim = c(0, 1), 
     type = "l", lwd = 3, 
     col = "royalblue")

#item B
lines(theta, IRT(theta, b = difficulty[2], a = 1.5), 
      lty = 4, lwd=3, col = "magenta")

## ----r------------------------------------------------------------------------
data = read.csv("data/difClass.csv", header = T, sep = ",")
head(data)

## ----r eval = T, echo = FALSE-------------------------------------------------
long = pivot_longer(data, !1:2, names_to = "item", 
             values_to = "correct")
prop_gender = long %>% 
  group_by(item, gender) %>%  
  summarise(prop = mean(correct), sd = sd(correct))

ggplot(prop_gender, 
       aes( x = item, y= prop, fill = gender)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ylim(0,1) + 
  theme_light() + ylab("Item probability") + 
  theme(legend.direction="horizontal", 
        legend.position = c(.5,.9), 
        legend.title = element_blank(), 
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12), 
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 12)) + 
  geom_hline(yintercept = .25, linetype = 2) +
  geom_hline(yintercept = .5, linetype = 2) +
  geom_hline(yintercept = .75, linetype = 2)


## ----r eval = F, echo = TRUE--------------------------------------------------
# long = pivot_longer(data, !1:2, names_to = "item",
#              values_to = "correct")
# prop_gender = long %>%
#   group_by(item, gender) %>%
#   summarise(prop = mean(correct), sd = sd(correct))
# 
# ggplot(prop_gender,
#        aes( x = item, y= prop, fill = gender)) + geom_bar(stat = "identity", position = position_dodge()) + ylim(0,1)
# 

## ----r------------------------------------------------------------------------
m1pl = tam.mml(data[, grep("item", colnames(data))], verbose = F)
m2pl = tam.mml.2pl(data[, grep("item", colnames(data))], irtmodel = "2PL", verbose = F)
m3pl = tam.mml.3pl(data[, grep("item", colnames(data))], est.guess = grep("item", colnames(data)),
                     verbose = F)
IRT.compareModels(m1pl, m2pl, m3pl)

## ----r------------------------------------------------------------------------
fit_m1pl = tam.modelfit(m1pl, progress = F)
fit_m1pl$statlist


## ----r------------------------------------------------------------------------
item_fit_1pl = IRT.itemfit(m1pl)
item_fit_1pl$RMSD_summary
 fit_m1pl$Q3_summary

## ----r eval = FALSE-----------------------------------------------------------
# est_theta = IRT.factor.scores(m1pl)$EAP
# lrt_dif = difGenLogistic(data[, !colnames(data) %in% c("id", "gender")],
#                            group = as.factor(data$gender), focal.names = "f",
#                            type = "udif",
#                            alpha = .001, p.adjust.method = "BH",
#                            match = est_theta,
#                            criterion = "LRT")

## ----r echo = FALSE-----------------------------------------------------------
est_theta = IRT.factor.scores(m1pl)$EAP
lrt_dif = difGenLogistic(data[, !colnames(data) %in% c("id", "gender")],
                           group = as.factor(data$gender), focal.names = "f",
                           type = "udif",
                           alpha = .001, p.adjust.method = "BH",
                           match = est_theta,
                           criterion = "LRT")
lrt_dif

## ----r------------------------------------------------------------------------
rajDif = difRaju(data[, !colnames(data) %in% c("id")], 
                  group = "gender",  focal.name = "f", 
                  model = "1PL", 
                  alpha = .001, p.adjust.method = "BH")

## ----r echo = FALSE-----------------------------------------------------------
rajDif = difRaju(data[, !colnames(data) %in% c("id")], 
                  group = "gender",  focal.name = "f", 
                  model = "1PL", 
                  alpha = .001, p.adjust.method = "BH")
rajDif

## ----r------------------------------------------------------------------------
lordDif = difLord(data[, !colnames(data) %in% "id"], 
                  group = "gender",  focal.name = "f", 
                  model = "1PL", 
                   alpha = .001, p.adjust.method = "BH")

## ----r echo = FALSE-----------------------------------------------------------
lordDif = difLord(data[, !colnames(data) %in% "id"], 
                  group = "gender",  focal.name = "f", 
                  model = "1PL", 
                   alpha = .001, p.adjust.method = "BH")
lordDif

## ----r eval = FALSE-----------------------------------------------------------
# lordDif$itemParInit

## ----r------------------------------------------------------------------------
item_par = lordDif$itemParInit
item_par[1:10, ]

## ----r------------------------------------------------------------------------
item_par[11:nrow(item_par), ]

## ----r eval = FALSE-----------------------------------------------------------
# itemFR = data.frame(cbind(item_par[11:nrow(item_par), ], item_par[1:10, ]))
# colnames(itemFR)[c(1,3)] = paste0(rep("b",2),  c("F", "R"))
# itemFR$constant = mean(itemFR$bR) - mean(itemFR$bF)
# itemFR$new_bR = itemFR$bR - itemFR$constant
# itemFR$DIF_correct = itemFR$bF- itemFR$new_bR
# itemFR

## ----r eval = TRUE, echo = FALSE----------------------------------------------
itemFR = data.frame(cbind(item_par[11:nrow(item_par), ], item_par[1:10, ]))
colnames(itemFR)[c(1,3)] = paste0(rep("b",2),  c("F", "R"))
itemFR$constant = mean(itemFR$bR) - mean(itemFR$bF)
itemFR$new_bR = itemFR$bR - itemFR$constant
itemFR$DIF_correct = itemFR$bF- itemFR$new_bR
itemFR

## ----r------------------------------------------------------------------------
my_items = data.frame(items = rownames(itemFR), 
                      focal = itemFR$bF, 
                      reference = itemFR$bR)
my_se = data.frame(items = rownames(itemFR), 
                      focal = itemFR$se.b., 
                      reference = itemFR$se.b..1)
my_items = pivot_longer(my_items, 
             cols = !items, 
             names_to = "group", 
             values_to = "estimate")

my_se = pivot_longer(my_se, 
             cols = !items, 
             names_to = "group", 
             values_to = "se")

my_items = merge(my_items, my_se)

ggplot(my_items, 
       aes(x = estimate, y = reorder(items, estimate), 
           color = group)) + geom_point() +
  geom_errorbarh(aes(xmin = estimate -se, xmax = estimate + se)) + theme_light() +
  theme(legend.position = c(.10, .6), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 24), 
        axis.text = element_text(size = 24), axis.title.y = element_blank())


## ----r eval = F, echo = TRUE--------------------------------------------------
# my_items = data.frame(items = rownames(itemFR),
#                       focal = itemFR$bF,
#                       reference = itemFR$bR)
# my_se = data.frame(items = rownames(itemFR),
#                       focal = itemFR$se.b.,
#                       reference = itemFR$se.b..1)
# my_items = pivot_longer(my_items,
#              cols = !items,
#              names_to = "group",
#              values_to = "estimate")
# 
# my_se = pivot_longer(my_se,
#              cols = !items,
#              names_to = "group",
#              values_to = "se")
# 
# my_items = merge(my_items, my_se)
# 
# ggplot(my_items,
#        aes(x = estimate, y = reorder(items, estimate),
#            color = group)) + geom_point() +
#   geom_errorbarh(aes(xmin = estimate -se, xmax = estimate + se)) + theme_light() +
#   theme(legend.position = c(.10, .6),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 24),
#         axis.text = element_text(size = 24), axis.title.y = element_blank())

