## ----setup, include=FALSE-----------------------------------------------------
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
set.seed(999)

# codice di Dr Enrico Toffaini per la simulazione di dati con dif
n <- 500
nitem <- 10
percDif <- 50

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

ndif <- round(nitem*percDif/100) # item con dif
d <- data.frame(id=rep(1:(n*2),each=nitem),
                gender=rep(c("m","f"),each=n*nitem),
                ability=rep(rnorm(n*2,0,1),each=nitem),
                item=paste("item",rep(1:nitem,times=n*2),sep=""),
                level=rep(rnorm(nitem,0,1),times=n*2),
                dif=NA)
d$dif <- rep(c(rep(0,nitem-ndif),rnorm(ndif,0.5,0.5)),times=n*2)
d$logit <- d$ability - d$level + d$dif * ifelse(d$gender=="m",0.5,-0.5)
d$prob <- logit2prob(d$logit)
d$resp <- rbinom(nrow(d),1,d$prob)

d2 <- reshape(d[,!colnames(d)%in%c("logit","prob","level","dif")],direction="wide",varying=levels(as.factor(d$item)),
              v.names="resp",timevar="item",idvar="id")
temp = d2[, !colnames(d2) %in% c("id", "gender", "ability")]

temp = temp[unique(d$item)]
d2 = cbind( d2[, colnames(d2) %in% c("id", "gender", "ability")], 
            temp)
data = d2
data$ability = NULL



## ----eval = F-----------------------------------------------------------------
#  data = read.csv("Dati/difClass.csv", header = T, sep = ",")

## ----echo = T, out.height="60%"-----------------------------------------------

small_gender = data[, c("id", "gender")]
small_responses = data[, -2]

responses = reshape(small_responses, direction = "long", varying = c(2:ncol(small_responses)), idvar = "id", v.names = "response", timevar = "item", times = names(small_responses)[-1])
long_data = merge(small_gender, responses, by = "id")

p = data.frame(table(long_data$item, 
                     long_data$resp))
p$prop = p$Freq/nrow(data)
p$Var1 = factor(p$Var1, 
                levels = unique(long_data$item))
colnames(p)[2] = "correct"
ggplot(p, 
       aes(x = Var1, y=prop,  
 
           fill = correct)) + geom_bar(stat = "identity") + 
  theme(axis.text = element_text(angle = 90)) + geom_hline(aes(yintercept=.50))



## -----------------------------------------------------------------------------
p = data.frame(table(long_data$item, long_data$resp, long_data$gender))
p$prop = p$Freq/nrow(data)
p$Var1 = factor(p$Var1, 
                levels = unique(long_data$item))
colnames(p)[3] = "gender"
ggplot(p[p$Var2 %in% 1,], 
       aes(x = Var1, y=prop, fill = gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme(axis.text = element_text(angle = 90)) + 
  geom_hline(aes(yintercept=.50)) 

## -----------------------------------------------------------------------------

m1pl = tam.mml(data[, grep("item", colnames(data))], verbose = F)

m2pl = tam.mml.2pl(data[, grep("item", colnames(data))], irtmodel = "2PL", verbose = F)

m3pl = tam.mml.3pl(data[, grep("item", colnames(data))], est.guess = grep("item", colnames(data)),
                     verbose = F)

IRT.compareModels(m1pl, m2pl, m3pl)


## -----------------------------------------------------------------------------
summary(m1pl)

## -----------------------------------------------------------------------------
f.m1pl = tam.modelfit(m1pl, progress = F)

f.m1pl$statlist



f.m1pl$modelfit.test

## -----------------------------------------------------------------------------
item.fit.1pl = IRT.itemfit(m1pl)

item.fit.1pl$chisquare_stat

item.fit.1pl$RMSD

item.fit.1pl$RMSD_summary


## -----------------------------------------------------------------------------
est_theta = IRT.factor.scores(m1pl)$EAP
lrt_dif = difGenLogistic(data[, !colnames(data) %in% c("id", "gender")],
                           group = as.factor(data$gender),
                           focal.names = "f",
                           type = "udif",
                           alpha = .001,
                           p.adjust.method = "BH",
                           match = est_theta,
                           criterion = "LRT")
lrt_dif

## -----------------------------------------------------------------------------
rajDif = difRaju(data[, !colnames(data) %in% c("id")], 
                  group = "gender", 
                  focal.name = "f", 
                  model = "1PL", 
                  p.adjust.method = "BH")
rajDif


## -----------------------------------------------------------------------------
lordDif = difLord(data[, !colnames(data) %in% "id"], 
                  group = "gender", 
                  focal.name = "f", 
                  model = "1PL", 
                  p.adjust.method = "BH")
lordDif

## ----eval = FALSE-------------------------------------------------------------
#  lordDif$itemParInit

## -----------------------------------------------------------------------------
item_par = lordDif$itemParInit

item_par[1:10, ]


## -----------------------------------------------------------------------------
item_par[11:nrow(item_par), ]

## -----------------------------------------------------------------------------
itemFR = data.frame(cbind(item_par[11:nrow(item_par), ], 
               item_par[1:10, ]))
colnames(itemFR)[c(1,3)] = paste0(rep("b",2),
                                  c("F", "R"))
itemFR$dif =itemFR$bF - itemFR$bR 
itemFR

## -----------------------------------------------------------------------------
itemFR$constant = mean(itemFR$bR) - mean(itemFR$bF)

itemFR$new.bR = itemFR$bR - itemFR$constant

itemFR$DIF.correct = itemFR$bF- itemFR$new.bR

itemFR

