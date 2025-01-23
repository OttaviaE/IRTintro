
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

IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}




## -----------------------------------------------------------------------------
# function for plotting the ICCs of the items
# returns a list with a ggplot graph (icc_graph) and a data set with the data
# used for plotting the ICC (icc_data)
irt_icc = function(model) {
  item_par = model$item
  est_theta = seq(-4,4, length.out=1000)
  item_prob = list()
  if (any(grep("guess", colnames(item_par))) == F) {
    for (i in 1:nrow(item_par)) {
      item_prob[[i]] = data.frame(theta = est_theta)
      item_prob[[i]]$it_p = IRT(item_prob[[i]]$theta,
                          b = item_par[i, "xsi.item"],
                          a = item_par[i, "B.Cat1.Dim1"])
      item_prob[[i]]$item = item_par[i, "item"]

}
  } else {
     for (i in 1:nrow(item_par)) {
      item_prob[[i]] = data.frame(theta = est_theta)
      item_prob[[i]]$it_p = IRT(item_prob[[i]]$theta,
                          b = item_par[i, "AXsi_.Cat1"],
                          a = item_par[i, "B.Cat1.Dim1"],
                          c = item_par[i, "guess"])
      item_prob[[i]]$item = item_par[i, "item"]

}
  }
  p = do.call("rbind", item_prob)
  gp = ggplot(p,
       aes(x = theta, y = it_p, group = item, col =
             item)) + geom_line(lwd = 1)
  object = list(icc_data = p,
              icc_graph = gp)

return(object)
}

## -----------------------------------------------------------------------------
irt_iif = function(model) {
  est_theta = IRT.factor.scores(model,
                              type = "EAP")$EAP
ii = IRT.informationCurves(model, theta = est_theta)

test_info = data.frame(theta = est_theta,
               info = ii$test_info_curve,
               se = ii$se_curve)

iif_info = list()
for(i in 1:nrow(ii$info_curves_item)) {
    iif_info[[i]] = data.frame(theta = est_theta)
    iif_info[[i]]$ii_item = ii$info_curves_item[i, ]
    iif_info[[i]]$item = dimnames(ii$info_curves_item)[[1]][i]
}

dat_info = do.call("rbind", iif_info)

info_tot = list(test_info = test_info,
                item_info = dat_info)

return(info_tot)
}



data = read.csv("data/dataES_stima.csv")
head(data)


vis_data = data
vis_data$id = paste0("s", 1:nrow(data))

vis_data = vis_data[, c(ncol(vis_data), 1:ncol(data))]
vis_data = pivot_longer(vis_data, names_to = "item", cols = -1)

prop_data = vis_data %>%
  group_by(item) %>%
  summarise(proportion = mean(value))

ggplot(prop_data,
       aes(x = reorder(item, proportion), y=proportion,
           fill = item)) + geom_bar(stat = "identity") +
  theme(axis.text = element_text(angle = 90)) + geom_hline(aes(yintercept=.50), linetype = 2) + ylab("Proportion correct") + xlab("") +
  theme_light() + ylim(0, 1) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 22))

## -----------------------------------------------------------------------------
sbj_data = data
sbj_data$id = paste0("s", 1:nrow(data))
sbj_data = sbj_data[, c(ncol(sbj_data), 1:ncol(data))]

sbj_data$sum = rowSums(sbj_data[,-1])

boxplot(sbj_data$sum)


## -----------------------------------------------------------------------------

item_lab = paste(colnames(data), collapse = " + ")
form = paste("latent =~", item_lab)

form


## -----------------------------------------------------------------------------
model = cfa(form, data = data,  ordered = colnames(data))
summary(model, fit.measures = T)

## -----------------------------------------------------------------------------
library(mokken)
mono_check = check.monotonicity(data)
summary(mono_check)

# l'item 1 è al limite, andrebbe eliminato ----
old_data = data

data = old_data[, !colnames(data) %in% "I01"]

mono_check = check.monotonicity(data)
summary(mono_check)

#a desso vanno tutti bene
item_lab = paste(colnames(data), collapse = " + ")
form = paste("latent =~", item_lab)

form

model = cfa(form, data = data,  ordered = colnames(data))
summary(model, fit.measures = T)

# migliroa anche la fit del modello

## -----------------------------------------------------------------------------
m1pl = tam.mml(data, verbose = F)

m2pl = tam.mml.2pl(data, irtmodel = "2PL", verbose = F)

m3pl = tam.mml.3pl(data, est.guess = colnames(data),
                     verbose = F)


## -----------------------------------------------------------------------------
IRT.compareModels(m1pl, m2pl, m3pl)

## -----------------------------------------------------------------------------
f.m1pl = tam.modelfit(m1pl, progress = F)
f.m1pl$statlist

f.m1pl$modelfit.test

## -----------------------------------------------------------------------------
fit_m1pl = tam.modelfit(m1pl, progress = F)

fit_m1pl$statlist



fit_m1pl$modelfit.test


## -----------------------------------------------------------------------------
fit_m1pl$Q3_summary

## ----eval = TRUE, echo = FALSE, out.width="60%"-------------------------------
icc_1pl = irt_icc(m1pl)$icc_data
item = m1pl$item_irt
icc_1pl = merge(icc_1pl, item)
icc_1pl$item_label = paste0(icc_1pl$item, ", b =",
                            round(icc_1pl$beta, 2), ", a = ", round(icc_1pl$alpha, 2))

ggplot(icc_1pl,
       aes(x = theta, y = it_p, group = item_label, color = item_label)) +
  geom_line(linewidth = 1.2) + theme_classic() +
  ylab("P(x =1)") + xlab(expression(theta)) +
  theme(legend.position = c(.15, .80),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 26)) +
  geom_hline(yintercept = .50, linetype = 2)



## ----echo = FALSE, out.width="60%"--------------------------------------------
label_item = icc_1pl[, c("item", "item_label")]
label_item = label_item %>% distinct()

info1pl = irt_iif(m1pl)
item_info = info1pl$item_info
item_info = merge(item_info, label_item)

ggplot(item_info,
       aes(x = theta, y = ii_item, group = item_label, color = item_label)) + geom_line(lwd = 1) + theme_light()+ xlab(expression(theta)) + ylab("Info")+
  theme(legend.position = c(.15, .80),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 26)) +
  geom_hline(yintercept = .50, linetype = 2)



## ----out.width="40%"----------------------------------------------------------

ggplot(info1pl$test_info,
       aes(x = theta, y = info)) + geom_line(lwd = 2) + theme_bw()



## ----out.width="40%"----------------------------------------------------------

ggplot(info1pl$test_info,
       aes(x = theta, y = se, col = "red")) + geom_line(lwd = 2) + theme_bw() + theme(legend.position = "none")



