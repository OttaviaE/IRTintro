library(TAM)
library(mokken)
library(lavaan)
library(difR)
library(sirt)
library(ggplot2)
library(tidyverse)
library(knitr)

# -----------
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


# -----------
m1pl = tam.mml(data[, grep("item", colnames(data))], verbose = F)
m2pl = tam.mml.2pl(data[, grep("item", colnames(data))], irtmodel = "2PL", verbose = F)
m3pl = tam.mml.3pl(data[, grep("item", colnames(data))], est.guess = grep("item", colnames(data)), verbose = F)
IRT.compareModels(m1pl, m2pl, m3pl)

# -----------
fit_m1pl = tam.modelfit(m1pl, progress = F)

fit_m1pl$statlist %>% kable(align = "c",
                            row.names = FALSE,
                            digits = rep(2, 4))

# -----------
item_fit_1pl = IRT.itemfit(m1pl)
str(item_fit_1pl)
item_fit_1pl$chisquare_stat


# -----------
item_fit_1pl$RMSD_summary %>% kable(aling = "c", digits = 2)
item_fit_1pl$RMSD %>% kable(aling = "c", digits = 2)


# -----------
data = read.csv("data/difClass.csv", header = T, sep = ",")
head(data)

# -----------
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

# -----------
m1pl = tam.mml(data[, grep("item", colnames(data))], verbose = F)
m2pl = tam.mml.2pl(data[, grep("item", colnames(data))], irtmodel = "2PL", verbose = F)
m3pl = tam.mml.3pl(data[, grep("item", colnames(data))], est.guess = grep("item", colnames(data)),
                     verbose = F)
IRT.compareModels(m1pl, m2pl, m3pl)

# -----------
fit_m1pl = tam.modelfit(m1pl, progress = F)
fit_m1pl$statlist

# -----------
item_fit_1pl = IRT.itemfit(m1pl)
item_fit_1pl$RMSD_summary
 fit_m1pl$Q3_summary

# -----------
est_theta = IRT.factor.scores(m1pl)$EAP
lrt_dif = difGenLogistic(data[, !colnames(data) %in% c("id", "gender")],
                           group = as.factor(data$gender), focal.names = "f",
                           type = "udif",
                           alpha = .001, p.adjust.method = "BH",
                           match = est_theta,
                           criterion = "LRT")
lrt_dif

# -----------
rajDif = difRaju(data[, !colnames(data) %in% c("id")],
                  group = "gender",  focal.name = "f",
                  model = "1PL",
                  alpha = .001, p.adjust.method = "BH")
rajDif

# -----------
lordDif = difLord(data[, !colnames(data) %in% "id"],
                  group = "gender",  focal.name = "f",
                  model = "1PL",
                   alpha = .001, p.adjust.method = "BH")
# -----------
item_par = lordDif$itemParInit
item_par[1:10, ]
item_par[11:nrow(item_par), ]

# -----------
itemFR = data.frame(cbind(item_par[11:nrow(item_par), ], item_par[1:10, ]))
colnames(itemFR)[c(1,3)] = paste0(rep("b",2),  c("F", "R"))
itemFR$constant = mean(itemFR$bR) - mean(itemFR$bF)
itemFR$new_bR = itemFR$bR - itemFR$constant
itemFR$DIF_correct = itemFR$bF- itemFR$new_bR
itemFR

