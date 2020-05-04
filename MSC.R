##### Project: Unite seagrass conservation and tourism in Mauritius

#### Required packages
require(ggplot2)
require(esc)
require(meta)
require(forcats)

#### Set base theme for plotting
mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black"),
                 legend.key = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.text.align = 0,
                 legend.title = element_text(size = 12, face = "bold"),
                 text = element_text(family = "Helvetica Neue"))


#### Meta-analysis
### Load data
loss <- read.csv("~/PATH/loss.csv")

### Overall
## Calculate Hedges' g and associated variables
g <- with(loss, esc_mean_se(grp1m = mean.degraded, grp1se = se.degraded, grp1n = n.degraded, 
                            grp2m = mean.control, grp2se = se.control, grp2n = n.control))

## Add effect sizes, standard errors and 95% confidence intervals to data frame
loss <- data.frame(loss, g[c(1:2,4:5)])

## Compute overall effect size, 95% confidence interval and p value
m <- metagen(data = loss, es, se, studlab = id, prediction = T, sm = "SMD")
m # p < 0.001, z = -10.74


### Split data frame to extract combined effect sizes of groups
carbon <- loss[2:11,]
fauna <- loss[13:18,]
species <- loss[23:26,]
production <- loss[21:22,]
other <- loss[c(1,12,19:20),]


### Carbon
## Calculate Hedges' g and associated variables
g.c <- with(carbon, esc_mean_se(grp1m = mean.degraded, grp1se = se.degraded, grp1n = n.degraded, 
                                grp2m = mean.control, grp2se = se.control, grp2n = n.control))

## Add effect sizes, standard errors and 95% confidence intervals to data frame
carbon <- data.frame(carbon, g.c[c(1:2,4:5)])

## Reorder data frame
carbon <- carbon[order(carbon$es),]

## Compute overall effect size, 95% confidence interval and p value
m.c <- metagen(data = carbon, es, se, studlab = id, prediction = T, sm = "SMD")
m.c # p < 0.001, z = -6.99, g = -2.39

## Create new data frame with individual and combined effect sizes and 95% confidence intervals
df.c <- rbind(carbon[,c(1,12,14:15)], 
              data.frame(id = "Cc", 
                         es = m.c$TE.fixed, 
                         ci.lo = m.c$lower.fixed, 
                         ci.hi = m.c$upper.fixed))



### Fauna
## Calculate Hedges' g and associated variables
g.f <- with(fauna, esc_mean_se(grp1m = mean.degraded, grp1se = se.degraded, grp1n = n.degraded, 
                               grp2m = mean.control, grp2se = se.control, grp2n = n.control))

## Add effect sizes, standard errors and 95% confidence intervals to data frame
fauna <- data.frame(fauna, g.f[c(1:2,4:5)])

## Reorder data frame
fauna <- fauna[order(fauna$es),]

## Compute overall effect size, 95% confidence interval and p value
m.f <- metagen(data = fauna, es, se, studlab = id, prediction = T, sm = "SMD")
m.f # p < 0.001, z = -4.96, g = -1.78

## Create new data frame with individual and combined effect sizes and 95% confidence intervals
df.f <- rbind(fauna[,c(1,12,14:15)], 
              data.frame(id = "Cf", 
                         es = m.f$TE.fixed, 
                         ci.lo = m.f$lower.fixed, 
                         ci.hi = m.f$upper.fixed))



### Species
## Calculate Hedges' g and associated variables
g.s <- with(species, esc_mean_se(grp1m = mean.degraded, grp1se = se.degraded, grp1n = n.degraded, 
                                 grp2m = mean.control, grp2se = se.control, grp2n = n.control))

## Add effect sizes, standard errors and 95% confidence intervals to data frame
species <- data.frame(species, g.s[c(1:2,4:5)])

## Reorder data frame
species <- species[order(species$es),]

## Compute overall effect size, 95% confidence interval and p value
m.s <- metagen(data = species, es, se, studlab = id, prediction = T, sm = "SMD")
m.s # p = 0.0014, z = -3.19, g = -0.98

## Create new data frame with individual and combined effect sizes and 95% confidence intervals
df.s <- rbind(species[,c(1,12,14:15)], 
              data.frame(id = "Cs", 
                         es = m.s$TE.fixed, 
                         ci.lo = m.s$lower.fixed, 
                         ci.hi = m.s$upper.fixed))



### Production
## Calculate Hedges' g and associated variables
g.p <- with(production, esc_mean_se(grp1m = mean.degraded, grp1se = se.degraded, grp1n = n.degraded, 
                                    grp2m = mean.control, grp2se = se.control, grp2n = n.control))

## Add effect sizes, standard errors and 95% confidence intervals to data frame
production <- data.frame(production, g.p[c(1:2,4:5)])

## Reorder data frame
production <- production[order(production$es),]

## Compute overall effect size, 95% confidence interval and p value
m.p <- metagen(data = production, es, se, studlab = id, prediction = T, sm = "SMD")
m.p # p < 0.001, z = -4.33, g = -2.08

## Create new data frame with individual and combined effect sizes and 95% confidence intervals
df.p <- rbind(production[,c(1,12,14:15)], 
              data.frame(id = "Cp", 
                         es = m.p$TE.fixed, 
                         ci.lo = m.p$lower.fixed, 
                         ci.hi = m.p$upper.fixed))



### Other
## Calculate Hedges' g and associated variables
g.o <- with(other, esc_mean_se(grp1m = mean.degraded, grp1se = se.degraded, grp1n = n.degraded, 
                               grp2m = mean.control, grp2se = se.control, grp2n = n.control))

## Add effect sizes, standard errors and 95% confidence intervals to data frame
other <- data.frame(other, g.o[c(1:2,4:5)])

## Reorder data frame
other <- other[order(other$es),]

## Compute overall effect size, 95% confidence interval and p value
m.o <- metagen(data = other, es, se, studlab = id, prediction = T, sm = "SMD")
m.o # p < 0.001, z = -4.87, g = -2.04

## Create new data frame with individual and combined effect sizes and 95% confidence intervals
df.o <- rbind(other[,c(1,12,14:15)], 
              data.frame(id = "Co", 
                         es = m.o$TE.fixed, 
                         ci.lo = m.o$lower.fixed, 
                         ci.hi = m.o$upper.fixed))



### Create master data frame
ggloss <- rbind(df.c, df.p, df.f, df.s, df.o, 
                data.frame(id = "C", 
                           es = m$TE.fixed, 
                           ci.lo = m$lower.fixed, 
                           ci.hi = m$upper.fixed))

### Lock order
ggloss$id <- factor(ggloss$id, levels = ggloss$id)


### Plot data
loss.plot <- ggplot(ggloss, aes(fct_rev(id), es)) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi), size = 0.5,
                  colour = c(rep("#000000",10),"#e64715",
                             rep("#000000",2),"#e64715",
                             rep("#000000",6),"#e64715",
                             rep("#000000",4),"#e64715",
                             rep("#000000",4),rep("#e64715",2))) +
  ylab(expression("Effect of seagrass loss (Hedges' "*italic("g")*")")) +
  scale_x_discrete(labels = c("All studies",
                              "Combined",
                              expression("Connolly (1995)  "*
                                           italic("Zostera muelleri")),
                              expression("Bulleri et al. (2020)  "*
                                           italic("Zostera muelleri")),
                              expression("Polte and Asmus (2006)  "*
                                           italic("Zostera noltei")),
                              expression("Eklöf et al. (2011)  "*
                                           italic("Zostera noltei")),
                              "Combined",
                              expression("Borg et al. (2010)  "*
                                           italic("Posidonia oceanica")),
                              expression("Vanderklift and Jacoby (2003)  "*
                                           italic("Posidonia australis")),
                              expression("Pillay et al. (2010)  "*
                                           italic("Zostera capensis")),
                              expression("Reed and Hovel (2006)  "*
                                           italic("Zostera marina")),
                              "Combined",
                              expression("Borg et al. (2010)  "*
                                           italic("Posidonia oceanica")),
                              expression("Polte and Asmus (2006)  "*
                                           italic("Zostera noltei")),
                              expression("Githaiga et al. (2019)  "*
                                           italic("Thalassia hemprichii")),
                              expression("Githaiga et al. (2019)  "*
                                           italic("Thalassia hemprichii")),
                              expression("Reed and Hovel (2006)  "*
                                           italic("Zostera marina")),
                              expression("Pillay et al. (2010)  "*
                                           italic("Zostera capensis")),
                              "Combined",
                              expression("Stutes et al. (2007)  "*
                                           italic("Halodule wrightii")),
                              expression("Dahl et al. (2016)  "*
                                           italic("Thalassia hemprichii")),
                              "Combined",
                              expression("Macreadie et al. (2014)  "*
                                           italic("Zostera nigricaulis")),
                              expression("Oreska et al. (2017)  "*
                                           italic("Zostera marina")),
                              expression("Githaiga et al. (2019)  "*
                                           italic("Thalassia hemprichii")),
                              expression("Macreadie et al. (2015)  "*
                                           italic("Posidonia australis")),
                              expression("Barañano et al. (2018)  "*
                                           italic("Zostera marina")),
                              expression("Trevathan-Tackett et al. (2018)  "*
                                           italic("Thalassia testudinum")),
                              expression("Trevathan-Tackett et al. (2018)  "*
                                           italic("Halodule wrightii")),
                              expression("Borg et al. (2010)  "*
                                           italic("Posidonia oceanica")),
                              expression("Dahl et al. (2016)  "*
                                           italic("Thalassia hemprichii")),
                              expression("Marbà et al. (2015)  "*
                                           italic("Posidonia australis")))) +
  geom_hline(yintercept = c(0,5)) +
  geom_vline(xintercept = c(32.5,21.5,18.5,11.5,6.5,1.5)) +
  annotate("text", x = c(22.5,19.5,12.5,7.5,6,5,4,3), y = rep(-30.5,8), 
           label = c(expression(bold("Carbon storage")),
                     expression(bold("Net production")),
                     expression(bold("Faunal abundance")),
                     expression(bold("Species richness")),
                     "Wave attenuation","Egg abundance",
                     "Biotic resistance","Average biomass"),
           size = 4.285714, hjust = 0) +
  scale_y_continuous(breaks = seq(-30, 5, by = 5)) +
  coord_flip(ylim = c(-30, 3.39), xlim = c(1, 31.93)) +
  theme(axis.title.y = element_blank()) + # modify base theme
  mytheme

loss.plot # print (dimensions: 7 x 8 in)



#### Monetary valuation of private vs. public goods
### Load data
goods <- read.csv("~/PATH/goods.csv")

### Add merged variable
goods$factor <- with(data = goods, expr = paste(sector, treatment))


### Plot data
goods.plot <- ggplot(data = goods, aes(fct_relevel(factor, "private degradation",
                                                   "private conservation",
                                                   "public degradation"), USD.yr)) +
  geom_col(aes(fill = fct_relevel(source, "tourism","diving","fisheries",
                                  "restoration","emissions","sequestration",
                                  "nutrients")), width = 0.7) +
  scale_fill_manual(values = c("#fac67e","#467289","#a2d3ea",
                               "#afab00","#898b8e","#363538",
                               "#fedf00","#c5add0"),
                    labels = c("Tourism", "Scuba diving", "Fisheries",
                               "Seagrass restoration", expression("CO"[2]*" emissions"),
                               "Climate regulation", "Nutrient cycling", "Erosion control"),
                    guide = guide_legend(title = "Source")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2.5) +
  annotate("text", x = c(1.5,3.5,1,2,3,4), y = c(rep(9.69*10^8,2),rep(-9.49*10^8,4)),
           label = c("Private lens", "Public lens", "Degradation",
                     "Conservation", "Degradation", "Conservation"), size = 4.285714) +
  labs(y = expression("US $ yr"^-1)) +
  scale_y_continuous(breaks = seq(-1000000000, 1000000000, by = 200000000),
                     labels = c(expression("-10×10"^8),expression("-8×10"^8),expression("-6×10"^8),
                                expression("-4×10"^8),expression("-2×10"^8),0, expression("2×10"^8), 
                                expression("4×10"^8),expression("6×10"^8),expression("8×10"^8),
                                expression("10×10"^8))) +
  coord_cartesian(ylim = c(-910000000, 910000000)) +
  theme(legend.justification = "top",
      # legend.position = c(.26, .27),
        axis.ticks.x = element_blank(), # modify base theme
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank()) +
  mytheme

goods.plot # print (dimensions: 4.13 x 8 in)



#### Clean up
### Detach packages
detach(package:ggplot2)
detach(package:esc)
detach(package:meta)
detach(package:forcats)

### Clear environment, plots and console
rm(list = ls())
graphics.off()
cat("\014")


