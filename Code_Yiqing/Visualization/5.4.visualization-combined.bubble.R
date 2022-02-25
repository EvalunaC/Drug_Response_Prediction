library(tidyr)
library()
#########################################################
###         Clinical_ERBB2_GDSC2
load("/Users/evaluna/Downloads/Results-2021-12-10/ERBB2\ Status\ Validation/Clinical_ERBB2_GDSC2.RData")
pred <- preds_from_testFrame
# Clinical outcome\
# From "breast_cancer_analysis.html"
clinDataBrca <- read.delim("~/Downloads/nationwidechildrens.org_clinical_patient_brca.txt", as.is=T)
her2status <- clinDataBrca[, "her2_status_by_ihc"]
names(her2status) <- clinDataBrca[, "bcr_patient_barcode"]

sampleNames <- colnames(pred)
theTumorSamples <- which(substring(sampleNames, 14, 16) == "01A") # identify the tumor samples, tumor samples annotated as "01" by TCGA, normal samples as "10".
newNames <- gsub(".", "-", substring(colnames(pred), 1, 12), fixed=T)


bcaPreds2 <- pred

colnames(bcaPreds2) <- newNames
#bcaPreds <- bcaPreds[theTumorSamples] # Only include the tumor samples in this analysis. Results on normal samples are meaningless.
sampsInBothDatasets <- clinDataBrca[, "bcr_patient_barcode"][clinDataBrca[, "bcr_patient_barcode"] %in% newNames]
sampsInBothDatasets_TF <- clinDataBrca[, "bcr_patient_barcode"] %in% newNames

her2Neg <- her2status[her2status == "Negative"]
her2Pos <- her2status[her2status == "Positive"]

pred_long2 <- melt(bcaPreds2)

pred_long2 <- pred_long2[pred_long2$Var2%in%clinDataBrca[, "bcr_patient_barcode"],]

pred_long2$ERBB2 <- 1
pred_long2[pred_long2$Var2%in%names(her2Neg),]$ERBB2 <- "Negative"
pred_long2[pred_long2$Var2%in%names(her2Pos),]$ERBB2 <- "Positive"
pred_long2 <- pred_long2[pred_long2$ERBB2!=1,]

ERBB2_stat <- pred_long2 %>% group_by(Var1,ERBB2) %>% summarise(value = list(value),Var1=Var1,ERBB2=ERBB2) %>% distinct() %>%
  spread(ERBB2, value) %>% 
  group_by(Var1) %>%
  mutate(p_value = t.test(unlist(Negative), unlist(Positive))$p.value,
         p_value_neglog = -log(t.test(unlist(Negative), unlist(Positive))$p.value),
         mean_diff=mean(unlist(Negative))-mean(unlist(Positive)))

ERBB2_stat$p_value_neglog_scaled = scale(ERBB2_stat$p_value_neglog)
ERBB2_stat$mean_diff_scaled = ERBB2_stat$mean_diff/sd(ERBB2_stat$mean_diff)

ERBB2_stat <- ERBB2_stat[ERBB2_stat$Var1 %in%c("General Random Forest","RF+Lasso2019(split-conformal)",
                                               "EN GLM","GR paper linear Ridge",
                                               "Weighted K-nearest neighbors","K-nearest neighbors",
                                               "Lasso","Principle Component Regression","Partial Least Square",
                                               "Ridge GLM","Support Vector Machine","Tree Bag","h2o deep learning"),]#,"h2o deep learning"

ERBB2_stat$Var1 <- factor(ERBB2_stat$Var1,
                          levels = c("General Random Forest","RF+Lasso2019(split-conformal)",
                                     "Support Vector Machine","Tree Bag","Partial Least Square","Lasso",
                                     "EN GLM","Ridge GLM","h2o deep learning","Principle Component Regression",
                                     "K-nearest neighbors","Weighted K-nearest neighbors","GR paper linear Ridge"))

ERBB2_stat2 <- ERBB2_stat
ERBB2_stat2$dataset <- "GDSC2"

#ggplot( ERBB2_stat,aes(1,forcats::fct_rev(Var1))) +
#  geom_point(aes( color = mean_diff  , size =p_value_neglog_scaled)) +
#  scale_x_discrete(position = "bottom") +
#  scale_radius(range = c(0, 11)) +
#  scale_color_gradient2(low = "purple", mid = "white",midpoint=0,high = "darkgreen") +
#  theme(legend.position = "right", 
## panel.grid.major = element_blank(),
#        legend.text = element_text(size = 8),
#        legend.title = element_text(size = 8)) +
#  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = 0), 
#                             label.position = "right",
#                             title.position = "right", 
#                             order = 1),
#         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
#  labs(size = "Area = Scaled -log(P)", color = "Scaled mean difference", x = NULL, y = NULL)
#
#########################################################
###         Clinical_ERBB2_GDSC1
load("~/Downloads/Output 2/Results-2021-12-10/ERBB2 Status Validation/Clinical_ERBB2_GDSC1.RData")
preds <- as.data.frame(preds_from_testFrame$`Ridge GLM`)
colnames(preds) <- "Ridge GLM"
preds$`GR paper linear Ridge` <- preds_from_testFrame$`GR paper linear Ridge`
preds$`Random Forest` <- preds_from_testFrame$`Random Forest`
preds$`General Random Forest` <- preds_from_testFrame$`General Random Forest`
preds$`EN GLM` <- preds_from_testFrame$`EN GLM`
preds$`Principle Component Regression` <- preds_from_testFrame$`Principle Component Regression`
preds$`Partial Least Square` <- preds_from_testFrame$`Partial Least Square`
preds$`K-nearest neighbors` <- preds_from_testFrame$`K-nearest neighbors`
preds$`Partial Least Square` <- preds_from_testFrame$`Partial Least Square`
preds$`Support Vector Machine` <- preds_from_testFrame$`Support Vector Machine`
preds$`RF+Lasso2019(split-conformal)` <- preds_from_testFrame$`RF+Lasso2019(split-conformal)`
preds$`Tree Bag` <- preds_from_testFrame$`Tree Bag`
preds$`h2o deep learning` <- t(preds_from_testFrame$`h2o deep learning`)
preds$`Lasso` <- preds_from_testFrame$Lasso
bcaPreds2 <- t(preds)

colnames(bcaPreds2) <- newNames

pred_long <- melt(bcaPreds2)

pred_long <- pred_long[pred_long$Var2%in%clinDataBrca[, "bcr_patient_barcode"],]

pred_long$ERBB2 <- 1
pred_long[pred_long$Var2%in%names(her2Neg),]$ERBB2 <- "Negative"
pred_long[pred_long$Var2%in%names(her2Pos),]$ERBB2 <- "Positive"
pred_long <- pred_long[pred_long$ERBB2!=1,]


ERBB2_stat <- pred_long %>% group_by(Var1,ERBB2) %>% summarize(value = list(value),Var1=Var1,ERBB2=ERBB2) %>% distinct() %>%

  spread(ERBB2, value) %>% 
  group_by(Var1) %>%
  mutate(p_value = t.test(unlist(Negative), unlist(Positive))$p.value,
         p_value_neglog = -log(t.test(unlist(Negative), unlist(Positive))$p.value),
         mean_diff=mean(unlist(Negative))-mean(unlist(Positive)))


ERBB2_stat <- ERBB2_stat[ERBB2_stat$Var1 %in%c("General Random Forest","RF+Lasso2019(split-conformal)",
                                               "EN GLM","GR paper linear Ridge",
                                               "Weighted K-nearest neighbors","K-nearest neighbors",
                                               "Lasso","Principle Component Regression","Partial Least Square",
                                               "Ridge GLM","Support Vector Machine","Tree Bag","h2o deep learning"),]#,"h2o deep learning"

ERBB2_stat$Var1 <- factor(ERBB2_stat$Var1,
                          levels = c("General Random Forest","RF+Lasso2019(split-conformal)",
                                     "Support Vector Machine","Tree Bag","Partial Least Square","Lasso",
                                     "EN GLM","Ridge GLM","h2o deep learning","Principle Component Regression",
                                     "K-nearest neighbors","Weighted K-nearest neighbors","GR paper linear Ridge"))

ERBB2_stat$p_value_neglog_scaled = scale(ERBB2_stat$p_value_neglog)
ERBB2_stat$mean_diff_scaled = ERBB2_stat$mean_diff/sd(ERBB2_stat$mean_diff)


ERBB2_stat1 <- ERBB2_stat
ERBB2_stat1$dataset <- "GDSC1"

ERBB2_stat <- rbind(ERBB2_stat1,ERBB2_stat2)
ERBB2_stat$vali <- "ERBB2 Comparison"


pdf("/Users/evaluna/Downloads/Output\ 2/5.4.BubblePlot_ERBB2_20220105.pdf",width=4)

ggplot( ERBB2_stat,aes(1,forcats::fct_rev(Var1),fill=p_value_neglog,size=mean_diff_scaled )) +
  geom_point(shape=22,stroke=0.5) +
  scale_x_discrete(position = "bottom") +
  scale_radius(range = c(0, 10)) +
  scale_fill_gradient2(low = "purple", mid = "white",midpoint=0,high = "darkgreen") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = 0), 
                             label.position = "right",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
  labs(size = "Scaled mean difference ", color = "Area = -log(P)", x = NULL, y = NULL)+facet_grid(~ dataset,scales = "free")
dev.off()

### ### ### ### ### ### ### ### 
### Binning
breaks <- c(-1,0.0001,0.001,0.01,0.05,0.1,2)
# specify interval/bin labels
tags <- c("<0.0001","<0.001","<0.01", "<0.05", "<0.1", ">0.1")
# bucketing values into bins
ERBB2_stat$sig <- cut(ERBB2_stat$p_value, 
                      breaks=breaks, 
                      include.lowest=TRUE, 
                      right=FALSE, 
                      labels=tags)
# inspect bins
summary(ERBB2_stat$sig)
########
"
Discrete plotting with significance:
Size represent mean diff
"
library(RColorBrewer)
ggplot( ERBB2_stat,aes(1,forcats::fct_rev(Var1),fill=sig,size=mean_diff_scaled )) +
  geom_point(shape=22,stroke=0.5) +
  scale_x_discrete(position = "bottom") +
  scale_radius(range = c(-1, 13)) +
  scale_fill_brewer(palette = "Greens",direction = -1) + theme_bw()+
#  scale_fill_discrete(low = "purple", mid = "white",midpoint=0,high = "darkgreen") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = 0), 
                             label.position = "right",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
  labs(size = "Scaled mean difference ", fill = "Area = -log(P)", x = NULL, y = NULL)+facet_grid(~ dataset,scales = "free")
########
"
Discrete plotting with significance:
Size represent Nothing
"

library(colorspace)
choose_palette()
pdf("/Users/evaluna/Downloads/Output\ 2/5.4.BubblePlot_ERBB2_20220105_discrete.pdf",width=4)
ggplot( ERBB2_stat,aes(1,forcats::fct_rev(Var1),fill=sig)) +
  geom_point(shape=22,stroke=0.5,size=8) +
  scale_x_discrete(position = "bottom") +
  scale_fill_manual(values=c("#1B5E20", "#388E3C", "#66BB6A","#A5D6A7","#D1C4E9","#9575CD")) +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +facet_grid(~ dataset,scales = "free")+
  labs( fill = "Area = -log(P)", x = NULL, y = NULL)+facet_grid(~ dataset,scales = "free")
dev.off()  
  
########
"
Discrete plotting with significance:
Cleveland’s dot plot
"
pdf("/Users/evaluna/Downloads/Output\ 2/5.4.BubblePlot_ERBB2_20220106_dotplot.pdf")
ERBB2_stat$Var1 <- factor(ERBB2_stat$Var1,
                          levels = rev(c("General Random Forest","RF+Lasso2019(split-conformal)",
                                         "Support Vector Machine","Tree Bag","Partial Least Square","Lasso",
                                         "EN GLM","Ridge GLM","h2o deep learning","Principle Component Regression",
                                         "K-nearest neighbors","Weighted K-nearest neighbors","GR paper linear Ridge")))


ERBB2_stat$s <- ifelse(ERBB2_stat$dataset=="GDSC1",22,23)
library(ggpubr)

 ggdotchart(ERBB2_stat, x = "Var1", y = "mean_diff_scaled",
 #          facet.by = "dataset",
           shape="dataset",
           color = "sig",                                # Color by groups
            add="segments",
           add.params=list(linetype ="solid",size=1,color="sig"),
           palette = c("#1B5E20", "#388E3C", "#66BB6A","#A5D6A7","#D1C4E9","#9575CD"), # Custom color palette
           sorting = "none",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = "p_value_neglog" ,                                # Large dot size
#           y.text.col = TRUE                            # Color y text by groups
           ggtheme = theme_bw()                        # ggplot2 theme
)+scale_shape_manual(values = c(17,18))+
#  theme_cleveland() +                                    # Add dashed grids  
  geom_vline(xintercept = 0, linetype = 2, color = "black")
#ggpar(dot, xlim=c(-1,5))

 ggdotchart(ERBB2_stat, x = "Var1", y = "mean_diff_scaled",
            facet.by = "dataset",
            shape=18,
            color = "sig",                                # Color by groups
            palette = c("#1B5E20", "#388E3C", "#66BB6A","#A5D6A7","#D1C4E9","#9575CD"), # Custom color palette
            sorting = "none",                       # Sort value in descending order
            rotate = TRUE,                                # Rotate vertically
            add="segments",
            dot.size = "p_value_neglog" ,                                # Large dot size
#                       y.text.col = TRUE,                            # Color y text by groups
           add.params=list(linetype ="solid",size=1,color="sig"),
            ggtheme = theme_bw()                        # ggplot2 theme
 )+theme_cleveland() 
 dev.off()
 