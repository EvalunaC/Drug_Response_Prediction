library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)

PCC_Combine_CTRP1 <- read.csv("/Users/evaluna/Downloads/Output\ 2/Results-2021-12-10/preds28/PCC/PCC_Combine_CTRP1.csv",header=TRUE)
PCC_Combine_CTRP2 <- read.csv("/Users/evaluna/Downloads/Output\ 2/Results-2021-12-10/preds28/PCC/PCC_Combine_CTRP2.csv",header=TRUE)
PCC_Combine_GDSC1 <- read.csv("/Users/evaluna/Downloads/Output\ 2/Results-2021-12-10/preds28/PCC/PCC_Combine_GDSC1.csv",header=TRUE)
PCC_Combine_GDSC2 <- read.csv("/Users/evaluna/Downloads/Output\ 2/Results-2021-12-10/preds28/PCC/PCC_Combine_GDSC2.csv",header=TRUE)

PCC_Combine_CTRP1 <- data.frame(method=PCC_Combine_CTRP1$method,drug=PCC_Combine_CTRP1$drug,variable="PCC",value=PCC_Combine_CTRP1$PCC_CV28,dataset="CTRP1")
PCC_Combine_CTRP2 <- data.frame(method=PCC_Combine_CTRP2$method,drug=PCC_Combine_CTRP2$drug,variable="PCC",value=PCC_Combine_CTRP2$PCC_CV28,dataset="CTRP2")
PCC_Combine_GDSC1 <- data.frame(method=PCC_Combine_GDSC1$method,drug=PCC_Combine_GDSC1$drug,variable="PCC",value=PCC_Combine_GDSC1$PCC_CV28,dataset="GDSC1")
PCC_Combine_GDSC2 <- data.frame(method=PCC_Combine_GDSC2$method,drug=PCC_Combine_GDSC2$drug,variable="PCC",value=PCC_Combine_GDSC2$PCC_CV28,dataset="GDSC2")

pcc <- rbind(PCC_Combine_CTRP1,PCC_Combine_CTRP2,PCC_Combine_GDSC1,PCC_Combine_GDSC2)

Stats_Combine_CTRP1 <- read.csv("/Users/evaluna/Downloads/Output\ 2/Results-2021-12-10/preds28/Stats/Stats_Combine_CTRP1.csv",header=TRUE)
Stats_Combine_CTRP2 <- read.csv("/Users/evaluna/Downloads/Output\ 2/Results-2021-12-10/preds28/Stats/Stats_Combine_CTRP2.csv",header=TRUE)
Stats_Combine_GDSC1 <- read.csv("/Users/evaluna/Downloads/Output\ 2/Results-2021-12-10/preds28/Stats/Stats_Combine_GDSC1.csv",header=TRUE)
Stats_Combine_GDSC2 <- read.csv("/Users/evaluna/Downloads/Output\ 2/Results-2021-12-10/preds28/Stats/Stats_Combine_GDSC2.csv",header=TRUE)


Stats_Combine_CTRP1 <- reshape::melt(as.data.frame(Stats_Combine_CTRP1[-1]), id=1:2)
Stats_Combine_CTRP2 <- reshape::melt(as.data.frame(Stats_Combine_CTRP2[-1]), id=1:2)
Stats_Combine_GDSC1 <- reshape::melt(as.data.frame(Stats_Combine_GDSC1[-1]), id=1:2)
Stats_Combine_GDSC2 <- reshape::melt(as.data.frame(Stats_Combine_GDSC2[-1]), id=1:2)


Stats_Combine_CTRP1$dataset="CTRP1"
Stats_Combine_CTRP2$dataset="CTRP2"
Stats_Combine_GDSC1$dataset="GDSC1"
Stats_Combine_GDSC2$dataset="GDSC2"

stat <- rbind(Stats_Combine_CTRP1,Stats_Combine_CTRP2,Stats_Combine_GDSC1,Stats_Combine_GDSC2)
data = rbind(stat,pcc)
#data <- data[data$value<=2 & data$value>=-0.3,] 


data_filter%>%
  ggplot(aes(x = value_scaled, y = method, fill = method,color=method_col)) +scale_y_discrete(limits=rev)+
  geom_density_ridges(quantile_lines = TRUE, alpha = 0.75,na.rm = TRUE,
                      quantiles = 2,size = 1)+facet_grid(~ variable+dataset,scales = "free")+#xlim(c(-1,3))
  scale_color_manual(values = c("#F70020","#072C8F"))



#ggplot(stat_scaled, aes(variable, forcats::fct_rev(method), fill = median_scaled, size = Q75range_scaled)) +
#  geom_point(shape = 21, stroke = 0) +
##  geom_hline(yintercept = seq(.5, 4.5, 1), size = .2) +
#  scale_x_discrete(position = "bottom") +
#  scale_radius(range = c(0, 20)) +
#  scale_fill_gradient(low = "blue", high = "red",breaks = c(-0.1, 0, 0.1), labels = c("Bad", "OK", "Great"), ) +
#  theme(legend.position = "bottom", 
#        panel.grid.major = element_blank(),
#        legend.text = element_text(size = 8),
#        legend.title = element_text(size = 8)) +
#  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
#                             label.position = "bottom",
#                             title.position = "right", 
#                             order = 1),
#         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
#  labs(size = "Area = Time Spent", fill = "Score:", x = NULL, y = NULL)+facet_grid(~ dataset,scales = "free")



stat_scaled2<- data_filter%>%
  group_by(variable,method) %>% 
  summarise(stat.mean=mean(as.double(value_scaled)),stat.sd=-sd(as.double(value_scaled)))

ggplot(stat_scaled2, aes(variable, forcats::fct_rev(method), fill = stat.mean, size = stat.sd)) +
  geom_point(shape = 21, stroke = 0) +
  #  geom_hline(yintercept = seq(.5, 4.5, 1), size = .2) +
  scale_x_discrete(position = "bottom") +
  scale_radius(range = c(0, 20)) +
  scale_fill_gradient(low = "blue", high = "red",breaks = c(-0.1, 0, 0.1), labels = c("Bad", "OK", "Great"), ) +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
  labs(size = "Area = Time Spent", fill = "Score:", x = NULL, y = NULL)




ggplot(stat_scaled, aes(x = variable, y = method)) + 
  geom_point(aes(size = stat.sd, fill = stat.mean), alpha = 0.75, shape = 21) + 
  scale_radius(range = c(0, 20)) +
  scale_fill_gradient(low = "blue", high = "red") +
  scale_size_continuous(limits = c(-1.05, -0.8), range = c(1,17) ) + 
  labs( x= "", y = "", size = "Relative Abundance (%)", fill = "")  + 
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 90, vjust = 0.3, hjust = 1), 
        axis.text.y = element_text(colour = "black", face = "bold", size = 11), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 12, face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
        legend.position = "right")
#  scale_fill_manual(values = stat.mean)  
#  scale_y_discrete(limits = rev(levels(stat_scaled$method))) 



ggplot(stat_scaled, aes(x=method, y=stat.mean, label=stat.mean)) + 
  geom_point(stat='identity', aes(col=dataset,size=stat.sd))  +
  scale_color_manual(name="stat.mean", 
                     labels = c("CTRP1", "CTRP2","GDSC1","GDSC2"), 
                     values = c("CTRP2"="#FB9701","GDSC1"="#1A7D00","GDSC2"="#072C8F")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()+facet_grid(~ variable+dataset,scales = "free",margins = "dataset")






numbers <- 1:96
num_to_well(numbers)














#####################################################################
## Summarize the stats by group 
(summ_bydataset <- data[!is.na(as.double(data$value)),] %>%
  group_by(dataset,variable,method) %>%
  summarise(stat.mean=mean(as.double(value)),stat.sd=sd(as.double(value)))
  %>% arrange(desc(dataset, variable,method)))

(summ_overall <- data[!is.na(as.double(data$value)),] %>%
    group_by(variable,method) %>%
    summarise(stat.mean=mean(as.double(value)),stat.sd=sd(as.double(value))))


summary_sep_clean <- summ_bydataset[summ_bydataset$method
                                    %in%
                                      c("rfG","rfinv2","EN","GR","KKNN","KNN",
                                       "Lasso","pcr","pls","ridgeglm","svm","treebag","NN"),]

library(tidyr)

data_scaled <- data[,c(1,3,4,5)] %>% group_by(dataset,variable) %>% mutate(value_scaled = scale(value))


data_wide <- dcast(data[,c(1,3,4,5)], method ~ dataset+variable, value.var="value")
write.csv(summ_bydataset,"C:/Users/EYC/Dropbox/2020.DrugResponse/Results/stat_bydataset.csv",quote = FALSE)
write.csv(summ_overall,"C:/Users/EYC/Dropbox/2020.DrugResponse/Results/stat_overall.csv",quote = FALSE)
######################################################
# filter and plot circle heat map
stat_select <- stat[stat$method
                   %in%
                    c("rfG","rfinv2","ENglm","GR","KKNN","KNN",
                      "Lasso","pcr","pls","ridgeglm","svm","treebag","NN"),]
 
data_nona <- data[!is.na(as.double(data$value)),]

summary_reshape <- data_nona[,c(1,3,4,5)]%>% 
  gather("dataset", key = variable, value = value) %>% 
  unite(combi, variable, Visit)
######################################################################################
#  2021-12-09 Use median and 75% quantile
######################################################################################

data$variable <- recode_factor(data$variable, Stats.RMSE = "RMSE", 
                               Stats.MAE = "MAE",Stats.R2="R2")
data_nona <- data[!is.na(as.double(data$value)),]
data_filter <- data_nona[data_nona$method %in%c("rfG","rfinv2","ENglm","GR","KNN",
                                                    "Lasso","pcr","pls","ridgeglm","svm","treebag","NN"),]

stat<- data_filter%>%
  group_by(dataset,variable,method) %>% 
  summarise(median=median(as.double(value)),Q75range=-(quantile(value, 0.75)-quantile(value, 0.25)))

stat$median <- ifelse(stat$variable%in%c("RMSE","MAE"), -stat$median , stat$median)
                      
ggplot(stat, aes(x = median, fill = dataset)) +            # Draw two histograms in same plot
  geom_histogram(alpha = 0.5, position = "identity")

ggplot(stat, aes(x=median, y=Q75range, shape=variable, color=dataset,size=2)) +
  geom_point()+labs(title="Before scaling")
  

stat_scaled <- stat %>% group_by(dataset,variable) %>% mutate(median_scaled = scale(median), Q75range_scaled=scale(Q75range))

ggplot(stat_scaled, aes(x=median_scaled, y=Q75range_scaled, shape=variable, color=dataset,size=2)) +
  geom_point()+labs(title="After scaling")

#Separated by dataset
library(wesanderson)
pal <- wes_palette("Zissou1", 100, type = "continuous")
library("tidyverse")


stat_scaled$method <- factor(stat_scaled$method,
                             levels = c("rfG","rfinv2","svm","treebag","pls","Lasso","ENglm","ridgeglm","NN","pcr","KNN","GR"))

pdf("/Users/evaluna/Downloads/Output\ 2/5.4.BubblePlot.pdf")
stat_combined_scaled$vali <- "Statistical Validation"
ggplot( stat_scaled,aes(variable,forcats::fct_rev(method))) +
  geom_point(aes( color = median_scaled+0.6 , size =Q75range_scaled)) +
    scale_x_discrete(position = "bottom") +
  scale_radius(range = c(0, 11)) +
  scale_color_gradient2(low = "darkblue", mid = "white", midpoint = 0,high = "#b9221f") +
  theme(legend.position = "bottom", 
#        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = 0), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
  labs(size = "Area = Scaled quantile range", fill = "Scaled median performance", x = NULL, y = NULL)+facet_grid(~ dataset,scales = "free")
#Combined by dataset
dev.off()
stat_combined<- data_filter%>%
  group_by(variable,method) %>% 
  summarise(median=median(as.double(value)),Q75range=-(quantile(value, 0.75)-quantile(value, 0.25)))

stat_combined$median <- ifelse(stat_combined$variable%in%c("RMSE","MAE"), -stat_combined$median , stat_combined$median)

stat_combined_scaled <- stat_combined %>% group_by(variable) %>% mutate(median_scaled = scale(median), Q75range_scaled=scale(Q75range))

#Separated by dataset
ggplot(stat_combined_scaled, aes(variable, forcats::fct_rev(method))) +
  geom_point(aes( color = median_scaled , size =Q75range_scaled)) +
  scale_x_discrete(position = "bottom") +
  scale_radius(range = c(0, 20)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",breaks = c(-1, 0, 1), labels = c("Bad", "OK", "Great") ,) +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = 0), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
  labs(size = "Area = Scaled quantile range", fill = "Scaled median performance", x = NULL, y = NULL)


ggplot(stat_combined_scaled, aes(variable, forcats::fct_rev(method), fill = stat.mean, size = stat.sd)) +
  geom_point(shape = 21, stroke = 0,aes( fill = median_scaled , size =Q75range_scaled)) +
  scale_x_discrete(position = "top") +
  scale_radius(range = c(0, 20)) +
  scale_fill_gradient2(low = muted("blue"), mid = "white", high = muted("red"),breaks = c(-1, 0, 1), labels = c("Bad", "OK", "Great"), ) +
#  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
  labs(size = "Area = Time Spent", fill = "Score:", x = NULL, y = NULL)#+facet_grid(~ variable,scales = "free")
######################################################################################
#  2022-01-05 Use bordered circle
######################################################################################
pdf("/Users/evaluna/Downloads/Output\ 2/5.4.BubblePlot_20220105_bordered.pdf")
ggplot( stat_scaled,aes(variable,forcats::fct_rev(method),fill=median_scaled+1.2,size=Q75range_scaled)) +
  geom_point(shape=21,stroke=0.5) +
  scale_x_discrete(position = "bottom") +
  scale_radius(range = c(0, 13)) +
  scale_fill_gradient2(low = "darkblue", mid = "white", midpoint = 0,high = "#b9221f") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = 0), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
  labs(size = "Area = Scaled quantile range", fill = "Scaled median performance", x = NULL, y = NULL)+facet_grid(~ dataset,scales = "free")
#Combined by dataset
dev.off()