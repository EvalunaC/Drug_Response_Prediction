######### leave-one-out-Figures
################## Yiqing's figure

library(ggplot2)
library(dplyr)

####
result_list<- list.files(path="/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/L-CV/homogenize/eva",pattern="csv")

result0 <- data.frame()
for (i in 1:length(result_list)){
  file <- paste("/home/Qiang/DrugResponse/GDSC1/Output/mid_evaluation/L-CV/homogenize/eva/",result_list[i], sep="",collapse = NULL)
  result_file <- read.csv(file, header = TRUE,na.strings="NA")
  result_file$method <- unlist(strsplit(unlist(strsplit(result_list[i], split = "\\_"))[4],split="\\."))[1]
  colnames(result_file) <- c("drug", "N.All", "SCC", "SCC.P","RMSE", "MAE", "R2","NRMSE","method")
  result0 <- rbind(result0,result_file)
}

lcv_result2 <- reshape2::melt(as.data.frame(result0[,c(3,7,5,8,6,9)]))
lcv_result2$dataset<-"GDSC1"

####
result_list <- list.files(path="/home/Qiang/DrugResponse/GDSC2/Output/mid_evaluation/L-CV/homogenize/eva",pattern="csv")

result0 <- data.frame()
for (i in 1:length(result_list)){
  file <- paste("/home/Qiang/DrugResponse/GDSC2/Output/mid_evaluation/L-CV/homogenize/eva/",result_list[i], sep="",collapse = NULL)
  result_file <- read.csv(file, header = TRUE,na.strings="NA")
  result_file$method <- unlist(strsplit(unlist(strsplit(result_list[i], split = "\\_"))[4],split="\\."))[1]
  colnames(result_file) <- c("drug", "N.All", "SCC", "SCC.P","RMSE", "MAE", "R2","NRMSE","method")
  result0 <- rbind(result0,result_file)
}

lcv_result1 <- reshape2::melt(as.data.frame(result0[,c(3,7,5,8,6,9)]))
lcv_result1$dataset<-"GDSC2"

###
result_list <- list.files(path="/home/Qiang/DrugResponse/CTRP1/Output/mid_evaluation/L-CV/homogenize/eva",pattern="csv")

result0 <- data.frame()
for (i in 1:length(result_list)){
  file <- paste("/home/Qiang/DrugResponse/CTRP1/Output/mid_evaluation/L-CV/homogenize/eva/",result_list[i], sep="",collapse = NULL)
  result_file <- read.csv(file, header = TRUE,na.strings="NA")
  result_file$method <- unlist(strsplit(unlist(strsplit(result_list[i], split = "\\_"))[4],split="\\."))[1]
  colnames(result_file) <- c("drug", "N.All", "SCC", "SCC.P","RMSE", "MAE", "R2","NRMSE","method")
  result0 <- rbind(result0,result_file)
}

lcv_result3 <- reshape2::melt(as.data.frame(result0[,c(3,7,5,8,6,9)]))
lcv_result3$dataset<-"CTRP1"

###
result_list <- list.files(path="/home/Qiang/DrugResponse/CTRP2/Output/mid_evaluation/L-CV/homogenize/eva",pattern="csv")

result0 <- data.frame()
for (i in 1:length(result_list)){
  file <- paste("/home/Qiang/DrugResponse/CTRP2/Output/mid_evaluation/L-CV/homogenize/eva/",result_list[i], sep="",collapse = NULL)
  result_file <- read.csv(file, header = TRUE,na.strings="NA")
  result_file$method <- unlist(strsplit(unlist(strsplit(result_list[i], split = "\\_"))[4],split="\\."))[1]
  colnames(result_file) <- c("drug", "N.All", "SCC", "SCC.P","RMSE", "MAE", "R2","NRMSE","method")
  result0 <- rbind(result0,result_file)
}

lcv_result4 <- reshape2::melt(as.data.frame(result0[,c(3,7,5,8,6,9)]))
lcv_result4$dataset<-"CTRP2"

lcv_result_all<-rbind(lcv_result1,lcv_result2,lcv_result3,lcv_result4)
lcv_result<-subset(lcv_result_all,variable != "NRMSE")

for (i in 1:nrow(lcv_result)){
  if (lcv_result$method[i] == "GR") { lcv_result$method[i] = "Linear Ridge"}
  if (lcv_result$method[i] == "rfG") { lcv_result$method[i] = "Random Forest"}
  if (lcv_result$method[i] == "rfinv2") { lcv_result$method[i] = "Regression Forest"}
  if (lcv_result$method[i] == "pls") { lcv_result$method[i] = "Partial Least Square"}
  if (lcv_result$method[i] == "ENglm") { lcv_result$method[i] = "Elastic Net"}
  if (lcv_result$method[i] == "Ridgeglm") { lcv_result$method[i] = "Ridge Regression"}
  if (lcv_result$method[i] == "pcr") { lcv_result$method[i] = "Principal Component Regression"}
  if (lcv_result$method[i] == "knn") { lcv_result$method[i] = "K-nearest Neighbors"}
  if (lcv_result$method[i] == "svm") { lcv_result$method[i] = "Support Vector Machine"}
  if (lcv_result$method[i] == "NN") { lcv_result$method[i] = "Neural Network"}
}

setwd("/home/Qiang/DrugResponse/")
save(lcv_result,file="LCV_result.RData")

##############
stat<- lcv_result%>%
  group_by(dataset,method,variable) %>% 
  #summarise(median=median(value,na.rm=T),Q75range=-(quantile(value, 0.75,na.rm=T)-quantile(value, 0.25,na.rm=T)))
  summarise(median=median(value,na.rm=T),Q75range=IQR(value,na.rm=T))

stat$median <- ifelse(stat$variable%in%c("RMSE","MAE"), -stat$median , stat$median)


##### scaled by dataset, method 
#stat_scaled <- stat %>% group_by(dataset,method) %>% mutate(median_scaled = scale(median), Q75range_scaled=scale(Q75range))

stat_scaled <- stat %>% group_by(dataset,variable) %>% mutate(median_scaled = scale(median), Q75range_scaled=scale(Q75range))

stat_scaled$method <- factor(stat_scaled$method,
                             levels = c("Random Forest","Regression Forest","Linear Ridge","Partial Least Square","Principal Component Regression","Ridge Regression","K-nearest Neighbors","Lasso","Elastic Net","Support Vector Machine","Neural Network"))
stat_scaled$dataset <- factor(stat_scaled$dataset, levels = c("GDSC1", "GDSC2", "CTRP1", "CTRP2"))

#write.csv(stat_scaled, file="/home/Qiang/DrugResponse/evaluation_stat_summary.csv")
range(stat_scaled$median_scaled) ##criteria_fill: -3.002178  1.545555 (-1,1)
h<-hist(stat_scaled$median_scaled, main="scaled_median", ylab="")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

range(stat_scaled$Q75range_scaled) ##criteria_size: -2.806859  3.010299 (-1,1)
h<-hist(stat_scaled$Q75range_scaled, main="scaled_IQR", ylab="")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

###  by dataset, criteria
pdf(file.path("/home/Qiang/DrugResponse/",'performance evaluation.pdf'),width= 6.5,height = 6)

ggplot(stat_scaled,aes(variable,forcats::fct_rev(method),fill=median_scaled,size=Q75range_scaled)) +
  geom_point(shape=21,stroke=0.5) +
  scale_x_discrete(position = "bottom") +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
  scale_radius(range = c(0, 20)) +
  #scale_fill_gradient2(low = "darkblue", mid = "white", midpoint = 0,high = "darkred") +
  #scale_fill_manual(values=c(  "#184275", "#b3202c" )) +
  #scale_colour_manual(values=c(  "#184275", "#b3202c"  )) +
  scale_fill_gradientn(oob = scales::squish,
                       #labels=c(-1,0,1),breaks=c(-3,-0.5,2),limits = c(-3,2),
                       labels=c(-2,0,2),breaks=c(-3,-0.5,2),limits = c(-3,2),
                       colours = colorRampPalette(c("blue", "white", "red"))(20)) +
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  scale_size_continuous(range = c(1,10),limits = c(-3,3.1), labels=c(-2,0,2),breaks=c(-2,0,3)) +
  
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        #axis.text.y = element_text(lineheight = 8, vjust = 1),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=0.2, fill=NA) ) +
  
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black", stroke = 1), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) +
  labs(size = "scaled interquartile range", fill = "scaled median", x = NULL, y = NULL)+facet_grid(~ dataset,scales = "free",space="free")

dev.off()


