result_list <- list.files(path="change this to the result file folder")

result0 <- data.frame()
for (i in 1:length(result_list)){
  file <- paste("change this to the result file folder",result_list[i], sep="",collapse = NULL)
  result_file <- read.csv(file, header = TRUE,na.strings="NA")
  result_file$method <- result_list[i]
  result0 <- rbind(result0,result_file)
}

lcv_result <- reshape2::melt(as.data.frame(result0[,3:9]))
