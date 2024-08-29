library(readxl)
df <- read_excel("D:\\Projects\\ISPS\\Exceedances_115.xlsx", sheet=2)
for (i in 1:7){
  quartiles <- quantile(as.numeric(unlist(df[,i+1])), probs = c(0.25, 0.5, 0.75))
  df[, paste0("Cluster_Dec", i)] <- cut(as.numeric(unlist(df[,i+1])), 
                                        breaks = c(-Inf, quartiles, Inf), 
                                        labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
                                        include.lowest = TRUE)
}

##Re ordering the dataframe with cluster column beside decade column
cluster_columns <- names(df)[-(1:8)]
decade_columns <- names(df)[2:8]
new_order = c(names(df)[1])
for (i in 1:7){
  new_order <- c(new_order, decade_columns[i], cluster_columns[i])
}

df <- df[, new_order]

#Export inn csv format
write.csv(df, "D:\\Projects\\ISPS\\Dec_wise_clusters.csv", row.names = TRUE)
