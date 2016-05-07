library("parallel")
#Create cluster
clus2 <- makeCluster(3)
#Option 1. Declare the function for each node
clusterEvalQ(clus2, calPeriod_car <- function(CarID,Date){
    select <- mobile[mobile$CarID==CarID & mobile$Date==Date,]
    select <- select[order(select$Time.Sec),]
    select$vardiff <- c(0, diff(select$Time.Sec))
    select[1,]$vardiff = -1
    select[nrow(select),]$vardiff = -2
    return (select) 
  }
)
#Option 2. Export it form base workspace
calPeriod_car <- function(CarID,Date){
  select <- mobile[mobile$CarID==CarID & mobile$Date==Date,]
  select <- select[order(select$Time.Sec),]
  select$vardiff <- c(0, diff(select$Time.Sec))
  select[1,]$vardiff = -1
  select[nrow(select),]$vardiff = -2
  return (select) 
}
clusterExport(clus2,"calPeriod_car")
clusterExport(clus2,"mobile")


mobile.Period <- do.call(rbind,parRapply(clus2,mobile.count[,c("CarID","Date")],function(row) calPeriod_car(row["CarID"],row["Date"])))

# Finish
stopCluster(clus2) 

write.csv(mobile.Period,file ="/Users/TLump_MAC/Thesis/Data/temp/mobilePeriodV3.csv")



