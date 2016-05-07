library("parallel")
#Create cluster
clus <- makeCluster(3)
#Option 1. Declare the function for each node
clusterEvalQ(clus, calPeriod_car <- function(CarID,Date){
                        select <- car[car$CarID==CarID & car$Date==Date,]
                        select <- select[order(select$Time.Sec),]
                        select$vardiff <- c(0, diff(select$Time.Sec))
                        select[1,]$vardiff = -1
                        select[nrow(select),]$vardiff = -2
                        return (select) 
                  }

             )

#Option 2. Export it form base workspace
calPeriod_car <- function(CarID,Date){
  select <- car[car$CarID==CarID & car$Date==Date,]
  select <- select[order(select$Time.Sec),]
  select$vardiff <- c(0, diff(select$Time.Sec))
  select[1,]$vardiff = -1
  select[nrow(select),]$vardiff = -2
  return (select) 
}
clusterExport(clus,"calPeriod_car")
clusterExport(clus,"car")


car.Period <- do.call(rbind,parRapply(clus,car.count[,c("CarID","Date")],function(row) calPeriod_car(row["CarID"],row["Date"])))

# Finish
stopCluster(clus)

write.csv(car.Period, file = "/Users/TLump_MAC/Thesis/Data/temp/carPeriod2.csv")
