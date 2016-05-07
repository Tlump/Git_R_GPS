# 20160504
# cal timediff in same day
#bug drive car in midnight

library("parallel")
#Create cluster
clus <- makeCluster(3)

#Option 1. Declare the function for each node
clusterEvalQ(clus,clusterTrip <- function(CarID,Date){
                    select <- car[car$CarID==CarID & car$Date==Date,]
                    select <- select[order(select$Time.Sec),]
                    # get timediff
                    select$timediff <- c(0, diff(select$Time.Sec))
                    select[1,]$timediff = -1
                    select[nrow(select),]$timediff = -2
                    # get TRIP
                    select$trip <- 0
                    j<-1
                    for(i in 1:nrow(select)){
                      select[i,]$trip <- j
                      if(select[i,]$timediff>=600 || select[i,]$timediff==-2){
                        j <- j+1
                      }
                    }
                    return(select)
                  }
)

#Option 2. Export it form base workspace
clusterTrip <- function(CarID,Date){
  select <- car[car$CarID==CarID & car$Date==Date,]
  select <- select[order(select$Time.Sec),]
  # get timediff
  select$timediff <- c(0, diff(select$Time.Sec))
  select[1,]$timediff = -1
  select[nrow(select),]$timediff = -2
  # get TRIP
  select$trip <- 0
  j<-1
  for(i in 1:nrow(select)){
    select[i,]$trip <- j
    if(select[i,]$timediff>=600 || select[i,]$timediff==-2){
      j <- j+1
    }
  }
  return(select)
}
clusterExport(clus,"clusterTrip")
clusterExport(clus,"car")


# main

# select car
carID.df <- plyr::count(car, c("CarID"))
carID.df <- carID.df[1:50,] 
carID <- carID.df$CarID
car <- car[car$CarID %in% carID , ]
car <- car[order(car$CarID,car$Time.Sec),]
car.count <- plyr::count(car, c("CarID","Date"))
rm(carID)
rm(carID.df)

car.clus <- do.call(rbind,parRapply(clus,car.count[,c("CarID","Date")],function(row) clusterTrip(row["CarID"],row["Date"])))

# Finish
stopCluster(clus)

write.csv(car.clus, file = "/Users/TLump_MAC/Thesis/Data/temp/car_clus.csv")
