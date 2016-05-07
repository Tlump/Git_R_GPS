#20160506
#cal time all day

library("parallel")
#Create cluster
clus <- makeCluster(3)

#Option 1. Declare the function for each node
clusterEvalQ(clus,clusterTrip <- function(CarID,x){
                    # select data by carID
                    select <- car[car$CarID==CarID,]
                    select <- select[order(select$Time.Sec),]
                    # init column
                    select$timediff <- -2 
                    select$trip <- 0
                    select$tripStatus <- "-"
                    #get trip
                    tripId<-1
                    select[1,]$trip <- tripId
                    select[1,]$timediff <- -1
                    select[1,]$tripStatus <- "O"
                    select[nrow(select),]$tripStatus <- "D"
                    
                    for(i in 2:nrow(select)){
                      select[i,]$timediff <- select[i,]$Time.Sec - select[(i-1),]$Time.Sec
                      # extract Trip using timediff 
                      if(select[i,]$timediff>=600){
                        tripId <- tripId+1
                        select[(i-1),]$tripStatus <- "D"
                        select[(i),]$tripStatus <- "O"
                      }
                      select[i,]$trip <- tripId
                    }
                    return(select)
                  }
)

#Option 2. Export it form base workspace
clusterTrip <- function(CarID,x){
  # select data by carID
 select <- car[car$CarID==CarID,]
 select <- select[order(select$Time.Sec),]
  # init column
  select$timediff <- -2 
  select$trip <- 0
  select$tripStatus <- "-"
  #get trip
  tripId<-1
  select[1,]$trip <- tripId
  select[1,]$timediff <- -1
  select[1,]$tripStatus <- "O"
  select[nrow(select),]$tripStatus <- "D" 
 
  for(i in 2:nrow(select)){
    select[i,]$timediff <- select[i,]$Time.Sec - select[(i-1),]$Time.Sec
    # extract Trip using timediff 
    if(select[i,]$timediff>=600){
      tripId <- tripId+1
      select[(i-1),]$tripStatus <- "D"
      select[(i),]$tripStatus <- "O"
    }
    select[i,]$trip <- tripId
  }
  return(select)
}
clusterExport(clus,"clusterTrip")
clusterExport(clus,"car")

# main
car.clus <- do.call(rbind,parRapply(clus,car.count[,c("CarID","x")],function(row) clusterTrip(row["CarID"],row["x"])))

# Finish
stopCluster(clus)

# select car
carID.df <- plyr::count(car, c("CarID"))
carID.df <- carID.df[1:200,] 
carID <- carID.df$CarID
car <- car[car$CarID %in% carID , ]
car <- car[order(car$CarID,car$Time.Sec),]
 car.count <- plyr::count(car, c("CarID"))
car.count$x <- 0
rm(carID)
rm(carID.df)

write.csv(car.clus, file = "/Users/TLump_MAC/Thesis/Data/temp/car_clus.csv")
