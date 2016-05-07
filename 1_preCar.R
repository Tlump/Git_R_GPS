library(rgdal)
library(dplyr)

# read File
car1 <- read.csv("/Users/TLump_MAC/Thesis/Data/GPS/All/2012-10/201210-01.txt",header = F,sep =',',stringsAsFactors=TRUE)
car2 <- read.csv("/Users/TLump_MAC/Thesis/Data/GPS/All/2012-10/201210-02.txt",header = F,sep =',',stringsAsFactors=TRUE)
car1 <- car1[!duplicated(car1),]
car2 <- car2[!duplicated(car2),]
car.all <- rbind(car1,car2)
car.all <- car[!duplicated(car.all),]
rm(car1)
rm(car2)

car <- car.all[car.all$V2 %in% c("CAR1","CAR2","CAR3","CAR4","CAR5","CAR6","CAR7","CAR8") , ]
car <- mutate(car,Time.Sec = (floor(V3/10000)*3600+floor((V3%%10000)/100)*60+V3%%100) )
car <- mutate(car,Latitude = (floor(V5/100)+((V5%%100)/60)))
car <- mutate(car,Longitude = (floor(V7/100)+((V7%%100)/60)))
names(car)[1] <- paste("CarID")
names(car)[3] <- paste("Time")
car <- mutate(car,Time = sprintf("%.2d:%.2d:%.2d",floor(Time/10000),floor((Time%%10000)/100),Time%%100))
names(car)[11] <- paste("Date")
car <- mutate(car,Date = sprintf("%.2d/%.2d/%.2d",floor(Date/10000),floor((Date%%10000)/100),Date%%100))
car$Time.Sec<-as.numeric(difftime(strptime( paste(car$Date, car$Time), "%d/%m/%y %H:%M:%S"), 
                                   strptime( paste("01/10/12","00:00:00"), "%d/%m/%y %H:%M:%S")
                                   ,units="sec")
                        )
car <- car[,c("CarID","Date","Latitude","Longitude","Time","Time.Sec")]

# Filter Data
car <- car[car$Date %in% c("14/10/12","16/10/12","19/10/12"), ]
carID.df <- plyr::count(car, c("CarID"))
carID.df <- carID.df[1:100,] 
carID <- carID.df$CarID
car <- car[car$CarID %in% carID , ]
car <- car[order(car$CarID,car$Time.Sec),]
car.count <- plyr::count(car, c("CarID","Date"))
rm(carID)
rm(carID.df)

# Tranform mobile data
mobile <- car.all[car.all$V2 %in% c("MOBILE1","MOBILE2","MOBILE3","MOBILE4","MOBILE5","MOBILE6","MOBILE7","MOBILE8") , ]
mobile <- mutate(mobile,Time.Sec = (floor(V3/10000)*3600+floor((V3%%10000)/100)*60+V3%%100) )
mobile <- mutate(mobile,Latitude = (floor(V5/100)+((V5%%100)/60)))
mobile <- mutate(mobile,Longitude = (floor(V7/100)+((V7%%100)/60)))
names(mobile)[1] <- paste("CarID")
names(mobile)[3] <- paste("Time")
mobile <- mutate(mobile,Time = sprintf("%.2d:%.2d:%.2d",floor(Time/10000),floor((Time%%10000)/100),Time%%100))
names(mobile)[11] <- paste("Date")
mobile <- mutate(mobile,Date = sprintf("%.2d/%.2d/%.2d",floor(Date/10000),floor((Date%%10000)/100),Date%%100))
mobile$Time.Sec<-as.numeric(difftime(strptime( paste(mobile$Date, mobile$Time), "%d/%m/%y %H:%M:%S"), 
                                   strptime( paste("01/10/12","00:00:00"), "%d/%m/%y %H:%M:%S")
                                   ,units="sec")
                        )
mobile <- mobile[,c("CarID","Date","Latitude","Longitude","Time","Time.Sec")]

# Filter Data
mobile <- mobile[mobile$Date%in% c("14/10/12","16/10/12","19/10/12"), ]
mobile.df <- plyr::count(mobile, c("CarID"))
mobile.df <- mobile.df[1:300,] 
mobileID <- mobile.df$CarID
mobile <- mobile[mobile$CarID %in% mobileID , ]
mobile <- mobile[order(mobile$CarID,mobile$Time.Sec),]
mobile.count <- plyr::count(mobile, c("CarID","Date"))
rm(mobileID)
rm(mobile.df)


write.csv(car.Period, file = "/Users/TLump_MAC/Thesis/Data/temp/carPeriod2.csv")
write.csv(mobile.Period, file = "/Users/TLump_MAC/Thesis/Data/temp/mobilePeriod2.csv")


save.image("~/Desktop/CarMobilePeriod.RData")
