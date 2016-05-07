library(rgdal)
library(dplyr)

taxi <- read.csv(file = "/Users/TLump_MAC/Thesis/Data/GPS/taxi/taxi-2015-09-01.txt",header = F,sep =',')
taxi <- taxi[!duplicated(taxi),]

taxi <- mutate(taxi,Time.Sec = (floor(V2/10000)*3600+floor((V2%%10000)/100)*60+V2%%100) )
taxi <- mutate(taxi,Latitude = (floor(V4/100)+((V4%%100)/60)))
taxi <- mutate(taxi,Longitude = (floor(V6/100)+((V6%%100)/60)))
names(taxi)[1] <- paste("CarID")
names(taxi)[2] <- paste("Time")
taxi <- mutate(taxi,Time = sprintf("%.2d:%.2d:%.2d",floor(Time/10000),floor((Time%%10000)/100),Time%%100))
names(taxi)[10] <- paste("Date")
taxi <- mutate(taxi,Date = sprintf("%.2d/%.2d/%.2d",floor(Date/10000),floor((Date%%10000)/100),Date%%100))
taxi$Time.Sec<-as.numeric(difftime(strptime( paste(taxi$Date, taxi$Time), "%d/%m/%y %H:%M:%S"), 
                                   strptime( paste("01/10/12","00:00:00"), "%d/%m/%y %H:%M:%S")
                                   ,units="sec")
                        )
taxi <- taxi[,c("CarID","Date","Latitude","Longitude","Time","Time.Sec")]

save.image("/Users/TLump_MAC/Thesis/Data/temp/taxi_20150901.RData")


carID.df <- plyr::count(taxi, c("CarID"))
carID.df <- carID.df[1:200,] 
carID <- carID.df$CarID
taxi <- taxi[taxi$CarID %in% carID , ]
taxi <- taxi[order(taxi$CarID,taxi$Time.Sec),]
taxi.count <- plyr::count(taxi, c("CarID","Date"))
