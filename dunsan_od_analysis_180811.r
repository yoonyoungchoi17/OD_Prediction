## wd: D:\rwd\rwd_ys_result\99_OD_estimation\dunsan_rds


rm(list=ls())

## reading files in the directory and save it as a list
a.day.list <- dir()
length(a.day.list)

#대전광역시내 법정동 리스트 읽기 Reading the ligal-dong list
dong.list <- read.csv("D:/rwd/rwd_ys_result/99_OD_estimation/daejeon_dong_list_180811.csv", header=T) #read
head(dong.list)

n.dong <- length(dong.list[,1])
dunsan.trip.table <- data.frame()

#
i<-2
for(i in 1:length(a.day.list)) {

  ## Reading Dunsan-dong RDS file sequentially  
  temp.data <- readRDS(a.day.list[i])
  head(temp.data)
  temp.dong.list <- as.numeric(temp.data[2,])
  n.time <- length(temp.data[,1])-2
 
  temp.dunsan.trip.table <- matrix("", nrow=n.time, ncol=n.dong+2)
  colnames(temp.dunsan.trip.table) <- c("day","t.index",dong.list[,1])
  temp.date <- substr(a.day.list[i],nchar(a.day.list[i])-9,nchar(a.day.list[i])-4)
  temp.dunsan.trip.table[,1] <- temp.date ##날짜 부여 Adding date of the data
  temp.dunsan.trip.table[,2] <- seq(1,n.time,by=1) ##시간id 부여 Adding time id
  head(temp.dunsan.trip.table)

  j <- 1
  for (j in 1:length(temp.dong.list)) {
    temp.col.id <- which(temp.data[2,j]==colnames(temp.dunsan.trip.table))
    temp.dunsan.trip.table[,temp.col.id] <- temp.data[3:length(temp.data[,1]),j]
  }
  head(temp.dunsan.trip.table)
  
  if(i==1) {
    dunsan.trip.table <- temp.dunsan.trip.table
  } else { 
    if( length(dunsan.trip.table[1,])==length(temp.dunsan.trip.table[1,]) ) {
      if( length(which(temp.date==dunsan.trip.table[,1])) == 0 ) {
	    dunsan.trip.table <- rbind(dunsan.trip.table, temp.dunsan.trip.table)
      } else {
	    print(paste0(temp.date," is already analyzed."))
	  }
    } else {
	  print(paste0("Culumn length of temp.data doesn't same with dunsan.trip.table: ",temp.date))
    }
  }
}

write.csv(dunsan.trip.table, "D:/rwd/rwd_ys_result/99_OD_estimation/aggregated_dunsan_trip_table2.csv", row.names=F)
