setwd("~/data_project/B001_group_projects/PATHChallenge")

schedule <- read.csv('path_schedule_clean.csv')
turnstile <- read.csv('turnstile_cleaned_2.csv')
link <- read.csv('stop_id_to_turnstile_name_2.csv')
names(turnstile) <- tolower(names(turnstile))
names(link) <- tolower(names(link))

for(i in unique(schedule$stop_name)){
  stop_id <- link$stop_id[grep(i, link$turnstile)[1]]
  schedule$stop_id[schedule$stop_name == i] <- stop_id
}

names(link)[1] <- 'station'
turnstile <- left_join(turnstile, link[,1:2])

write.csv(schedule, file = 'schedule.csv')
write.csv(turnstile, file = 'turnstile.csv')
write.csv(link, file = 'link.csv')


####################################################################################################
##    create data for possible travel
###################################################################################################
schedule <- read.csv('schedule.csv')
turnstile <- read.csv('turnstile.csv')
link <- read.csv('link.csv')

library(dplyr)
library(lubridate)

#some cleaning get a time variable for both dataframe to be able to compare them. It is in minute beginning at midnight
turnstile$date_time <- ymd_hms(turnstile$date_time)
turnstile$day <- format((turnstile$date_time), '%a')
turnstile$hour <- hour(turnstile$date_time)
turnstile$min <- minute(turnstile$date_time)
turnstile$time <- turnstile$hour*60+turnstile$min
turnstile$weekday <- ifelse(!(turnstile$day %in% c('Sat', 'Sun')), TRUE, FALSE)
flux <- turnstile %>% group_by(stop_id, weekday, hour, min, time) %>% dplyr::summarise(count = n(), mentry = mean(entry), mexit = mean(exit), flux = mentry - mexit)

schedule$hour <- hour(hms(schedule$departure_time))
schedule$min <- minute(hms(schedule$departure_time))
schedule$time <- schedule$hour*60+schedule$min

#graphical display of flux of people per station
pdf(file = 'flux_per_station.pdf')
for(i in unique(flux$stop_id)){
  f <- flux[flux$stop_id == i & flux$weekday == TRUE,]
  s <- schedule[schedule$stop_id == i & schedule$service_name == 'Yearly Service (Mon-Fri)',]
  stop_name <- as.character(s[1,'stop_name'])
  plot(flux~time, f, pch = 20)
  points(rep(0, dim(s)[1])~s$time, col = 'red', pch = '|')
  title(stop_name)
}
dev.off()

week <- schedule %>% filter(service_name == 'Yearly Service (Mon-Fri)')
turnweek <- flux %>% filter(weekday) 
write.csv(week, file = 'week.csv')
write.csv(turnweek, file = 'turnweek.csv')
#this station dataframe is a link to help to know what is the name for a stop_id plus I actually use it to know if a station is in manhattan or NJ
station <- data.frame(stop_id = unique(week$stop_id),  stop_name = sapply(unique(week$stop_id), function(x) unique(week$stop_name[week$stop_id == x])))
station$inny <- ifelse(station$stop_id %in% c(26726, 26725, 26722, 26723, 26724, 26734), TRUE, FALSE)
write.csv(station, file = 'station.csv')

# for every 15 min  block find for every station what train they could take and what station and time they could go out
# i assume that:
# people that take a train in manhattan don't want to go back in manhattan (except for christopher street)
# people that change from newark line to yellow line do it at journal square and not groves
# people that come from JS ou groves take the yellow line, and don't go through hoboken

find_station_data.f <- function(x){
  #this function takes an 'out' df and has for goal to find the number of people exiting at each of the possible exit
  output = rep(0, dim(x)[1])
  for(i in 1:dim(x)[1]){
    t <- x[i,]
    if(t$exittime >= 1440){t$exittime = 0}
    output[i] <- (turnweek %>% filter(stop_id == t$stop_id & time == t$exittime))$mexit
  }
  return(output)
}
add_stop.f <- function(out){
  ## add station if ppl change train put limit on the possibilities like if come from manhattan don't change in a train to go back from manhattan. also look only at change at JS and not at grove
  route <- out$route_id[1]
  direction <- out$direction_id[1]  
  if(route == 1024 | route == 861){ #33rd st to JS via hob
    if(direction == 0){ ## toward nj
      js <- out %>% filter(stop_id == 26731)
      toadd <- week %>% filter(stop_id == js$stop_id, direction_id == 0, route_id == 862, between(time, js$time,js$time+15)) %>% select(stop_id, time, route_id, direction_id, trip_id) 
      toadd <- toadd %>% filter(stop_id != 26731)
    }else if(route == 861 & direction == 1 & 26732 %in% out$stop_id){
      pa <- out %>% filter(stop_id == 26732)
      toadd <- week %>% filter(stop_id == js$stop_id, direction_id == 0, route_id == 860, between(time, pa$time,pa$time+15)) %>% select(stop_id, time, route_id, direction_id, trip_id)
    }else{return(NULL)}
  }else if(route == 862){
    if(direction == 1){
      if(26731 %in% out$stop_id){
        js <- out %>% filter(stop_id == 26731)
        t1 <- week %>% filter(stop_id == js$stop_id, direction_id == 1, route_id == 861, between(time, js$time,js$time+15)) %>% select(trip_id)
        if(nrow(t1) > 0){
          toadd <- week %>% filter(trip_id == t1$trip_id & stop_id %in% c(26732, 26726,2625,2622,2623,2624)) %>% select(stop_id, time, route_id, direction_id, trip_id)
        }else{toadd <- NULL}        
        t2 <- week %>% filter(stop_id == js$stop_id, direction_id == 1, route_id == 1024, between(time, js$time,js$time+15)) %>% select(trip_id)
        if(nrow(t2) > 0){
          toadd2 <- week %>% filter(trip_id == t2$trip_id & stop_id %in% c(26730, 26732)) %>% select(stop_id, time, route_id, direction_id, trip_id)}else{toadd2 <- NULL}
        toadd <- rbind(toadd, toadd2) 
      }else{return(NULL)} 
    }else{return(NULL)}  
  }else if(route == 860){
    if(direction == 1){#green line toward nyc can change to yellow and then even to red toward nj
      if(26732 %in% out$stop_id){
        pa <- out %>% filter(stop_id == 26732)
        t1 <- week %>% filter(stop_id == pa$stop_id, direction_id == 0, route_id == 861, between(time, pa$time,pa$time+15)) %>% select(trip_id)
        if(nrow(t1) > 0){
          toadd <- week %>% filter(trip_id == t1$trip_id & stop_id %in% c(26728, 26731)) %>% select(stop_id, time, route_id, direction_id, trip_id)
        }else{toadd <- NULL}        
        t2 <- week %>% filter(stop_id == pa$stop_id, direction_id == 0, route_id == 1024, between(time, pa$time,pa$time+15)) %>% select(trip_id)
        if(nrow(t2) > 0){
          toadd2 <- week %>% filter(trip_id == t2$trip_id & stop_id %in% c(26728, 26731)) %>% select(stop_id, time, route_id, direction_id, trip_id)}else{toadd2 <- NULL}
        toadd <- rbind(toadd, toadd2) 
        if(!is.null(toadd) & nrow(toadd) > 0){
          js <- toadd %>% filter(stop_id == 26731)
          if(nrow(js) < 1){js <- toadd %>% filter(stop_id == 26728)}
          if(nrow(js) > 1){js <- js[1,]}
          t3 <- week %>% filter(stop_id == js$stop_id, direction_id == 0, route_id == 862, between(time, js$time,js$time+15)) %>% select(trip_id)
          if(nrow(t3) > 0){
            toadd3 <- week %>% filter(trip_id == t3$trip_id & stop_id %in% c(26729, 26733)) %>% select(stop_id, time, route_id, direction_id, trip_id)}else{toadd3 <- NULL}
          toadd <- rbind(toadd, toadd3) 
        }else{return(NULL)} 
      }else{return(NULL)}        
      }else{return(NULL)}  
  }else{return(NULL)}
  return(toadd)  
}    
#859 hob to 33d and back blue
#1024 JS to 33 via hob yellow and blue
#860 hob to wtc green
#862 nw to wtc  red
#861  JS to 33d yellow
get_possible_travel.f <- function(posstrip, week){
  #this function get where and when the possible exit is for each entry point and time
  output = list()
  for(i in 1:dim(posstrip)[1]){
    out <- week %>% filter(trip_id == posstrip[i,1] & stop_sequence > posstrip[i,2]) %>% select(stop_id, time, route_id, direction_id, trip_id)
    if(nrow(out) != 0){
      toadd <- add_stop.f(out)
    }else{toadd = NULL}
    output[[i]] <- rbind(out, toadd)
  }
  output = do.call(rbind, output)
  return(output)
}
rm_manhattan.f <- function(out){
  tm <- out %>% filter(direction_id == 1) #if the only possible exits are towrd manhattan
  if(nrow(tm) == nrow(out)){return(out)}else{
    tm <- tm[-which(tm$stop_id %in% c(26722, 26723, 26724)),]
    return(tm)
  }
}
exit_per_entry_time.f <- function(time, week, station, en){
  # this function takes a 'time' df aka a df from the turnstile data filtered by hour and for each station het the number of ppl that enter and look where they could possibly exit the system
  output <- list()
  for(i in 1:dim(time)[1]){
    t <- time[i,]
    #entryt <- t$entrytime 
    posstrip <-  week %>% filter(stop_id == t$stop_id & between(time, t$time, (t$time + 30))) %>% select(trip_id, stop_sequence, route_id, direction_id)
    if(dim(posstrip)[1] == 0){
      posstrip <-  week %>% filter(stop_id == t$stop_id & time > t$time) %>% filter(time == min(time)) %>% select(trip_id, stop_sequence, route_id, direction_id)
    }
    posstrip$route_dir <- paste(posstrip$route_id,posstrip$direction_id, sep = '_')
    if(length(unique(posstrip$route_dir)) > 1){
      posstrip <-  posstrip[!duplicated(posstrip$route_dir),]
    }
    if(station$inny[which(station$stop_id == t$stop_id)] & t$stop_id != 26726 & nrow(posstrip) > 1){
      #if the stop is in manhattan except for christopher st can only go to nj
      posstrip <- posstrip %>% filter(direction_id == 0) 
    }
    out <- get_possible_travel.f(posstrip, week)
    if(t$stop_id %in% c(26725, 26722, 26723) & any(out$direction_id == 1)){#wont go toward 33rd st from 9th or 14th
      out <- rm_manhattan.f(out)      
    }
    # I'm rounding. it is definitely not perfect but it make it a lot easier to have only one time value
    
    if(nrow(out) != 0){
      out$exittime <- round(out$time/15)*15
      out <- out[!(duplicated(out$stop_id) & duplicated(out$exittime)),]
      out$exittime[which(out$exittime >= 1440)] <- out$exittime[which(out$exittime >= 1440)] - 1440
      out$n_exit = find_station_data.f(out)
      out$idx <- paste('t', en, i, sep = "")
      out <- data.frame(entry_stop = t$stop_id, entrytime = t$time, n_entry = t$mentry, out)
      output[[i]] <- out
    }
    }
  output <- do.call(rbind, output)
  return(output)
}

library(doParallel)
library(foreach)
registerDoParallel(cores=4)
possible_travel <- foreach(i = unique(turnweek$time)) %dopar% {
  time <- turnweek %>% filter(time == i)  
  exit_per_entry_time.f(time, week, station,i) 
}
possible_travel <- do.call(rbind, possible_travel)
save(possible_travel, file = 'possible_travel')


########################################################################################################
## try to make sens of these data now....
########################################################################################################