setwd("~/data_project/B001_group_projects/SylvieCode")

library(dplyr)
load('possible_travel')
station <- read.csv('station.csv')
turnstile <- read.csv('turnstile.csv')
week <- read.csv('week.csv')
turnweek <- read.csv('turnweek.csv')
sum(turnweek$mexit) - sum(turnweek$mentry)  #diff 123586

by_exit <- possible_travel %>% group_by(exittime, stop_id)

by_exit$n_entry <- round(by_exit$n_entry)
by_exit$n_exit <- round(by_exit$n_exit)


###############################################################################################################
##this code take each exit and looks where people are entering as i got from previous code. It means that people that can't enter a train because it is too full are left on the side and stays in the dataframe
###############################################################################################################

get_travel.f <- function(x){
    rem <- get_n_remove.f(x)
    t <- x %>% select(entry_stop, entrytime, stop_id, exittime, route_id, direction_id, trip_id,idx)
    if(length(rem) == 1 & rem[1] == 0){t$n_people <- 0}else{
      rem[x$n_entry < rem] <- x$n_entry[x$n_entry < rem] 
      t$n_people <- rem
    }
  return(t)
}
get_rounded.f <- function(v, n){
  rounded <- round(v)
  dif <- rounded - v
  ndif <- n - sum(rounded)
  if(ndif > 0){
    tochoose <- which(dif < 0)
    replace <- vector()
    for(i in 1:ndif){
      replace <- c(replace,tochoose[which.min(dif[tochoose])])
      tochoose <- tochoose[-which.min(dif[tochoose])]
    }
    rounded[replace] <- rounded[replace] + 1
  }else if(ndif < 0){
    tochoose <- which(dif > 0)
    replace <- vector()
    for(i in 1:abs(ndif)){
      replace <- c(replace,tochoose[which.max(dif[tochoose])])
      tochoose <- tochoose[-which.max(dif[tochoose])]
    }
    rounded[replace] <- rounded[replace] - 1
  }
  return(rounded)
}
get_n_remove.f <- function(x){
  n <- x$n_exit[1]
  p <- x$n_entry/sum(x$n_entry)
  real <- n*p
  if(is.na(real[1])){rmv <- 0}else{rmv <- get_rounded.f(real,n)}  
  return(rmv)  
}
remove_from_entry.f <- function(df, x){
  x <- x %>% filter(n_people > 0)
  for(i in 1:dim(x)[1]){
    tmp <- df %>% filter(idx == as.character(x$idx[i]))
    tmp$n_entry <- tmp$n_entry - as.numeric(x$n_people[i])
    tmp2 <- df %>% filter(idx != as.character(x$idx[i]))
    df <- rbind(tmp, tmp2)
  }
  return(df)
}

tochange <- as.data.frame(by_exit)
tochange$exitidx <- paste(tochange$exittime, tochange$stop_id,sep = '_')
#round(abs(sum(tochange %>% distinct(exitidx) %>% select(n_exit)) - sum(tochange %>% distinct(idx) %>% select(n_entry)))/96)
#maybe should do it by entrance since there is less ppl entering than exiting
travel <- list()
latertrain <- list()
tochange <- arrange(tochange, exittime, stop_id)
tochange$uidx <- 1:dim(tochange)[1]
lim <- length(unique(tochange$exitidx))
for(i in 1:lim){
  idx <- unique(tochange$exitidx)[i]
  tmp <- tochange[which(tochange$exitidx == idx),]
  #if(length(unique(tmp$route_id)) > 1){break}
  trav <- get_travel.f(tmp)
  torm <- trav %>% select(idx, n_people)
  if(any(torm$n_people > 0)){
    tmp$n_exit <- tmp$n_exit - sum(torm$n_people)
    tmp$n_entry <- tmp$n_entry - torm$n_people
    if(any(tmp$n_exit > 0)){
      et <- min(tmp$entrytime)
      if(et < 30){et = 1440 - et}
      prev <- 15
      tmp2 <- tochange %>% filter(entry_stop %in% tmp$entry_stop & between(entrytime, et-prev, et) & route_id %in% tmp$route_id & direction_id %in% tmp$direction_id & !(idx %in% tmp$idx)) %>% distinct(idx)
     while(sum(tmp2$n_entry) < tmp$n_exit[1] & prev <= 90){
        prev <- prev + 15
        tmp2 <- tochange %>% filter(entry_stop %in% tmp$entry_stop & between(entrytime, et-prev, et) & route_id %in% tmp$route_id & direction_id %in% tmp$direction_id & !(idx %in% tmp$idx)) %>% distinct(idx)
      }
     if(nrow(tmp2) >0){
        tmp2$n_exit = tmp$n_exit[1]
        trav2 <- get_travel.f(tmp2)
        torm2 <- trav2 %>% select(idx, n_people)
       if(any(torm2$n_people > 0)){
          latertrain[[i]] <- data.frame(torm2, uidx  = tmp2$uidx, wait = et - tmp2$entrytime)
          tmp2$n_exit <- 0
          tmp2$n_entry <- tmp2$n_entry - torm2$n_people
          tmp$n_exit = tmp$n_exit - sum(torm2$n_people)
          tmp <- rbind(tmp, tmp2)
          torm <- rbind(torm, torm2)
       }
     }
    }  
    notuse <- tochange[which(!(tochange$uidx %in% tmp$uidx)),]    
    notuse <- remove_from_entry.f(notuse, torm) #as there is repetition of line need to remove in each repetition
    tochange <- rbind(tmp, notuse)
    if(any(tochange$n_entry < 0 )){break}
  }
  travel[[i]] <- trav
  tochange <- arrange(tochange, exittime, stop_id)
}
travel <- do.call(rbind, travel)
changed1 <- tochange

save(travel, file = 'travel')

entryleft <- tochange %>% distinct(idx)
quantile(entryleft$n_entry)
sum(entryleft$n_entry)
plot(n_entry~entrytime, entryleft, col = 'red', pch = 20)


exitleft <- tochange %>% distinct(exitidx)
quantile(exitleft$n_exit)
sum(exitleft$n_exit)

get_other_entry.f <- function(s,e){
  #this function get the point where people enter this train when they change lines
  if((s$route_id[1] == 861 | s$route_id[1] == 1024) & s$direction_id[1] == 1){#js to 33d st
    if(is.na(s$entry[1])){s$entry[1] = 0}
    s$entry[1] = s$entry[1] + sum(e$entry[!(e$stop_id %in% s$stop_id)])
    if(sum(e$entry[!(e$stop_id %in% s$stop_id)]) > 0){s$transfer[1] <- sum(e$entry[!(e$stop_id %in% s$stop_id)])}
  }else if(s$route_id[1] == 859 & s$direction_id[1] == 0){
    if(is.na(s$entry[which(s$stop_id == 26731)])){s$entry[which(s$stop_id == 26731)] = 0}
    s$entry[which(s$stop_id == 26731)] = s$entry[which(s$stop_id == 26731)] + sum(e$entry[!(e$stop_id %in% s$stop_id)])
    if(sum(e$entry[!(e$stop_id %in% s$stop_id)]) > 0){s$transfer[which(s$stop_id == 26731)] <- sum(e$entry[!(e$stop_id %in% s$stop_id)])}
  }else if(s$route_id[1] == 860 & s$direction_id[1] == 0){
    if(is.na(s$entry[which(s$stop_id == 26732)])){s$entry[which(s$stop_id == 26732)] = 0}
    s$entry[which(s$stop_id == 26732)] = s$entry[which(s$stop_id == 26732)] + sum(e$entry[!(e$stop_id %in% s$stop_id)])
    if(sum(e$entry[!(e$stop_id %in% s$stop_id)]) > 0){s$transfer[which(s$stop_id == 26732)] <- sum(e$entry[!(e$stop_id %in% s$stop_id)])}
  }else if(s$route_id[1] == 862 & s$direction_id[1] == 0){
    if(is.na(s$entry[which(s$stop_id == 26731)])){s$entry[which(s$stop_id == 26731)] = 0}
    s$entry[which(s$stop_id == 26731)] = s$entry[which(s$stop_id == 26731)] + sum(e$entry[!(e$stop_id %in% s$stop_id)])
    if(sum(e$entry[!(e$stop_id %in% s$stop_id)]) > 0){s$transfer[which(s$stop_id == 26731)] <- sum(e$entry[!(e$stop_id %in% s$stop_id)])}
  } else if(s$route_id[1] == 861 & s$direction_id[1] == 0){
    if(is.na(s$entry[which(s$stop_id == 26732)])){s$entry[which(s$stop_id == 26732)] = 0}
    s$entry[which(s$stop_id == 26732)] = s$entry[which(s$stop_id == 26732)] + sum(e$entry[!(e$stop_id %in% s$stop_id)])
    if(sum(e$entry[!(e$stop_id %in% s$stop_id)]) > 0){s$transfer[which(s$stop_id == 26732)] <- sum(e$entry[!(e$stop_id %in% s$stop_id)])}
  }
  return(s)
}

#859 hob to 33d and back blue
#1024 JS to 33 via hob yellow and blue
#860 hob to wtc green
#862 nw to wtc  red
#861  JS to 33d yellow
groupedtravel <- travel %>% group_by(trip_id, entry_stop, stop_id) %>% summarise(count = sum(n_people))
train <- list()
tid <- unique(week$trip_id)[44:1030]
for(i in tid){
  #options(warn = w); cat("\n warn =", i, "\n")
  sched <- schedule %>% filter(trip_id == as.character(i))
  tr <- groupedtravel %>% filter(trip_id == as.character(i))
  enter <- tr %>% group_by(stop_id = entry_stop) %>% dplyr::summarise(entry = sum(count))
  exit <- tr %>% group_by(stop_id) %>% dplyr::summarise(exit = sum(count))
  sched <- left_join(sched,enter)
  sched <- left_join(sched,exit)
  sched$transfer <- 0
  if(any(!(enter$stop_id %in% sched$stop_id))){ # it means there is a transfer, people come from another line
    sched <- get_other_entry.f(sched, enter)
  }
  sched <- sched %>%  select(stop_id, departure_time, trip_id, route_id, direction_id, hour, min, time, entry, exit, transfer)
  sched$entry[is.na(sched$entry)] = 0
  sched$exit[is.na(sched$exit)] = 0
  sched$intrain <- cumsum(sched$entry) - cumsum(sched$exit)
  train[[i]] <- sched
}

train <- do.call(rbind, train)

id.df <- data.frame(trip_id = unique(train$trip_id), id = seq(1, length(unique(train$trip_id)), 1)) 
train <- left_join(train, id.df)
write.csv(train, file = 'train.csv')

max(train$intrain)/7
quantile(train$intrain)


###############################################################################################################
# now let's look at the station flux
###############################################################################################################
#should take each station cumsum entry and remove ppl leaving station per train...
turnweek$mentry <- round(turnweek$mentry)
turnweek$mexit <- round(turnweek$mexit)

flux <- turnweek %>% group_by(stop_id)
train$idx <- 1:dim(train)[1]
station_flux <- list()
for(i in unique(flux$stop_id)){
  stat <- flux %>% filter(stop_id == i)
  tr <- train %>% filter(stop_id == i)
  onestat <- list()
  k <- 1
  left <- 0
  for(j in stat$time){
    if(j == 0){b <- 1440 - 15}else{b <- j-15}
    s <- stat %>% filter(time == j)
    before <- stat %>% filter(time == b)
    s$instation <- s$mentry + left
    t <- tr %>% filter(between(time, j, j+15))
    if(nrow(t) > 0){
      if(any(t$transfer > 0)){s$instation <- s$instation + sum(t$transfer)}
      left <- s$instation - sum(t$entry)
      if(left < 0){left <- 0}#not sure why sometimes I have neg numbers there..... I actually think it was because i forgot the else... now should be ok.
      onestat[[k]] <- data.frame(stop_id = s$stop_id, time = s$time, instop = s$instation, takingtrain = t$entry, left, traintime = t$time, leavingtrain = t$exit, leavingstop = s$mexit, idx = t$idx)   
      k <- k + 1
    }else{left <- s$instation}
    if(left < before$mexit){left <- 0}else{left <- left - before$mexit}
  }
  station_flux[[i]] <- do.call(rbind, onestat) 
}
station_flux <- do.call(rbind, station_flux)
station_flux <- station_flux[which(!duplicated(station_flux$idx)),]

train$departure_time <- as.character(train$departure_time) 
pf <- left_join(train, station_flux, by='idx')
pf <- pf %>% select(stop_id = stop_id.x, time = time.y, instop,route_id, direction_id, trip_id, id, traintime, hour, min, takingtrain, intrain, left,  leavingtrain, leavingstop)
write.csv(pf, file = 'station_train.csv')


#######################################################################################################
##
#########################################################################################################
train <- read.csv('station_train.csv', row.names = NULL)
station <- read.csv('station.csv', row.names = NULL)
#sched <- read.csv('schedule.csv', row.names = NULL)

library(ggplot2)
library(grid)
library(dplyr)


t <- train[which(train$trip_id == '92875A744B2171'),]
t <- train[train$id == 171,]
t <- train[train$id == 109,]
t <- train[train$id == 7,]

t <- left_join(t, station, by = 'stop_id')
t$idx <- 1:dim(t)[1]
l <- dim(t)[1]
ggplot(data=t, aes(x = idx, y = 1, label = stop_name)) + 
  geom_point(aes(size = intrain), colour = '#0000FF') + 
  geom_point(aes(x = idx-0.5, y = 1.25, size = instop), colour = '#4EEE94')+ 
  geom_point(aes(x = idx-0.25, y = 1.12, size = takingtrain), col = '#00F5FF')+ 
  geom_point(aes(x = idx+0.25, y = 0.88, size = leavingtrain), col = '#FF8C00')+ 
  geom_point(aes(x = idx+0.5, y = 0.75, size = left), colour = '#FF0000')+ 
  geom_text(aes(y = 1.5)) + 
  geom_text(aes(x = (l+2), y = 1.25, label = 'In station before'))+
  geom_text(aes(x = (l+2), y = 1.12, label = 'Taking the train'))+
  geom_text(aes(x = (l+2), y = 1, label = 'In the train'))+
  geom_text(aes(x = (l+2), y = 0.88, label = 'Leaving the train'))+
  geom_text(aes(x = (l+2), y = 0.75, label = 'Left in the station'))+
  geom_text(aes(x = (l/2), y = 0.55, label = 'Train direction'))+
  geom_segment(aes(x = 1, y = 0.6, xend = l, yend = 0.6), arrow = arrow(length = unit(0.5, 'cm')))+
  scale_size(range = c(0, 30), name = 'Number of people')+
  scale_y_continuous(limits = c(0.5, 1.5))+
  scale_x_continuous(limits = c(0, (l+3)))+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key = element_blank()
    )

#pf[which(pf$intrain > 700),]


