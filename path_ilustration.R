setwd("/media/data/data_project/B001_group_projects/SylvieCode")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(png)
library(animation)

x <- c(1,1.2,2,3,3.5,3.5,4,5,5,5,5,5,5)
y <- c(1,1,1,1,2,3,1,1,2.5,2.9,3.3,3.6,4)
xy <- data.frame(x,y)
ggplot(xy, aes(x,y)) + geom_point(size = 4) + scale_y_continuous(limits = c(1,4)) + scale_x_continuous(limits = c(1,5)) 
  theme_bw() + theme(panel.grid = element_blank(), axis.title =element_blank(), axis.text = element_blank(),  axis.ticks = element_blank()) 
ggsave('station.eps')

station <- read.csv('station.csv', row.names=NULL)
img <- readPNG('station.png')
# get x and y for each station
station$x <- c(3.45,4.85,4.87,4.87,4.87,4.87,4.95,3.98,3.02,2.05,1.3,1.1,3.45)
station$y <- c(3,2.6,2.9,3.27,3.55,3.93,1.15,1.15,1.2,1.2,1.12,1.12,2.05)
station$idx <- 1:dim(station)[1]
nwp <- station[13,]
js1 <- station[10,]
lines <- read.csv('schedule.csv', row.names = NULL)
lines <- lines %>% group_by(route_id, direction_id, stop_name, stop_sequence) %>% select(route_id, direction_id, stop_name, stop_sequence) %>% summarize(count = n())

get_routes.f <- function(l){
  out <- paste(l$stop_name[which(l$stop_sequence == 1)], l$stop_name[which(l$stop_sequence == max(l$stop_sequence))], sep = ' -> ')
  out <- data.frame(count = l$count[1], route = out)
  return(out)
}
routes <- do.call(rbind, lapply(split(lines, lines$count), get_routes.f))
routes$rid <- 1:dim(routes)[1] 
lines <- left_join(lines, routes)

get_station.f <- function(l, stat, ee){
  if(stat %in% l$stop_name){
    n <- l$stop_sequence[which(l$stop_name == stat)]
    if(ee == 'entry' & n != max(l$stop_sequence)){   
      out <- l %>% filter(stop_sequence > n) 
    }else if(ee == 'exit' & n != min(l$stop_sequence)){
      out <- l %>% filter(stop_sequence < n)   
    }else{return(NULL)}
    out <- arrange(data.frame(out), stop_sequence)
   out$statid <- 1:dim(out)[1]
   return(out)
  }else{return(NULL)}
} 
## get entry at Newport
enterPav <- lapply(split(lines, lines$count), get_station.f, 'Pav', 'entry')
enterPav <- do.call(rbind, enterPav)
enterPav <- left_join(enterPav, station)
enterPav <- enterPav[which(enterPav$route_id != 1024),]
enterPav$rid <- as.factor(enterPav$rid)
levels(enterPav$rid) <- c(1:nlevels(enterPav$rid))

## get exit at JS
exitJS <- lapply(split(lines, lines$count), get_station.f, 'Jou', 'exit')
exitJS <- do.call(rbind, exitJS)
exitJS <- left_join(exitJS, station)
exitJS <- exitJS[which(exitJS$route_id != 1024),]
exitJS$rid <- as.factor(exitJS$rid)
levels(exitJS$rid) <- c(1:nlevels(exitJS$rid))

oopt <- ani.options(interval = 1)

get_animation.f <- function(stationdf, action, station, pt){
  for (i in 1:length(levels(stationdf$rid))){
    ln <- stationdf %>% filter(rid == i)
    change <- FALSE
    when <- 0
    if(action == 'entry' & ln$route[1] == '33r -> Jou'){
      toadd <- ln[1:2,]
      toadd$x <- c(station$x[which(station$stop_name %in% c('Har', 'New'))])
      toadd$y <- c(station$y[which(station$stop_name %in% c('Har', 'New'))])
      toadd$statid <- c(max(ln$statid)-1, max(ln$statid)) + 2
      ln <- rbind(ln, toadd)
      change <- TRUE
      when <- max(ln$statid)
    }else if(action == 'exit' & ln$route[1] == '33r -> Jou'){
      toadd <- ln[1,]
      toadd$x <- c(station$x[which(station$stop_name == 'Hob')])
      toadd$y <- c(station$y[which(station$stop_name == 'Hob')])
      toadd$statid <- max(ln$statid) + 1
      ln <- rbind(ln, toadd)
      change <- TRUE
      when <- max(ln$statid)
    }
    for(j in 1:max(ln$statid)){
      txt <- ln$route[1]
      sn <- ln %>% filter(statid <= j) 
      g <- pt + geom_point(data = sn, aes(x, y), colour = 'red', size = 10) + geom_text(aes(x = 1.7, y = 3.9), label = txt, colour = 'yellow', size = 16)
      if(change & j == when){
        g <- g + geom_text(aes(x = 2, y = 2.5), label = 'Transfert!', colour = 'red', size = 16)
      }
      print(g)
      animation::ani.pause()
    }
    animation::ani.pause()
    animation::ani.pause()
  }
}
exit_animation = function(){
  
  p <- ggplot(station, aes(x,y)) + scale_y_continuous(limits = c(1,4)) + scale_x_continuous(limits = c(1,5))+
    annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
    geom_point(data = nwp, size = 1, colour = 'black') + 
    theme(panel.grid = element_blank(), axis.title =element_blank(), axis.text = element_blank(),  axis.ticks = element_blank())
  
  print(p + geom_text(aes(x = 2.6, y = 3.9), label = 'Destination from Newport', colour = 'yellow', size = 16)) 
  animation::ani.pause()
  p1 <- p + geom_point(data = nwp, size = 20, colour = 'blue') 
  animation::ani.pause()
  print(p1)
  get_animation.f(enterPav, 'entry', station, p1)
  print(p)
   print(p + geom_text(aes(x = 2.6, y = 3.9), label = 'Origins to Journal Square', colour = 'yellow', size = 16)) 
   animation::ani.pause()
   p2 <- p + geom_point(data = js1, aes(x,y), size = 20, colour = 'blue') 
   
   print(p2)
  animation::ani.pause()
  print(p2)
  animation::ani.pause()
  get_animation.f(exitJS, 'exit', station, p2)
  print(p)
}
saveHTML(exit_animation(), autoplay = FALSE, loop = FALSE, verbose = TRUE, outdir = "images", htmlfile ='entry_exit.html', ani.height = 500, ani.width = 800,  single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")



######################################################################################################
## sankey
####################################################################################################
ep <- enterPav %>% select(stop_name, route, rid)
ep$rid <- as.numeric(ep$rid)
ep$route <- as.character(ep$route)

toadd <- lines[which(lines$route == unique(lines$route)[7] & lines$stop_name %in% c('Har', 'New')),] %>% select(stop_name, route) %>% mutate(rid = 3)
ep <- rbind(ep, toadd[,-c(1,2)])

ep$w <- ifelse(ep$stop_name != 'Jou', 1,3)
w1 <- unlist(ep %>% group_by(rid) %>% summarise(ntrain =n()) %>% select(ntrain), use.names = FALSE)

fromNwp <- data.frame(From = c(rep('Newport',length(unique(ep$rid))),rep(unique(ep$route)[-5], c(1,2,2,5)), rep('Jou',2)),
            To = c(unique(ep$route)[-5], as.character(ep$stop_name)),
            Weight = c(w1, ep$w[1:10], 1,1)
              )
save(fromNwp, file = 'fromNwp')
sankEntry <- gvisSankey(fromNwp, from="From", to="To", weight="Weight",
                     options=list(
                       sankey="{link: {color: { fill: '#FA8500' } },
                            node: { color: { fill: '#AB0000' },
                            label: { color: '#000087' } }}"))
plot(sankEntry)
save(sankEntry, file = 'SankEntry')

js <- exitJS%>% select(stop_name, route, rid)
js$rid <- as.numeric(js$rid)
js$route <- as.character(js$route)
toadd <- lines[which(lines$route == unique(lines$route)[4] & lines$stop_name == 'Hob'),] %>% select(stop_name, route) %>% mutate(rid = 1)
js <- rbind(js, toadd[,-c(1,2)])


toJS <- data.frame(From = c('Hob', as.character(js$stop_name[1:12]), unique(js$route)[-4]),
To = c('Pav', rep(unique(js$route)[-4], c(7,2,3)), rep('Journal Square',3)),
Weight = c(4,c(19,8,9,10,7,6,1),c(7,3),c(28,1,1),c(60,10,30))
)

sankExit<- gvisSankey(toJS, from="From", to="To", weight="Weight",
                        options=list(
                          sankey="{link: {color: { fill: '#FA9A46' } },
                            node: { color: { fill: '#AB0000' },
                            label: { color: '#000087' } }}", width = 500))
plot(sankExit)

save(sankExit, file = 'sankExit')

###############################################################################################################
## Travel 
#############################################################################################################
train <- read.csv('station_train.csv', row.names = NULL)
station <- read.csv('station.csv', row.names = NULL)
library(ggplot2)
library(grid)
library(dplyr)
#t <- train[which(train$trip_id == '92875A744B2171'),]

j3 <- train %>% filter(direction_id == 1, route_id == 861)
j3$idx <- rep(1:8, dim(j3)[1]/8)
station <- station %>% select(stop_id, stop_name)
j3 <- left_join(j3, station, by = 'stop_id')
j3 <- arrange(j3, traintime)
j3$anim_id <- 1:dim(j3)[1]
mx <- max(j3$left, j3$instop, j3$intrain)
size <- data.frame(value = c(0:mx), s = seq(0,40, length.out = mx+1))
get_size.f <- function(v, size){
  v <- data.frame(value = v)
  out <- left_join(v, size)
  return(round(out$s))
}
j3[c('instop', 'takingtrain', 'intrain', 'left', 'leavingtrain')] <- apply(j3[c('instop', 'takingtrain', 'intrain', 'left', 'leavingtrain')], 2, get_size.f, size)
l <- 8
oopt <- ani.options(interval = 0.1)
flux_animation <- function(){
for(i in j3$anim_id){  
    tmp <- j3[which(j3$anim_id <= i),]
    time <- paste(j3$hour[which(j3$anim_id == i)], j3$min[which(j3$anim_id == i)], sep = ':')
    if(any(duplicated(tmp$idx))){
      rm <- which(tmp$idx == tmp$idx[dim(tmp)[1]])[1]
      rmj3 <- which(j3$anim_id == tmp$anim_id[rm])
      tmp <- tmp[-rm,]
      j3 <- j3[-rmj3,]
    }
   p <- ggplot(data=tmp, aes(x = idx, y = 1, label = stop_name)) + 
     geom_point(size = tmp$intrain, colour = '#0000FF') + 
     geom_point(aes(x = idx-0.5, y = 1.25), colour = '#4EEE94', size = tmp$instop)+ 
     geom_point(aes(x = idx-0.25, y = 1.12), col = '#00F5FF', size = tmp$takingtrain)+ 
     geom_point(aes(x = idx+0.25, y = 0.88), col = '#FF8C00', size = tmp$leavingtrain)+ 
     geom_point(aes(x = idx+0.5, y = 0.75), colour = '#FF0000', size = tmp$left)+ 
     geom_text(aes(y = 1.5)) + 
     geom_text(aes(x = (l+2), y = 1.25, label = 'In station before'))+
     geom_text(aes(x = (l+2), y = 1.12, label = 'Taking the train'))+
     geom_text(aes(x = (l+2), y = 1, label = 'In the train'))+
     geom_text(aes(x = (l+2), y = 0.88, label = 'Leaving the train'))+
     geom_text(aes(x = (l+2), y = 0.75, label = 'Left in the station'))+
     geom_text(aes(x = (l/2), y = 0.55, label = 'Train direction'))+
     geom_text(aes(x = (l+2), y = 1.5, label = paste(hour)))+
     geom_segment(aes(x = 1, y = 0.6, xend = l, yend = 0.6), arrow = arrow(length = unit(0.5, 'cm')))+
     #scale_size(range = c(0, 30), name = 'Number of people')+
     scale_y_continuous(limits = c(0.5, 1.5))+
     scale_x_continuous(limits = c(0, (l+3)))+
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid= element_blank(),
      legend.position = 'none'
    )
   print(p)
   animation::ani.pause()
  }
}

saveHTML(flux_animation(), autoplay = FALSE, loop = FALSE, verbose = TRUE, outdir = "imgflux", htmlfile ='flux.html', ani.height = 500, ani.width = 800,  single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")






