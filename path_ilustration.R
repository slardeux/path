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
alltrain <- read.csv('station_train.csv', row.names = NULL)
alltrain <- read.csv('station_train_corrected.csv', row.names = NULL)
station <- read.csv('station.csv', row.names = NULL)
library(ggplot2)
library(grid)
library(dplyr)
library(gridExtra)

get_size.f <- function(v, size){
  v <- data.frame(value = sqrt(v))
  out <- left_join(v, size)
  return(round(out$s))
}
get_todraw.f <- function(df, station, sz = TRUE){
  if(df$route_id[1] == 861){l<-8}else if(df$route_id[1] == 862){l<-6}
  df$idx <- rep(1:l, dim(df)[1]/l)
  station <- station %>% select(stop_id, stop_name)
  df <- left_join(df, station, by = 'stop_id')
  mx <- max(df$left, df$instop, df$intrain, na.rm = TRUE)
  size <- data.frame(value = sqrt(c(0:mx)), s = seq(0,40, length.out = mx+1))
  if(sz){
  df[c('instop', 'takingtrain', 'intrain', 'left', 'leavingtrain')] <- apply(df[c('instop', 'takingtrain', 'intrain', 'left', 'leavingtrain')], 2, get_size.f, size)  
  }
  return(df)
}

trains <- alltrain %>% filter(route_id %in% c(861, 862))

trains$routdir <- paste(trains$route_id, trains$direction_id, sep = '_')
t <- lapply(split(trains, trains$routdir), get_todraw.f, station)
t <- lapply(split(trains, trains$routdir), get_todraw.f, station, sz = FALSE)
#commonmintime <- max(sapply(t, function(x) min(x$traintime, na.rm= TRUE)))
tr <- do.call(rbind, t)
tr <- na.omit(tr)
tr <- tr %>% filter(between(traintime, 358, 1260))
tr <- arrange(tr, traintime)
animtime <- data.frame(traintime = unique(tr$traintime), anim_id = c(1:length(unique(tr$traintime))))
tr <- left_join(tr, animtime)



I have an entrepreneurial mindset and co-own a business where I am responsible for all technical aspects including development of the e-commerce website, stock management, accounting, and visualization. 

I have an entrepreneurial mindset and co-own a business where I am responsible for all technical aspects including development of the e-commerce website, stock management, accounting, and visualization. 
m33 <- tr%>% filter(direction_id == 0, route_id == 861)
nw <- tr %>% filter(direction_id == 1, route_id == 862)
wn <- tr %>% filter(direction_id == 0, route_id == 862)

get_toplot.f <- function(df, i){
  tmp <- df[which(df$anim_id <= i),]
  if(any(duplicated(tmp$idx))){
    rm <- which(tmp$idx == tmp$idx[dim(tmp)[1]])[1]
    rmdf <- which(df$anim_id == tmp$anim_id[rm])
    tmp <- tmp[-rm,]
    df <- df[-rmdf,]
  }
  return(list(df,tmp))
}
get_initial_plot.f <- function(l){
  tmp <- data.frame(x = c(1:l))
  p <- ggplot(data=tmp, aes(x = x, y=1)) + geom_point(colour = 'white') +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.grid= element_blank(),
      legend.position = 'none'
    )
  return(p)
}
get_plot.f <- function(tmp, l){
  tmp$hourmin <- paste(tmp$hour[dim(tmp)[1]], tmp$min[dim(tmp)[1]], sep = ':')
  tmp$l <- l
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
    geom_text(aes(x = (l+2), y = 1.5, label = hourmin),  colour = 'red', size = 10)+
    geom_segment(aes(x = 1, y = 0.6, xend = l, yend = 0.6), arrow = arrow(length = unit(0.5, 'cm')))+
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
  return(p) 
}

oopt <- ani.options(interval = 0.05)
flux_animation <- function(){
  p1 <- get_initial_plot.f(8)
  p2 <- get_initial_plot.f(8)
  p3 <- get_initial_plot.f(6)
  p4 <- get_initial_plot.f(6)
  for(i in tr$anim_id){ 
    j3tmp <- get_toplot.f(j3, i)
    j3 <- j3tmp[[1]]
    m33tmp <- get_toplot.f(m33, i)
    m33 <- m33tmp[[1]]
    nwtmp <- get_toplot.f(nw, i)
    nw <- nwtmp[[1]]
    wntmp <- get_toplot.f(wn, i)
    wn <- wntmp[[1]]
    
    if(nrow(j3tmp[[2]]) > 0){p1 <- get_plot.f(j3tmp[[2]],8)}
    if(nrow(m33tmp[[2]]) > 0){p2 <- get_plot.f(m33tmp[[2]],8)}
    if(nrow(nwtmp[[2]]) > 0){p3 <- get_plot.f(nwtmp[[2]],8)}
    if(nrow(wntmp[[2]]) > 0){p4 <- get_plot.f(wntmp[[2]],8)}    
    grid.arrange(p1, p2,p3,p4, ncol = 2)
    animation::ani.pause()
  }
}

saveHTML(flux_animation(), autoplay = FALSE, loop = FALSE, verbose = TRUE, outdir = "imgflux", htmlfile ='flux.html', ani.height = 500, ani.width = 1000,  single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")


#################################################################################################
# in tarin histograms
###################################################################################################

j3 <- j3 %>% filter(stop_name != '33r')
j3$stop_name <- j3$stop_name[,drop = TRUE]
j3$stop_name <- factor(j3$stop_name, levels = levels(j3$stop_name)[c(6,5,7,4,3,1,2)])
#levels(j3$stop_name) <- levels(j3$stop_name)[c(6,5,7,4,3,1,2)]

ggplot(j3, aes(x = traintime, y = intrain)) + geom_bar(stat = 'identity', colour = 'blue')+
  facet_wrap(~stop_name, ncol = 4) + ylab('Number of person in train') + 
  geom_hline(yintercept = 700, colour = 'red')+
  theme_bw() + 
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = 'none'
    )


nw <- nw %>% filter(stop_name != 'WTC')
nw$stop_name <- nw$stop_name[,drop = TRUE]
nw$stop_name <- factor(nw$stop_name, levels = levels(nw$stop_name)[c(5,3,4,2,1)])
#levels(j3$stop_name) <- levels(j3$stop_name)[c(6,5,7,4,3,1,2)]

ggplot(nw, aes(x = time, y = intrain)) + geom_bar(stat = 'identity', colour = 'blue')+
  facet_wrap(~stop_name, ncol = 3) + ylab('Number of person in train') + 
  geom_hline(yintercept = 800, colour = 'red')+
  theme_bw() + 
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = 'none'
  )





