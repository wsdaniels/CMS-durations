# Description: Makes plots for CMS durations manuscript
# Author: William Daniels (wdaniels@mines.edu)
# Last Updated: May 1, 2024

# Clear environment
if(!is.null(dev.list())){dev.off()}
rm(list = ls())

# Import necessary libraries
library(lubridate)
library(zoo)
library(scales)

if (commandArgs()[1] == "RStudio"){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}



# START USER INPUT
#---------------------------------------------------------------------------

duration.estimates <- readRDS('../output_data/case_study_duration_estimates.RData')

save.dir <- '../'

# END OF USER INPUT - NO MODIFICATION NECESSARY BELOW THIS POINT
#---------------------------------------------------------------------------




# STEP 1: PARSE OUT RESULTS
#---------------------------------------------------------------------------

all.durations          <- duration.estimates$all.durations
est.durations          <- duration.estimates$est.durations
info.list              <- duration.estimates$info.list
spikes                 <- duration.estimates$spikes
rate.est.all.events    <- duration.estimates$rate.est.all.events
loc.est.all.events     <- duration.estimates$loc.est.all.events
source.names           <- duration.estimates$source.names
start.bounds           <- duration.estimates$start.bounds
end.bounds             <- duration.estimates$end.bounds
original.durations     <- duration.estimates$original.durations
max.obs                <- duration.estimates$max.obs
error.lower.all.events <- duration.estimates$error.lower.all.events
error.upper.all.events <- duration.estimates$error.upper.all.events

# Pull event "event numbers" that uniquely identify each naive event
event.nums <- na.omit(unique(spikes$events))

# Number of naive events
n.ints <- length(event.nums)



# STEP 2: PLOT EACH EVENT, ASSOCIATED INFORMATION MASK, AND DURATION DISTRIBUTION
#---------------------------------------------------------------------------

for (t in 1:n.ints){
  
  print(paste0(t, "/", n.ints))
  
  par(mfrow = c(2,1))
  par(mar = c(2,2,2,2))
  
  this.mask <- seq(min(which(spikes$events == event.nums[t])),
                   max(which(spikes$events == event.nums[t])))
  
  info.mask <- info.list[[which(names(info.list) == loc.est.all.events[t])]]
  
  start.time <- spikes$time[this.mask][1]- hours(12)
  end.time <- spikes$time[this.mask][length(this.mask)]+ hours(12)
  
  to.plot <- source.names
  
  if (loc.est.all.events[t] %in% to.plot){
    
    ylim.vals <- c(event.nums[t] - 1, event.nums[t] + 1)
    
    plot(spikes$time, spikes$events, pch = 19, xlim = c(start.time, end.time), ylim = ylim.vals,
         col = NA, main = "")
    mtext(loc.est.all.events[t], adj = 0)
    
    abline(v = spikes$time[!is.na(info.mask$events)], col = alpha("royalblue", 0.25))
    
    segments(x0 = spikes$time[this.mask], y0 = event.nums[t] - 0.25, y1 = event.nums[t] + 0.25)
    
    for (tt in seq(1,n.ints)[-t]){
      other.event.mask <- seq(min(which(spikes$events == event.nums[tt])),
                              max(which(spikes$events == event.nums[tt])))
      segments(x0 = spikes$time[other.event.mask], y0 = event.nums[t] - 0.25, y1 = event.nums[t] + 0.25,
               col = "gray44")
    }
    
    segments(x0 = start.bounds[t], y0 = event.nums[t] - 0.5, y1 = event.nums[t] + 0.5, col = "red", lwd = 2)
    segments(x0 = end.bounds[t], y0 = event.nums[t] - 0.5, y1 = event.nums[t] + 0.5, col = "red", lwd= 2)
    segments(x0 = spikes$time[this.mask], y0 = event.nums[t] - 0.25, y1 = event.nums[t] + 0.25)
    
    xlim.vals <- c(0, ceiling(max(c(all.durations[[t]]))))
    
    hist(all.durations[[t]], xlim = xlim.vals, main = t, breaks = seq(0,max(xlim.vals)))
    
    abline(v = mean(all.durations[[t]]), col = "blue", lwd = 6)
    abline(v = original.durations[t], col = "purple", lwd = 4)
    
    
  }
  
}



# STEP 3: PLOT EQUIPMENT-LEVEL DURATION DISTRIBUTIONS
#---------------------------------------------------------------------------

# Top plot shows distribution using proposed duration model
# Bottom plot shows distribution using naive method 

# Mean is used to create a point estimate for each event

n.s <- length(source.names)

par(mfrow = c(2,1))
par(mar = c(2,2,2,2))

for (i in 1:n.s){
  
  breaks.max <- ceiling(max(est.durations[[i]], original.durations))
  xlim.max <- 12
  
  
  hist(est.durations[[i]], xlim = c(0,xlim.max), breaks = seq(0,breaks.max,0.5),
       main = source.names[i], freq= F)
  abline(v = mean(est.durations[[i]]), col = "royalblue", lwd= 3)
  
  
  source.mask <- loc.est.all.events == source.names[i]
  
  hist(original.durations[source.mask], xlim = c(0,xlim.max),
       breaks = seq(0,breaks.max,0.5), main = "", freq = F)
  abline(v = mean(original.durations[source.mask]), col = "purple", lwd= 3)
  
}


# STEP 4: PLOT DURATION DISTRIBUTIONS OF ALL EQUIPMENT GROUPS TOGETHER
#---------------------------------------------------------------------------


# Set colors for plots
tank.color <- "#3062CF" #blue
wellhead.east.color <- "#9147B8" #purple
separator.east.color <- "#7EAD52" #green
separator.west.color <- "#F1C30E" #gold
wellhead.west.color <- "#C7383C" #red
cols.for.hists <- c(wellhead.east.color, tank.color, wellhead.west.color, separator.west.color, separator.east.color)

par(mfrow = c(2,3))
par(mar = c(2,2,2,2))

xlim.max <- max(ceiling(sapply(est.durations, max)))+1

for (i in 1:n.s){
  
  hist(est.durations[[i]], xlim = c(0,xlim.max), breaks = seq(0,xlim.max,2),
       main = source.names[i], freq= F)
  abline(v = mean(est.durations[[i]]), col = cols.for.hists[i], lwd= 3)
  mtext(round(mean(est.durations[[i]]),2), side = 3, line = -1)
}





# STEP 5: PLOT TIME SERIES OF DLQ RESULTS
#---------------------------------------------------------------------------

# Get start, mid, and end times for each event
event.start.times <- event.end.times <- event.mid.times <- vector(length = length(event.nums))
for (i in 1:length(event.nums)){
  event.start.times[i] <- spikes$time[min(which(spikes$events == event.nums[i]))]
  event.end.times[i]   <- spikes$time[max(which(spikes$events == event.nums[i]))]
  event.mid.times[i]   <- mean(c(event.start.times[i], event.end.times[i]))
}


# Set plotting colors for estimated leak location data
rate.est.all.events.cols <- vector(length = n.ints)
rate.est.all.events.cols[loc.est.all.events == "Wellheads1"] <- wellhead.east.color
rate.est.all.events.cols[loc.est.all.events == "Wellheads2"] <- wellhead.west.color
rate.est.all.events.cols[loc.est.all.events == "Separator"] <- separator.west.color
rate.est.all.events.cols[loc.est.all.events == "Production.Unit"] <- separator.east.color
rate.est.all.events.cols[loc.est.all.events == "Tanks"] <- tank.color

times <- spikes$time

plot.divs <- c(seq(min(times), max(times)+days(3), by = "5 days"))

axis.points <- seq(min(round_date(times, "day")-days(1)),
                   max(round_date(times, "day")+days(1)),
                   by = "day")
axis.labs <- paste0(month.abb[month(axis.points)],
                    "-", day(axis.points))

minor.axis.points <- seq(min(round_date(times, "day")-days(1)),
                         max(round_date(times, "day")+days(1)),
                         by = "6 hours")




png(paste0(save.dir, 'case_study_time_series_1.png'),
    res = 100, width = 1920, height = 1080, pointsize = 32)

par(mgp = c(2, 0.75, 0))
par(mar = c(1.5, 3.75, 0.5, 3.75))
par(mfrow = c(4,1))


for (i in 1:4){
  
  ylim.max <- 8
  
  plot(times, max.obs, col = "white", 
       ylim = c(0,ylim.max),
       xlim = c(plot.divs[i], plot.divs[i+1]),
       ylab = "",
       yaxt = "n",
       xlab = "",
       xaxt= "n")
  
  axis(side = 1, at = axis.points, labels = rep("", length(axis.points)), lwd = 4)
  axis(side = 1, at = axis.points, labels = axis.labs, lwd = 0, line = -0.4)
  axis(side = 1, at = minor.axis.points, labels = NA)
  
  rect(xleft = event.start.times, 
       xright = event.end.times,
       ybottom = 0, ytop = ylim.max, 
       col = alpha(rate.est.all.events.cols, 0.25))
  
  lines(times, max.obs, col = "gray45")
  
  points(event.mid.times, rate.est.all.events, pch = 19)
  
  segments(x0 = event.mid.times, 
           y0 = error.lower.all.events,
           y1 = error.upper.all.events,
           lwd = 2)
  
  mtext("Methane Concentration [ppm]", side = 2, outer = T, line = -1.25,
        col = "gray45")
  
  axis(side = 2, at = seq(0,8,by=2), col.axis = "gray45", col.ticks = "gray45")
  
  mtext("Methane Emission Rate [kg/hr]", side = 4, outer = T, line = -1.25)
  
  axis(side = 4, at = seq(0,8,by=2))
  
  
}


dev.off()



png(paste0(save.dir, 'case_study_time_series_2.png'),
    res = 100, width = 1920, height = 1080, pointsize = 32)

par(mgp = c(2, 0.75, 0))
par(mar = c(1.5, 3.75, 0.5, 3.75))
par(mfrow = c(4,1))


for (i in 5:8){
  
  ylim.max <- 8
  
  plot(times, max.obs, col = "white", 
       ylim = c(0,ylim.max),
       xlim = c(plot.divs[i], plot.divs[i+1]),
       ylab = "",
       yaxt = "n",
       xlab = "",
       xaxt= "n")
  
  axis(side = 1, at = axis.points, labels = rep("", length(axis.points)), lwd = 4)
  axis(side = 1, at = axis.points, labels = axis.labs, lwd = 0, line = -0.4)
  axis(side = 1, at = minor.axis.points, labels = NA)
  
  rect(xleft = event.start.times, 
       xright = event.end.times,
       ybottom = 0, ytop = ylim.max, 
       col = alpha(rate.est.all.events.cols, 0.25))
  
  lines(times, max.obs, col = "gray45")
  
  points(event.mid.times, rate.est.all.events, pch = 19)
  
  segments(x0 = event.mid.times, 
           y0 = error.lower.all.events,
           y1 = error.upper.all.events,
           lwd = 2)
  
  mtext("Methane Concentration [ppm]", side = 2, outer = T, line = -1.25,
        col = "gray45")
  
  axis(side = 2, at = seq(0,8,by=2), col.axis = "gray45", col.ticks = "gray45")
  
  mtext("Methane Emission Rate [kg/hr]", side = 4, outer = T, line = -1.25)
  
  axis(side = 4, at = seq(0,8,by=2))
  
  if (i == 8){
    
    legend("right",
           legend = c("Wellheads1", "Wellheads2", "Separator", "Production Unit", "Tanks"),
           fill = alpha(c(tank.color, wellhead.west.color, separator.west.color, wellhead.east.color, separator.east.color), 0.25),
           border = c("black", "black", "black", "black", "black"),
           cex = 0.9, 
           inset = c(0.005, 0))
    
  }
  
  
}


dev.off()




png(paste0(save.dir, 'case_study_time_series_3.png'),
    res = 100, width = 1920, height = 1080, pointsize = 32)

par(mgp = c(2, 0.75, 0))
par(mar = c(1.5, 3.75, 0.5, 3.75))
par(mfrow = c(4,1))


for (i in 9:12){
  
  ylim.max <- 8
  
  plot(times, max.obs, col = "white", 
       ylim = c(0,ylim.max),
       xlim = c(plot.divs[i], plot.divs[i+1]),
       ylab = "",
       yaxt = "n",
       xlab = "",
       xaxt= "n")
  
  axis(side = 1, at = axis.points, labels = rep("", length(axis.points)), lwd = 4)
  axis(side = 1, at = axis.points, labels = axis.labs, lwd = 0, line = -0.4)
  axis(side = 1, at = minor.axis.points, labels = NA)
  
  rect(xleft = event.start.times, 
       xright = event.end.times,
       ybottom = 0, ytop = ylim.max, 
       col = alpha(rate.est.all.events.cols, 0.25))
  
  lines(times, max.obs, col = "gray45")
  
  points(event.mid.times, rate.est.all.events, pch = 19)
  
  segments(x0 = event.mid.times, 
           y0 = error.lower.all.events,
           y1 = error.upper.all.events,
           lwd = 2)
  
  mtext("Methane Concentration [ppm]", side = 2, outer = T, line = -1.25,
        col = "gray45")
  
  axis(side = 2, at = seq(0,8,by=2), col.axis = "gray45", col.ticks = "gray45")
  
  mtext("Methane Emission Rate [kg/hr]", side = 4, outer = T, line = -1.25)
  
  axis(side = 4, at = seq(0,8,by=2))
  
}


dev.off()







png(paste0(save.dir, 'case_study_time_series_4.png'),
    res = 100, width = 1920, height = 1080, pointsize = 32)

par(mgp = c(2, 0.75, 0))
par(mar = c(1.5, 3.75, 0.5, 3.75))
par(mfrow = c(4,1))


for(i in 13:14){
  
  ylim.max <- 8
  
  plot(times, max.obs, col = "white", 
       ylim = c(0,ylim.max),
       xlim = c(plot.divs[i], plot.divs[i+1]),
       ylab = "",
       yaxt = "n",
       xlab = "",
       xaxt= "n")
  
  axis(side = 1, at = axis.points, labels = rep("", length(axis.points)), lwd = 4)
  axis(side = 1, at = axis.points, labels = axis.labs, lwd = 0, line = -0.4)
  axis(side = 1, at = minor.axis.points, labels = NA)
  
  rect(xleft = event.start.times, 
       xright = event.end.times,
       ybottom = 0, ytop = ylim.max, 
       col = alpha(rate.est.all.events.cols, 0.25))
  
  lines(times, max.obs, col = "gray45")
  
  points(event.mid.times, rate.est.all.events, pch = 19)
  
  segments(x0 = event.mid.times, 
           y0 = error.lower.all.events,
           y1 = error.upper.all.events,
           lwd = 2)
  
  mtext("Methane Concentration [ppm]", side = 2, outer = T, line = -1.25,
        col = "gray45")
  
  axis(side = 2, at = seq(0,8,by=2), col.axis = "gray45", col.ticks = "gray45")
  
  mtext("Methane Emission Rate [kg/hr]", side = 4, outer = T, line = -1.25)
  
  axis(side = 4, at = seq(0,8,by=2))
  
  if (i == 14){
    
    legend("right",
           legend = c("Wellheads1", "Wellheads2", "Separator", "Production Unit", "Tanks"),
           fill = alpha(c(tank.color, wellhead.west.color, separator.west.color, wellhead.east.color, separator.east.color), 0.25),
           border = c("black", "black", "black", "black", "black"),
           cex = 0.9, 
           inset = c(0.005, 0))
    
  }
  
  
}


dev.off()



# STEP 6: PLOT CASE STUDY SUMMARY DATA
#---------------------------------------------------------------------------

png(paste0(save.dir, '/case_study_summary.png'),
    res = 100, pointsize = 28, width = 1920*0.9, height = 1080*0.66)

par(mgp = c(1.5, 0.5, 0))
par(mar = c(3, 1, 1, 1))
# par(oma = c(0,0,0.5,3))

layout.mat <- matrix(c(1,1,1,2,2,2), nrow = 1, byrow = T)
layout(layout.mat)


plot.new()

wellhead1.color <- "#9147B8" #purple
wellhead2.color <- "#C7383C" #red
separator.color <- "#F1C30E" #gold
production.unit.color <- "#7EAD52" #green
tank.color <- "#3062CF" #blue

wellhead1.pch <- 22 # square
wellhead2.pch <- 25 # down triangle
separator.pch <- 24 # up triangle
production.unit.pch <- 23 # diamond
tank.pch <- 21 # circle

names(est.durations)
cols.for.summary <- c(production.unit.color, wellhead1.color, wellhead2.color, separator.color, tank.color)
pch.for.summary <- c(production.unit.pch, wellhead1.pch, wellhead2.pch, separator.pch, tank.pch)

est.duration.means <- sapply(est.durations, mean)
this.order <- rev(order(est.duration.means))

est.duration.means <- est.duration.means[this.order]
est.durations.tmp <- est.durations[this.order]
cols.for.summary <- cols.for.summary[this.order]
pch.for.summary <- pch.for.summary[this.order]

adj <- 0.12
right.adj <- 0.4




plot(est.duration.means, (1:5)+adj, 
     xlim = c(0,17), ylim = c(0.8, 5.4),
     pch = pch.for.summary,
     # pch = 19,
     col = cols.for.summary,
     bg = cols.for.summary,
     yaxt = "n", cex = 1.15, xaxt = "n",
     xlab = "Duration [hours]", ylab = "")

axis(side = 1, at = seq(0,16,2))

abline(v = seq(0,16, by = 2), lty = 1, col = "gray80")

points(est.duration.means, (1:5)+adj,
       pch = pch.for.summary,
       # pch = 19,
       col = cols.for.summary,
       bg = cols.for.summary,
       cex = 1.15)

segments(y0 = (1:5)+adj,
         x0 = sapply(est.durations.tmp, function(X) quantile(X, probs = 0.05)),
         x1 = sapply(est.durations.tmp, function(X) quantile(X, probs = 0.95)),
         col = cols.for.summary,
         lwd = 3, lty = 2)


for (i in 1:5){
  
  source.mask <- loc.est.all.events == source.names[this.order][i]
  
  segments(x0 = quantile(original.durations[source.mask], probs = 0.05),
           x1 = quantile(original.durations[source.mask], probs = 0.95),
           y0 = i-adj,
           col = cols.for.summary[i],
           lty = 3, lwd = 3)
  
  points(mean(original.durations[source.mask]), i-adj, 
         pch = pch.for.summary[i],
         cex = 1.25,
         col = cols.for.summary[i], bg = cols.for.summary[i])
  
  
}

set1 <- c(3,4,5)
set2 <- c(1,2)


text(x = sapply(est.durations.tmp[set1], function(X) quantile(X, probs = 0.95)) + right.adj, 
     y = set1 + adj,
     label = paste0(sprintf("%.1f", round(sapply(est.durations.tmp[set1], mean), 1)),
                    " [",
                    sprintf("%.1f", round(sapply(est.durations.tmp[set1], function(X) quantile(X, probs = 0.05)), 1)),
                    ", ",
                    sprintf("%.1f", round(sapply(est.durations.tmp[set1], function(X) quantile(X, probs = 0.95)), 1)),
                    "]"),
     adj = 0)


text(x = sapply(est.durations.tmp[set2], function(X) quantile(X, probs = 0.95)) - 3.5, 
     y = set2 + 0.275,
     label = paste0(sprintf("%.1f", round(sapply(est.durations.tmp[set2], mean), 1)),
                    " [",
                    sprintf("%.1f", round(sapply(est.durations.tmp[set2], function(X) quantile(X, probs = 0.05)), 1)),
                    ", ",
                    sprintf("%.1f", round(sapply(est.durations.tmp[set2], function(X) quantile(X, probs = 0.95)), 1)),
                    "]"),
     adj = 0)


for (i in 1:5){
  
  source.mask <- loc.est.all.events == source.names[this.order][i]
  
  if (i %in% set2){
    this.right.adj <- -3.5
  } else {
    this.right.adj <- right.adj
  }
  
  text(x = sapply(est.durations.tmp, function(X) quantile(X, probs = 0.95))[i] + this.right.adj, 
       y = i-adj,
       label = paste0(sprintf("%.1f", round(mean(original.durations[source.mask]), 1)),
                      " [",
                      sprintf("%.1f", round(quantile(original.durations[source.mask], probs = 0.05), 1)),
                      ", ",
                      sprintf("%.1f", round(quantile(original.durations[source.mask], probs = 0.95), 1)),
                      "]"),
       adj = 0)
  
  
}


source.names.to.plot <- c("Production Unit", "Wellheads1", "Wellheads2", "Separator", "Tank")

text(x = 0.2, 
     y = (1:5)+0.35,
     label = source.names.to.plot[this.order],
     adj = 0)



dev.off()





# STEP 7: PLOT EXAMPLE SNAPSHOT MEASUREMENT FIGURES
#---------------------------------------------------------------------------

# Set colors for plots
tank.color <- "#3062CF" #blue
wellhead.east.color <- "#9147B8" #purple
separator.east.color <- "#7EAD52" #green
separator.west.color <- "#F1C30E" #gold
wellhead.west.color <- "#C7383C" #red

wellhead1.color <- wellhead.east.color
wellhead2.color <- wellhead.west.color
separator.color <- separator.west.color
production.unit.color <- separator.east.color
tank.color <- tank.color


png(paste0(save.dir, 'case_study_example1.png'),
    res = 100, pointsize = 28, width = 1920, height = 1080*0.6)

par(mgp = c(1.5, 0.5, 0))
par(mar = c(3, 4, 1, 1))
par(oma = c(0,0,0.5,3))

layout.mat <- matrix(c(1,1,1,1,1,2,2,2), nrow = 1, byrow = T)

layout(layout.mat)


ylim.max <- 6
rect.max <- 4

plot.start <- as_datetime('2023-09-27T20:00:00', tz = "America/New_York")
plot.end <- as_datetime('2023-09-28T10:30:00', tz = "America/New_York")


minor.axis.points <- seq(round_date(plot.start, "day")-days(1),
                         round_date(plot.end, "day")+days(1),
                         by = "2 hours")


plot(times, max.obs, col = "white", 
     ylim = c(-1,7.25),
     xlim = c(plot.start, plot.end),
     ylab = "",
     lwd = 1.2,
     xlab = "Time of day",
     xpd = NA,
     yaxt = "n",
     xaxt= "n")

axis(side = 2, at = seq(0,6,2), col.axis = "gray45", col.ticks = "gray45")

axis(side = 1, at = minor.axis.points, labels = paste0(hour(minor.axis.points), ":00"))

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 6, 
     col = alpha(rate.est.all.events.cols, 0.25))

lines(times, max.obs, col = "gray45", lwd = 1.2)

mtext("Concentration [ppm]", side = 2, cex = 0.75, line =2.25,
      col = "gray45")

snapshot.time <- as_datetime('2023-09-28T5:50:00', tz = "America/New_York")

segments(x0 = snapshot.time,
         y0 = 0, y1 = 6, col = "black", lwd= 4, lty = 2)

par(new = TRUE)


plot(event.mid.times, rate.est.all.events, 
     pch = 19,
     ylim = c(-0.5,3.75-0.125),
     cex = 1.2,
     xlim = c(plot.start, plot.end), axes = F, ylab = "", xlab = "")

axis(side = 4, at= seq(0,3))

mtext("Emission Rate [kg/hr]", side = 4, cex = 0.75, line = 2.25)



segments(x0 = event.mid.times, 
         y0 = error.lower.all.events,
         y1 = error.upper.all.events,
         lwd = 3)




start.bounds <- event.starts <- end.bounds <- event.ends <- 
  combine.start <- combine.end <- vector(length = n.ints)

# Loop through events
for (t in 1:n.ints){
  
  this.mask <- seq(min(which(spikes$events == event.nums[t])),
                   max(which(spikes$events == event.nums[t])))
  
  info.mask <- info.list[[which(names(info.list) == loc.est.all.events[t])]]
  
  event.start <- this.mask[1]
  event.end <- this.mask[length(this.mask)]
  
  buffer.time <- 10
  
  use.start.buffer <- any(is.na(info.mask$events)[seq(max(1, this.mask[1]-1-buffer.time), 
                                                      this.mask[1]-1)])
  use.end.buffer <- any(is.na(info.mask$events)[seq(this.mask[length(this.mask)]+1,
                                                    min(nrow(spikes), this.mask[length(this.mask)]+1+buffer.time))])
  
  to.remove <- sort(unique(c(this.mask-buffer.time*use.start.buffer, this.mask, this.mask+buffer.time*use.end.buffer)))
  
  to.remove <- to.remove[to.remove > 0]
  to.remove <- to.remove[to.remove < (nrow(spikes)+1)]
  
  tmp <- info.mask$events
  tmp[to.remove] <- NA
  tmp <- which(!is.na(tmp))
  
  start.diff <- event.start - tmp 
  start.diff <- ifelse(start.diff < 0, NA, start.diff)
  start.bound <- spikes$time[tmp[which.min(start.diff)]] + minutes(1)
  
  end.diff <- tmp - event.end 
  end.diff <- ifelse(end.diff < 0, NA, end.diff)
  end.bound <- spikes$time[tmp[which.min(end.diff)]] - minutes(1)
  
  if (length(end.bound) == 0){
    end.bound <- spikes$time[length(spikes$time)]
  }
  
  if (length(start.bound) == 0){
    start.bound <- spikes$time[1]
  }
  
  if (t > 1){
    previous.event.times <- na.omit(spikes$time[spikes$events == event.nums[t-1]])
    previous.event.end <- previous.event.times[length(previous.event.times)]
    start.bound <- max(previous.event.end, start.bound)
  }
  
  if (t < n.ints){
    next.event.times <- na.omit(spikes$time[spikes$events == event.nums[t+1]])
    next.event.start <- next.event.times[1]
    end.bound <- min(next.event.start, end.bound)
  }
  
  start.bounds[t] <- start.bound
  end.bounds[t] <- end.bound
  event.starts[t] <- spikes$time[this.mask][1]
  event.ends[t] <- spikes$time[this.mask][length(this.mask)]
  
  
  if (t > 1){
    
    previous.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t-1])),
                                            max(which(spikes$events == event.nums[t-1])))]
    length.check <- F
    
    if (any(previous.event.times >= start.bound)){ 
      length.check <- T 
    } else {
      this.diff <- min(abs(as.numeric(difftime(start.bound, previous.event.times, units = "min"))))
      if (this.diff <= buffer.time){
        length.check <- T
      }
    }
    
    same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t-1]
    
    if (length.check & same.loc.est){ combine.start[t] <- T }
  }
  
  
  if (t < n.ints){
    
    next.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t+1])),
                                        max(which(spikes$events == event.nums[t+1])))]
    length.check <- F
    
    if (any(next.event.times <= end.bound)){ 
      length.check <- T 
    } else {
      this.diff <- min(abs(as.numeric(difftime(next.event.times, end.bound, units = "min"))))
      if (this.diff <= buffer.time){
        length.check <- T
      }
    }
    
    same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t+1]
    
    if (length.check & same.loc.est){ combine.end[t] <- T }
  }
  
} # end loop over events

event.starts <- as_datetime(event.starts, tz = "America/New_York")
event.ends <-   as_datetime(event.ends,   tz = "America/New_York")
start.bounds <- as_datetime(start.bounds, tz = "America/New_York")
end.bounds <-   as_datetime(end.bounds,   tz = "America/New_York")


n.samples <- 100000


all.weights <- vector(mode = "list", length = n.ints)
names(all.weights) <- loc.est.all.events

hard.stops <- vector(length = n.ints)

rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
rate.range <- rate.range[2] - rate.range[1]



t.ind <- which(spikes$events[spikes$time == snapshot.time] == event.nums) 

t <- t.ind

max.to.combine.start <- which(!(seq(1,n.ints) %in% (t-which(combine.end))))[1]-1
max.to.combine.end   <- which(!(seq(1,n.ints) %in% (which(combine.start)-t)))[1]-1

start.combine.options <- seq(0, max.to.combine.start)
end.combine.options   <- seq(0, max.to.combine.end)

start.time.sample <- vector(mode = "list", length = length(start.combine.options))
end.time.sample <- vector(mode = "list", length = length(end.combine.options))

for (i in 1:length(start.combine.options)){
  possible.start.times <- seq(start.bounds[t-start.combine.options[i]], event.starts[t-start.combine.options[i]], by = "1 min")
  start.time.sample[[i]] <- sample(possible.start.times, n.samples, replace = T)
}

for (i in 1:length(end.combine.options)){
  possible.end.times <- seq(event.ends[t+end.combine.options[i]], end.bounds[t+end.combine.options[i]], by = "1 min")
  end.time.sample[[i]] <- sample(possible.end.times, n.samples, replace = T)
}



n.start.options <- length(start.combine.options)
n.end.options <- length(end.combine.options)

similarity.score <- vector(length = n.start.options)

for (i in 1:n.start.options){
  jump <- start.combine.options[i]
  similarity.score[i] <- 1 - abs(rate.est.all.events[t-jump] - rate.est.all.events[t]) / rate.range
}

similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
similarity.score[is.na(similarity.score)] <- 0.5


if (n.start.options > 1){
  
  relative.probs <- cumprod(similarity.score)
  
  for (i in seq(length(relative.probs)-1, 1)){
    relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
  }
  
} else {
  relative.probs <- 1
}


start.sample.nums <- round(n.samples * relative.probs)
all.starts <- c()

for (i in 1:n.start.options){
  all.starts <- c(all.starts, sample(start.time.sample[[i]], start.sample.nums[i], replace = T))
}

all.starts <- sample(all.starts, n.samples, replace = T)

similarity.score <- vector(length = n.end.options)

for (i in 1:n.end.options){
  jump <- end.combine.options[i]
  similarity.score[i] <- 1 - abs(rate.est.all.events[t+jump] - rate.est.all.events[t]) / rate.range
}

similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
similarity.score[is.na(similarity.score)] <- 0.5

if (n.end.options > 1){
  
  relative.probs <- cumprod(similarity.score)
  
  for (i in seq(length(relative.probs)-1, 1)){
    relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
  }
  
} else {
  relative.probs <- 1
}


end.sample.nums <- round(n.samples * relative.probs)
all.ends <- c()

for (i in 1:n.end.options){
  all.ends <- c(all.ends, sample(end.time.sample[[i]], end.sample.nums[i], replace = T))
}

all.ends <- sample(all.ends, n.samples, replace = T)


durations.to.plot <- as.numeric(difftime(all.ends, all.starts, units = "hour"))




sim.ind <- which(names(info.list) == loc.est.all.events[t])

info.mask <- info.list[[sim.ind]]


times.to.plot <- !is.na(info.mask$events)

abline(h = -0.35, col = "gray80")

segments(x0 = info.mask$time[times.to.plot],
         y0 = -0.35, y1 = -0.35, lwd = 8)



hist(durations.to.plot, freq = F, ylab = "", yaxt = "n", ylim = c(0,0.5),
     main = "", xaxt = "n", xlim = c(0,10), xlab = "Duration [hours]",
     xpd = NA)

axis(side = 1, at = seq(0, 10, 2))

segments(x0 = mean(durations.to.plot), y0 = -999, y1 = 0.4, col = wellhead1.color, lwd = 4)
segments(x0 = quantile(durations.to.plot, probs = c(0.05, 0.95)), y0 = -999, y1 = 0.4, col = wellhead1.color, lwd = 4, lty = 4)

axis(side = 4, at = seq(0,0.4, by = 0.2))

mtext("Density", side = 4, line = 2.25, cex = 0.75)



dev.off()



naive.duration <- as.numeric(difftime(event.ends[t.ind], event.starts[t.ind], units = "hours"))

mean.duration <- mean(durations.to.plot)
max.duration <- max(durations.to.plot)

round(mean.duration / naive.duration, 1)

round(max.duration / naive.duration, 1)



# Set colors for plots
tank.color <- "#3062CF" #blue
wellhead.east.color <- "#9147B8" #purple
separator.east.color <- "#7EAD52" #green
separator.west.color <- "#F1C30E" #gold
wellhead.west.color <- "#C7383C" #red

wellhead1.color <- wellhead.east.color
wellhead2.color <- wellhead.west.color
separator.color <- separator.west.color
production.unit.color <- separator.east.color
tank.color <- tank.color


png(paste0(save.dir, 'case_study_example2.png'),
    res = 100, pointsize = 28, width = 1920, height = 1080*0.6)

par(mgp = c(1.5, 0.5, 0))
par(mar = c(3, 4, 1, 1))
par(oma = c(0,0,0.5,3))


layout.mat <- matrix(c(1,1,1,1,1,2,2,2), nrow = 1, byrow = T)

layout(layout.mat)


ylim.max <- 6
rect.max <- 4


plot.start <- as_datetime('2023-10-7T17:50:00', tz = "America/New_York")
plot.end <- as_datetime('2023-10-8T21:00:00', tz = "America/New_York")


snapshot.time <- as_datetime('2023-10-8T14:40:00', tz = "America/New_York")


minor.axis.points <- seq(round_date(plot.start, "day")-days(1),
                         round_date(plot.end, "day")+days(1),
                         by = "3 hours")

# For visual clarity, doesn't affect duration results
max.obs[max.obs > 2] <- 2


plot(times, max.obs, col = "white", 
     ylim = c(-1,7.25),
     xlim = c(plot.start, plot.end),
     ylab = "",
     lwd = 1.2,
     xlab = "Time of day",
     xpd = NA,
     yaxt = "n",
     xaxt= "n")

axis(side = 2, at = seq(0,6,2), col.axis = "gray45", col.ticks = "gray45")

axis(side = 1, at = minor.axis.points, labels = paste0(hour(minor.axis.points), ":00"))

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 6, 
     col = alpha(rate.est.all.events.cols, 0.25))

lines(times, max.obs, col = "gray45", lwd = 1.2)

mtext("Concentration [ppm]", side = 2, cex = 0.75, line =2.25,
      col = "gray45")

segments(x0 = snapshot.time,
         y0 = 0, y1 = 6, col = "black", lwd= 4, lty = 2)

par(new = TRUE)


plot(event.mid.times, rate.est.all.events, 
     pch = 19,
     ylim = c(-1,7.25),
     cex = 1.2,
     xlim = c(plot.start, plot.end), axes = F, ylab = "", xlab = "")

axis(side = 4, at= seq(0,6,2))

mtext("Emission Rate [kg/hr]", side = 4, cex = 0.75, line = 2.25)



segments(x0 = event.mid.times, 
         y0 = error.lower.all.events,
         y1 = error.upper.all.events,
         lwd = 3)


start.bounds <- event.starts <- end.bounds <- event.ends <- 
  combine.start <- combine.end <- vector(length = n.ints)

# Loop through events
for (t in 1:n.ints){
  
  this.mask <- seq(min(which(spikes$events == event.nums[t])),
                   max(which(spikes$events == event.nums[t])))
  
  info.mask <- info.list[[which(names(info.list) == loc.est.all.events[t])]]
  
  event.start <- this.mask[1]
  event.end <- this.mask[length(this.mask)]
  
  buffer.time <- 10
  
  use.start.buffer <- any(is.na(info.mask$events)[seq(max(1, this.mask[1]-1-buffer.time), 
                                                      this.mask[1]-1)])
  use.end.buffer <- any(is.na(info.mask$events)[seq(this.mask[length(this.mask)]+1,
                                                    min(nrow(spikes), this.mask[length(this.mask)]+1+buffer.time))])
  
  to.remove <- sort(unique(c(this.mask-buffer.time*use.start.buffer, this.mask, this.mask+buffer.time*use.end.buffer)))
  
  to.remove <- to.remove[to.remove > 0]
  to.remove <- to.remove[to.remove < (nrow(spikes)+1)]
  
  tmp <- info.mask$events
  tmp[to.remove] <- NA
  tmp <- which(!is.na(tmp))
  
  start.diff <- event.start - tmp 
  start.diff <- ifelse(start.diff < 0, NA, start.diff)
  start.bound <- spikes$time[tmp[which.min(start.diff)]] + minutes(1)
  
  end.diff <- tmp - event.end 
  end.diff <- ifelse(end.diff < 0, NA, end.diff)
  end.bound <- spikes$time[tmp[which.min(end.diff)]] - minutes(1)
  
  if (length(end.bound) == 0){
    end.bound <- spikes$time[length(spikes$time)]
  }
  
  if (length(start.bound) == 0){
    start.bound <- spikes$time[1]
  }
  
  if (t > 1){
    previous.event.times <- na.omit(spikes$time[spikes$events == event.nums[t-1]])
    previous.event.end <- previous.event.times[length(previous.event.times)]
    start.bound <- max(previous.event.end, start.bound)
  }
  
  if (t < n.ints){
    next.event.times <- na.omit(spikes$time[spikes$events == event.nums[t+1]])
    next.event.start <- next.event.times[1]
    end.bound <- min(next.event.start, end.bound)
  }
  
  start.bounds[t] <- start.bound
  end.bounds[t] <- end.bound
  event.starts[t] <- spikes$time[this.mask][1]
  event.ends[t] <- spikes$time[this.mask][length(this.mask)]
  
  
  if (t > 1){
    
    previous.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t-1])),
                                            max(which(spikes$events == event.nums[t-1])))]
    length.check <- F
    
    if (any(previous.event.times >= start.bound)){ 
      length.check <- T 
    } else {
      this.diff <- min(abs(as.numeric(difftime(start.bound, previous.event.times, units = "min"))))
      if (this.diff <= buffer.time){
        length.check <- T
      }
    }
    
    same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t-1]
    
    if (length.check & same.loc.est){ combine.start[t] <- T }
  }
  
  
  if (t < n.ints){
    
    next.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t+1])),
                                        max(which(spikes$events == event.nums[t+1])))]
    length.check <- F
    
    if (any(next.event.times <= end.bound)){ 
      length.check <- T 
    } else {
      this.diff <- min(abs(as.numeric(difftime(next.event.times, end.bound, units = "min"))))
      if (this.diff <= buffer.time){
        length.check <- T
      }
    }
    
    same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t+1]
    
    if (length.check & same.loc.est){ combine.end[t] <- T }
  }
  
} # end loop over events

event.starts <- as_datetime(event.starts, tz = "America/New_York")
event.ends <-   as_datetime(event.ends,   tz = "America/New_York")
start.bounds <- as_datetime(start.bounds, tz = "America/New_York")
end.bounds <-   as_datetime(end.bounds,   tz = "America/New_York")


n.samples <- 100000


all.weights <- vector(mode = "list", length = n.ints)
names(all.weights) <- loc.est.all.events


hard.stops <- vector(length = n.ints)

rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
rate.range <- rate.range[2] - rate.range[1]



t.ind <- which(spikes$events[spikes$time == snapshot.time] == event.nums) 

t <- t.ind

max.to.combine.start <- which(!(seq(1,n.ints) %in% (t-which(combine.end))))[1]-1
max.to.combine.end   <- which(!(seq(1,n.ints) %in% (which(combine.start)-t)))[1]-1

start.combine.options <- seq(0, max.to.combine.start)
end.combine.options   <- seq(0, max.to.combine.end)

start.time.sample <- vector(mode = "list", length = length(start.combine.options))
end.time.sample <- vector(mode = "list", length = length(end.combine.options))

for (i in 1:length(start.combine.options)){
  possible.start.times <- seq(start.bounds[t-start.combine.options[i]], event.starts[t-start.combine.options[i]], by = "1 min")
  start.time.sample[[i]] <- sample(possible.start.times, n.samples, replace = T)
}

for (i in 1:length(end.combine.options)){
  possible.end.times <- seq(event.ends[t+end.combine.options[i]], end.bounds[t+end.combine.options[i]], by = "1 min")
  end.time.sample[[i]] <- sample(possible.end.times, n.samples, replace = T)
}



n.start.options <- length(start.combine.options)
n.end.options <- length(end.combine.options)

similarity.score <- vector(length = n.start.options)

for (i in 1:n.start.options){
  jump <- start.combine.options[i]
  similarity.score[i] <- 1 - abs(rate.est.all.events[t-jump] - rate.est.all.events[t]) / rate.range
}

similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
similarity.score[is.na(similarity.score)] <- 0.5


if (n.start.options > 1){
  
  relative.probs <- cumprod(similarity.score)
  
  for (i in seq(length(relative.probs)-1, 1)){
    relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
  }
  
} else {
  relative.probs <- 1
}

start.sample.nums <- round(n.samples * relative.probs)
all.starts <- c()

for (i in 1:n.start.options){
  all.starts <- c(all.starts, sample(start.time.sample[[i]], start.sample.nums[i], replace = T))
}

all.starts <- sample(all.starts, n.samples, replace = T)

similarity.score <- vector(length = n.end.options)

for (i in 1:n.end.options){
  jump <- end.combine.options[i]
  similarity.score[i] <- 1 - abs(rate.est.all.events[t+jump] - rate.est.all.events[t]) / rate.range
}

similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
similarity.score[is.na(similarity.score)] <- 0.5

if (n.end.options > 1){
  
  relative.probs <- cumprod(similarity.score)
  
  for (i in seq(length(relative.probs)-1, 1)){
    relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
  }
  
} else {
  relative.probs <- 1
}


end.sample.nums <- round(n.samples * relative.probs)
all.ends <- c()

for (i in 1:n.end.options){
  all.ends <- c(all.ends, sample(end.time.sample[[i]], end.sample.nums[i], replace = T))
}

all.ends <- sample(all.ends, n.samples, replace = T)


durations.to.plot <- as.numeric(difftime(all.ends, all.starts, units = "hour"))




sim.ind <- which(names(info.list) == loc.est.all.events[t])

info.mask <- info.list[[sim.ind]]


times.to.plot <- !is.na(info.mask$events)

abline(h = -0.75, col = "gray80")

segments(x0 = info.mask$time[times.to.plot],
         y0 = -0.75, y1 = -0.75, lwd = 8)



hist(durations.to.plot, freq = F, ylab = "", yaxt = "n", ylim = c(0,0.375),
     main = "", xaxt = "n", xlim = c(0,30), xlab = "Duration [hours]",
     xpd = NA)

axis(side = 1, at = seq(0, 30, 5))

segments(x0 = mean(durations.to.plot), y0 = -999, y1 = 0.3, col = tank.color, lwd = 4)
segments(x0 = quantile(durations.to.plot, probs = c(0.05, 0.95)), y0 = -999, y1 = 0.3, col = tank.color, lwd = 4, lty = 4)

axis(side = 4, at = seq(0,0.3, by = 0.1))

mtext("Density", side = 4, line = 2.25, cex = 0.75)



dev.off()




naive.duration <- as.numeric(difftime(event.ends[t.ind], event.starts[t.ind], units = "hours"))

mean.duration <- mean(durations.to.plot)
max.duration <- max(durations.to.plot)

round(mean.duration / naive.duration, 1)

round(max.duration / naive.duration, 1)


# Set colors for plots
tank.color <- "#3062CF" #blue
wellhead.east.color <- "#9147B8" #purple
separator.east.color <- "#7EAD52" #green
separator.west.color <- "#F1C30E" #gold
wellhead.west.color <- "#C7383C" #red

wellhead1.color <- wellhead.east.color
wellhead2.color <- wellhead.west.color
separator.color <- separator.west.color
production.unit.color <- separator.east.color
tank.color <- tank.color


png(paste0(save.dir, '/case_study_example3.png'),
    res = 100, pointsize = 28, width = 1920, height = 1080*0.6)

par(mgp = c(1.5, 0.5, 0))
par(mar = c(3, 4, 1, 1))
par(oma = c(0,0,0.5,3))


layout.mat <- matrix(c(1,1,1,1,1,2,2,2), nrow = 1, byrow = T)

layout(layout.mat)


ylim.max <- 6
rect.max <- 4


plot.start <- as_datetime('2023-9-23T12:00:00', tz = "America/New_York")
plot.end <- as_datetime('2023-9-24T16:00:00', tz = "America/New_York")


snapshot.time <- as_datetime('2023-9-24T09:45:00', tz = "America/New_York")


minor.axis.points <- seq(round_date(plot.start, "day")-days(1),
                         round_date(plot.end, "day")+days(1),
                         by = "3 hours")

# For visual clarity, doesn't affect duration results
max.obs[max.obs > 15] <- 15


plot(times, max.obs, col = "white", 
     ylim = c(-7.25/4,3*7.25),
     xlim = c(plot.start, plot.end),
     ylab = "",
     lwd = 1.2,
     xlab = "Time of day",
     xpd = NA,
     yaxt = "n",
     xaxt= "n")

axis(side = 2, at = seq(0,3*6,3), col.axis = "gray45", col.ticks = "gray45")

axis(side = 1, at = minor.axis.points, labels = paste0(hour(minor.axis.points), ":00"))

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 3*6, 
     col = alpha(rate.est.all.events.cols, 0.25))

lines(times, max.obs, col = "gray45", lwd = 1.2)

mtext("Concentration [ppm]", side = 2, cex = 0.75, line =2.25,
      col = "gray45")

segments(x0 = snapshot.time,
         y0 = 0, y1 = 3*6, col = "black", lwd= 4, lty = 2)

par(new = TRUE)


plot(event.mid.times, rate.est.all.events, 
     pch = 19,
     ylim = c(-7.25*2/4,6*7.25),
     cex = 1.2,
     xlim = c(plot.start, plot.end), axes = F, ylab = "", xlab = "")

axis(side = 4, at= seq(0,6*6,6))

mtext("Emission Rate [kg/hr]", side = 4, cex = 0.75, line = 2.25)



segments(x0 = event.mid.times, 
         y0 = error.lower.all.events,
         y1 = error.upper.all.events,
         lwd = 3)


start.bounds <- event.starts <- end.bounds <- event.ends <- 
  combine.start <- combine.end <- vector(length = n.ints)

# Loop through events
for (t in 1:n.ints){
  
  this.mask <- seq(min(which(spikes$events == event.nums[t])),
                   max(which(spikes$events == event.nums[t])))
  
  info.mask <- info.list[[which(names(info.list) == loc.est.all.events[t])]]
  
  event.start <- this.mask[1]
  event.end <- this.mask[length(this.mask)]
  
  buffer.time <- 10
  
  use.start.buffer <- any(is.na(info.mask$events)[seq(max(1, this.mask[1]-1-buffer.time), 
                                                      this.mask[1]-1)])
  use.end.buffer <- any(is.na(info.mask$events)[seq(this.mask[length(this.mask)]+1,
                                                    min(nrow(spikes), this.mask[length(this.mask)]+1+buffer.time))])
  
  to.remove <- sort(unique(c(this.mask-buffer.time*use.start.buffer, this.mask, this.mask+buffer.time*use.end.buffer)))
  
  to.remove <- to.remove[to.remove > 0]
  to.remove <- to.remove[to.remove < (nrow(spikes)+1)]
  
  tmp <- info.mask$events
  tmp[to.remove] <- NA
  tmp <- which(!is.na(tmp))
  
  start.diff <- event.start - tmp 
  start.diff <- ifelse(start.diff < 0, NA, start.diff)
  start.bound <- spikes$time[tmp[which.min(start.diff)]] + minutes(1)
  
  end.diff <- tmp - event.end 
  end.diff <- ifelse(end.diff < 0, NA, end.diff)
  end.bound <- spikes$time[tmp[which.min(end.diff)]] - minutes(1)
  
  if (length(end.bound) == 0){
    end.bound <- spikes$time[length(spikes$time)]
  }
  
  if (length(start.bound) == 0){
    start.bound <- spikes$time[1]
  }
  
  if (t > 1){
    previous.event.times <- na.omit(spikes$time[spikes$events == event.nums[t-1]])
    previous.event.end <- previous.event.times[length(previous.event.times)]
    start.bound <- max(previous.event.end, start.bound)
  }
  
  if (t < n.ints){
    next.event.times <- na.omit(spikes$time[spikes$events == event.nums[t+1]])
    next.event.start <- next.event.times[1]
    end.bound <- min(next.event.start, end.bound)
  }
  
  start.bounds[t] <- start.bound
  end.bounds[t] <- end.bound
  event.starts[t] <- spikes$time[this.mask][1]
  event.ends[t] <- spikes$time[this.mask][length(this.mask)]
  
  
  if (t > 1){
    
    previous.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t-1])),
                                            max(which(spikes$events == event.nums[t-1])))]
    length.check <- F
    
    if (any(previous.event.times >= start.bound)){ 
      length.check <- T 
    } else {
      this.diff <- min(abs(as.numeric(difftime(start.bound, previous.event.times, units = "min"))))
      if (this.diff <= buffer.time){
        length.check <- T
      }
    }
    
    same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t-1]
    
    if (length.check & same.loc.est){ combine.start[t] <- T }
  }
  
  
  if (t < n.ints){
    
    next.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t+1])),
                                        max(which(spikes$events == event.nums[t+1])))]
    length.check <- F
    
    if (any(next.event.times <= end.bound)){ 
      length.check <- T 
    } else {
      this.diff <- min(abs(as.numeric(difftime(next.event.times, end.bound, units = "min"))))
      if (this.diff <= buffer.time){
        length.check <- T
      }
    }
    
    same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t+1]
    
    if (length.check & same.loc.est){ combine.end[t] <- T }
  }
  
} # end loop over events

event.starts <- as_datetime(event.starts, tz = "America/New_York")
event.ends <-   as_datetime(event.ends,   tz = "America/New_York")
start.bounds <- as_datetime(start.bounds, tz = "America/New_York")
end.bounds <-   as_datetime(end.bounds,   tz = "America/New_York")


n.samples <- 100000


all.weights <- vector(mode = "list", length = n.ints)
names(all.weights) <- loc.est.all.events


hard.stops <- vector(length = n.ints)

rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
rate.range <- rate.range[2] - rate.range[1]



t.ind <- which(spikes$events[spikes$time == snapshot.time] == event.nums) 

t <- t.ind

max.to.combine.start <- which(!(seq(1,n.ints) %in% (t-which(combine.end))))[1]-1
max.to.combine.end   <- which(!(seq(1,n.ints) %in% (which(combine.start)-t)))[1]-1

start.combine.options <- seq(0, max.to.combine.start)
end.combine.options   <- seq(0, max.to.combine.end)

start.time.sample <- vector(mode = "list", length = length(start.combine.options))
end.time.sample <- vector(mode = "list", length = length(end.combine.options))

for (i in 1:length(start.combine.options)){
  possible.start.times <- seq(start.bounds[t-start.combine.options[i]], event.starts[t-start.combine.options[i]], by = "1 min")
  start.time.sample[[i]] <- sample(possible.start.times, n.samples, replace = T)
}

for (i in 1:length(end.combine.options)){
  possible.end.times <- seq(event.ends[t+end.combine.options[i]], end.bounds[t+end.combine.options[i]], by = "1 min")
  end.time.sample[[i]] <- sample(possible.end.times, n.samples, replace = T)
}



n.start.options <- length(start.combine.options)
n.end.options <- length(end.combine.options)

similarity.score <- vector(length = n.start.options)

for (i in 1:n.start.options){
  jump <- start.combine.options[i]
  similarity.score[i] <- 1 - abs(rate.est.all.events[t-jump] - rate.est.all.events[t]) / rate.range
}

similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)

similarity.score[is.na(similarity.score)] <- 0.5


if (n.start.options > 1){
  
  relative.probs <- cumprod(similarity.score)
  
  for (i in seq(length(relative.probs)-1, 1)){
    relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
  }
  
} else {
  relative.probs <- 1
}


start.sample.nums <- round(n.samples * relative.probs)
all.starts <- c()

for (i in 1:n.start.options){
  all.starts <- c(all.starts, sample(start.time.sample[[i]], start.sample.nums[i], replace = T))
}

all.starts <- sample(all.starts, n.samples, replace = T)

similarity.score <- vector(length = n.end.options)

for (i in 1:n.end.options){
  jump <- end.combine.options[i]
  similarity.score[i] <- 1 - abs(rate.est.all.events[t+jump] - rate.est.all.events[t]) / rate.range
}

similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)

similarity.score[is.na(similarity.score)] <- 0.5

if (n.end.options > 1){
  
  relative.probs <- cumprod(similarity.score)
  
  for (i in seq(length(relative.probs)-1, 1)){
    relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
  }
  
} else {
  relative.probs <- 1
}


end.sample.nums <- round(n.samples * relative.probs)
all.ends <- c()

for (i in 1:n.end.options){
  all.ends <- c(all.ends, sample(end.time.sample[[i]], end.sample.nums[i], replace = T))
}

all.ends <- sample(all.ends, n.samples, replace = T)


durations.to.plot <- as.numeric(difftime(all.ends, all.starts, units = "hour"))




sim.ind <- which(names(info.list) == loc.est.all.events[t])

info.mask <- info.list[[sim.ind]]


times.to.plot <- !is.na(info.mask$events)

abline(h = -3.25, col = "gray80")

segments(x0 = info.mask$time[times.to.plot],
         y0 = -3.25, y1 = -3.25, lwd = 8)



hist(durations.to.plot, freq = F, ylab = "", yaxt = "n", ylim = c(0,0.125),
     main = "", xaxt = "n", xlim = c(0,20), xlab = "Duration [hours]",
     xpd = NA)

axis(side = 1, at = seq(0, 20, 5))

segments(x0 = mean(durations.to.plot), y0 = -999, y1 = 0.1, col = tank.color, lwd = 4)
segments(x0 = quantile(durations.to.plot, probs = c(0.05, 0.95)), y0 = -999, y1 = 0.1, col = tank.color, lwd = 4, lty = 4)

axis(side = 4, at = seq(0,0.10, by = 0.05))

mtext("Density", side = 4, line = 2.25, cex = 0.75)



dev.off()




naive.duration <- as.numeric(difftime(event.ends[t.ind], event.starts[t.ind], units = "hours"))

mean.duration <- mean(durations.to.plot)
max.duration <- max(durations.to.plot)

round(mean.duration / naive.duration, 1)

round(max.duration / naive.duration, 1)





# STEP 7: PLOT EXHAUSTIVE SEARCH RESULTS
#---------------------------------------------------------------------------

start.bounds <- event.starts <- end.bounds <- event.ends <- 
  combine.start <- combine.end <- vector(length = n.ints)

# Loop through events
for (t in 1:n.ints){
  
  this.mask <- seq(min(which(spikes$events == event.nums[t])),
                   max(which(spikes$events == event.nums[t])))
  
  info.mask <- info.list[[which(names(info.list) == loc.est.all.events[t])]]
  
  event.start <- this.mask[1]
  event.end <- this.mask[length(this.mask)]
  
  buffer.time <- 10
  
  use.start.buffer <- any(is.na(info.mask$events)[seq(max(1, this.mask[1]-1-buffer.time), 
                                                      this.mask[1]-1)])
  use.end.buffer <- any(is.na(info.mask$events)[seq(this.mask[length(this.mask)]+1,
                                                    min(nrow(spikes), this.mask[length(this.mask)]+1+buffer.time))])
  
  to.remove <- sort(unique(c(this.mask-buffer.time*use.start.buffer, this.mask, this.mask+buffer.time*use.end.buffer)))
  
  to.remove <- to.remove[to.remove > 0]
  to.remove <- to.remove[to.remove < (nrow(spikes)+1)]
  
  tmp <- info.mask$events
  tmp[to.remove] <- NA
  tmp <- which(!is.na(tmp))
  
  start.diff <- event.start - tmp 
  start.diff <- ifelse(start.diff < 0, NA, start.diff)
  start.bound <- spikes$time[tmp[which.min(start.diff)]] + minutes(1)
  
  end.diff <- tmp - event.end 
  end.diff <- ifelse(end.diff < 0, NA, end.diff)
  end.bound <- spikes$time[tmp[which.min(end.diff)]] - minutes(1)
  
  if (length(end.bound) == 0){
    end.bound <- spikes$time[length(spikes$time)]
  }
  
  if (length(start.bound) == 0){
    start.bound <- spikes$time[1]
  }
  
  if (t > 1){
    previous.event.times <- na.omit(spikes$time[spikes$events == event.nums[t-1]])
    previous.event.end <- previous.event.times[length(previous.event.times)]
    start.bound <- max(previous.event.end, start.bound)
  }
  
  if (t < n.ints){
    next.event.times <- na.omit(spikes$time[spikes$events == event.nums[t+1]])
    next.event.start <- next.event.times[1]
    end.bound <- min(next.event.start, end.bound)
  }
  
  start.bounds[t] <- start.bound
  end.bounds[t] <- end.bound
  event.starts[t] <- spikes$time[this.mask][1]
  event.ends[t] <- spikes$time[this.mask][length(this.mask)]
  
  
  if (t > 1){
    
    previous.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t-1])),
                                            max(which(spikes$events == event.nums[t-1])))]
    length.check <- F
    
    if (any(previous.event.times >= start.bound)){ 
      length.check <- T 
    } else {
      this.diff <- min(abs(as.numeric(difftime(start.bound, previous.event.times, units = "min"))))
      if (this.diff <= buffer.time){
        length.check <- T
      }
    }
    
    same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t-1]
    
    if (length.check & same.loc.est){ combine.start[t] <- T }
  }
  
  
  if (t < n.ints){
    
    next.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t+1])),
                                        max(which(spikes$events == event.nums[t+1])))]
    length.check <- F
    
    if (any(next.event.times <= end.bound)){ 
      length.check <- T 
    } else {
      this.diff <- min(abs(as.numeric(difftime(next.event.times, end.bound, units = "min"))))
      if (this.diff <= buffer.time){
        length.check <- T
      }
    }
    
    same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t+1]
    
    if (length.check & same.loc.est){ combine.end[t] <- T }
  }
  
} # end loop over events

event.starts <- as_datetime(event.starts, tz = "America/New_York")
event.ends <-   as_datetime(event.ends,   tz = "America/New_York")
start.bounds <- as_datetime(start.bounds, tz = "America/New_York")
end.bounds <-   as_datetime(end.bounds,   tz = "America/New_York")






# possible.times <- spikes$time[!is.na(spikes$events)]


# snapshot.time <- as_datetime('2023-09-28T5:50:00', tz = "America/New_York")

under.est.factor.mean <- under.est.factor.max <- vector(length = length(event.nums))

for (p in 1:length(event.nums)){
  
  print(paste0(p, "/", length(event.nums)))
  
  # snapshot.time <- possible.times[p]
  
  snapshot.time <- na.omit(spikes$time[spikes$events == event.nums[p]])[1]
  
  n.samples <- 100000
  
  
  all.weights <- vector(mode = "list", length = n.ints)
  names(all.weights) <- loc.est.all.events
  
  hard.stops <- vector(length = n.ints)
  
  rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
  rate.range <- rate.range[2] - rate.range[1]
  
  
  
  t.ind <- which(spikes$events[spikes$time == snapshot.time] == event.nums) 
  
  t <- t.ind
  
  max.to.combine.start <- which(!(seq(1,n.ints) %in% (t-which(combine.end))))[1]-1
  max.to.combine.end   <- which(!(seq(1,n.ints) %in% (which(combine.start)-t)))[1]-1
  
  start.combine.options <- seq(0, max.to.combine.start)
  end.combine.options   <- seq(0, max.to.combine.end)
  
  start.time.sample <- vector(mode = "list", length = length(start.combine.options))
  end.time.sample <- vector(mode = "list", length = length(end.combine.options))
  
  for (i in 1:length(start.combine.options)){
    possible.start.times <- seq(start.bounds[t-start.combine.options[i]], event.starts[t-start.combine.options[i]], by = "1 min")
    start.time.sample[[i]] <- sample(possible.start.times, n.samples, replace = T)
  }
  
  for (i in 1:length(end.combine.options)){
    possible.end.times <- seq(event.ends[t+end.combine.options[i]], end.bounds[t+end.combine.options[i]], by = "1 min")
    end.time.sample[[i]] <- sample(possible.end.times, n.samples, replace = T)
  }
  
  
  
  n.start.options <- length(start.combine.options)
  n.end.options <- length(end.combine.options)
  
  similarity.score <- vector(length = n.start.options)
  
  for (i in 1:n.start.options){
    jump <- start.combine.options[i]
    similarity.score[i] <- 1 - abs(rate.est.all.events[t-jump] - rate.est.all.events[t]) / rate.range
  }
  
  similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
  similarity.score[is.na(similarity.score)] <- 0.5
  
  
  if (n.start.options > 1){
    
    relative.probs <- cumprod(similarity.score)
    
    for (i in seq(length(relative.probs)-1, 1)){
      relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
    }
    
  } else {
    relative.probs <- 1
  }
  
  
  start.sample.nums <- round(n.samples * relative.probs)
  all.starts <- c()
  
  for (i in 1:n.start.options){
    all.starts <- c(all.starts, sample(start.time.sample[[i]], start.sample.nums[i], replace = T))
  }
  
  all.starts <- sample(all.starts, n.samples, replace = T)
  
  similarity.score <- vector(length = n.end.options)
  
  for (i in 1:n.end.options){
    jump <- end.combine.options[i]
    similarity.score[i] <- 1 - abs(rate.est.all.events[t+jump] - rate.est.all.events[t]) / rate.range
  }
  
  similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
  similarity.score[is.na(similarity.score)] <- 0.5
  
  if (n.end.options > 1){
    
    relative.probs <- cumprod(similarity.score)
    
    for (i in seq(length(relative.probs)-1, 1)){
      relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
    }
    
  } else {
    relative.probs <- 1
  }
  
  
  end.sample.nums <- round(n.samples * relative.probs)
  all.ends <- c()
  
  for (i in 1:n.end.options){
    all.ends <- c(all.ends, sample(end.time.sample[[i]], end.sample.nums[i], replace = T))
  }
  
  all.ends <- sample(all.ends, n.samples, replace = T)
  
  
  durations.to.plot <- as.numeric(difftime(all.ends, all.starts, units = "hour"))
  
  
  
  
  sim.ind <- which(names(info.list) == loc.est.all.events[t])
  
  info.mask <- info.list[[sim.ind]]
  
  
  times.to.plot <- !is.na(info.mask$events)
  
  
  
  
  naive.duration <- as.numeric(difftime(event.ends[t.ind], event.starts[t.ind], units = "hours"))
  
  mean.duration <- mean(durations.to.plot)
  max.duration <- max(durations.to.plot)
  
  # round(mean.duration / naive.duration, 1)
  
  # round(max.duration / naive.duration, 1)
  
  under.est.factor.mean[p] <- mean.duration / naive.duration
  
  under.est.factor.max[p] <- max.duration / naive.duration
  
  
}



png(paste0(save.dir, 'under_est_hists.png'),
    res= 100, pointsize = 24, width = 1920, height = 1080)

cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")

xlim.vals <- c(0,80)
ylim.vals <- c(0,250)

par(mar = c(3.5,3.5,1.5,2))

par(mgp = c(2.5,1,0))

layout.mat <- matrix(c(1,2,3,3), nrow = 2)

layout(layout.mat)

hist(under.est.factor.mean, xlim = xlim.vals, ylim = ylim.vals, xlab = "", main = "Using mean as point estimate",
     ylab = "Number of emission events", col = alpha(cols[2], 0.75))

hist(under.est.factor.max, xlim = xlim.vals, ylim = ylim.vals, xlab = "Factor by which naive method underestimates proposed method",
     main = "Using max as point estimate",
     col = alpha(cols[3], 0.75),
     ylab = "Number of emission events")

out.mean <- ecdf(under.est.factor.mean)
out.max <- ecdf(under.est.factor.max)



x <- seq(1,80, by = 0.01)
plot(x, out.mean(x), type = "l", lwd = 4, ylim = c(0,1), col = cols[2],
     xlab = "Factor by which naive method underestimates proposed method",
     ylab = "Percent of emission events")

lines(x, out.max(x), col = cols[3], lwd = 4)

abline(v = seq(0,80, by= 5), col = "gray45", lty = 2)

abline(h = seq(0,1,by = 0.2), col = "gray45", lty = 2)




legend("bottomright", c("Using mean as point estimate", "Using max as point estimate"), col = c(cols[2:3]), lwd = 5 )


dev.off()


