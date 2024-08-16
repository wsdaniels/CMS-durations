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

source("./HELPER_functions.R")

save.dir <- "../figures/case_study/"
data <- readRDS('../output_data/case_study_duration_estimates.RData')



# Parse out results
all.durations          <- data$all.durations
info.list              <- data$info.list
spikes                 <- data$spikes
rate.est.all.events    <- data$rate.est.all.events
loc.est.all.events     <- data$loc.est.all.events
source.names           <- data$source.names
start.bounds           <- data$start.bounds
end.bounds             <- data$end.bounds
original.durations     <- data$original.durations
max.obs                <- data$max.obs
error.lower.all.events <- data$error.lower.all.events
error.upper.all.events <- data$error.upper.all.events

# Pull event "event numbers" that uniquely identify each naive event
event.nums <- na.omit(unique(spikes$events))

# Number of naive events
n.ints <- length(event.nums)

# Pull out times
times <- spikes$time





# STEP 2: PLOT EACH EVENT, ASSOCIATED INFORMATION MASK, AND DURATION DISTRIBUTION
#---------------------------------------------------------------------------



if (F){
  
  true.durations.matching <- true.rates.matching <- true.locs.matching <- 
    false.positive <- vector(length = n.ints)
  for (t in 1:n.ints){
    
    print(t)
    
    # png(paste0(save.dir, t, ".png"),
    #     width = 1920, height = 1080, res =100, pointsize = 24)
    
    par(mfrow = c(2,1))
    par(mar = c(2,2,2,2))
    
    this.mask <- seq(min(which(spikes$events == event.nums[t])),
                     max(which(spikes$events == event.nums[t])))
    
    info.mask <- info.list[[which(names(info.list) == loc.est.all.events[t])]]
    
    start.time <- spikes$time[this.mask][1]- hours(12)
    end.time <- spikes$time[this.mask][length(this.mask)]+ hours(12)
    
    to.plot <- c("Wellhead.West", "Wellhead.East", "Separator.West", "Separator.East", "Tank")
    
    if (loc.est.all.events[t] %in% to.plot){
      
      ylim.vals <- c(event.nums[t] - 1, event.nums[t] + 1)
      
      plot(times, spikes$events, pch = 19, xlim = c(start.time, end.time), ylim = ylim.vals,
           col = NA, main = "")
      mtext(loc.est.all.events[t], adj = 0)
      
      abline(v = times[!is.na(info.mask$events)], col = alpha("royalblue", 0.25))
      
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
      
    }
    
    if (length(which(overlap.matrix[,t])) == 0){
      
      false.positive[t] <- T
      true.durations.matching[t] <- true.rates.matching[t] <- true.locs.matching[t] <- NA
      
      if (loc.est.all.events[t] %in% to.plot){
        plot(1,1, main = t)
      }
      
    } else if (length(which(overlap.matrix[,t])) == 1){
      
      leak.ind <- which(overlap.matrix[,t])[1]
      
      this.true.duration <- as.numeric(difftime(singlesource.leaks$tc_ExpEndDatetime[leak.ind],
                                                singlesource.leaks$tc_ExpStartDatetime[leak.ind],
                                                units = "hour"))
      
      true.rates.matching[t] <- singlesource.leaks$tc_C1MassFlow[leak.ind]
      true.locs.matching[t] <- singlesource.leaks$tc_EquipmentGroupID[leak.ind]
      
      true.durations.matching[t] <- this.true.duration
      
      if (loc.est.all.events[t] %in% to.plot){
        
        mtext(singlesource.leaks$tc_EquipmentGroupID[leak.ind], adj = 1)
        
        segments(x0 = singlesource.leaks$tc_ExpStartDatetime[leak.ind],
                 x1 = singlesource.leaks$tc_ExpEndDatetime[leak.ind],
                 y0 = event.nums[t] + 0.5,
                 col = "forestgreen", lwd = 2)
        
        
        xlim.vals <- c(0, ceiling(max(c(this.true.duration, all.durations[[t]]))))
        
        hist(all.durations[[t]], xlim = xlim.vals, main = t, breaks = seq(0,max(xlim.vals)))
        
        
        abline(v = this.true.duration, col = "orange", lwd = 8)
        abline(v = mean(all.durations[[t]]), col = "blue", lwd = 6)
        abline(v = original.durations[t], col = "purple", lwd = 4)
        
      }
      
    } else {
      if (loc.est.all.events[t] %in% to.plot){
        plot(1,1, main = t, pch = 19)
      }
      true.durations.matching[t] <- true.rates.matching[t] <- true.locs.matching[t] <- NA
      print("MORE THAN 1 MATCH")
    }
    
    
    
  }
  
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

wellhead1.color <- "#9147B8" #purple
wellhead2.color <- "#C7383C" #red
separator.color <- "#F1C30E" #gold
production.unit.color <- "#7EAD52" #green
tank.color <- "#3062CF" #blue

# Set plotting colors for estimated leak location data
rate.est.all.events.cols <- vector(length = n.ints)
rate.est.all.events.cols[loc.est.all.events == "Wellheads1"] <- wellhead1.color
rate.est.all.events.cols[loc.est.all.events == "Wellheads2"] <- wellhead2.color
rate.est.all.events.cols[loc.est.all.events == "Separator"] <- separator.color
rate.est.all.events.cols[loc.est.all.events == "Production.Unit"] <- production.unit.color
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

num.ts.plots <- ceiling(length(plot.divs)/4)

for (p in 1:num.ts.plots){
  
  png(paste0(save.dir, 'case_study_time_series_', p, '.png'),
      res = 100, width = 1920, height = 1080, pointsize = 32)
  
  par(mgp = c(2, 0.75, 0))
  par(mar = c(1.5, 3.75, 0.5, 3.75))
  par(mfrow = c(4,1))
  
  week.nums <- (1:4) + 4*(p-1)
  
  week.nums <- week.nums[week.nums < length(plot.divs)]
  
  for (i in week.nums){
    
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
      
      #     legend("right",
      #            legend = c("Wellheads1", "Wellheads2", "Separator", "Production Unit", "Tanks"),
      #            fill = alpha(c(wellhead1.color, wellhead2.color, separator.color, production.unit.color, tank.color), 0.25),
      #            border = c("black", "black", "black", "black", "black"),
      #            cex = 0.9,
      #            inset = c(0.005, 0))
    }
  }
  dev.off()
}





# STEP 6: PLOT CASE STUDY SUMMARY DATA
#---------------------------------------------------------------------------

# Number of sources
n.s <- length(source.names)

# Initialize variables to hold duration estimates by equipment group
# Mean of distribution of possible durations is used here
est.durations <- vector(mode = "list", length = n.s)
names(est.durations) <- source.names

# Aggregate duration estimates by source
for (i in 1:length(all.durations)){
  list.ind <- which(names(all.durations)[i] == source.names)
  est.durations[[list.ind]] <- c(est.durations[[list.ind]], mean(all.durations[[i]]))
}




png(paste0(save.dir, '/case_study_summary.png'),
    res = 100, pointsize = 28, width = 1920*0.9, height = 1080*0.66)

par(mgp = c(1.5, 0.5, 0))
par(mar = c(3, 1, 1, 1))

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

# plot(est.duration.means, (1:5)+adj, 
#      xlim = c(0,17), ylim = c(0.8, 5.4),
#      pch = pch.for.summary,
#      col = cols.for.summary,
#      bg = cols.for.summary,
#      yaxt = "n", cex = 1.15, xaxt = "n",
#      xlab = "Duration [hours]", ylab = "")
# 
# axis(side = 1, at = seq(0,16,2))

plot(est.duration.means, (1:5)+adj, 
     xlim = c(0,19), ylim = c(0.8, 5.4),
     pch = pch.for.summary,
     col = cols.for.summary,
     bg = cols.for.summary,
     yaxt = "n", cex = 1.15, xaxt = "n",
     xlab = "Duration [hours]", ylab = "")

axis(side = 1, at = seq(0,18,2))

abline(v = seq(0,18, by = 2), lty = 1, col = "gray80")

points(est.duration.means, (1:5)+adj,
       pch = pch.for.summary,
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

# set1 <- c(2,3,4,5)
# set2 <- c(1)

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



# rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
# rate.range <- rate.range[2] - rate.range[1]





png(paste0(save.dir, 'case_study_example1.png'),
    res = 100, pointsize = 28, width = 1920, height = 1080*0.6)

par(mgp = c(1.75, 0.5, 0))
par(mar = c(3, 3.25, 1.5, 0.75))

layout.mat <- matrix(c(1,1,1,1,1,1,1,2,2,2,2), nrow = 1, byrow = T)
layout(layout.mat)

ylim.max <- 6
rect.max <- 4

plot.start <- as_datetime('2023-09-27T20:00:00', tz = "America/New_York")
plot.end <- as_datetime('2023-09-28T10:30:00', tz = "America/New_York")

minor.axis.points <- seq(round_date(plot.start, "day")-days(1),
                         round_date(plot.end, "day")+days(1),
                         by = "2 hours")

plot(times, max.obs, col = "white", type = "l", 
     ylim = c(0,10),
     xlim = c(plot.start, plot.end),
     ylab = "Background-removed methane concentration [ppm]",
     lwd = 1.2,
     xlab = "Time of day",
     xpd = NA,
     yaxt = "n",
     xaxt= "n")



snapshot.time <- as_datetime('2023-09-28T5:50:00', tz = "America/New_York")

t.ind <- which(event.nums == spikes$event[spikes$time == snapshot.time])

sim.ind <- which(names(info.list) == loc.est.all.events[t.ind])

info.mask <- info.list[[sim.ind]]

info.event.nums <- unique(na.omit(info.mask$events))
info.n.ints <- length(info.event.nums)

for (i in 1:info.n.ints){
  
  this.mask <- info.mask$events == info.event.nums[i]
  this.mask[is.na(this.mask)] <- F
  
  rect(xleft = info.mask$time[this.mask][1],
       xright = info.mask$time[this.mask][sum(this.mask)],
       ybottom = -99, ytop = 99,
       border = NA,
       col = alpha("gray45", 0.3))
}

# text(x = event.mid.times, y = 8, labels = round(rate.est.all.events, 1))

axis(side = 2, at = seq(0,10,2))

axis(side = 1, at = minor.axis.points, labels = paste0(hour(minor.axis.points), ":00"))

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 8, 
     # col = alpha(rate.est.all.events.cols, 0.1),
     col = "white",
     border = NA, lwd = 3)

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 8, 
     col = alpha(rate.est.all.events.cols, 0.1),
     # col = NA,
     border = rate.est.all.events.cols, lwd = 3)

lines(times, max.obs, lwd = 1.6, col = "gray45")

# mtext("Methane concentration [ppm]", side = 2, cex = 0.75, line =2.25)


# segments(x0 = snapshot.time,
#          y0 = -99, y1 = 99, col = "black", lwd= 4, lty = 2)

# text(x = event.mid.times, y = 10.8, labels = round(rate.est.all.events, 1),
#      xpd = NA)



durations.to.plot <- all.durations[[t.ind]]

hist(durations.to.plot, freq = F, ylab = "Density", yaxt = "n", ylim = c(0,0.4),
     main = "", xaxt = "n", xlim = c(0,12), xlab = "Duration [hours]",
     xpd = NA)

axis(side = 1, at = seq(0, 12, 2))

line.end <- 0.33

segments(x0 = min(durations.to.plot), y0 = -999, y1 = line.end, col = "#1B9E77", lwd = 4, lty = 3)
segments(x0 = mean(durations.to.plot), y0 = -999, y1 = line.end, col = "#E7298A", lwd = 4, lty = 2)
segments(x0 = max(durations.to.plot), y0 = -999, y1 = line.end, col = "#E7298A", lwd = 4, lty = 2)
# segments(x0 = quantile(durations.to.plot, probs = c(0.05, 0.95)), y0 = -999, y1 = 0.4, col = wellhead1.color, lwd = 4, lty = 4)

axis(side = 2, at = seq(0,0.5, by = 0.1))

abline(h = 0)

# mtext("Density", side = 2, line = 2.25, cex = 0.75)


data$start.similarity.scores[[t.ind]]
data$end.similarity.scores[[t.ind]]

dev.off()




# naive.duration <- as.numeric(difftime(event.ends[t.ind], event.starts[t.ind], units = "hours"))
naive.duration <- original.durations[t.ind]

mean.duration <- mean(durations.to.plot)
max.duration <- max(durations.to.plot)

round(mean.duration / naive.duration, 1)
round(max.duration / naive.duration, 1)





png(paste0(save.dir, 'case_study_example2.png'),
    res = 100, pointsize = 28, width = 1920, height = 1080*0.6)

par(mgp = c(1.75, 0.5, 0))
par(mar = c(3, 3.25, 1.5, 0.75))

layout.mat <- matrix(c(1,1,1,1,1,1,1,2,2,2,2), nrow = 1, byrow = T)
layout(layout.mat)

ylim.max <- 6
rect.max <- 4


plot.start <- as_datetime('2023-10-7T18:50:00', tz = "America/New_York")
plot.end <- as_datetime('2023-10-8T19:00:00', tz = "America/New_York")


snapshot.time <- as_datetime('2023-10-8T14:40:00', tz = "America/New_York")

minor.axis.points <- seq(round_date(plot.start, "day")-days(1),
                         round_date(plot.end, "day")+days(1),
                         by = "3 hours")

plot(times, max.obs, col = "white", type = "l", 
     ylim = c(0,10),
     xlim = c(plot.start, plot.end),
     ylab = "Background-removed methane concentration [ppm]",
     lwd = 1.2,
     xlab = "Time of day",
     xpd = NA,
     yaxt = "n",
     xaxt= "n")




t.ind <- which(event.nums == spikes$event[spikes$time == snapshot.time])

sim.ind <- which(names(info.list) == loc.est.all.events[t.ind])

info.mask <- info.list[[sim.ind]]

info.event.nums <- unique(na.omit(info.mask$events))
info.n.ints <- length(info.event.nums)

for (i in 1:info.n.ints){
  
  this.mask <- info.mask$events == info.event.nums[i]
  this.mask[is.na(this.mask)] <- F
  
  rect(xleft = info.mask$time[this.mask][1],
       xright = info.mask$time[this.mask][sum(this.mask)],
       ybottom = -99, ytop = 99,
       border = NA,
       col = alpha("gray45", 0.3))
}


axis(side = 2, at = seq(0,10,2))

axis(side = 1, at = minor.axis.points, labels = paste0(hour(minor.axis.points), ":00"))

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 8, 
     # col = alpha(rate.est.all.events.cols, 0.1),
     col = "white",
     border = NA, lwd = 3)

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 8, 
     col = alpha(rate.est.all.events.cols, 0.1),
     # col = NA,
     border = rate.est.all.events.cols, lwd = 3)

lines(times, max.obs, lwd = 1.6, col = "gray45")

# mtext("Methane concentration [ppm]", side = 2, cex = 0.75, line =2.25)


# segments(x0 = snapshot.time,
#          y0 = -99, y1 = 99, col = "black", lwd= 4, lty = 2)

# text(x = event.mid.times, y = 10.8, labels = round(rate.est.all.events, 1),
#      xpd = NA)



durations.to.plot <- all.durations[[t.ind]]

hist(durations.to.plot, freq = F, ylab = "Density", yaxt = "n", ylim = c(0,0.4),
     main = "", xaxt = "n", xlim = c(0,24), xlab = "Duration [hours]",
     xpd = NA)

axis(side = 1, at = seq(0, 24, 4))

line.end <- 0.33

segments(x0 = min(durations.to.plot), y0 = -999, y1 = line.end, col = "#1B9E77", lwd = 4, lty = 3)
segments(x0 = mean(durations.to.plot), y0 = -999, y1 = line.end, col = "#E7298A", lwd = 4, lty = 2)
segments(x0 = max(durations.to.plot), y0 = -999, y1 = line.end, col = "#E7298A", lwd = 4, lty = 2)
# segments(x0 = quantile(durations.to.plot, probs = c(0.05, 0.95)), y0 = -999, y1 = 0.4, col = wellhead1.color, lwd = 4, lty = 4)

axis(side = 2, at = seq(0,0.5, by = 0.1))

abline(h = 0)

# mtext("Density", side = 2, line = 2.25, cex = 0.75)


data$start.similarity.scores[[t.ind]]
data$end.similarity.scores[[t.ind]]

dev.off()




# naive.duration <- as.numeric(difftime(event.ends[t.ind], event.starts[t.ind], units = "hours"))
naive.duration <- original.durations[t.ind]

mean.duration <- mean(durations.to.plot)
max.duration <- max(durations.to.plot)

round(mean.duration / naive.duration, 1)
round(max.duration / naive.duration, 1)




png(paste0(save.dir, 'case_study_example3.png'),
    res = 100, pointsize = 28, width = 1920, height = 1080*0.6)

par(mgp = c(1.75, 0.5, 0))
par(mar = c(3, 3.25, 1.5, 0.75))

layout.mat <- matrix(c(1,1,1,1,1,1,1,2,2,2,2), nrow = 1, byrow = T)
layout(layout.mat)

ylim.max <- 6
rect.max <- 4

plot.start <- as_datetime('2023-9-13T22:00:00', tz = "America/New_York")
plot.end <- as_datetime('2023-9-14T14:00:00', tz = "America/New_York")

snapshot.time <- as_datetime('2023-9-14T03:00:00', tz = "America/New_York")

minor.axis.points <- seq(round_date(plot.start, "day")-days(1),
                         round_date(plot.end, "day")+days(1),
                         by = "2 hours")

plot(times, max.obs, col = "white", type = "l", 
     ylim = c(0,10),
     xlim = c(plot.start, plot.end),
     ylab = "Background-removed methane concentration [ppm]",
     lwd = 1.2,
     xlab = "Time of day",
     xpd = NA,
     yaxt = "n",
     xaxt= "n")




t.ind <- which(event.nums == spikes$event[spikes$time == snapshot.time])

sim.ind <- which(names(info.list) == loc.est.all.events[t.ind])

info.mask <- info.list[[sim.ind]]

info.event.nums <- unique(na.omit(info.mask$events))
info.n.ints <- length(info.event.nums)

for (i in 1:info.n.ints){
  
  this.mask <- info.mask$events == info.event.nums[i]
  this.mask[is.na(this.mask)] <- F
  
  rect(xleft = info.mask$time[this.mask][1],
       xright = info.mask$time[this.mask][sum(this.mask)],
       ybottom = -99, ytop = 99,
       border = NA,
       col = alpha("gray45", 0.3))
}


axis(side = 2, at = seq(0,10,2))

axis(side = 1, at = minor.axis.points, labels = paste0(hour(minor.axis.points), ":00"))

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 8, 
     # col = alpha(rate.est.all.events.cols, 0.1),
     col = "white",
     border = NA, lwd = 3)

rect(xleft = event.start.times, 
     xright = event.end.times,
     ybottom = 0, ytop = 8, 
     col = alpha(rate.est.all.events.cols, 0.1),
     # col = NA,
     border = rate.est.all.events.cols, lwd = 3)

lines(times, max.obs, lwd = 1.6, col = "gray45")


# segments(x0 = snapshot.time,
#          y0 = -99, y1 = 99, col = "black", lwd= 4, lty = 2)

# text(x = event.mid.times, y = 10.8, labels = round(rate.est.all.events, 1),
#      xpd = NA)



durations.to.plot <- all.durations[[t.ind]]

hist(durations.to.plot, freq = F, ylab = "Density", yaxt = "n", ylim = c(0,0.4),
     main = "", xaxt = "n", xlim = c(0,12), xlab = "Duration [hours]",
     xpd = NA)

axis(side = 1, at = seq(0, 12, 2))

line.end <- 0.33

segments(x0 = min(durations.to.plot), y0 = -999, y1 = line.end, col = "#1B9E77", lwd = 4, lty = 3)
segments(x0 = mean(durations.to.plot), y0 = -999, y1 = line.end, col = "#E7298A", lwd = 4, lty = 2)
segments(x0 = max(durations.to.plot), y0 = -999, y1 = line.end, col = "#E7298A", lwd = 4, lty = 2)
# segments(x0 = quantile(durations.to.plot, probs = c(0.05, 0.95)), y0 = -999, y1 = 0.4, col = wellhead1.color, lwd = 4, lty = 4)

axis(side = 2, at = seq(0,0.5, by = 0.1))

abline(h = 0)

# mtext("Density", side = 2, line = 2.25, cex = 0.75)


data$start.similarity.scores[[t.ind]]
data$end.similarity.scores[[t.ind]]

dev.off()


# naive.duration <- as.numeric(difftime(event.ends[t.ind], event.starts[t.ind], units = "hours"))
naive.duration <- original.durations[t.ind]

mean.duration <- mean(durations.to.plot)
max.duration <- max(durations.to.plot)

round(mean.duration / naive.duration, 1)
round(max.duration / naive.duration, 1)



# STEP 7: PLOT EXHAUSTIVE SEARCH RESULTS
#---------------------------------------------------------------------------

under.est.factor.mean <- under.est.factor.max <- vector(length = n.ints)

for (t in 1:n.ints){
  
  mean.duration <- mean(all.durations[[t]])
  max.duration <- max(all.durations[[t]])
  
  naive.duration <- original.durations[t]
  
  under.est.factor.mean[t] <- mean.duration / naive.duration
  under.est.factor.max[t] <- max.duration / naive.duration
  
  
}




png(paste0(save.dir, 'under_est_hists.png'),
    res= 100, pointsize = 24, width = 1920, height = 1080)

cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")

xlim.vals <- c(0,70)
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








png(paste0(save.dir, 'method_illustration.png'),
    res = 100, pointsize = 28, width = 1920, height = 1080*0.6)

par(mgp = c(1.75, 0.5, 0))
par(mar = c(3, 3.25, 1.5, 0.75))

layout.mat <- matrix(c(1,1,1,1,1,1,1,2,2,2,2), nrow = 1, byrow = T)
layout(layout.mat)

ylim.max <- 6
rect.max <- 4

plot.start <- as_datetime('2023-10-25T13:00:00', tz = "America/New_York")
plot.end <- as_datetime('2023-10-26T7:00:00', tz = "America/New_York")

snapshot.time <- as_datetime('2023-10-25T18:00:00', tz = "America/New_York")

minor.axis.points <- seq(round_date(plot.start, "day")-days(1),
                         round_date(plot.end, "day")+days(1),
                         by = "2 hours")


t.ind <- which(event.nums == spikes$event[spikes$time == snapshot.time])

to.keep <- spikes$events == event.nums[t.ind]
to.keep[is.na(to.keep)] <- F

max.obs.tmp <- max.obs

max.obs.tmp[!to.keep] <- 0

plot(times, max.obs.tmp, col = "white", type = "l", 
     ylim = c(0,12),
     xlim = c(plot.start, plot.end),
     ylab = "Background-removed methane concentration [ppm]",
     lwd = 1.2,
     xlab = "Time of day",
     xpd = NA,
     yaxt = "n",
     xaxt= "n")

event.start <- as_datetime(event.start.times[t.ind], tz = "America/New_York")
event.end <- as_datetime(event.end.times[t.ind], tz = "America/New_York")



rect(xleft = event.start - days(2),
     xright = event.end,
     ybottom = -99, ytop = 99,
     border = NA,
     col = alpha("gray45", 0.3))

info.gap <- minutes(4.5*60)

rect(xleft = event.end + info.gap,
     xright = event.end + days(2),
     ybottom = -99, ytop = 99,
     border = NA,
     col = alpha("gray45", 0.3))


axis(side = 2, at = seq(0,12,2))

axis(side = 1, at = minor.axis.points, labels = paste0(hour(minor.axis.points), ":00"))

rect(xleft = event.start.times[t.ind], 
     xright = event.end.times[t.ind],
     ybottom = 0, ytop = 9, 
     # col = alpha(rate.est.all.events.cols, 0.1),
     col = "white",
     border = NA, lwd = 3)

rect(xleft = event.start.times[t.ind], 
     xright = event.end.times[t.ind],
     ybottom = 0, ytop = 9, 
     col = alpha("#1B9E77", 0.1),
     # col = NA,
     border = "#1B9E77", lwd = 3)

abline(v = as_datetime(event.start.times[t.ind]), col = "#D95F02", lwd = 4)
abline(v = event.end.times[t.ind], col = "#D95F02", lwd = 4, lty = 4)
abline(v = as_datetime(event.end.times[t.ind]) + minutes(minute(info.gap)), col = "#D95F02", lwd = 4, lty = 4)


lines(times, max.obs.tmp, lwd = 2, col = "gray45")


# segments(x0 = snapshot.time,
#          y0 = -99, y1 = 99, col = "black", lwd= 4, lty = 2)

# text(x = event.mid.times, y = 10.8, labels = round(rate.est.all.events, 1),
#      xpd = NA)

this.naive.duration <- as.numeric(difftime(event.end,event.start, units = "hours"))

possible.extra.times <- sample.int(minute(info.gap), size = 100000, replace = T) / 60

durations.to.plot <- this.naive.duration + possible.extra.times

hist(durations.to.plot, freq = F, ylab = "Density", yaxt = "n", ylim = c(0,0.4),
     main = "", xaxt = "n", xlim = c(3.5,10.25), xlab = "Duration [hours]",
     breaks = 10,
     xpd = NA)

axis(side = 1, at = seq(4, 10, 2))

line.end <- 0.33

segments(x0 = min(durations.to.plot), y0 = -999, y1 = line.end, col = "#1B9E77", lwd = 4, lty = 3)
segments(x0 = mean(durations.to.plot), y0 = -999, y1 = line.end, col = "#E7298A", lwd = 4, lty = 2)
segments(x0 = max(durations.to.plot), y0 = -999, y1 = line.end, col = "#E7298A", lwd = 4, lty = 2)
# segments(x0 = quantile(durations.to.plot, probs = c(0.05, 0.95)), y0 = -999, y1 = 0.4, col = wellhead1.color, lwd = 4, lty = 4)

axis(side = 2, at = seq(0,0.5, by = 0.1))

abline(h = 0)

# mtext("Density", side = 2, line = 2.25, cex = 0.75)


data$start.similarity.scores[[t.ind]]
data$end.similarity.scores[[t.ind]]

dev.off()