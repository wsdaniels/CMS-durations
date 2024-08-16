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


pd.to.fd <- function(pd){
  pd <- as.matrix(pd)
  tmp <- pd + 1
  tmp <- ifelse(tmp < 1, -1/tmp, tmp)
  return(as.data.frame(tmp))
}

# START USER INPUT
#---------------------------------------------------------------------------

# Select experiment to run:
#   1) ADED 2022
#   2) ADED 2023
#   3) Stanford
run.type <- 3


# END OF USER INPUT - NO MODIFICATION NECESSARY BELOW THIS POINT
#---------------------------------------------------------------------------


if (run.type == 1){ n.r <- 8
} else if (run.type == 2){ n.r <- 10
} else if (run.type == 3){ n.r <- 6 }

slopes <- as.data.frame(matrix(NA, nrow = n.r, ncol = 6))
names(slopes) <- c("naive.lower", "naive", "naive.upper",
                   "proposed.lower", "proposed", "proposed.upper")

r2 <- as.data.frame(matrix(NA, nrow = n.r, ncol = 2))
names(r2) <- c("naive", "proposed")




for (number.of.sensors in 1:n.r){
  
  
  # STEP 1: PREP DATA
  #---------------------------------------------------------------------------
  
  # Read in duration estimates
  if (run.type == 1){
    
    save.dir <- "../figures/ADED2022/"
    data <- readRDS(paste0('../output_data/ADED2022_duration_estimates_best_arrangement_', number.of.sensors, '_sensors.RData'))
    
  } else if (run.type == 2){
    
    save.dir <- "../figures/ADED2023/"
    data <- readRDS(paste0('../output_data/ADED2023_duration_estimates_best_arrangement_', number.of.sensors, '_sensors.RData'))
    
  } else if (run.type == 3){
    
    save.dir <- "../figures/Stanford/"
    data <- readRDS(paste0('../output_data/Stanford_duration_estimates_best_arrangement_', number.of.sensors, '_sensors.RData'))
  }
  
  
  
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
  
  
  
  
  # STEP 2: GET SINGLESOURCE LEAK INFORMATION
  #---------------------------------------------------------------------------
  
  
  # Grab either ADED 2022 or 2023 leak data
  if (run.type == 1){
    leak.data <- suppressWarnings(read.csv('../input_data/ADED2022_leak_data.csv'))
  } else if (run.type == 2){
    leak.data <- suppressWarnings(read.csv('../input_data/ADED2023_leak_data.csv'))
  } else if (run.type == 3){
    leak.data <- readRDS('../input_data/Stanford_leak_data.RData')
  }
  
  if (run.type %in% c(1,2)){
    
    # Rename equipment groups for clarity
    leak.data$tc_EquipmentGroupID[leak.data$tc_EquipmentGroupID == "4T"] <- "Tank"
    leak.data$tc_EquipmentGroupID[leak.data$tc_EquipmentGroupID == "4W"] <- "Wellhead.West"
    leak.data$tc_EquipmentGroupID[leak.data$tc_EquipmentGroupID == "4S"] <- "Separator.West"
    leak.data$tc_EquipmentGroupID[leak.data$tc_EquipmentGroupID == "5W"] <- "Wellhead.East"
    leak.data$tc_EquipmentGroupID[leak.data$tc_EquipmentGroupID == "5S"] <- "Separator.East"
    
    # Convert time strings to datetime objects
    leak.data$tc_ExpStartDatetime <- as_datetime(as_datetime(leak.data$tc_ExpStartDatetime), tz = "America/Denver")
    leak.data$tc_ExpEndDatetime   <- as_datetime(as_datetime(leak.data$tc_ExpEndDatetime),   tz = "America/Denver")
    
    # Mask in leak data during simulation time frame
    to.keep <- leak.data$tc_ExpStartDatetime >= times[1] & leak.data$tc_ExpStartDatetime <= times[length(times)]
    leak.data <- leak.data[to.keep,]
    
    # Sort by leak start time
    leak.data <- leak.data[order(leak.data$tc_ExpStartDatetime), ]
    
    # Mask for multisource leaks
    multisource <- rep(F, nrow(leak.data))
    multisource[leak.data$tc_ExpEPCount > 1] <- T
    
    # Separate out the single and multisource leaks
    singlesource.leaks <- leak.data[!multisource, ]
    multisource.leaks <- leak.data[multisource, ]
    
    # Number of METEC emission events
    n.leaks <- nrow(singlesource.leaks)
    
    # Initialize vector to hold estimated events that should be removed because
    # they overlap with a multisource leak
    to.remove <- vector(length = length(event.nums))
    
    # Remove events that overlap with a multisource leak
    for (i in 1:length(event.nums)){
      
      # Mask in this event
      this.spike <- which(spikes$events == event.nums[i])
      these.times <- spikes$time[this.spike]
      
      # Loop through multisource leaks
      for (j in 1:nrow(multisource.leaks)){
        
        leak.start <- multisource.leaks$tc_ExpStartDatetime[j]
        leak.stop  <- multisource.leaks$tc_ExpEndDatetime[j]
        
        # If there is any overlap between estimated event and leak, remove
        # the event
        if (any(these.times >= leak.start & these.times <= leak.stop)){
          spikes$events[this.spike] <- NA
          to.remove[i] <- T
          break
        }
      }
    }
    
    
    # Get event numbers after filtering out events that overlap with multisource leaks
    event.nums <- na.omit(unique(spikes$events))
    
    # Number of events after filtering multisource leaks
    n.ints <- length(event.nums)
    
    # Filter out all naive events that occurred during a multi-source release
    rate.est.all.events    <- rate.est.all.events[!to.remove]
    error.lower.all.events <- error.lower.all.events[!to.remove]
    error.upper.all.events <- error.upper.all.events[!to.remove]
    loc.est.all.events     <- loc.est.all.events[!to.remove]
    all.durations          <- all.durations[!to.remove]
    start.bounds           <- start.bounds[!to.remove]
    end.bounds             <- end.bounds[!to.remove]
    original.durations     <- original.durations[!to.remove]
    
    
    
    # Initialize vector to hold estimated events that should be removed because
    # they overlap with a source not included in the study
    to.remove <- vector(length = length(event.nums))
    
    # Remove events that overlap with a source not included in the study
    for (i in 1:length(event.nums)){
      
      # Mask in this event
      this.spike <- which(spikes$events == event.nums[i])
      these.times <- spikes$time[this.spike]
      
      # Loop through leaks from source not included in this study
      for (j in which(singlesource.leaks$tc_EquipmentGroupID == "Underground")){
        
        leak.start <- singlesource.leaks$tc_ExpStartDatetime[j]
        leak.stop  <- singlesource.leaks$tc_ExpEndDatetime[j]
        
        # If there is any overlap between estimated event and leak, remove
        # the event
        if (any(these.times >= leak.start & these.times <= leak.stop)){
          spikes$events[this.spike] <- NA
          to.remove[i] <- T
        }
      }
    }
    
    
    # Get event numbers after filtering out events that overlap with multisource leaks
    event.nums <- na.omit(unique(spikes$events))
    
    # Number of events after filtering multisource leaks
    n.ints <- length(event.nums)
    
    # Filter out all naive events that occurred during a multi-source release
    rate.est.all.events    <- rate.est.all.events[!to.remove]
    error.lower.all.events <- error.lower.all.events[!to.remove]
    error.upper.all.events <- error.upper.all.events[!to.remove]
    loc.est.all.events     <- loc.est.all.events[!to.remove]
    all.durations          <- all.durations[!to.remove]
    start.bounds           <- start.bounds[!to.remove]
    end.bounds             <- end.bounds[!to.remove]
    original.durations     <- original.durations[!to.remove]
    
    
    
    # Initialize vector to hold estimated events that should be removed because
    # they were combined with a naive event that overlapped with a multisource
    # controlled release
    to.remove <- vector(length = length(event.nums))
    
    # Remove events that overlap with a source not included in the study
    for (i in 1:length(event.nums)){
      
      # Mask in this event
      this.spike <- which(spikes$events == event.nums[i])
      these.times <- spikes$time[this.spike]
      
      if (i == length(event.nums)){
        should.not.combine.check <- !any(loc.est.all.events[c(i-1)] == loc.est.all.events[i])
      } else {
        should.not.combine.check <- !any(loc.est.all.events[c(i-1, i+1)] == loc.est.all.events[i])
      }
      was.combined.check <- max(all.durations[[i]]) > (end.bounds[i] - start.bounds[i])
      
      if (should.not.combine.check & was.combined.check){
        spikes$events[this.spike] <- NA
        to.remove[i] <- T
      }
    }
    
    # Get event numbers after filtering out events that overlap with multisource leaks
    event.nums <- na.omit(unique(spikes$events))
    
    # Number of events after filtering multisource leaks
    n.ints <- length(event.nums)
    
    # Filter out all naive events that occurred during a multi-source release
    rate.est.all.events    <- rate.est.all.events[!to.remove]
    error.lower.all.events <- error.lower.all.events[!to.remove]
    error.upper.all.events <- error.upper.all.events[!to.remove]
    loc.est.all.events     <- loc.est.all.events[!to.remove]
    all.durations          <- all.durations[!to.remove]
    start.bounds           <- start.bounds[!to.remove]
    end.bounds             <- end.bounds[!to.remove]
    original.durations     <- original.durations[!to.remove]
    
  } else if (run.type == 3){
    
    singlesource.leaks <- leak.data
    
    singlesource.leaks$rate.kg.hr <- singlesource.leaks$rate.kg.hr * 1000
    
    singlesource.leaks$localization.est <- rep("short", nrow(singlesource.leaks))
    
    colnames(singlesource.leaks) <- c("tc_ExpStartDatetime", "tc_ExpEndDatetime", "tc_C1MassFlow", "tc_ExpDurationHrs", "tc_EquipmentGroupID")
    
    singlesource.leaks$tc_ExpStartDatetime <- as_datetime(as_datetime(singlesource.leaks$tc_ExpStartDatetime), tz = "US/Arizona")
    singlesource.leaks$tc_ExpEndDatetime   <- as_datetime(as_datetime(singlesource.leaks$tc_ExpEndDatetime),   tz = "US/Arizona")
    
    n.leaks <- nrow(singlesource.leaks)
    
    diff.times <- vector(length = n.leaks-1)
    for (i in 2:n.leaks){
      diff.times[i-1] <- difftime(singlesource.leaks$tc_ExpStartDatetime[i],
                                  singlesource.leaks$tc_ExpEndDatetime[i-1],
                                  units = "mins")
    }

    to.combine <- diff.times < 15

    # plot(diff.times, ylim = c(0,100))
    # points(which(to.combine), diff.times[to.combine], col = "red")

    new.leak.data <- singlesource.leaks[1,]
    new.ind <- 1
    to.average <- singlesource.leaks$tc_C1MassFlow[1]

    for (i in 2:n.leaks){

      if (to.combine[i-1]){

        new.leak.data$tc_ExpEndDatetime[new.ind] <- singlesource.leaks$tc_ExpEndDatetime[i]
        to.average <- c(to.average, singlesource.leaks$tc_C1MassFlow[i])

      } else {

        new.leak.data$tc_C1MassFlow[new.ind] <- mean(to.average)
        new.ind <- new.ind + 1
        new.leak.data <- rbind(new.leak.data, singlesource.leaks[i,])
        to.average <- singlesource.leaks$tc_C1MassFlow[i]
      }
    }

    new.leak.data$tc_ExpDurationHrs <- as.numeric(difftime(new.leak.data$tc_ExpEndDatetime, new.leak.data$tc_ExpStartDatetime, units = "hours"))
    singlesource.leaks <- new.leak.data
    n.leaks <- nrow(singlesource.leaks)
    
  } # end if tree over run type
  
  
  
  
  # STEP 7: DETERMINE WHICH NAIVE EVENTS OVERLAP WITH A CONTROLLED RELEASE
  #---------------------------------------------------------------------------
  
  # Initialize matrix to hold information on which controlled release
  # each naive event overlaps with.
  # Rows are the METEC controlled releases, columns are naive events.
  # True indicates an estimated event overlaps with a controlled release.
  overlap.matrix <- matrix(NA, nrow = n.leaks, ncol = n.ints)
  overlap.matrix[is.na(overlap.matrix)] <- F
  
  # Fill in the overlap matrix. First loop over METEC emissions.
  for (l in 1:n.leaks){
    
    # Grab start and stop times of the METEC emission
    leak.start <- singlesource.leaks$tc_ExpStartDatetime[l]
    leak.end   <- singlesource.leaks$tc_ExpEndDatetime[l]
    
    # Create minute sequence over duration of METEC emission
    leak.times <- seq(leak.start, leak.end, by = "1 min")
    leak.times <- round_date(leak.times, unit = "min")
    
    # Loop over predicted events.
    for (t in 1:n.ints){
      
      # Create minute sequence over duration of this predicted event.
      this.mask <- seq(min(which(spikes$events == event.nums[t])),
                       max(which(spikes$events == event.nums[t])))
      event.times <- spikes$time[this.mask]
      
      # Store overlap status
      if (any(leak.times %in% event.times)){
        overlap.matrix[l,t] <- T
      }
    }
  }
  
  
  
  
  # STEP 9: GET THE TRUE DURATIONS OF THE RELEASES THAT OVERLAP WITH THE ESTIMATED EVENTS
  #---------------------------------------------------------------------------
  
  # Initialize vectors to hold overlapping truth data
  true.durations.matching <- true.rates.matching <- true.locs.matching <- vector(length = n.ints)
  
  for (t in 1:n.ints){
    
    # False positive naive events
    if (length(which(overlap.matrix[,t])) == 0){
      
      true.durations.matching[t] <- true.rates.matching[t] <- true.locs.matching[t] <- NA
      
      # Naive events that overlap with a controlled release
    } else if (length(which(overlap.matrix[,t])) == 1){
      
      leak.ind <- which(overlap.matrix[,t])
      
      this.true.duration <- as.numeric(difftime(singlesource.leaks$tc_ExpEndDatetime[leak.ind],
                                                singlesource.leaks$tc_ExpStartDatetime[leak.ind],
                                                units = "hour"))
      
      true.rates.matching[t] <- singlesource.leaks$tc_C1MassFlow[leak.ind]
      true.locs.matching[t] <- singlesource.leaks$tc_EquipmentGroupID[leak.ind]
      
      true.durations.matching[t] <- this.true.duration
      
      # Naive events that overlap with more than one controlled release
    } else {
      
      true.durations.matching[t] <- true.rates.matching[t] <- true.locs.matching[t] <- NA
      print("ERROR: MORE THAN 1 MATCH")
      
    }
  }
  
  
  
  
  
  # STEP 2: PLOT EACH EVENT, ASSOCIATED INFORMATION MASK, AND DURATION DISTRIBUTION
  #---------------------------------------------------------------------------
  
  if (F){
    
    true.durations.matching <- true.rates.matching <- true.locs.matching <- 
      false.positive <- vector(length = n.ints)
    
    for (t in 1:n.ints){
      
      print(t)
      
      par(mfrow = c(2,1))
      par(mar = c(2,2,2,2))
      
      this.mask <- seq(min(which(spikes$events == event.nums[t])),
                       max(which(spikes$events == event.nums[t])))
      
      info.mask <- info.list[[which(names(info.list) == loc.est.all.events[t])]]
      
      start.time <- spikes$time[this.mask][1]- hours(12)
      end.time <- spikes$time[this.mask][length(this.mask)]+ hours(12)
      
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
      
      if (length(which(overlap.matrix[,t])) == 0){
        
        false.positive[t] <- T
        true.durations.matching[t] <- true.rates.matching[t] <- true.locs.matching[t] <- NA
        
        plot(1,1, main = t)
        
      } else if (length(which(overlap.matrix[,t])) == 1){
        
        leak.ind <- which(overlap.matrix[,t])[1]
        
        this.true.duration <- as.numeric(difftime(singlesource.leaks$tc_ExpEndDatetime[leak.ind],
                                                  singlesource.leaks$tc_ExpStartDatetime[leak.ind],
                                                  units = "hour"))
        
        true.rates.matching[t] <- singlesource.leaks$tc_C1MassFlow[leak.ind]
        true.locs.matching[t] <- singlesource.leaks$tc_EquipmentGroupID[leak.ind]
        
        true.durations.matching[t] <- this.true.duration
        
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
        
      } else {
        
        plot(1,1, main = t, pch = 19)
        print("MORE THAN 1 MATCH")
        
      }
    }
  }
  
  
  
  duration.mean <- sapply(all.durations, mean)
  duration.bound.lower <- sapply(all.durations, function(X) quantile(X, probs = 0.05))
  duration.bound.upper <- sapply(all.durations, function(X) quantile(X, probs = 0.95))
  
  p.errors <- (duration.mean - true.durations.matching) / true.durations.matching
  
  proposed.fit <- lm(duration.mean ~ true.durations.matching - 1)
  
  proposed.slope <- proposed.fit$coefficients[1]
  proposed.r2 <- summary(proposed.fit)$r.squared
  
  
  naive.fit <- lm(original.durations ~ true.durations.matching - 1)
  
  naive.slope <- naive.fit$coefficients[1]
  naive.r2 <- summary(naive.fit)$r.squared
  
  
  
  slopes$proposed[number.of.sensors] <- proposed.fit$coefficients[1]
  slopes$proposed.lower[number.of.sensors] <- confint(proposed.fit)[1]
  slopes$proposed.upper[number.of.sensors] <- confint(proposed.fit)[2]
  
  slopes$naive[number.of.sensors] <- naive.fit$coefficients[1]
  slopes$naive.lower[number.of.sensors] <- confint(naive.fit)[1]
  slopes$naive.upper[number.of.sensors] <- confint(naive.fit)[2]
  
  r2$naive[number.of.sensors] <- naive.r2
  r2$proposed[number.of.sensors] <- proposed.r2
}







if (run.type == 1){
  name1 <- "ADED2022"
  ylim.vals <- c(-4,1)
} else if (run.type == 2){
  name1 <- "ADED2023"
  ylim.vals <- c(-1,1)
} else if (run.type == 3){
  name1 <- "Stanford"
  ylim.vals <- c(-1,1)
}

# STEP 10: MAKE PARITY PLOT
#---------------------------------------------------------------------------

if (run.type == 1){
  name1 <- "ADED2022"
} else if (run.type == 2){
  name1 <- "ADED2023"
} else if (run.type == 3){
  name1 <- "Stanford"
}





if (run.type == 2) {
  
  
  png(paste0(save.dir, 'METEC_and_summary_best_solution_', name1, '_', number.of.sensors, '_sensors.png'),
      width = 1920, height = 990, res = 100, pointsize = 24)
  
  library(fields)
  
  par(mfrow = c(1,2))
  
  par(mgp = c(2, 0.75, 0))
  par(mar = c(0, 1, 1, 0))
  par(oma = c(3.25, 2, 0, 4.25))
  
  
  
  tank.color <- "#3062CF" #blue
  wellhead.east.color <- "#9147B8" #purple
  separator.east.color <- "#7EAD52" #green
  separator.west.color <- "#F1C30E" #gold
  wellhead.west.color <- "#C7383C" #red
  
  tank.pch <- 21 # circle
  wellhead.east.pch <- 22 # square
  separator.east.pch <- 23 # diamond
  separator.west.pch <- 24 # up triangle
  wellhead.west.pch <- 25 # down triangle
  
  duration.mean <- sapply(all.durations, mean)
  duration.bound.lower <- sapply(all.durations, function(X) quantile(X, probs = 0.05))
  duration.bound.upper <- sapply(all.durations, function(X) quantile(X, probs = 0.95))
  
  p.errors <- (duration.mean - true.durations.matching) / true.durations.matching
  p.errors.naive <- (original.durations - true.durations.matching) / true.durations.matching
  
  if (run.type %in% c(1,2)){
    
    plot.cols <- vector(length = n.ints)
    plot.cols[true.locs.matching == "Tank"] <- tank.color
    plot.cols[true.locs.matching == "Wellhead.West"] <- wellhead.west.color
    plot.cols[true.locs.matching == "Separator.West"] <- separator.west.color
    plot.cols[true.locs.matching == "Wellhead.East"] <- wellhead.east.color
    plot.cols[true.locs.matching == "Separator.East"] <- separator.east.color
    plot.cols[plot.cols == "FALSE"] <- NA
    
    plot.pch <- vector(length = n.ints)
    plot.pch[true.locs.matching == "Tank"] <- tank.pch
    plot.pch[true.locs.matching == "Wellhead.West"] <- wellhead.west.pch
    plot.pch[true.locs.matching == "Separator.West"] <- separator.west.pch
    plot.pch[true.locs.matching == "Wellhead.East"] <- wellhead.east.pch
    plot.pch[true.locs.matching == "Separator.East"] <- separator.east.pch
    plot.pch[plot.pch == "FALSE"] <- NA
    
    axis.vals <- c(0,9)
    
  } else if (run.type == 3){
    plot.cols <- rep(tank.color, n.ints)
    plot.pch <- rep(tank.pch, n.ints)
    axis.vals <- c(0,10)
    
  }
  
  plot(true.durations.matching, duration.mean,
       pch = plot.pch, 
       asp = 1, col = "white", 
       xlim = axis.vals, ylim = axis.vals,
       xlab = "True duration [hrs]",
       ylab = "Estimated duration [hrs]", xpd = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/2, by = 1/2), y2 = seq(0,20*2, by = 1*2),
               col = "gray92", lineCol = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/1.5, by = 1/1.5), y2 = seq(0,20*1.5, by = 1*1.5),
               col = "gray84", lineCol = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/1.25, by = 1/1.25), y2 = seq(0,20*1.25, by = 1*1.25),
               col = "gray76", lineCol = NA)
  
  segments(x0 = 0, y0 = 0, x1 = 40, y1 = 40, lwd = 3, lty = 1) # 1:1
  
  segments(x0 = true.durations.matching, 
           y0 = duration.bound.lower, y1 = duration.bound.upper,
           col = alpha(plot.cols, 0.5))
  
  points(true.durations.matching, duration.mean, 
         col = plot.cols, bg = plot.cols, pch = plot.pch)
  
  points(true.durations.matching, original.durations,
         col = plot.cols, pch = plot.pch, lwd = 2.5)
  
  
  proposed.fit <- lm(duration.mean ~ true.durations.matching - 1)
  
  proposed.slope <- proposed.fit$coefficients[1]
  proposed.r2 <- summary(proposed.fit)$r.squared
  
  segments(x0 = 0, y0 = 0, x1 = (20)/proposed.slope, y1 = 20, lwd = 4, col = "gray40", lty = 2) # best fit
  
  naive.fit <- lm(original.durations ~ true.durations.matching - 1)
  
  naive.slope <- naive.fit$coefficients[1]
  naive.r2 <- summary(naive.fit)$r.squared
  
  segments(x0 = 0, y0 = 0, x1 = (20)/naive.slope, y1 = 20, lwd = 4, col = "gray40", lty = 3) # best fit
  
  
  round(100 * sum(p.errors < 1 & p.errors > -1/2, na.rm= T) / sum(!is.na(p.errors)), 1)
  
  round(100 * sum(p.errors < 0.25 & p.errors > -0.2, na.rm= T) / sum(!is.na(p.errors)), 1)
  
  round(100 * sum(p.errors.naive < 1 & p.errors.naive > -1/2, na.rm= T) / sum(!is.na(p.errors.naive)), 1)
  
  
  # legend("bottomleft",
  #        legend = c("Tank", "West Wellhead", "West Separator", "East Wellhead", "East Separator"),
  #        col = c(tank.color, wellhead.west.color, separator.west.color, wellhead.east.color, separator.east.color),
  #        pt.bg = c(tank.color, wellhead.west.color, separator.west.color, wellhead.east.color, separator.east.color),
  #        pch = c(tank.pch, wellhead.west.pch, separator.west.pch, wellhead.east.pch, separator.east.pch),
  #        pt.cex = 1.25,
  #        # cex = 1.15,
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  
  
  
  # legend("bottomleft",
  #        legend = c("Best fit: proposed method", "Best fit: naive method"),
  #        lty = c(2,3),
  #        lwd = 4,
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  # 
  # legend("bottomleft",
  #        legend = c("Proposed method (best fit line)", "Naive method (best fit line)"),
  #        pch = tank.pch,
  #        pt.bg = c("black", NA),
  #        pt.lwd = c(2.5,2),
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  
  
  
  slopes.pd <- slopes -1
  
  slopes.fd <- pd.to.fd(slopes.pd)
  
  slopes.fd <- as.matrix(slopes.fd)
  
  slopes.fd.to.plot <- ifelse(slopes.fd > 0, slopes.fd - 1, slopes.fd + 1)
  
  slopes.fd.to.plot <- as.data.frame(slopes.fd.to.plot)
  
  naive.col <- "gray25"
  proposed.col <- "gray25"
  
  plot(slopes.fd.to.plot$naive, pch = 19, ylim = c(-0.97, 0.97), yaxt = "n",
       xaxt = "n", xlab = "Number of sensors", xpd = NA, ylab = "", col = "white")
  
  alpha.val <- 0.175
  
  rect(xleft=-99, xright = 99,
       ybottom = -0.25, ytop = 0.25, col = alpha("forestgreen", alpha.val))
  
  
  
  rect(xleft=-99, xright = 99,
       ybottom = -0.5, ytop = -0.25, col = alpha("orange", alpha.val))
  rect(xleft=-99, xright = 99,
       ybottom = 0.25, ytop = 0.5, col = alpha("orange", alpha.val))
  
  
  
  rect(xleft=-99, xright = 99,
       ybottom = -99, ytop = -0.5, col = alpha("darkred", alpha.val))
  rect(xleft=-99, xright = 99,
       ybottom = 0.5, ytop = 99, col = alpha("darkred", alpha.val))
  
  mtext("Factor error of best fit line", side = 4, line = 3.25)
  
  points(slopes.fd.to.plot$naive, pch = 21, col = naive.col, lwd = 2.5)
  
  axis(side = 1, at = 1:10)
  
  lines(slopes.fd.to.plot$naive, col = naive.col, lty = 3, lwd = 4)
  
  envelopePlot(y1 = slopes.fd.to.plot$naive.lower, y2 = slopes.fd.to.plot$naive.upper,
               x1 = 1:n.r,
               lineCol = NA, col = alpha(naive.col, 0.25))
  
  
  points(slopes.fd.to.plot$proposed, pch = 19, col = proposed.col)
  
  lines(slopes.fd.to.plot$proposed, col = proposed.col, lwd = 4, lty = 2)
  
  envelopePlot(y1 = slopes.fd.to.plot$proposed.lower, y2 = slopes.fd.to.plot$proposed.upper,
               x1 = 1:n.r,
               lineCol = NA, col = alpha(proposed.col, 0.25))
  
  axis(side = 4,
       at = c(-1,-0.5,-0.25, 0, 0.25, 0.5, 1),
       labels = c("-2x", "-1.5x", "-1.25x", "1x", "1.25x", "1.5x", "2x"), las = 1)
  
  abline(h = 0)
  
  
  dev.off()
  
} else if (run.type == 1){
  
  
  png(paste0(save.dir, 'METEC_and_summary_best_solution_', name1, '_', number.of.sensors, '_sensors.png'),
      width = 1920, height = 990, res = 100, pointsize = 24)
  
  library(fields)
  
  par(mfrow = c(1,2))
  
  par(mgp = c(2, 0.75, 0))
  par(mar = c(0, 1, 1, 0))
  par(oma = c(3.25, 2, 0, 4.25))
  
  
  
  tank.color <- "#3062CF" #blue
  wellhead.east.color <- "#9147B8" #purple
  separator.east.color <- "#7EAD52" #green
  separator.west.color <- "#F1C30E" #gold
  wellhead.west.color <- "#C7383C" #red
  
  tank.pch <- 21 # circle
  wellhead.east.pch <- 22 # square
  separator.east.pch <- 23 # diamond
  separator.west.pch <- 24 # up triangle
  wellhead.west.pch <- 25 # down triangle
  
  duration.mean <- sapply(all.durations, mean)
  duration.bound.lower <- sapply(all.durations, function(X) quantile(X, probs = 0.05))
  duration.bound.upper <- sapply(all.durations, function(X) quantile(X, probs = 0.95))
  
  p.errors <- (duration.mean - true.durations.matching) / true.durations.matching
  p.errors.naive <- (original.durations - true.durations.matching) / true.durations.matching
  
  if (run.type %in% c(1,2)){
    
    plot.cols <- vector(length = n.ints)
    plot.cols[true.locs.matching == "Tank"] <- tank.color
    plot.cols[true.locs.matching == "Wellhead.West"] <- wellhead.west.color
    plot.cols[true.locs.matching == "Separator.West"] <- separator.west.color
    plot.cols[true.locs.matching == "Wellhead.East"] <- wellhead.east.color
    plot.cols[true.locs.matching == "Separator.East"] <- separator.east.color
    plot.cols[plot.cols == "FALSE"] <- NA
    
    plot.pch <- vector(length = n.ints)
    plot.pch[true.locs.matching == "Tank"] <- tank.pch
    plot.pch[true.locs.matching == "Wellhead.West"] <- wellhead.west.pch
    plot.pch[true.locs.matching == "Separator.West"] <- separator.west.pch
    plot.pch[true.locs.matching == "Wellhead.East"] <- wellhead.east.pch
    plot.pch[true.locs.matching == "Separator.East"] <- separator.east.pch
    plot.pch[plot.pch == "FALSE"] <- NA
    
    axis.vals <- c(0,11)
    
  } else if (run.type == 3){
    plot.cols <- rep(tank.color, n.ints)
    plot.pch <- rep(tank.pch, n.ints)
    axis.vals <- c(0,10)
    
  }
  
  plot(true.durations.matching, duration.mean,
       pch = plot.pch, 
       asp = 1, col = "white", 
       xlim = axis.vals, ylim = axis.vals,
       xlab = "True duration [hrs]",
       ylab = "Estimated duration [hrs]", xpd = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/2, by = 1/2), y2 = seq(0,20*2, by = 1*2),
               col = "gray92", lineCol = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/1.5, by = 1/1.5), y2 = seq(0,20*1.5, by = 1*1.5),
               col = "gray84", lineCol = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/1.25, by = 1/1.25), y2 = seq(0,20*1.25, by = 1*1.25),
               col = "gray76", lineCol = NA)
  
  segments(x0 = 0, y0 = 0, x1 = 40, y1 = 40, lwd = 3, lty = 1) # 1:1
  
  segments(x0 = true.durations.matching, 
           y0 = duration.bound.lower, y1 = duration.bound.upper,
           col = alpha(plot.cols, 0.5))
  
  points(true.durations.matching, duration.mean, 
         col = plot.cols, bg = plot.cols, pch = plot.pch)
  
  points(true.durations.matching, original.durations,
         col = plot.cols, pch = plot.pch, lwd = 2.5)
  
  
  proposed.fit <- lm(duration.mean ~ true.durations.matching - 1)
  
  proposed.slope <- proposed.fit$coefficients[1]
  proposed.r2 <- summary(proposed.fit)$r.squared
  
  segments(x0 = 0, y0 = 0, x1 = (20)/proposed.slope, y1 = 20, lwd = 4, col = "gray40", lty = 2) # best fit
  
  naive.fit <- lm(original.durations ~ true.durations.matching - 1)
  
  naive.slope <- naive.fit$coefficients[1]
  naive.r2 <- summary(naive.fit)$r.squared
  
  segments(x0 = 0, y0 = 0, x1 = (20)/naive.slope, y1 = 20, lwd = 4, col = "gray40", lty = 3) # best fit
  
  
  round(100 * sum(p.errors < 1 & p.errors > -1/2, na.rm= T) / sum(!is.na(p.errors)), 1)
  
  round(100 * sum(p.errors < 0.25 & p.errors > -0.2, na.rm= T) / sum(!is.na(p.errors)), 1)
  
  round(100 * sum(p.errors.naive < 1 & p.errors.naive > -1/2, na.rm= T) / sum(!is.na(p.errors.naive)), 1)
  
  # legend("bottomleft",
  #        legend = c("Tank", "West Wellhead", "West Separator", "East Wellhead", "East Separator"),
  #        col = c(tank.color, wellhead.west.color, separator.west.color, wellhead.east.color, separator.east.color),
  #        pt.bg = c(tank.color, wellhead.west.color, separator.west.color, wellhead.east.color, separator.east.color),
  #        pch = c(tank.pch, wellhead.west.pch, separator.west.pch, wellhead.east.pch, separator.east.pch),
  #        pt.cex = 1.25,
  #        # cex = 1.15,
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  
  
  
  # legend("bottomleft",
  #        legend = c("Best fit: proposed method", "Best fit: naive method"),
  #        lty = c(2,3),
  #        lwd = 4,
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  # 
  # legend("bottomleft",
  #        legend = c("Proposed method (best fit line)", "Naive method (best fit line)"),
  #        pch = tank.pch,
  #        pt.bg = c("black", NA),
  #        pt.lwd = c(2.5,2),
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  
  
  
  slopes.pd <- slopes -1
  
  slopes.fd <- pd.to.fd(slopes.pd)
  
  slopes.fd <- as.matrix(slopes.fd)
  
  slopes.fd.to.plot <- ifelse(slopes.fd > 0, slopes.fd - 1, slopes.fd + 1)
  
  slopes.fd.to.plot <- as.data.frame(slopes.fd.to.plot)
  
  naive.col <- "gray25"
  proposed.col <- "gray25"
  
  plot(slopes.fd.to.plot$naive, pch = 19, ylim = c(-2.5, 0.97), yaxt = "n",
       xaxt = "n", xlab = "Number of sensors", xpd = NA, ylab = "", col = "white")
  
  
  alpha.val <- 0.175
  
  rect(xleft=-99, xright = 99,
       ybottom = -0.25, ytop = 0.25, col = alpha("forestgreen", alpha.val))
  
  
  
  rect(xleft=-99, xright = 99,
       ybottom = -0.5, ytop = -0.25, col = alpha("orange", alpha.val))
  rect(xleft=-99, xright = 99,
       ybottom = 0.25, ytop = 0.5, col = alpha("orange", alpha.val))
  
  
  
  rect(xleft=-99, xright = 99,
       ybottom = -99, ytop = -0.5, col = alpha("darkred", alpha.val))
  rect(xleft=-99, xright = 99,
       ybottom = 0.5, ytop = 99, col = alpha("darkred", alpha.val))
  
  mtext("Factor error of best fit line", side = 4, line = 3.25)
  
  points(slopes.fd.to.plot$naive, pch = 21, col = naive.col, lwd = 2.5)
  
  axis(side = 1, at = 1:10)
  
  lines(slopes.fd.to.plot$naive, col = naive.col, lty = 3, lwd = 4)
  
  envelopePlot(y1 = slopes.fd.to.plot$naive.lower, y2 = slopes.fd.to.plot$naive.upper,
               x1 = 1:n.r,
               lineCol = NA, col = alpha(naive.col, 0.25))
  
  
  points(slopes.fd.to.plot$proposed, pch = 19, col = proposed.col)
  
  lines(slopes.fd.to.plot$proposed, col = proposed.col, lwd = 4, lty = 2)
  
  envelopePlot(y1 = slopes.fd.to.plot$proposed.lower, y2 = slopes.fd.to.plot$proposed.upper,
               x1 = 1:n.r,
               lineCol = NA, col = alpha(proposed.col, 0.25))
  
  axis(side = 4, 
       at = c(-2,-1,-0.5,-0.25, 0, 0.25, 0.5, 1),
       labels = c("-3x", "-2x", "-1.5x", "-1.25x", "1x", "1.25x", "1.5x", "2x"), las = 1)
  
  abline(h = 0)
  
  
  dev.off()
  
} else if (run.type == 3){
  
  events.to.remove <- c(63, 62, 57, 58, 56, 55, 54, 53, 35, 34, 31, 30)
  
  
  
  png(paste0(save.dir, 'METEC_and_summary_best_solution_', name1, '_', number.of.sensors, '_sensors.png'),
      width = 1920, height = 990, res = 100, pointsize = 24)
  
  library(fields)
  
  par(mfrow = c(1,2))
  
  par(mgp = c(2, 0.75, 0))
  par(mar = c(0, 1, 1, 0))
  par(oma = c(3.25, 2, 0, 4.25))
  
  
  
  tank.color <- "#3062CF" #blue
  wellhead.east.color <- "#9147B8" #purple
  separator.east.color <- "#7EAD52" #green
  separator.west.color <- "#F1C30E" #gold
  wellhead.west.color <- "#C7383C" #red
  
  tank.pch <- 21 # circle
  wellhead.east.pch <- 22 # square
  separator.east.pch <- 23 # diamond
  separator.west.pch <- 24 # up triangle
  wellhead.west.pch <- 25 # down triangle
  
  duration.mean <- sapply(all.durations, mean)
  duration.bound.lower <- sapply(all.durations, function(X) quantile(X, probs = 0.05))
  duration.bound.upper <- sapply(all.durations, function(X) quantile(X, probs = 0.95))
  
  p.errors <- (duration.mean - true.durations.matching) / true.durations.matching
  p.errors.naive <- (original.durations - true.durations.matching) / true.durations.matching
  
  if (run.type %in% c(1,2)){
    
    plot.cols <- vector(length = n.ints)
    plot.cols[true.locs.matching == "Tank"] <- tank.color
    plot.cols[true.locs.matching == "Wellhead.West"] <- wellhead.west.color
    plot.cols[true.locs.matching == "Separator.West"] <- separator.west.color
    plot.cols[true.locs.matching == "Wellhead.East"] <- wellhead.east.color
    plot.cols[true.locs.matching == "Separator.East"] <- separator.east.color
    plot.cols[plot.cols == "FALSE"] <- NA
    
    plot.pch <- vector(length = n.ints)
    plot.pch[true.locs.matching == "Tank"] <- tank.pch
    plot.pch[true.locs.matching == "Wellhead.West"] <- wellhead.west.pch
    plot.pch[true.locs.matching == "Separator.West"] <- separator.west.pch
    plot.pch[true.locs.matching == "Wellhead.East"] <- wellhead.east.pch
    plot.pch[true.locs.matching == "Separator.East"] <- separator.east.pch
    plot.pch[plot.pch == "FALSE"] <- NA
    
    axis.vals <- c(0,11)
    
  } else if (run.type == 3){
    plot.cols <- rep(tank.color, n.ints)
    plot.pch <- rep(tank.pch, n.ints)
    axis.vals <- c(0,7)
    
  }
  
  duration.mean <- duration.mean
  true.durations.matching <- true.durations.matching
  original.durations <- original.durations
  
  duration.mean.filtered <- duration.mean[-events.to.remove]
  true.durations.matching.filtered <- true.durations.matching[-events.to.remove]
  original.durations.filtered <- original.durations[-events.to.remove]
  
  plot(true.durations.matching, duration.mean,
       pch = plot.pch, 
       asp = 1, col = "white", 
       xlim = axis.vals, ylim = axis.vals,
       xlab = "True duration [hrs]",
       ylab = "Estimated duration [hrs]", xpd = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/2, by = 1/2), y2 = seq(0,20*2, by = 1*2),
               col = "gray92", lineCol = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/1.5, by = 1/1.5), y2 = seq(0,20*1.5, by = 1*1.5),
               col = "gray84", lineCol = NA)
  
  envelopePlot(x1 = seq(0, 20, by= 1), y1 = seq(0, 20/1.25, by = 1/1.25), y2 = seq(0,20*1.25, by = 1*1.25),
               col = "gray76", lineCol = NA)
  
  segments(x0 = 0, y0 = 0, x1 = 40, y1 = 40, lwd = 3, lty = 1) # 1:1
  
  segments(x0 = true.durations.matching, 
           y0 = duration.bound.lower, y1 = duration.bound.upper,
           col = alpha(plot.cols, 0.5))
  
  points(true.durations.matching[events.to.remove], duration.mean[events.to.remove],
         col = "red", pch =19, cex = 1.5)
  
  points(true.durations.matching[events.to.remove], original.durations[events.to.remove],
         col = "red", pch =19, cex = 1.5)
  
  points(true.durations.matching, duration.mean, 
         col = plot.cols, bg = plot.cols, pch = plot.pch)
  
  points(true.durations.matching, original.durations,
         col = "cyan2", pch = plot.pch, lwd = 2.5)
  
  
  
  
  
  proposed.fit <- lm(duration.mean ~ true.durations.matching - 1)
  
  proposed.slope <- proposed.fit$coefficients[1]
  proposed.r2 <- summary(proposed.fit)$r.squared
  
  segments(x0 = 0, y0 = 0, x1 = (20)/proposed.slope, y1 = 20, lwd = 4, col = "gray40", lty = 2) # best fit
  
  naive.fit <- lm(original.durations ~ true.durations.matching - 1)
  
  naive.slope <- naive.fit$coefficients[1]
  naive.r2 <- summary(naive.fit)$r.squared
  
  segments(x0 = 0, y0 = 0, x1 = (20)/naive.slope, y1 = 20, lwd = 4, col = "gray40", lty = 3) # best fit
  
  
  
  
  
  proposed.fit.filtered <- lm(duration.mean.filtered ~ true.durations.matching.filtered - 1)
  
  proposed.slope.filtered <- proposed.fit.filtered$coefficients[1]
  proposed.r2.filtered <- summary(proposed.fit.filtered)$r.squared
  
  segments(x0 = 0, y0 = 0, x1 = (20)/proposed.slope.filtered, y1 = 20, lwd = 4, col = "red", lty = 2) # best fit
  
  naive.fit.filtered <- lm(original.durations.filtered ~ true.durations.matching.filtered - 1)
  
  naive.slope.filtered <- naive.fit.filtered$coefficients[1]
  naive.r2.filtered <- summary(naive.fit.filtered)$r.squared
  
  segments(x0 = 0, y0 = 0, x1 = (20)/naive.slope.filtered, y1 = 20, lwd = 4, col = "red", lty = 3) # best fit
  
  
  round(100 * sum(p.errors < 1 & p.errors > -1/2, na.rm= T) / sum(!is.na(p.errors)), 1)
  
  round(100 * sum(p.errors < 0.25 & p.errors > -0.2, na.rm= T) / sum(!is.na(p.errors)), 1)
  
  round(100 * sum(p.errors.naive < 1 & p.errors.naive > -1/2, na.rm= T) / sum(!is.na(p.errors.naive)), 1)
  
  
  
  
  # legend("bottomleft",
  #        legend = c(""),
  #        col = c("red"),
  #        pch = 19,
  #        # cex = 1.15,
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  
  
  
  # legend("bottomleft",
  #        legend = c("Best fit: proposed method", "Best fit: naive method"),
  #        lty = c(2,3),
  #        lwd = 4,
  #        col = "red",
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  
  # legend("bottomleft",
  #        legend = c("Proposed method (best fit line)", "Naive method (best fit line)"),
  #        pch = tank.pch,
  #        pt.bg = c("black", NA),
  #        pt.lwd = c(2.5,2),
  #        box.col = "white",
  #        inset = c(0.1, 0.05))
  
  
  
  slopes.pd <- slopes -1
  
  slopes.fd <- pd.to.fd(slopes.pd)
  
  slopes.fd <- as.matrix(slopes.fd)
  
  slopes.fd.to.plot <- ifelse(slopes.fd > 0, slopes.fd - 1, slopes.fd + 1)
  
  slopes.fd.to.plot <- as.data.frame(slopes.fd.to.plot)
  
  naive.col <- "gray25"
  proposed.col <- "gray25"
  
  # plot(slopes.fd.to.plot$naive, pch = 19, ylim = c(-1.25, 1.25), yaxt = "n",
  #      xaxt = "n", xlab = "Number of sensors", xpd = NA, ylab = "", col = "white")
  
  plot(slopes.fd.to.plot$naive, pch = 19, ylim = c(-3, 0.97), yaxt = "n",
       xaxt = "n", xlab = "Number of sensors", xpd = NA, ylab = "", col = "white")
  
  alpha.val <- 0.175
  
  rect(xleft=-99, xright = 99,
       ybottom = -0.25, ytop = 0.25, col = alpha("forestgreen", alpha.val))
  
  
  
  rect(xleft=-99, xright = 99,
       ybottom = -0.5, ytop = -0.25, col = alpha("orange", alpha.val))
  rect(xleft=-99, xright = 99,
       ybottom = 0.25, ytop = 0.5, col = alpha("orange", alpha.val))
  
  
  
  rect(xleft=-99, xright = 99,
       ybottom = -99, ytop = -0.5, col = alpha("darkred", alpha.val))
  rect(xleft=-99, xright = 99,
       ybottom = 0.5, ytop = 99, col = alpha("darkred", alpha.val))
  
  mtext("Factor error of best fit line", side = 4, line = 3.25)
  
  points(slopes.fd.to.plot$naive, pch = 21, col = naive.col, lwd = 2.5)
  
  axis(side = 1, at = 1:10)
  
  lines(slopes.fd.to.plot$naive, col = naive.col, lty = 3, lwd = 4)
  
  envelopePlot(y1 = slopes.fd.to.plot$naive.lower, y2 = slopes.fd.to.plot$naive.upper,
               x1 = 1:n.r,
               lineCol = NA, col = alpha(naive.col, 0.25))
  
  
  points(slopes.fd.to.plot$proposed, pch = 19, col = proposed.col)
  
  lines(slopes.fd.to.plot$proposed, col = proposed.col, lwd = 4, lty = 2)
  
  envelopePlot(y1 = slopes.fd.to.plot$proposed.lower, y2 = slopes.fd.to.plot$proposed.upper,
               x1 = 1:n.r,
               lineCol = NA, col = alpha(proposed.col, 0.25))
  
  
  axis(side = 4,
       at = c(-3,-2,-1,-0.5,-0.25, 0, 0.25, 0.5, 1),
       labels = c("-4x","-3x","-2x", "-1.5x", "-1.25x", "1x", "1.25x", "1.5x", "2x"), las = 1)
  
  
  abline(h = 0)
  
  
  dev.off()
  
}

