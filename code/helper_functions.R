# Define function to create a logarithmic spaced sequence (used later)
lseq <- function(from, to, length.out) {
  exp(seq(log(from), log(to), length.out = length.out))
}


get.sensor.arrangement <- function(run.type, which.arrangement, data, z){
  
  # Sensor arrangements for ADED 2022
  if (run.type == 1){
    
    if (which.arrangement == "best"){
      
      if (z == 1){
        sensors.to.use <- c("NW")
      } else if (z == 2){
        sensors.to.use <- c("NW", "SE")
      } else if (z == 3){
        sensors.to.use <- c("N", "SE",  "W" )
      } else if (z == 4){
        sensors.to.use <- c("E",  "N",  "SE", "W" )
      } else if (z == 5){
        sensors.to.use <- c("E",  "N",  "NW", "SE", "W" )
      } else if (z == 6){
        sensors.to.use <- c("E",  "N",  "NW", "SE", "SW", "W" )
      } else if (z == 7){
        sensors.to.use <- c("E",  "N",  "NE", "NW", "SE", "SW", "W" )
      } else if (z == 8){
        sensors.to.use <- c("E",  "N",  "NE", "NW", "S",  "SE", "SW", "W" )
      }  
      
    } else if (which.arrangement == "median"){
      
      if (z == 1){
        sensors.to.use <- c('N')
      } else if (z == 2){
        sensors.to.use <- c('N', 'S')
      } else if (z == 3){
        sensors.to.use <- c('E', 'NE', 'W')
      } else if (z == 4){
        sensors.to.use <- c('N', 'NE', 'NW', 'SE')
      } else if (z == 5){
        sensors.to.use <- c('E', 'N', 'NE', 'NW', 'SE')
      } else if (z == 6){
        sensors.to.use <- c('E', 'N', 'NE', 'NW', 'SE', 'SW')
      } else if (z == 7){
        sensors.to.use <- c('E', 'N', 'NE', 'S', 'SE', 'SW', 'W')
      } else if (z == 8){
        sensors.to.use <- c('E', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'W')
      }  
      
    } else if (which.arrangement == "mean"){
      
      if (z == 1){
        sensors.to.use <- c('E')
      } else if (z == 2){
        sensors.to.use <- c('NE', 'NW')
      } else if (z == 3){
        sensors.to.use <- c('E', 'N', 'S')
      } else if (z == 4){
        sensors.to.use <- c('E', 'N', 'NE', 'W')
      } else if (z == 5){
        sensors.to.use <- c('E', 'N', 'NW', 'S', 'SW')
      } else if (z == 6){
        sensors.to.use <- c('E', 'N', 'NW', 'S', 'SE', 'SW')
      } else if (z == 7){
        sensors.to.use <- c('E', 'N', 'NE', 'NW', 'S', 'SW', 'W')
      } else if (z == 8){
        sensors.to.use <- c('E', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'W')
      }  
      
    } else if (which.arrangement == "worst"){
      
      if (z == 1){
        sensors.to.use <- c('NE')
      } else if (z == 2){
        sensors.to.use <- c('N', 'NE')
      } else if (z == 3){
        sensors.to.use <- c('NE', 'S', 'SW')
      } else if (z == 4){
        sensors.to.use <- c('N', 'NE', 'S', 'SW')
      } else if (z == 5){
        sensors.to.use <- c('E', 'NE', 'S', 'SE', 'SW')
      } else if (z == 6){
        sensors.to.use <- c('E', 'N', 'NE', 'S', 'SE', 'SW')
      } else if (z == 7){
        sensors.to.use <- c('E', 'N', 'NE', 'NW', 'S', 'SE', 'SW')
      } else if (z == 8){
        sensors.to.use <- c('E', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'W')
      }  
      
    }
    
    # Sensor arrangements for ADED 2023
  } else if (run.type == 2){
    
    if (which.arrangement == "best"){
      
      if (z == 1){
        sensors.to.use <- c('N')
      } else if (z == 2){
        sensors.to.use <- c('N', 'SW')
      } else if (z == 3){
        sensors.to.use <- c('ENE', 'NW', 'SW')
      } else if (z == 4){
        sensors.to.use <- c('ESE', 'N', 'SW', 'WNW')
      } else if (z == 5){
        sensors.to.use <- c('ENE', 'N', 'SE', 'SW', 'WNW')
      } else if (z == 6){
        sensors.to.use <- c('ENE', 'N', 'S', 'SE', 'SW', 'WNW')
      } else if (z == 7){
        sensors.to.use <- c('ENE', 'N', 'NW', 'S', 'SE', 'SW', 'WNW')
      } else if (z == 8){
        sensors.to.use <- c('ENE', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'WNW')
      } else if (z == 9){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'WNW')
      } else if (z == 10){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'WNW', 'WSW')
      }
      
    } else if (which.arrangement == "median"){
      
      if (z == 1){
        sensors.to.use <- c('NE')
      } else if (z == 2){
        sensors.to.use <- c('S', 'SE')
      } else if (z == 3){
        sensors.to.use <- c('ESE', 'N', 'NE')
      } else if (z == 4){
        sensors.to.use <- c('N', 'NE', 'SW', 'WSW')
      } else if (z == 5){
        sensors.to.use <- c('N', 'NE', 'S', 'SW', 'WSW')
      } else if (z == 6){
        sensors.to.use <- c('ENE', 'ESE', 'NW', 'S', 'SE', 'WNW')
      } else if (z == 7){
        sensors.to.use <- c('ESE', 'NE', 'S', 'SE', 'SW', 'WNW', 'WSW')
      } else if (z == 8){
        sensors.to.use <- c('ENE', 'ESE', 'NW', 'S', 'SE', 'SW', 'WNW', 'WSW')
      } else if (z == 9){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'SE', 'SW', 'WNW', 'WSW')
      } else if (z == 10){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'WNW', 'WSW')
      }
      
      
    } else if (which.arrangement == "mean"){
      
      if (z == 1){
        sensors.to.use <- c('NE')
      } else if (z == 2){
        sensors.to.use <- c('ENE', 'SE')
      } else if (z == 3){
        sensors.to.use <- c('ENE', 'NE', 'S')
      } else if (z == 4){
        sensors.to.use <- c('ESE', 'SW', 'WNW', 'WSW')
      } else if (z == 5){
        sensors.to.use <- c('ENE', 'NE', 'NW', 'S', 'WNW')
      } else if (z == 6){
        sensors.to.use <- c('ENE', 'N', 'NE', 'SW', 'WNW', 'WSW')
      } else if (z == 7){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'S', 'SE', 'WNW')
      } else if (z == 8){
        sensors.to.use <- c('ESE', 'NE', 'NW', 'S', 'SE', 'SW', 'WNW', 'WSW')
      } else if (z == 9){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'SE', 'SW', 'WNW', 'WSW')
      } else if (z == 10){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'WNW', 'WSW')
      }
      
    } else if (which.arrangement == "worst"){
      
      if (z == 1){
        sensors.to.use <- c('WSW')
      } else if (z == 2){
        sensors.to.use <- c('WNW', 'WSW')
      } else if (z == 3){
        sensors.to.use <- c('NW', 'WNW', 'WSW')
      } else if (z == 4){
        sensors.to.use <- c('N', 'NW', 'WNW', 'WSW')
      } else if (z == 5){
        sensors.to.use <- c('N', 'NE', 'NW', 'WNW', 'WSW')
      } else if (z == 6){
        sensors.to.use <- c('ENE', 'N', 'NE', 'NW', 'WNW', 'WSW')
      } else if (z == 7){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'WNW', 'WSW')
      } else if (z == 8){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'S', 'WNW', 'WSW')
      } else if (z == 9){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'S', 'SE', 'WNW', 'WSW')
      } else if (z == 10){
        sensors.to.use <- c('ENE', 'ESE', 'N', 'NE', 'NW', 'S', 'SE', 'SW', 'WNW', 'WSW')
      }
      
    }
    
    # Sensor arrangements for Stanford releases
  } else if (run.type == 3){
    
    if (which.arrangement == "best"){
      
      if (z == 1){
        sensors.to.use <- c('2908')
      } else if (z == 2){
        sensors.to.use <- c('2907', '2908')
      } else if (z == 3){
        sensors.to.use <- c('2907', '2908', '2909')
      } else if (z == 4){
        sensors.to.use <- c('2906', '2907', '2908', '2909')
      } else if (z == 5){
        sensors.to.use <- c('2906', '2907', '2908', '2909', '2910')
      } else if (z == 6){
        sensors.to.use <- c('2906', '2907', '2908', '2909', '2910', '2911')
      }
      
    } else if (which.arrangement == "median"){
      
      if (z == 1){
        sensors.to.use <- c('2910')
      } else if (z == 2){
        sensors.to.use <- c('2910', '2911')
      } else if (z == 3){
        sensors.to.use <- c('2909', '2910', '2911')
      } else if (z == 4){
        sensors.to.use <- c('2906', '2908', '2910', '2911')
      } else if (z == 5){
        sensors.to.use <- c('2907', '2908', '2909', '2910', '2911')
      } else if (z == 6){
        sensors.to.use <- c('2906', '2907', '2908', '2909', '2910', '2911')
      }
      
    } else if (which.arrangement == "mean"){
      
      if (z == 1){
        sensors.to.use <- c('2909')
      } else if (z == 2){
        sensors.to.use <- c('2906', '2908')
      } else if (z == 3){
        sensors.to.use <- c('2906', '2908', '2910')
      } else if (z == 4){
        sensors.to.use <- c('2907', '2908', '2910', '2911')
      } else if (z == 5){
        sensors.to.use <- c('2907', '2908', '2909', '2910', '2911')
      } else if (z == 6){
        sensors.to.use <- c('2906', '2907', '2908', '2909', '2910', '2911')
      }
      
    } else if (which.arrangement == "worst"){
      
      if (z == 1){
        sensors.to.use <- c('2906')
      } else if (z == 2){
        sensors.to.use <- c('2906', '2910')
      } else if (z == 3){
        sensors.to.use <- c('2906', '2907', '2910')
      } else if (z == 4){
        sensors.to.use <- c('2906', '2907', '2910', '2911')
      } else if (z == 5){
        sensors.to.use <- c('2906', '2907', '2909', '2910', '2911')
      } else if (z == 6){
        sensors.to.use <- c('2906', '2907', '2908', '2909', '2910', '2911')
      }
      
    }
    
    # Sensor arrangements for AMI case study
  } else if (run.type == 4){
    
    sensors.to.use <- colnames(data$obs)
  }
  
  return(sensors.to.use)
  
}

remove.background <- function(times, obs, going.up.threshold, amp.threshold, gap.time){
  # Removes background from concentration time series
  # obs: matrix with columns representing the different CMS sensors and
  #      rows representing the time steps
  
  # Skip sensors that have only NA values
  to.use <- which(apply(obs, 2, function(X) !all(is.na(X))))
  
  # Loop through sensor units
  for (j in to.use){
    
    # Grab observations from this sensor
    this.raw.obs <- obs[,j]
    
    # Remove the NA's at the beginning and end of the time series
    # (some sensors start late or end early)
    to.keep <- !is.na(this.raw.obs)
    trimmed.obs <- this.raw.obs[to.keep]
    trimmed.times <- times[to.keep]
    
    # Flag spikes
    spikes <- find.spikes(obs = trimmed.obs,
                          times = trimmed.times,
                          going.up.threshold = going.up.threshold,
                          amp.threshold = amp.threshold,
                          make.plot = F)
    
    # Add points immediately before and after the spike to the spike mask
    # This better captures the full event, as there is some delay between the 
    # start of an emission and when the sensors first see an enhancement
    for (i in na.omit(unique(spikes$events))){
      min.ind <- min(which(spikes$events == i))
      max.ind <- max(which(spikes$events == i))
      spikes$events[c(max(c(min.ind-1, 1)), min(c(max.ind+1,nrow(spikes))))] <- i
    }
    
    # Pull out integers that uniquely identify an event
    event.nums <- na.omit(unique(spikes$events))
    
    # If there are at least two events
    if (length(event.nums) > 1){
      
      # Loop through spikes and combine spikes that are separated by less than gap.time minutes
      for (i in 2:length(event.nums)){
        
        # Mask in this spike and last spike
        this.spike <- spikes$events == event.nums[i]
        previous.spike <- spikes$events == event.nums[i-1]
        
        # Clean up
        this.spike[is.na(this.spike)] <- F
        previous.spike[is.na(previous.spike)] <- F
        
        # Get start time of current spike and end time of previous spike
        this.spike.start.time <- spikes$time[this.spike][1]
        previous.spike.end.time <- spikes$time[previous.spike][length(spikes$time[previous.spike])]
        
        # Compute time difference
        time.diff <- difftime(this.spike.start.time, previous.spike.end.time, units = "mins")
        
        # Check gap
        if (minutes(time.diff) < minutes(gap.time)){
          spikes$events[this.spike] <- event.nums[i-1]
          event.nums[i] <- event.nums[i-1]
        }
      }
    }
    
    # Grab the new event numbers after combining events in the previous for loop
    event.nums <- na.omit(unique(spikes$events))
    
    # If there are any events
    if (length(event.nums) > 0){
      
      # Loop through events and (1) fill in any gaps between events with the correct
      # event number, and (2) estimate background as mean of first and last spike 
      # points, which occur before the sharp increase and after the spike has 
      # returned to return.threshold percent of the max value
      for (i in 1:length(event.nums)){
        
        # Fill in gaps
        first.ob <- min(which(spikes$events == event.nums[i]))
        last.ob <- max(which(spikes$events == event.nums[i]))
        this.mask <- first.ob:last.ob
        spikes$events[this.mask] <- event.nums[i]
        
        # Estimate background using observations before and after spike
        # This is just the first and last observation within the spike mask,
        # since we already added the points before and after the spike to the 
        # spike mask earlier on
        b.left <- trimmed.obs[first.ob]
        b.right <- trimmed.obs[last.ob]
        b <- mean(c(b.left, b.right))
        
        # Remove background from this spike
        trimmed.obs[this.mask] <- trimmed.obs[this.mask] - b
      }
      
    }
    
    # Remove background from all non-spike data. Since by definition these points
    # are not in an event, their concentration value is directly taken as the 
    # background estimate. Hence removing background is just setting the value to zero
    trimmed.obs[is.na(spikes$events)] <- 0
    
    # Set any negative values to zero. Negative value would arise if the 
    # background estimate was too large (greater than actual concentration value)
    trimmed.obs[trimmed.obs < 0] <- 0
    
    # Save background removed data
    obs[to.keep, j] <- trimmed.obs
  }
  
  return(obs)
  
}


perform.event.detection <- function(times, max.obs, gap.time, length.threshold){
  # Creates the naive emission durations that do not account for CMS non-detect times
  
  # Create data frame with time steps and event mask
  spikes <- data.frame(time = times, events = max.obs > 0)
  
  # Find gaps between events that are shorter than gap.time and turn them into events
  #   to.replace holds the indices that need to be switched from F to T
  #   first.gap is an indicator for the first gap (which should not be replaced,
  #   regardless of length)
  #   false.seq holds the indices of each sequence of FALSEs (non-events)
  to.replace <- c()
  first.gap <- T
  false.seq <- c()
  
  # Loop through times
  for (i in 1:length(times)){
    
    # If not a spike, add index to false.sequence
    if (!spikes$events[i]){
      false.seq <- c(false.seq, i)
      
      # Otherwise, check length of false sequence, if greater than gap time,
      # save those indices to replace later
    } else if (spikes$events[i]){
      
      if (length(false.seq) <= gap.time & !first.gap){
        to.replace <- c(to.replace, false.seq)
      }
      
      first.gap <- F
      false.seq <- c()
    }
  }
  
  # Replace gaps shorter than gap.time with T (meaning they are in an event)
  spikes$events[to.replace] <- T
  
  # Now we replace the T/F with an integer to distinguish between events
  # Start by replacing F with NA and T with zero
  spikes$events[!spikes$events] <- NA
  spikes$events[spikes$events] <- 0
  
  # Get indices of spikes
  spike.points <- which(!is.na(spikes$events))
  count <- 0
  
  # Loop through spike points, if last point was not a spike, increase counter
  for (i in spike.points){
    
    if (is.na(spikes$events[i-1])){
      count <- count + 1
      spikes$events[i] <- count
    } else {
      spikes$events[i] <- count
    }
  }
  
  # Get integers that uniquely define the different events
  event.nums <- na.omit(unique(spikes$events))
  
  # Filter events by the length threshold
  for (i in 1:length(event.nums)){
    this.spike <- which(spikes$events == event.nums[i])
    
    if (length(this.spike) < length.threshold){
      spikes$events[this.spike] <- NA
    }
  }
  
  return(spikes)
  
}



perform.localization <- function(spikes, obs, sims){
  # Estimates emission source for each naive event
  
  source.names <- names(sims)
  
  # Pull event "event numbers" that uniquely identify each naive event
  event.nums <- na.omit(unique(spikes$events))
  
  # Number of naive events
  n.ints <- length(event.nums)
  
  n.s <- length(sims)
  n.r <- ncol(obs)
  
  # Matrix to hold alignment metric for each event and potential source
  metrics <- matrix(NA, nrow = n.ints, ncol = n.s)
  
  # Loop through events
  for (t in 1:n.ints){
    
    # Mask in this event
    this.mask <- seq(min(which(spikes$events == event.nums[t])),
                     max(which(spikes$events == event.nums[t])))
    
    # Loop through potential sources
    for (s in 1:n.s){
      
      # Grab simulation predictions from this source
      preds <- as.matrix(sims[[s]])
      
      # Initialize variables that will hold the predictions and observations
      # that we will compare to make the localization estimate
      all.obs.to.compare <- all.preds.to.compare <- c()
      
      # Loop through sensors
      for (r in 1:n.r){
        
        # Mask in only this event and this sensor
        obs.int <- obs[this.mask, r]
        preds.int <- preds[this.mask, r] 
        
        # Only compare non-NA observations
        to.compare <- !is.na(obs.int) 
        
        # Save predictions and observations to compare
        all.obs.to.compare <- c(all.obs.to.compare, obs.int[to.compare])
        all.preds.to.compare <- c(all.preds.to.compare, preds.int[to.compare])
        
      } # end loop through sensors
      
      # At least one non-NA observation is required to compute metric
      if (!all(is.na(all.obs.to.compare))){
        
        # Rename for brevity
        x <- all.preds.to.compare
        y <- all.obs.to.compare
        
        # Replace tiny values with zero
        x <- ifelse(x < 1e-30, 0, x)
        y <- ifelse(y < 1e-30, 0, y)
        
        # If there are nonzero values to compare, compute correlation
        if (all(x == 0) | all(y==0)){
          metrics[t,s] <- NA
        } else {
          metrics[t,s] <- cor(x, y, use = "complete.obs", method = "pearson")  
        }
        
      } # end if to check if there is at least one non-NA observation
    } # end loop through sources
  } # end loop through events
  
  
  # Convert NAs to zeros
  metrics[is.na(metrics)] <- 0
  
  loc.est.all.events <- vector(length = n.ints)
  for (t in 1:n.ints){
    
    # Get metric values for this event
    these.metrics <- metrics[t,]
    
    # Get source name corresponding to largest metric value
    loc.est.all.events[t] <- source.names[which.max(these.metrics)]
    
  }
  
  return(loc.est.all.events)
  
}




perform.quantification <- function(times, spikes, obs, sims, loc.est.all.events, print.report = F){
  # Estimates emission rate for each naive event
  
  # Pull event "event numbers" that uniquely identify each naive event
  event.nums <- na.omit(unique(spikes$events))
  
  # Number of naive events
  n.ints <- length(event.nums)
  
  n.r <- ncol(obs)
  source.names <- names(sims)
  
  # Vectors to hold localization and quantification results
  all.q.vals <- vector(mode = "list", length = n.ints)
  
  # Loop through events
  for (t in 1:n.ints){
    
    if (print.report){
      print(paste0("Quantification step: ", t, " / ", n.ints))
    }
    
    # Mask in this event
    this.mask <- seq(min(which(spikes$events == event.nums[t])),
                     max(which(spikes$events == event.nums[t])))
    
    sim.ind <- which(loc.est.all.events[t] == source.names)
    
    # Get predictions from most likely source and observations during this event
    event.preds <- as.matrix(sims [[sim.ind]][this.mask, ])
    event.obs   <- as.matrix(obs  [this.mask, ])
    event.times <- times[this.mask]
    
    # Vectors to hold preds and obs when both are in a spike
    all.preds.to.compare <- all.obs.to.compare <- c()
    
    # Loop through sensors and save when both obs and preds are in a spike
    for (r in 1:n.r){
      
      # Get predictions and observations for this sensor
      these.preds <- event.preds[,r]
      these.obs <- event.obs[,r]
      
      if (all(is.na(these.obs))){ next }
      
      # Find spikes in both predictions and observations.
      # We will only use data in which both a observation and prediction is in a spike
      # to estimate the emission rate. This reduces impact of forward model inadequacies.
      preds.spikes <- find.spikes(event.times, these.preds, amp.threshold = 1, make.plot = F)
      obs.spikes   <- find.spikes(event.times, these.obs,   amp.threshold = 1, make.plot = F)
      
      # Mask for times in which both preds and obs are in a spike
      both.in.spike.mask <- !is.na(preds.spikes$events) & !is.na(obs.spikes$events)
      
      # If there are any such time steps..
      if (sum(both.in.spike.mask) > 0){
        
        # Save data to compare
        all.preds.to.compare <- c(all.preds.to.compare, these.preds[both.in.spike.mask])
        all.obs.to.compare   <- c(all.obs.to.compare,   these.obs  [both.in.spike.mask])
      } 
      
    } # End loop over sensors
    
    
    # If there is enough data to compare, compute rate estimate and 
    # percent difference between obs and preds (will be used for UQ)
    if (length(all.preds.to.compare) > 4){
      
      # Number of times to sample from predictions and observations
      n.samples <- 500
      
      # Vector to hold emission rate estimate for each sample
      q.vals <- vector(length = n.samples)
      
      # Loop through samples
      for (o in 1:n.samples){
        
        # Sample from predictions and observations
        this.sample <- sample.int(n = length(all.preds.to.compare), 
                                  size = length(all.preds.to.compare)/2,
                                  replace = T)
        
        # Define a grid of emission rate values to optimize over
        q.grid <- lseq(0.0001, 3000, length.out = 2000)
        
        # Vector to hold sum of squared errors
        sse <- vector(length = length(q.grid))
        
        # Loop through possible emission rates
        for (zz in 1:length(q.grid)){
          
          # Scale predictions by this emission rate
          qxp <- q.grid[zz] * all.preds.to.compare
          
          # Compute root mean square error
          sse[zz] <- sqrt( mean( (all.obs.to.compare[this.sample] - qxp[this.sample])^2, na.rm = T) )
          
        } # End loop through grid of emission rates
        
        # If RMSE is monotonically increasing, there is no optimal q 
        if (all(diff(sse) > 0)){
          q.vals[o] <- NA
          
          # Otherwise, save best rate and percent differences
        } else {
          q.vals[o] <- q.grid[which.min(sse)]
        }
        
      } # End loop through samples
      
      # Save samples. Note: 3.6 multiplier converts from g/s to kg/hr
      all.q.vals[[t]] <- q.vals * 3.6
      
      # If there are not enough time steps in which both observations and
      # predictions are in a spike, then do not estimate a rate
    } else {
      all.q.vals[[t]] <- NA
    }
    
  } # End loop through events
  
  names(all.q.vals) <- loc.est.all.events
  
  return(all.q.vals)
  
}


scale.sims <- function(times, sims, spikes, loc.est.all.events, rate.est.all.events){
  # Scales simulated concentrations by the estimated emission rate of the nearest 
  # naive event
  
  # Pull event "event numbers" that uniquely identify each naive event
  event.nums <- na.omit(unique(spikes$events))
  
  # Number of naive events
  n.ints <- length(event.nums)
  
  # Loop through naive events
  for (t in 1:n.ints){
    
    # Get index of the forward simulations corresponding to the localization estimate
    sim.ind <- which(names(sims) == loc.est.all.events[t])
    
    # Get end time of previous event
    if (t > 1){
      previous.event.times <- na.omit(spikes$time[spikes$events == event.nums[t-1]])
      previous.event.end <- previous.event.times[length(previous.event.times)]
    } else {
      previous.event.end <- spikes$time[1]
    }
    
    # Get start time of following event
    if (t < n.ints){
      next.event.times <- na.omit(spikes$time[spikes$events == event.nums[t+1]])
      next.event.start <- next.event.times[1]
    } else {
      next.event.start <- spikes$time[length(spikes$time)]
    }
    
    # Get time interval of this event
    this.int <- range(spikes$time[spikes$events == event.nums[t]], na.rm = T)
    
    # Get times halfway between previous event end and this event start, 
    # and between this event end and following event start
    start.halfway.time <- difftime(this.int[1], previous.event.end, units = "hours")/2
    end.halfway.time   <- difftime(next.event.start, this.int[2], units = "hours")/2
    
    # Expand time interval of this event by the halfway times
    this.int <- interval(this.int[1] - start.halfway.time, this.int[2] + end.halfway.time)
    
    # Mask in times for this event
    time.mask <- times %within% this.int
    
    # Get rate estimate for this event
    this.q <- rate.est.all.events[t] / 3.6
    
    # If rate estimate for this event in not available, use mean of all rate estimates for this source
    if (is.na(this.q)){
      this.loc.est <- loc.est.all.events[t]
      this.q <- mean(rate.est.all.events[loc.est.all.events == this.loc.est], na.rm = T)
    }
    
    # Scale simulations during the time interval for this event by the rate estimate
    sims[[sim.ind]][time.mask, ] <- sims[[sim.ind]][time.mask, ] * this.q
  }
  
  return(sims)
  
}


create.info.mask <- function(times, sims, gap.time = 0, length.threshold = 15){
  # Determines which time periods have information from the CMS sensors and 
  # which do not.
  
  # Initialize variables to hold information mask
  info.list <- vector(mode = "list", length = length(sims))
  names(info.list) <- names(sims)
  
  # Loop through potential emission sources
  for (i in 1:length(sims)){
    
    # Get forward simulation for source i
    this.sim <- sims[[i]]
    
    # Filter out small spikes from simulations less than 0.75 kg/hr
    bgr.sim <- remove.background(times, this.sim, 
                                 going.up.threshold = 0.25, amp.threshold = 0.75,
                                 gap.time = gap.time)
    
    # Take minute-by-minute maximum across CMS sensors
    max.bgr.sim <- apply(bgr.sim, 1, max, na.rm = T)
    
    # Cluster spikes using event detection algorithm to create periods of information
    info.list[[i]] <- perform.event.detection(times, max.bgr.sim, gap.time = gap.time, length.threshold = length.threshold)
    
  }
  
  return(info.list)
  
}




get.combine.info <- function(spikes, info.list, loc.est.all.events, tz){
  # Figure out which naive events have non-zero probability of being combined with their neighbors
  
  # Pull event "event numbers" that uniquely identify each naive event
  event.nums <- na.omit(unique(spikes$events))
  
  # Number of naive events
  n.ints <- length(event.nums)
  
  # Initialize variables
  start.bounds <- event.starts <- end.bounds <- event.ends <- 
    combine.start <- combine.end <- vector(length = n.ints)
  
  # Loop through events
  for (t in 1:n.ints){
    
    # Mask in this event 
    this.mask <- seq(min(which(spikes$events == event.nums[t])),
                     max(which(spikes$events == event.nums[t])))
    
    # Get information mask for estimated emission source for event t
    info.mask <- info.list[[which(names(info.list) == loc.est.all.events[t])]]
    
    # Get start and end times for this event
    event.start <- this.mask[1]
    event.end <- this.mask[length(this.mask)]
    
    # Add a buffer to the beginning and end of this event
    # This makes it so very small periods of information (<10 min) 
    # do not "block" start and end time sampling from large periods of no information
    buffer.time <- 10 # [minutes]
    
    # Use the buffer if there are any periods of no information within the buffer time 
    # from the event
    use.start.buffer <- any(is.na(info.mask$events)[seq(max(1, this.mask[1]-1-buffer.time), 
                                                        this.mask[1]-1)])
    use.end.buffer <- any(is.na(info.mask$events)[seq(this.mask[length(this.mask)]+1,
                                                      min(nrow(spikes), this.mask[length(this.mask)]+1+buffer.time))])
    
    # Get time steps of the event, including buffer if applicable
    to.remove <- sort(unique(c(this.mask-buffer.time*use.start.buffer, this.mask, this.mask+buffer.time*use.end.buffer)))
    
    # Filtering if event is at very beginning or end of experiment time range
    to.remove <- to.remove[to.remove > 0]
    to.remove <- to.remove[to.remove < (nrow(spikes)+1)]
    
    # Get times in which we have information, excluding the times of the event 
    # and buffer
    tmp <- info.mask$events
    tmp[to.remove] <- NA
    tmp <- which(!is.na(tmp))
    
    # Find last time of information to be used as the "start bound", or the 
    # earliest possible time that the event might have started
    start.diff <- event.start - tmp 
    start.diff <- ifelse(start.diff < 0, NA, start.diff)
    start.bound <- spikes$time[tmp[which.min(start.diff)]] + minutes(1)
    
    # Find first time of information to be used as the "end bound", or the 
    # latest possible time that the event might have ended
    end.diff <- tmp - event.end 
    end.diff <- ifelse(end.diff < 0, NA, end.diff)
    end.bound <- spikes$time[tmp[which.min(end.diff)]] - minutes(1)
    
    # Error handling if event is near the end of the time series
    if (length(end.bound) == 0){
      end.bound <- spikes$time[length(spikes$time)]
    }
    
    # Error handling if event is near the beginning of the time series
    if (length(start.bound) == 0){
      start.bound <- spikes$time[1]
    }
    
    # Get times of previous event and move the start bound if it overlaps with
    # the previous event
    if (t > 1){
      previous.event.times <- na.omit(spikes$time[spikes$events == event.nums[t-1]])
      previous.event.end <- previous.event.times[length(previous.event.times)]
      start.bound <- max(previous.event.end, start.bound)
    }
    
    # Get times of following event and move the end bound if it overlaps with
    # the following event
    if (t < n.ints){
      next.event.times <- na.omit(spikes$time[spikes$events == event.nums[t+1]])
      next.event.start <- next.event.times[1]
      end.bound <- min(next.event.start, end.bound)
    }
    
    # Save data
    start.bounds[t] <- start.bound
    end.bounds[t] <- end.bound
    event.starts[t] <- spikes$time[this.mask][1]
    event.ends[t] <- spikes$time[this.mask][length(this.mask)]
    
    # If the event is not the first event, check if it can be combined with the previous event
    if (t > 1){
      
      # Get previous event times
      previous.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t-1])),
                                              max(which(spikes$events == event.nums[t-1])))]
      
      # Initialize check variable to false
      length.check <- F
      
      # If previous event is within "buffer time" from the period of no information surrounding the 
      # current event, then the "length check" is passed
      if (any(previous.event.times >= start.bound)){ 
        length.check <- T 
      } else {
        this.diff <- min(abs(as.numeric(difftime(start.bound, previous.event.times, units = "min"))))
        if (this.diff <= buffer.time){
          length.check <- T
        }
      }
      
      # Check if previous event has the same localization estimate
      same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t-1]
      
      # Combine if both length check and localization check are passed
      if (length.check & same.loc.est){ combine.start[t] <- T }
    }
    
    
    # If the event is not the last event, check if it can be combined with the following event
    if (t < n.ints){
      
      # Get next event times
      next.event.times <- spikes$time[seq(min(which(spikes$events == event.nums[t+1])),
                                          max(which(spikes$events == event.nums[t+1])))]
      
      # Initialize check variable to false
      length.check <- F
      
      # If following event is within "buffer time" from the period of no information surrounding the 
      # current event, then the "length check" is passed
      if (any(next.event.times <= end.bound)){ 
        length.check <- T 
      } else {
        this.diff <- min(abs(as.numeric(difftime(next.event.times, end.bound, units = "min"))))
        if (this.diff <= buffer.time){
          length.check <- T
        }
      }
      
      # Check if following event has the same localization estimate
      same.loc.est <- loc.est.all.events[t] == loc.est.all.events[t+1]
      
      # Combine if both length check and localization check are passed
      if (length.check & same.loc.est){ combine.end[t] <- T }
    }
    
  } # end loop over events
  
  # Convert to date time objects
  event.starts <- as_datetime(event.starts, tz = tz)
  event.ends <-   as_datetime(event.ends,   tz = tz)
  start.bounds <- as_datetime(start.bounds, tz = tz)
  end.bounds <-   as_datetime(end.bounds,   tz = tz)
  
  # Save everything
  out <- list(event.starts = event.starts,
              event.ends = event.ends,
              start.bounds = start.bounds,
              end.bounds = end.bounds,
              combine.start = combine.start,
              combine.end = combine.end)
  
  return(out)
  
  
}





get.durations <- function(spikes, info.list, loc.est.all.events, rate.est.all.events, tz){
  # Get distribution of possible durations for a given naive event
  
  source.names <- names(info.list)
  
  # Get information about which events can be combined with their neighbors
  out <- get.combine.info(spikes = spikes, info.list = info.list, loc.est.all.events, tz = tz)
  
  # Parse out info
  event.starts <- out$event.starts
  event.ends <- out$event.ends
  start.bounds <- out$start.bounds
  end.bounds <- out$end.bounds
  combine.start <- out$combine.start
  combine.end <- out$combine.end
  
  # Number of start and end time samples
  n.samples <- 100000
  
  n.ints <- length(loc.est.all.events)
  n.s <- length(source.names)
  
  # Initialize variable to hold all sampled durations
  all.durations <- vector(mode = "list", length = n.ints)
  names(all.durations) <- loc.est.all.events
  
  # Initialize variable to hold sampled durations by source
  est.durations <- vector(mode = "list", length = n.s)
  names(est.durations) <- source.names
  n.est <- sapply(est.durations, function(X) length(X)/n.samples)
  
  # Initialize variable to indicate which events had "hard stops".
  # These are the events that started and stopped fully within periods of information
  hard.stops <- vector(length = n.ints)
  
  # Get 5/95th percentile range of emission rate estimates. Will be used to normalize
  # probability of recombining event.
  rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
  rate.range <- rate.range[2] - rate.range[1]
  
  start.similarity.scores <- end.similarity.scores <- vector(mode = "list", length = n.ints)
  
  # Loop through events
  for (t in 1:n.ints){
    
    # See how many preceding and following events can be recombined with event t
    max.to.combine.start <- which(!(seq(1,n.ints) %in% (t-which(combine.end))))[1]-1
    max.to.combine.end   <- which(!(seq(1,n.ints) %in% (which(combine.start)-t)))[1]-1
    
    # Create sequence of possible recombine options
    start.combine.options <- seq(0, max.to.combine.start)
    end.combine.options   <- seq(0, max.to.combine.end)
    
    # Initialize variables to hold sampled start and end times from each even that can be recombined
    # These will be combined later, weighted by their probability of being recombined
    start.time.sample <- vector(mode = "list", length = length(start.combine.options))
    end.time.sample <- vector(mode = "list", length = length(end.combine.options))
    
    # Sample from all event start times that could be combined with event t
    for (i in 1:length(start.combine.options)){
      possible.start.times <- seq(start.bounds[t-start.combine.options[i]], event.starts[t-start.combine.options[i]], by = "1 min")
      start.time.sample[[i]] <- sample(possible.start.times, n.samples, replace = T)
    }
    
    # sample from all event end times that could be combined with event t
    for (i in 1:length(end.combine.options)){
      possible.end.times <- seq(event.ends[t+end.combine.options[i]], end.bounds[t+end.combine.options[i]], by = "1 min")
      end.time.sample[[i]] <- sample(possible.end.times, n.samples, replace = T)
    }
    
    # Indicate if event started and stopped fully within periods of information
    if (length(unique(unlist(start.time.sample))) == 1 & length(unique(unlist(end.time.sample))) == 1){
      hard.stops[t] <- T
    }
    
    # Number of recombine options
    n.start.options <- length(start.combine.options)
    n.end.options <- length(end.combine.options)
    
    # Vector to hold a "similarity score" that defines how similar event t's emission 
    # rate estimate is to the emission rates of all possible events that could be recombined 
    # with event t
    similarity.score <- vector(length = n.start.options)
    
    # Similarity score calculated as described in paper
    for (i in 1:n.start.options){
      jump <- start.combine.options[i]
      similarity.score[i] <- 1 - abs(rate.est.all.events[t-jump] - rate.est.all.events[t]) / rate.range
    }
    
    # Similarity score could be less than zero because we normalize using the 5/95
    # range rather than min/max range, so set negative values to zero
    similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
    
    # If a rate estimate of a given event is NA, assign equal probability of combining and
    # not combining
    similarity.score[is.na(similarity.score)] <- 0.5
    
    start.similarity.scores[[t]] <- similarity.score
    
    # Compute cumulative probability of combining given event with event t, based on the 
    # probability of combining the intermediate events
    if (n.start.options > 1){
      relative.probs <- cumprod(similarity.score)
      for (i in seq(length(relative.probs)-1, 1)){
        relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
      }
    } else {
      relative.probs <- 1
    }
    
    # Number of samples to take from each event, based on probability of recombining
    start.sample.nums <- round(n.samples * relative.probs)
    all.starts <- c()
    
    # Loop through events that can be recombined, sample start times
    for (i in 1:n.start.options){
      all.starts <- c(all.starts, sample(start.time.sample[[i]], start.sample.nums[i], replace = T))
    }
    
    # Return number of samples to n.samples
    all.starts <- sample(all.starts, n.samples, replace = T)
    
    # Vector to hold a "similarity score" that defines how similar event t's emission 
    # rate estimate is to the emission rates of all possible events that could be recombined 
    # with event t
    similarity.score <- vector(length = n.end.options)
    
    # Similarity score calculated as described in paper
    for (i in 1:n.end.options){
      jump <- end.combine.options[i]
      similarity.score[i] <- 1 - abs(rate.est.all.events[t+jump] - rate.est.all.events[t]) / rate.range
    }
    
    # Similarity score could be less than zero because we normalize using the 5/95
    # range rather than min/max range, so set negative values to zero
    similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
    
    # If a rate estimate of a given event is NA, assign equal probability of combining and
    # not combining
    similarity.score[is.na(similarity.score)] <- 0.5
    
    end.similarity.scores[[t]] <- similarity.score
    
    # Compute cumulative probability of combining given event with event t, based on the 
    # probability of combining the intermediate events
    if (n.end.options > 1){
      relative.probs <- cumprod(similarity.score)
      for (i in seq(length(relative.probs)-1, 1)){
        relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
      }
    } else {
      relative.probs <- 1
    }
    
    # Number of samples to take from each event, based on probability of recombining
    end.sample.nums <- round(n.samples * relative.probs)
    all.ends <- c()
    
    # Loop through events that can be recombined, sample end times
    for (i in 1:n.end.options){
      all.ends <- c(all.ends, sample(end.time.sample[[i]], end.sample.nums[i], replace = T))
    }
    
    # Return number of samples to n.samples
    all.ends <- sample(all.ends, n.samples, replace = T)
    
    # Compute all possible durations by taking difference of sampled start and end times
    all.durations[[t]] <- as.numeric(difftime(all.ends, all.starts, units = "hour"))
    
    # Save durations by source
    sim.ind <- which(source.names == loc.est.all.events[t])
    est.durations[[sim.ind]] <- c(est.durations[[sim.ind]], all.durations[[t]])
    
    
  } # end loop over events
  
  # Save everything
  out <- list(all.durations = all.durations, 
              est.durations = est.durations,
              event.starts = event.starts,
              event.ends = event.ends,
              start.bounds = start.bounds,
              end.bounds = end.bounds,
              hard.stops = hard.stops,
              start.similarity.scores = start.similarity.scores,
              end.similarity.scores = end.similarity.scores)
  
  return(out)
  
}








get.event.counts <- function(spikes, info.list, loc.est.all.events, rate.est.all.events, tz){
  # Get number of events by source, accounting for possibility to recombine events
  
  source.names <- names(info.list)
  
  # Get information about which events can be combined with their neighbors
  out <- get.combine.info(spikes = spikes, info.list = info.list, loc.est.all.events, tz = tz)
  
  # Parse everything out
  event.starts <- out$event.starts
  event.ends <- out$event.ends
  start.bounds <- out$start.bounds
  end.bounds <- out$end.bounds
  combine.start <- out$combine.start
  combine.end <- out$combine.end
  
  # Number of samples
  n.samples <- 10000
  
  n.s <- length(info.list)
  
  # Get 5/95th percentile range of emission rate estimates. Will be used to normalize
  # probability of recombining event.
  rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
  rate.range <- rate.range[2] - rate.range[1]
  
  # Initialize matrix to hold event counts by source.
  # Each row is a different realization of the Monte Carlo
  event.counts <- matrix(0, nrow = n.samples, ncol = n.s)
  colnames(event.counts) <- source.names
  
  n.ints <- length(loc.est.all.events)
  
  # Loop through number of samples
  for (z in 1:n.samples){
    
    # t iterator tracks number of events
    continue <- T
    t <- 1
    
    # Do until you reach last event
    while(continue){
      
      # See how many of the following events can be recombined with event t
      max.to.combine.end <- which(!(seq(1,n.ints) %in% (which(combine.start)-t)))[1]-1
      
      # Create sequence of possible recombine options
      end.combine.options <- seq(0, max.to.combine.end)
      
      # Number of recombine options
      n.end.options <- length(end.combine.options)
      
      # Initialize vector to hold similarity scores of events that can be recombined
      similarity.score <- vector(length = n.end.options)
      
      # Similarity score calculated as described in paper
      for (i in 1:n.end.options){
        jump <- end.combine.options[i]
        similarity.score[i] <- 1 - abs(rate.est.all.events[t+jump] - rate.est.all.events[t]) / rate.range
      }
      
      # Similarity score could be less than zero because we normalize using the 5/95
      # range rather than min/max range, so set negative values to zero
      similarity.score <- ifelse(similarity.score < 0, 0, similarity.score)
      
      # If a rate estimate of a given event is NA, assign equal probability of combining and
      # not combining
      similarity.score[is.na(similarity.score)] <- 0.5
      
      # Compute cumulative probability of combining given event with event t, based on the 
      # probability of combining the intermediate events
      if (n.end.options > 1){
        relative.probs <- cumprod(similarity.score)
        for (i in seq(length(relative.probs)-1, 1)){
          relative.probs[i] <- relative.probs[i] - sum(relative.probs[seq(i+1,length(relative.probs))])
        }
      } else {
        relative.probs <- 1
      }
      
      # Number to recombine. Will be different each iteration of the Monte Carlo
      n.to.combine <- sample.int(n=length(relative.probs), size = 1, prob = relative.probs)
      
      # Get estimated source and corresponding index to save event counts
      sim.ind <- which(colnames(event.counts) == loc.est.all.events[t])
      
      # Update event counts
      event.counts[z, sim.ind] <- event.counts[z, sim.ind] + 1
      
      # Iterate t (which loops through naive events) by the number of events that were
      # combined. This essentially "skips over" them
      t <- t + n.to.combine
      
      # Once you reach the last naive event, stop while loop
      if (t > n.ints){
        continue <- F
      }
      
    } # end loop over events
    
  } # end loop over samples
  
  return(event.counts)
  
}