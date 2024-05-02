
remove.background <- function(obs, going.up.threshold, amp.threshold, gap.time){
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


perform.event.detection <- function(max.obs, gap.time, length.threshold){
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




perform.quantification <- function(spikes, obs, sims, loc.est.all.events, print.report = F){
  # Estimates emission rate for each naive event
  
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
      # rate.est.all.events[t]    <- NA
      # error.lower.all.events[t] <- NA
      # error.upper.all.events[t] <- NA
      
      all.q.vals[[t]] <- NA
    }
    
  } # End loop through events
  
  names(all.q.vals) <- loc.est.all.events
  
  return(all.q.vals)
  
}


scale.sims <- function(sims){
  # Scales simulated concentrations by the estimated emission rate of the nearest 
  # naive event
  
  for (t in 1:n.ints){
    
    sim.ind <- which(names(sims) == loc.est.all.events[t])
    
    if (t > 1){
      previous.event.times <- na.omit(spikes$time[spikes$events == event.nums[t-1]])
      previous.event.end <- previous.event.times[length(previous.event.times)]
    } else {
      previous.event.end <- spikes$time[1]
    }
    
    if (t < n.ints){
      next.event.times <- na.omit(spikes$time[spikes$events == event.nums[t+1]])
      next.event.start <- next.event.times[1]
    } else {
      next.event.start <- spikes$time[length(spikes$time)]
    }
    
    this.int <- range(spikes$time[spikes$events == event.nums[t]], na.rm = T)
    
    start.halfway.time <- difftime(this.int[1], previous.event.end, units = "hours")/2
    end.halfway.time   <- difftime(next.event.start, this.int[2], units = "hours")/2
    
    this.int <- interval(this.int[1] - start.halfway.time, this.int[2] + end.halfway.time)
    
    time.mask <- times %within% this.int
    
    this.q <- rate.est.all.events[t] / 3.6
    
    if (is.na(this.q)){
      this.loc.est <- loc.est.all.events[t]
      
      this.q <- mean(rate.est.all.events[loc.est.all.events == this.loc.est], na.rm = T)
    }
    
    sims[[sim.ind]][time.mask, ] <- sims[[sim.ind]][time.mask, ] * this.q
  }
  
  return(sims)
  
}


create.info.mask <- function(sims, gap.time = 0, length.threshold = 15){
  
  
  info.list <- vector(mode = "list", length = length(sims))
  names(info.list) <- names(sims)
  
  for (i in 1:length(sims)){
    
    this.sim <- sims[[i]]
    
    # bgr.sim <- remove.background(this.sim,
    #                              going.up.threshold = 0.25, amp.threshold = 0.75,
    #                              gap.time = 30)
    bgr.sim <- remove.background(this.sim,
                                 going.up.threshold = 0.25, amp.threshold = 0.75,
                                 gap.time = gap.time)
    
    max.bgr.sim <- apply(bgr.sim, 1, max, na.rm = T)
    
    # info.list[[i]] <- perform.event.detection(max.bgr.sim, gap.time = 30, length.threshold = 15)
    info.list[[i]] <- perform.event.detection(max.bgr.sim, gap.time = gap.time, length.threshold = length.threshold)
    
  }
  
  return(info.list)
  
}




get.combine.info <- function(spikes, info.list, tz){
  
  
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
  
  event.starts <- as_datetime(event.starts, tz = tz)
  event.ends <-   as_datetime(event.ends,   tz = tz)
  start.bounds <- as_datetime(start.bounds, tz = tz)
  end.bounds <-   as_datetime(end.bounds,   tz = tz)
  
  out <- list(event.starts = event.starts,
              event.ends = event.ends,
              start.bounds = start.bounds,
              end.bounds = end.bounds,
              combine.start = combine.start,
              combine.end = combine.end)
  
  return(out)
  
  
}





get.durations <- function(spikes, info.list, tz){
  
  
  out <- get.combine.info(spikes = spikes, info.list = info.list, tz = tz)
  
  event.starts <- out$event.starts
  event.ends <- out$event.ends
  start.bounds <- out$start.bounds
  end.bounds <- out$end.bounds
  combine.start <- out$combine.start
  combine.end <- out$combine.end
  
  
  n.samples <- 100000
  
  all.durations <- vector(mode = "list", length = n.ints)
  names(all.durations) <- loc.est.all.events
  
  est.durations <- vector(mode = "list", length = n.s)
  names(est.durations) <- source.names
  n.est <- sapply(est.durations, function(X) length(X)/n.samples)
  
  hard.stops <- vector(length = n.ints)
  
  rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
  rate.range <- rate.range[2] - rate.range[1]
  
  for (t in 1:n.ints){
    
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
    
    if (length(unique(unlist(start.time.sample))) == 1 & length(unique(unlist(end.time.sample))) == 1){
      hard.stops[t] <- T
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
    
    all.durations[[t]] <- as.numeric(difftime(all.ends, all.starts, units = "hour"))
    
    
    sim.ind <- which(source.names == loc.est.all.events[t])
    est.durations[[sim.ind]] <- c(est.durations[[sim.ind]], all.durations[[t]])
    
    
  } # end loop over events
  
  out <- list(all.durations = all.durations, 
              est.durations = est.durations,
              event.starts = event.starts,
              event.ends = event.ends,
              start.bounds = start.bounds,
              end.bounds = end.bounds,
              hard.stops = hard.stops)
  
  return(out)
  
}








get.event.counts <- function(spikes, info.list, tz){
  
  out <- get.combine.info(spikes = spikes, info.list = info.list, tz = tz)
  
  event.starts <- out$event.starts
  event.ends <- out$event.ends
  start.bounds <- out$start.bounds
  end.bounds <- out$end.bounds
  combine.start <- out$combine.start
  combine.end <- out$combine.end
  
  n.samples <- 10000
  
  rate.range <- quantile(rate.est.all.events, probs = c(0.05, 0.95), na.rm = T)
  rate.range <- rate.range[2] - rate.range[1]
  
  event.counts <- matrix(0, nrow = n.samples, ncol = n.s)
  colnames(event.counts) <- source.names
  
  for (z in 1:n.samples){
    
    continue <- T
    t <- 1
    
    while(continue){
      
      max.to.combine.end <- which(!(seq(1,n.ints) %in% (which(combine.start)-t)))[1]-1
      
      end.combine.options <- seq(0, max.to.combine.end)
      
      n.end.options <- length(end.combine.options)
      
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
      
      
      n.to.combine <- sample.int(n=length(relative.probs), size = 1, prob = relative.probs)
      
      sim.ind <- which(colnames(event.counts) == loc.est.all.events[t])
      
      event.counts[z, sim.ind] <- event.counts[z, sim.ind] + 1
      
      t <- t + n.to.combine
      
      if (t > n.ints){
        continue <- F
      }
      
    } # end loop over events
    
  } # end loop over samples
  
  return(event.counts)
  
}