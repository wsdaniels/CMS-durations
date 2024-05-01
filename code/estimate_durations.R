# Description: Estimate emission durations using CMS, accounting for 
#              CMS non-detect times.
# Author: William Daniels (wdaniels@mines.edu)
# Last Updated: May 1, 2024

# Clear environment
if(!is.null(dev.list())){dev.off()}
rm(list = ls())

# Import necessary libraries
library(lubridate)
library(zoo)
library(scales)

# Source helper files which contain helper functions 
source("/Users/wdaniels/Documents/code/DLQ/code/HELPER_spike_detection_algorithm.R")
source("/Users/wdaniels/Documents/code/CMS-durations/code/helper_functions.R")

# Read in simulation data
data <- readRDS('/Users/wdaniels/Documents/code/CMS-durations/input_data/case_study_forward_model_output.RData')

# Pull out sensor observations and replace NA's that are not on edge of 
# the time series with interpolated values
obs <- na.approx(data$obs, na.rm = F)

# Number of sensors
n.r <- ncol(obs)

# Pull out time stamps of observations and simulations
times <- data$times

# Pull out the simulation predictions
sims <- data[5:length(data)]

# Grab source info
n.s <- length(sims) 
source.names <- names(sims)

# Define function to create a logarithmic spaced sequence (used later)
lseq <- function(from, to, length.out) {
  exp(seq(log(from), log(to), length.out = length.out))
}


# Remove background from CMS observations
obs <- remove.background(obs,
                         going.up.threshold = 0.25, amp.threshold = 0.75, 
                         gap.time = 30)

# Create minute-by-minute maximum value time series across all CMS sensors
max.obs <- apply(obs, 1, max, na.rm = T)

# Identify spikes in the max.obs time series. These are the "naive events"
spikes <- perform.event.detection(max.obs, gap.time = 30, length.threshold = 15)

# Pull event "event numbers" that uniquely identify each naive event
event.nums <- na.omit(unique(spikes$events))

# Number of naive events
n.ints <- length(event.nums)

# Estimate source location for each naive event
loc.est.all.events <- perform.localization(spikes, obs, sims)

# Estimate emission rate for each naive event
all.q.vals <- perform.quantification(spikes, obs, sims, loc.est.all.events, print.report = T)

# saveRDS(all.q.vals, '/Users/wdaniels/Documents/papers/duration_model/case_study/all_q_vals.RData')
# all.q.vals <- readRDS('/Users/wdaniels/Documents/papers/duration_model/case_study/all_q_vals.RData')

# Grab emission rate point estimate and 90% interval for each naive event from the MC output
rate.est.all.events <- sapply(all.q.vals, mean, na.rm = T)
error.lower.all.events <- sapply(all.q.vals, function(X) quantile(X, probs = 0.05, na.rm = T))
error.upper.all.events <- sapply(all.q.vals, function(X) quantile(X, probs = 0.95, na.rm = T))

# Scale simulations by the estimated emission rate for each naive event
sims <- scale.sims(sims)

# Create information mask based on simulated concentrations
info.list <- create.info.mask(sims)

# Estimate durations. "out" contains a number of different fields (see below).
out <- get.durations(spikes = spikes, info.list = info.list, tz = "America/New_York")

# Grab distribution of possible durations for each naive event
all.durations <- out$all.durations

# Grab equipment-level duration distributions
est.durations <- out$est.durations

# Grab start time of naive events
event.starts <- out$event.starts

# Grab end time of naive events
event.ends <- out$event.ends

# Grab earliest possible start time for each naive event. The "start bounds"
start.bounds <- out$start.bounds

# Grab latest possible end time for each naive event. The "end bounds"
end.bounds <- out$end.bounds

# Calculate naive event durations
original.durations <- as.numeric(difftime(event.ends, event.starts, units = "hours"))

# Get distribution of possible event counts for each equipment group
event.counts <- get.event.counts(spikes = spikes, info.list = info.list, tz = "America/Denver")

# Compute total time of experiment window
total.time <- as.numeric(difftime(range(times)[2], range(times)[1], units = "days"))

# Scale event counts to annual-basis
num.events.per.year <- 365 * event.counts / total.time

# Get mean and 90% interval on emission frequencies for each equipment group
freq.mean <- apply(num.events.per.year, 2, mean)
freq.int <- apply(num.events.per.year, 2, function(X) quantile(X, probs = c(0.05, 0.95)))
frequency.results <- rbind(freq.mean, freq.int[1,], freq.int[2,])
print(t(round(frequency.results, 0))) # number of events per year


# Initialize variables to hold duration estimates by equipment group
# Mean of distribution of possible durations is used here
est.durations <- vector(mode = "list", length = n.s)
names(est.durations) <- source.names

# Grab durations
for (i in 1:length(all.durations)){
  list.ind <- which(names(all.durations)[i] == source.names)
  est.durations[[list.ind]] <- c(est.durations[[list.ind]], mean(all.durations[[i]]))
}

# Get mean and 90% interval for each equipment group across all emission events
est.average.durations <- sapply(est.durations, mean)
est.min.interval <- sapply(est.durations, function(X) quantile(X, probs = 0.05))
est.max.interval <- sapply(est.durations, function(X) quantile(X, probs = 0.95))


duration.out <- list(all.durations = all.durations,
                     est.durations = est.durations,
                     start.bounds = start.bounds,
                     end.bounds = end.bounds,
                     original.durations = original.durations,
                     info.list = info.list,
                     spikes = spikes,
                     max.obs = max.obs,
                     rate.est.all.events = rate.est.all.events,
                     error.lower.all.events = error.lower.all.events,
                     error.upper.all.events = error.upper.all.events,
                     loc.est.all.events = loc.est.all.events,
                     source.names = source.names)


saveRDS(duration.out, file = "/Users/wdaniels/Documents/code/CMS-durations/output_data/case_study_duration_estimates.RData")



