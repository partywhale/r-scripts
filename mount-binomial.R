# Call ggplot2 & glue packages
library(ggplot2)
library(glue)
# Call theme_partywhale() custom theme
source("C:/Users/James/Desktop/R/theme_partywhale.R")


# Find probability of success using binomial probability density function
# Number of successes = 0, number of trials = 125, probability = 0.008
# Since success is inverse of failure, we're actually finding for failure and inverting
1 - dbinom(0, 125, 0.008)
# Result is 0.633597


# Finding the probability of finding exactly 1, 2, 3,... 7 mounts over 125 runs
# Note the sum of these values is 0.6335883973, very close to 0.633597
# We could go higher, but the values get smaller and smaller
dbinom(1:7, 125, 0.008)
# Output:
# [1] 0.3693578615 0.1846789308 0.0610631948 0.0150195762 0.0029312399
# [6] 0.0004727806 0.0000648167
# If we want all probabilities, we can use dbinom(1:125, 125, 0.008)
sum(dbinom(1:125, 125, 0.008))


# Plot our binomial distribution, including 0, up to 10, with ggplot2
y <- dbinom(0:10, 125, 0.008)
x <- (0:10)
qplot(x, y, colour = I("#F9858F"), size = I(4), shape = I(16)) + 
  scale_x_continuous(breaks = seq(0, 10, 2)) + 
  labs(title = "Binomial: Probability Mass Function",
       subtitle = "where p = 0.008 and n = 125",
       x = "Number of mount drops",
       y = "Probability") + 
  theme_partywhale()


# Cumulative distribution function
# Probability of x or fewer successes
# Not very useful information for this case
y <- pbinom(0:10, 125, 0.008)
x <- 0:10
qplot(x, y, colour = I("#F9858F"), size = I(4), shape = I(16)) + 
  scale_x_continuous(breaks = seq(0, 10, 2)) + 
  scale_y_continuous(breaks = seq(0.00, 1.00, 0.20)) + 
  expand_limits(y = 0) + 
  labs(title = "Binomial: Cumulative Distribution Function",
       subtitle = "where p = 0.008 and n = 125",
       x = "Number of mount drops",
       y = "Probability") + 
  theme_partywhale()


# Inverse of the cumulative distribution function - more useful!
# Probability of finding greater than a specified number of successes
# 0 = probability of 1 or more successes, i.e. 0.633597
# 1 = probability of 2 more more successes, i.e. 0.264239, etc.
y <- pbinom(0:10, 125, 0.008, lower.tail = FALSE)
x <- 0:10
qplot(x, y, colour = I("#F9858F"), size = I(4), shape = I(16)) + 
  scale_x_continuous(breaks = seq(0, 10, 2)) + 
  labs(title = "Binomial: Inverse Cumulative Distribution Function",
       subtitle = "where p = 0.008 and n = 125",
       x = "Number of mount drops, greater than",
       y = "Probability") + 
  theme_partywhale()
# Could also use 1 - pbinom(0:10, 125, 0.008)
# But R has this built in to the pbinom function


# We can use rbinom() to simulate many players doing many runs
# 10 players doing 125 runs each
rbinom(10, 125, 0.008)
# 1000 players doing 125 runs each
rbinom(1000, 125, 0.008)


# Creating a dataframe with simulated data, 100,000 players doing 125 runs
successes <- c(as.numeric(rbinom(100000, 125, 0.008)))
id <- c(as.numeric(1:length(successes)))
df <- data.frame("id" = id, "successes" = successes)
# str(df)
# Using ggplot2 to graph a histogram of our simulated data
simHistogram <- ggplot(df, aes(x = successes)) + 
  geom_histogram(binwidth = 1,
                 alpha = 0.60,
                 color = "dark red",
                 aes(fill = ..count..)) + 
  scale_fill_gradient("Frequency", low = "blue", high = "red") + 
  labs(title = "Histogram of Number of Mount Drops",
       subtitle = "Based on 100,000 simulations of 125 dungeon runs",
       x = "Number of mount drops",
       y = "Frequency") + 
  theme_partywhale()
simHistogram
# A univariate frequency table, if we want exact numbers
histTable <- table(successes)
histTable


# What about if we get 100,000 players to do 861 runs (where p = 99.9%)?
# Plot our binomial distribution, including 0, up to 25, n = 861
y <- dbinom(0:20, 861, 0.008)
x <- (0:20)
qplot(x, y, colour = I("#F9858F"), size = I(4), shape = I(16)) + 
  scale_x_continuous(breaks = seq(0, 20, 5)) + 
  labs(title = "Binomial: Probability Mass Function",
       subtitle = "where p = 0.008 and n = 861",
       x = "Number of mount drops",
       y = "Probability") + 
  theme_partywhale()
# And a simulation
successes <- c(as.numeric(rbinom(100000, 861, 0.008)))
id <- c(as.numeric(1:length(successes)))
df <- data.frame("id" = id, "successes" = successes)
# str(df)
# Using ggplot2 to graph a histogram of our simulated data
simHistogram <- ggplot(df, aes(x = successes)) + 
  geom_histogram(binwidth = 1,
                 alpha = 0.60,
                 color = "dark red",
                 aes(fill = ..count..)) + 
  scale_fill_gradient("Frequency", low = "blue", high = "red") + 
  labs(title = "Histogram of Number of Mount Drops",
       subtitle = "Based on 100,000 simulations of 861 dungeon runs",
       x = "Number of mount drops",
       y = "Frequency") + 
  theme_partywhale()
simHistogram
# A univariate frequency table, if we want exact numbers
histTable <- table(successes)
histTable


# Plot geometric distribution
# Probability it will take n failures before a success
# 300 failures before a success
dgeom(300, 0.008)
# Simple Plot
z <- 0:1000
plot(z, dgeom(z, 0.008), type = "h")
# Fancy Plot Setup - Make Dataframe
geomSingleProb <- c(as.numeric(dgeom(0:1000, 0.008)))
dfGeomDist <- data.frame("geomSingleProb" = geomSingleProb, "x" = 0:1000)
# Fancy Plot - ggplot2
simGeomHistogram <- ggplot(dfGeomDist, aes(x = x, y = geomSingleProb, fill = geomSingleProb, width = 1)) + 
  geom_col(alpha = 0.60,
           color = NA
           ) + 
  scale_fill_gradient("Probability", low = "blue", high = "red") + 
  labs(title = "Geometric Distribution",
       x = "Number of Trials",
       y = "Probability") + 
  theme_partywhale()
simGeomHistogram

# Test and record number of trials needed for a success
p <- 0.008                                         # Probability of success
trials <- runif(1e6) < p                           # Make 1 million trials
sim <- diff(c(TRUE, which(trials)))                # Find length of trials required
geomMean <- mean(sim)                              # Report mean length (number of trials)
geomMedian <- median(sim)                          # Report median length
geomMean
# Simple univariate frequency table
numTrials <- table(sim)
numTrials
# Dataframe for 'number of trials to first success' simulation
simnum <- c(as.numeric(sim))
df2 <- data.frame("simnum" = simnum)
# Histogram for 'number of trials to first success' simulation
simhist <- ggplot(df2, aes(x = simnum)) + 
  geom_histogram(binwidth = 1,
                 alpha = 0.60,
                 #color = "dark red",
                 aes(fill = ..count..)) + 
  scale_fill_gradient("Frequency", low = "blue", high = "red") + 
  geom_vline(aes(xintercept = geomMean),
             color = "deepskyblue", 
             linetype = "dashed", 
             size = 1) + 
  geom_text(aes(geomMean, 72, label = round(geomMean, 1), hjust = -0.25), color = "deepskyblue") + 
  geom_text(aes(geomMean, 75, label = "Mean", hjust = -0.30), color = "deepskyblue") + 
  geom_vline(aes(xintercept=geomMedian),
             color = "darkolivegreen2", 
             linetype = "dashed", 
             size = 1) + 
  geom_text(aes(geomMedian, 72, label = round(geomMedian, 1), hjust = 1.75), color = "darkolivegreen2") + 
  geom_text(aes(geomMedian, 75, label = "Median", hjust = 1.2), color = "darkolivegreen2") + 
  labs(title = "Histogram of Number of Trials to First Success",
       subtitle = "Based on 1,000,000 simulations",
       x = "Trials Required",
       y = "Frequency") + 
  theme_partywhale()
simhist

# Number of 'players' doing trials until they succeed
sum(trials) # Output: 7978



# using rgeom() for players as base unit rather than runs
g <- rgeom(100000, 0.008)
mean(g)
median(g)
table(g)

gNum <- c(as.numeric(g))
df <- data.frame("gNum" = gNum)

simhist <- ggplot(df, aes(x = gNum)) + 
  geom_histogram(binwidth = 1,
                 alpha = 0.60,
                 #color = "dark red",
                 aes(fill = ..count..)) + 
  scale_fill_gradient("Frequency", low = "blue", high = "red") + 
  labs(title = "Histogram of Number of Trials to First Success",
       subtitle = "Based on 100,000 simulations",
       x = "Trials Required",
       y = "Frequency") + 
  theme_partywhale()
simhist

sum(g)
