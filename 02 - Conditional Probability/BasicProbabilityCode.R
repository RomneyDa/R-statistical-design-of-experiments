# Dallin Romney
# Design of Experiments HW2
# January 24, 2019

# Data
rm(list = ls()) # clear workspace

car.df=data.frame(subsystem=c("brake.pads", "calipers", "brake.line",
                              "master.cylinder", "head.cover","engine.block",
                              "gear", "rear.diff", "front.diff", "wiring",
                              "alternator"),
                  p.fault=c(1/500, 1/1000, 1/100, 1/1500, 1/2000, 1/2500,
                            1/1500, 1/500, 1/550, 1/60, 1/100),
                p.failure=c(1/100000, 1/2000, 1/20000, 1/1500,
                              1/400000, 1/800000, 1/7000, 1/3000, 1/3000,
                              1/600, 1/150))

# PART I

# PART I.C
# gearFailureDist returns a distribution of how many gears fail in a set of transmissions
# t is the number of transmissions
# P is the probability of failure for one gear
# N is the number of gears in each transmission
# distribution is a data frame with the results
gearFailureDist = function(t, P, N){
  
  numFailedGears = 0:N
  
  # Use binomial theorem to populate vector of number of transmissions failed for each # gears failed
  numTransmissions = t*((1-P)^N)
  for (i in 1:N) {
    numTransmissions = c(numTransmissions, t*choose(N, i)*(1-P)^(N-i)*(P^i)) 
  }
  distribution = data.frame(numFailedGears, round(numTransmissions)) # Compile the results into a frame

  return(distribution)
}
# Distribution for 100000 6-gear transmissions with a 1/30 probability of a gear failing
dist = gearFailureDist(100000, 1/30, 6)
dist

# PART I.N
numCars = 100000000 # Number of cars to analyze

probAnyFailure = 1 - prod(1 - car.df$p.failure)         # Probability of any failure occuring
probComponents = car.df$p.failure/probAnyFailure        # Vector of probability that each component caused failure
car.df$expected = numCars*probAnyFailure*probComponents # Expected numbers of cars with failures

# Randomly generate the same number of cars and compare to expected results
car.df$failures = 0
for (subsystem.index in 1:length(car.df$subsystem)){
  car.df$failures[subsystem.index] <- rbinom(1, numCars, car.df$p.failure[subsystem.index])
}
car.df$failures
car.df$prop = car.df$failures/car.df$expected
car.df

# PART II

# PART II.D

slotMachine = function(numDrums, drumSize, numBlank){

  # Determine probabilities of each prize option (or no prize)
  pBig = (1/drumSize)^numDrums
  pSmall = ((drumSize - numBlank)/drumSize)^numDrums - pBig
  pNothing = 1 - pBig - pSmall
  probs = c(pBig, pSmall, pNothing)
  
  prizes = c("Big Prize", "Small Prize", "No Prize")
  
  # Randomly return a result!
  return(sample(prizes, 1, replace = TRUE, prob = probs))
}

# PART II.E

numTrials = 1000000         # Number of times to play
outputs = rep(NA,numTrials) # Empty vector for results
money = 0

# Play the slot machine a certain number of times
for(i in 1:numTrials){
  
  output = slotMachine(3, 12, 7)
  
  # Dish out prizes
  if(output == "Small Prize"){
    money = money - 17.64
  }
  if(output == "Big Prize"){
    money = money - 3000
  }
  # Populate vector of outputs
  outputs[i - 1] = output
  
  # Player pays $3 to play each time
  money = money + 3 
}
margin = money/(numTrials*3)
margin
table(outputs)/numTrials
table(outputs)

