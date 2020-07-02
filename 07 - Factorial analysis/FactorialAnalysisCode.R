########################################
# Dallin Romney                        #
########################################

rm(list = ls()) # Clear workspace
cat("\014")     # Clear console (control + L)

############## PART I: Open Stats Problems ##############

########## Problem 5-7

drillSpeed  = factor(c(rep(125, 8), rep(200, 8)))
feedRate    = factor(rep(c(rep(0.015, 2), rep(0.030, 2), rep(0.045, 2), rep(0.060, 2)), 2))
thrustForce = c(2.70, 2.78, 2.45, 2.49, 2.60, 2.72, 2.75, 2.86,
                2.83, 2.86, 2.85, 2.80, 2.86, 2.87, 2.94, 2.88)

# Create a dataframe from the factors and response variable
problem1Data = data.frame(drillSpeed, feedRate, thrustForce, stringsAsFactors = FALSE)

with(problem1Data, interaction.plot(drillSpeed, feedRate, thrustForce))

problem1AOV0 = aov(thrustForce~drillSpeed+feedRate, data = problem1Data)
summary(problem1AOV0)

problem1AOV = aov(thrustForce~drillSpeed*feedRate, data = problem1Data)
summary(problem1AOV)

# Check assumptions, look at residuals
plot(problem1AOV$residuals)

par(mfrow=c(2,2))
plot(problem1AOV)

########## Problem 5-11

position    = factor(c(rep(1, 9), rep(2, 9)))
temperature = factor(rep(c(rep(800, 3), rep(825, 3), rep(850, 3)), 2))
density     = c(570, 565, 583, 1063, 1080, 1043, 565, 510, 590,
                528, 547, 521, 988, 1026, 1004, 526, 538, 532)


# Create a dataframe from the factors and response variable
problem2Data = data.frame(position, temperature, density, stringsAsFactors = FALSE)

with(problem2Data, interaction.plot(position, temperature, density))

problem2AOV = aov(density~position+temperature, data = problem2Data)
summary(problem2AOV)

# Check assumptions, look at residuals
plot(problem2AOV$residuals)

par(mfrow=c(2,2))
plot(problem2AOV)

########## Problem 5-17
cycleTime    = factor(c(rep(40, 18), rep(50, 18), rep(60, 18)))
temperature  = factor(rep(c(rep(300, 9), rep(350, 9)), 3))
operator     = factor(rep(c(1, 1, 1, 2, 2, 2, 3, 3, 3), 6))
score = c(23, 24, 25, 27, 28, 26, 31, 32, 29, 24, 23, 28, 38, 36, 35, 34, 36, 39,
          36, 35, 36, 34, 38, 39, 33, 34, 35, 37, 39, 35, 34, 38, 36, 34, 36, 31,
          28, 24, 27, 35, 35, 34, 26, 27, 25, 26, 29, 25, 36, 37, 34, 28, 26, 24)

# Create a dataframe from the factors and response variable
problem3Data = data.frame(cycleTime, temperature, operator, score, stringsAsFactors = FALSE)

with(problem3Data, interaction.plot(cycleTime, operator, score))
with(problem3Data, interaction.plot(cycleTime, temperature, score))
with(problem3Data, interaction.plot(operator, temperature, score))

problem3AOV = aov(score~cycleTime*temperature*operator, data = problem3Data)
summary(problem3AOV)

# Check assumptions, look at residuals
plot(problem3AOV$residuals)

par(mfrow=c(2,2))
plot(problem3AOV)

############## PART II: Multi-Factor Design Problem ##############

brand = factor(c(rep(1, 12), rep(2, 12)))
temp  = factor(rep(c(rep('cold', 4), rep('warm', 4), rep('hot', 4)), 2))
dirt  = c(4, 5, 6, 5,  7,  9,  8, 12, 10, 12, 11, 9, 
          6, 6, 4, 4, 13, 15, 12, 12, 12, 13, 10, 13)

# Create a dataframe from the factors and response variable
designData = data.frame(brand, temp, dirt, stringsAsFactors = FALSE)

par(mfrow=c(2,1))
with(designData, interaction.plot(brand, temp, dirt))

designAOV = aov(dirt~brand+temp, data = designData)
summary(designAOV)

# Check assumptions, look at residuals
plot(designAOV$residuals)

par(mfrow=c(2,2))
plot(designAOV)

