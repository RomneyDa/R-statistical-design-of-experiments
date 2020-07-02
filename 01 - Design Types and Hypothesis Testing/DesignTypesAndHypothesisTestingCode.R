# Dallin Romney

######### Arbuthnot tutorial #########

# Load data from online source
# source("http://www.openintro.org/stat/data/arbuthnot.R")

# Inspect data
# arbuthnot
# dim(arbuthnot)
# names(arbuthnot)
# arbuthnot$boys

# Plot girls and boys individually
# plot(x = arbuthnot$year, y = arbuthnot$girls)
# plot(x = arbuthnot$year, y = arbuthnot$girls, type = "l")

# ?plot
# 5218 + 4683
# arbuthnot$boys + arbuthnot$girls

# Plot sum of girls and boys
# plot(arbuthnot$year, arbuthnot$boys + arbuthnot$girls, type = "l")

# 5218/4683
# arbuthnot$boys/arbuthnot$girls
# 5218/(5218 + 4683)
# arbuthnot$boys/(arbuthnot$boys + arbuthnot$girls)
# arbuthnot$boys > arbuthnot$girls

######### Present #########

# Load, inspect and assign data
source("http://www.openintro.org/stat/data/present.R")
present
dim(present)
names(present)
b <- present$boys
g <- present$girls
yr <- present$year

# Test for significance on difference of means
t.test(b, g)

# Plot boy to girl ratio
plot(x = present$year, y = b/g)
max(b/g)
min(b/g)
100-100/(1+min(b/g)) # Population boys percentage
100-100/(1+max(b/g))

# Find year of largest total number of births
yr[which.max(b+g)]
max(b+g)
