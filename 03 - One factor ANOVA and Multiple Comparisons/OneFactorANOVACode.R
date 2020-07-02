########################################
# Dallin Romney                        #
# Design of Experiments                #
########################################

rm(list = ls())
cat("\014")  

################### Problem 3-4 ###########################

# Initialize data
Temperature = as.factor(c(100, 100, 100, 100, 100, 125, 125, 125, 125, 150, 150, 150, 150, 150, 175, 175, 175, 175))
Density = c(21.8, 21.9, 21.7, 21.6, 21.7, 21.7, 21.4, 21.5, 21.4, 21.9, 21.8, 21.8, 21.6, 21.5, 21.9, 21.7, 21.8, 21.4)

# Visualize data
boxplot(Density~Temperature, main="Density vs. Temperature")

# Perform and view ANOVA analysis
DT_ANOVA_model = aov(Density~Temperature)
summary(DT_ANOVA_model)
plot(DT_ANOVA_model) # show 4 residual plots to verify assumptions

################### Problem 3-12 ###########################

# Initialize data
Design = as.factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4))
Noise = c(19, 20, 19, 30, 8, 80, 61, 73, 56, 80, 47, 26, 25, 35, 50, 95, 46, 83, 78, 97)

# Visualize data
boxplot(Noise~Design, main="Circuit Noise vs. Design")

# Perform and view ANOVA analysis
CD_ANOVA_model = aov(Noise~Design)
summary(CD_ANOVA_model)
plot(CD_ANOVA_model) # show 4 residual plots to verify assumptions

# Perform and visualize pairwise test (Tukey's HSD test, multiple comparisons)
CD_TukeyTest = TukeyHSD(CD_ANOVA_model, conf.level = 0.95)
CD_TukeyTest

plot(CD_TukeyTest)

################### PART 2 ###########################

# Read in and organize data
tires <- read.csv("C:/Users/Dallin/Google Drive/School/Design of Experiments/Homework/HW3/tires.csv")
Brands = tires$Brands
Mileage = tires$Mileage

# Visualize data
boxplot(Mileage~Brands, main="Mileage vs. Brand")

# Delete outliers - outlier at Milestar, 41.05
log = (Mileage == 41.05 & Brands == "Milestar")
Brands = Brands[!log]
Mileage = Mileage[!log]

# Perform and view ANOVA analysis
ANOVA_model = aov(Mileage~Brands)
summary(ANOVA_model)
plot(ANOVA_model) # show 4 residual plots to verify assumptions

# Perform and visualize pairwise test (Tukey's HSD test, multiple comparisons)
TukeyTest = TukeyHSD(ANOVA_model, conf.level = 0.95)
TukeyTest

plot(TukeyTest)

