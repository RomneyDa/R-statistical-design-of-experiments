########################################
# Dallin Romney - u1087199             #
# Design of Experiments Homework 4     #
# February 26, 2019                    #
########################################

rm(list = ls()) # Clear workspace
cat("\014")     # Clear console (control + L)

############## OPEN STATISTICS PROBLEMS ###############

#6.41
#predicted = as.factor(c(rep("book", 76), rep("print", 31), rep("online", 19)))
#actual    = as.factor(c(rep("book", 71), rep("print", 30), rep("online", 25)))
#chisq.test(df$Predicted, df$Actual, correct = "False")

############## ASSEMBLY LINE FAULT INVESTIGATION ##############

# Read in and organize data
assemblyLine = read.csv("C:/Users/Dallin/Google Drive/School/Design of Experiments/Homework/HW4/lineFaultData.csv")
response     = assemblyLine$response
line         = assemblyLine$line
device       = assemblyLine$device

# Visualize data
boxplot(response~line,   main = "Response time vs. Line")
boxplot(response~device, main = "Response time vs. Device")

# No outliers appear to be extreme, so I will include them

# Perform and view ANOVA analyses
lineANOVA   = aov(response~line)
deviceANOVA = aov(response~device)

summary(lineANOVA)
summary(deviceANOVA)

# show 4 residual plots for each, to verify assumptions
plot(lineANOVA)
plot(deviceANOVA) 

# Perform and visualize pairwise test (Tukey's HSD test, multiple comparisons)
lineTukey   = TukeyHSD(lineANOVA,   conf.level = 0.95)
deviceTukey = TukeyHSD(deviceANOVA, conf.level = 0.95)

lineTukey
deviceTukey

plot(lineTukey)
plot(deviceTukey)

# Perform ANOVA on each device (line within device)
log_motorController1 = device == "motor_controller1"
log_motorController2 = device == "motor_controller2"
log_pressureSwitch   = device == "pressure_switch"
log_sequenceReader   = device == "sequence_reader"
log_timingCircuit    = device == "timing_circuit"

ANOVA_motorController1 = aov(aov(response[log_motorController1]~line[log_motorController1]))
ANOVA_motorController2 = aov(aov(response[log_motorController2]~line[log_motorController2]))
ANOVA_pressureSwitch   = aov(aov(response[log_pressureSwitch  ]~line[log_pressureSwitch  ]))
ANOVA_sequenceReader   = aov(aov(response[log_sequenceReader  ]~line[log_sequenceReader  ]))
ANOVA_timingCircuit    = aov(aov(response[log_timingCircuit   ]~line[log_timingCircuit   ]))

summary(ANOVA_motorController1)
summary(ANOVA_motorController2)
summary(ANOVA_pressureSwitch)
summary(ANOVA_sequenceReader)
summary(ANOVA_timingCircuit)

# Visualize devices with significant differences between lines
boxplot(response[log_pressureSwitch]~line[log_pressureSwitch], main = "Response time vs. Line in Pressure Sensors")
boxplot(response[log_sequenceReader]~line[log_sequenceReader], main = "Response time vs. Line in Sequence Readers")

# Perform pairwise tests on devices with significant differences between lines
pressureSensorTukey = TukeyHSD(ANOVA_pressureSwitch, conf.level = 0.95)
sequenceReaderTukey = TukeyHSD(ANOVA_sequenceReader, conf.level = 0.95)

pressureSensorTukey
sequenceReaderTukey

# Count device frequencies
table(device, line)
