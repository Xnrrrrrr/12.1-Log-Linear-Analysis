# Read the data into R as a data frame
lab12.data <- read.delim("lab 12 sra 365 wc-1.dat", header = TRUE)

#Step 2

table <- xtabs(~ per_sensitive_v2 + dys_detect_v2 + cost_controls_v2, data = lab12.data)

# Step 2: Run all models (0 to 8)

#Substitute var1, var2, and var3 with the names of your three categorical variables
#Substitute table with the name of your contingency table
##Run models 0 to 8

model0 = loglm(~per_sensitive_v2 + dys_detect_v2 + cost_controls_v2 + per_sensitive_v2:dys_detect_v2 + dys_detect_v2:cost_controls_v2 + per_sensitive_v2:cost_controls_v2 + per_sensitive_v2:dys_detect_v2:cost_controls_v2, data = table, fit = TRUE) #GOOD

model1 = loglm(~per_sensitive_v2 + dys_detect_v2 + cost_controls_v2 + per_sensitive_v2:dys_detect_v2 + dys_detect_v2:cost_controls_v2 + per_sensitive_v2:cost_controls_v2, data = table, fit = TRUE) # GOOD

model2 = loglm(~per_sensitive_v2 + dys_detect_v2 + cost_controls_v2 + per_sensitive_v2:dys_detect_v2 + dys_detect_v2:cost_controls_v2, data = table, fit = TRUE) # GOOD

model3 = loglm(~per_sensitive_v2 + dys_detect_v2 + cost_controls_v2 + per_sensitive_v2:dys_detect_v2 + per_sensitive_v2:cost_controls_v2, data = table, fit = TRUE) #GOOD

model4 = loglm(~per_sensitive_v2 + dys_detect_v2 + cost_controls_v2 + dys_detect_v2:cost_controls_v2 + per_sensitive_v2:cost_controls_v2, data = table, fit = TRUE) #GOOS

model5 = loglm(~per_sensitive_v2 + dys_detect_v2 + per_sensitive_v2:dys_detect_v2, data = table, fit = TRUE) # GOOD

model6 = loglm(~dys_detect_v2 + cost_controls_v2 + dys_detect_v2:cost_controls_v2, data = table, fit = TRUE) # GOOD

model7 = loglm(~per_sensitive_v2 + cost_controls_v2 + per_sensitive_v2:cost_controls_v2, data = table, fit = TRUE) # GOOD

model8 = loglm(~per_sensitive_v2 + dys_detect_v2 + cost_controls_v2, data = table, fit = TRUE) # GOOD

#Substitute var1, var2, and var3 with the names of your three categorical variables
#Substitute table with the name of your contingency table
##Run models 0 to 8

model0 = loglm(~var1 + var2 + var3 + var1:var2 + var2:var3 + var1:var3 + var1:var2:var3, data = table, fit = TRUE)

model1 = loglm(~var1 + var2 + var3 + var1:var2 + var2:var3 + var1:var3, data = table, fit = TRUE)

model2 = loglm(~var1 + var2 + var3 + var1:var2 + var2:var3, data = table, fit = TRUE)

model3 = loglm(~var1 + var2 + var3 + var1:var2 + var1:var3, data = table, fit = TRUE)

model4 = loglm(~var1 + var2 + var3 + var2:var3 + var1:var3, data = table, fit = TRUE)

model5 = loglm(~var1 + var2 + var1:var2, data = table, fit = TRUE)

model6 = loglm(~var2 + var3 + var2:var3, data = table, fit = TRUE)

model7 = loglm(~var1 + var3 + var1:var3, data = table, fit = TRUE)

model8 = loglm(~var1 + var2 + var3, data = table, fit = TRUE)
# var 1 = per_sensitive_v2
# var 2 = dys_detect_v2
# var 3 = cost_controls_v2
# Step 3: Print tables for each model
for (i in 1:length(models)) {
  cat("\nModel:", names(models)[i], "\n")
  print(summary(models[[i]]))
}

#Step 2

#Run the following summary statements one at a time

summary(model0) #saturated model with perfect fit

summary(model1) #if significant stop; model0 is your best fitting model; else test model 2

summary(model2) #if significant stop; model1 is your best fitting model; else test model 3

summary(model3) #if significant stop; model2 is your best fitting model; else test model 4

summary(model4) #if significant stop; model3 is your best fitting model; else test model 5

summary(model5) #if significant stop; model4 is your best fitting model; else test model 6

summary(model6) #if significant stop; model5 is your best fitting model; else test model 7

summary(model7) #if significant stop; model6 is your best fitting model; else test model 8

summary(model8) #if significant model7 is your best fitting model; else choose model 8

#Step 4

Fast.sub <- subset(lab12.data, dys_detect_v2 == "Fast")
Slow.sub <- subset(lab12.data, dys_detect_v2 == "Slow")

#Step 5

# Runs a chi-square analysis on the first subset of the data
chisq.test(Fast.sub$per_sensitive_v2, Fast.sub$cost_controls_v2)

# Runs a chi-square analysis on the second subset of the data
chisq.test(Slow.sub$per_sensitive_v2, Slow.sub$cost_controls_v2)




# Create a contingency table of the two categorical variables
contingency_table <- table(lab12.data$dys_detect_v2, lab12.data$per_sensitive_v2)

CrossTable(lab12.data$dys_detect_v2, lab12.data$per_sensitive_v2, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")




#Creates a plot among Animal, Training, and Dance
# Creates a plot among Animal, Training, and Dance
mosaicplot(table, shade = TRUE, main = "per_sensitive_v2, dys_detect_v2, & cost_controls_v2")
