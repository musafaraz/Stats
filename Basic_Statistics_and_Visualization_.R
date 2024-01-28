#UPLOAD THE DATASET

# Load necessary libraries
library(ggplot2)
library(viridis)
library(ggthemes)
library(olsrr)
library(jtools)
library(moments)
library(lmtest)
library(patchwork)

###################################
###Section 1: Summary Statistics###
###################################
View(melanoma)
# Get a summary of the 'melanoma' dataset
summary(melanoma) # This will provide statistical summary for each column in the dataset
# Convert 'status' variable to factor with labels 'death_melanoma', 'alive', 'death_other'
melanoma$status <- factor(melanoma$status, labels = c("death_melanoma", "alive", "death_other"))
# Convert 'sex' variable to factor with labels 'Female' and 'Male'
melanoma$sex <- factor(melanoma$sex, labels = c("Female", "Male"))
# Convert 'ulcer' variable to factor with labels 'Absent' and 'Present'
melanoma$ulcer <- factor(melanoma$ulcer, labels = c("Absent", "Present"))
# View the 'melanoma' dataset again to confirm if changes have been made
View(melanoma)
# Get a summary of the 'melanoma' dataset again to see the changes
summary(melanoma)


###################################
###Section 2: Graphical Summary####
###################################

#For Distribution of Status

# Calculate percentage of each status
status_counts <- table(melanoma$status)
percentages <- prop.table(status_counts) * 100
# Create a data frame with percentages
status_data <- data.frame(
  status = names(percentages),
  percentage = as.numeric(percentages)
)
colors <- viridis_pal()(length(percentages)) # Define color-blind-friendly colors for statuses
# Create the donut chart using ggplot2 with color-blind-friendly colors
donut_status <- ggplot(data = status_data, aes(x = 2, y = percentage, fill = status)) +
  geom_bar(stat = "identity", color = "black", width = 1, position = "stack") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), color = "white") +  # Set text color to white
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18)
  ) +
  ggtitle("Donut Chart of Status in Melanoma Dataset") +
  scale_fill_manual(values = colors) +
  xlim(0.5, 2.5)
donut_status

# For Distribution of Thickness
boxplot_thickness <- ggplot(melanoma, aes(y = thickness)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red") +
  labs(
    y = "Thickness",
    title = "Boxplot of Melanoma Thickness"
  ) +
  scale_fill_viridis(discrete = TRUE) +  # Using viridis color palette
  theme_minimal()
boxplot_thickness


#For Distribution of Age
# Set colorblind-friendly palette
cb_palette <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#D55E00", "#CC79A7")

# Histogram with density plot for age
ggplot(melanoma, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = cb_palette[6], color = "black", alpha = 0.6) +
  geom_density(fill = cb_palette[6], alpha = 0.6) +
  labs(title = "Distribution of Age with Density Plot",
       x = "Age",
       y = "Density") +
  theme_minimal()

############################################
###Section 3: Regression and Correlation####
############################################

# Run regression for time ~ thickness
reg_model1 <- lm(time ~ thickness, data = melanoma)
# Summary of the regression results
summary(reg_model1)
#Test for Homoskedasticity
ols_plot_resid_fit(reg_model1) #a funnel shape is found
ols_test_breusch_pagan(reg_model1)
#Test for Normality
ols_plot_resid_hist(reg_model1)
shapiro.test(reg_model1$residuals)
#Test for Linearity
# Scatter plot for 'Thickness' vs. 'Time'
plot(melanoma$thickness, melanoma$time,
     xlab = "Thickness (mm)", ylab = "Time (days)",
     main = "Scatter Plot: Thickness vs. Time",
     pch = 20,          # Use solid circle point shape
     cex = 1.5,         # Set point size
     col = "black")     # Set point color to black
fit <- lm(melanoma$time ~ melanoma$thickness)
abline(fit, col = "red", lwd = 2) 
# Trendline in red with increased line width
legend("topleft", legend = "Trendline", col = "red", 
       lty = 1, lwd = 2)
grid(col = "gray")  # Add gridlines
###################################################################



# Run regression for time ~ age
reg_model2 <- lm(time ~ age, data = melanoma)
# Summary of the regression results
summary(reg_model2)
#Test for Homoskedasticity
ols_plot_resid_fit(reg_model2) 
ols_test_breusch_pagan(reg_model2)
#Test for Normality
ols_plot_resid_hist(reg_model2)
shapiro.test(reg_model2$residuals)
# Scatter plot for 'Time' vs. 'Age' with a trendline
plot(melanoma$age, melanoma$time,
     xlab = "Age", ylab = "Time (days)",
     main = "Scatter Plot: Time vs. Age",
     pch = 20,          # Use solid circle point shape
     cex = 1.5,         # Set point size
     col = "black")     # Set point color to black

fit <- lm(melanoma$time ~ melanoma$age)
abline(fit, col = "red", lwd = 2)  # Trendline in red 
#with increased line width
legend("topright", legend = "Trendline", col = "red", lty = 1, lwd = 2)
grid(col = "gray")  # Add gridlines





###################################################################################



# Run regression for thickness ~ age
reg_model3 <- lm(thickness ~ age, data = melanoma)
# Summary of the regression results
summary(reg_model3)
# Plotting residuals against fitted values for homoscedasticity
plot(reg_model3, which = 1)  # 1 for residuals vs. fitted plot
ols_test_breusch_pagan(reg_model3)
#Test for Normality
ols_plot_resid_hist(reg_model3)
shapiro.test(reg_model3$residuals)
# Scatter plot for 'Thickness' vs. 'Age' with a trendline
plot(melanoma$age, melanoma$thickness,
     xlab = "Age", ylab = "Thickness (mm)",
     main = "Scatter Plot: Thickness vs. Age",
     pch = 20,          # Use solid circle point shape
     cex = 1.5,         # Set point size
     col = "black")     # Set point color to black

fit_thickness_age <- lm(melanoma$thickness ~ melanoma$age)
abline(fit_thickness_age, col = "blue", lwd = 2)  # Trendline in blue with increased line width

legend("topright", legend = "Trendline", col = "blue", lty = 1, lwd = 2)
grid(col = "gray")  # Add gridlines
##############################################################

cor(melanoma$time, melanoma$thickness)
cor(melanoma$time, melanoma$age)
cor(melanoma$thickness, melanoma$age)

# Correlation between 'time' and 'thickness' with significance level
cor_test_time_thickness <- cor.test(melanoma$time, melanoma$thickness)
cat("Correlation between time and thickness:", cor_test_time_thickness$estimate, "\n")
cat("p-value:", cor_test_time_thickness$p.value, "\n\n")

# Correlation between 'time' and 'age' with significance level
cor_test_time_age <- cor.test(melanoma$time, melanoma$age)
cat("Correlation between time and age:", cor_test_time_age$estimate, "\n")
cat("p-value:", cor_test_time_age$p.value, "\n\n")

# Correlation between 'thickness' and 'age' with significance level
cor_test_thickness_age <- cor.test(melanoma$thickness, melanoma$age)
cat("Correlation between thickness and age:", cor_test_thickness_age$estimate, "\n")
cat("p-value:", cor_test_thickness_age$p.value, "\n")

# Scatter plot for 'time' and 'thickness' with line of best fit
plot(melanoma$time, melanoma$thickness, xlab = "Time", ylab = "Thickness", main = "Scatter Plot: Time vs Thickness")
fit_thickness <- lm(melanoma$thickness ~ melanoma$time)  # Fit linear model
abline(fit_thickness, col = "red")  # Plot line of best fit

# Scatter plot for 'time' and 'age' with line of best fit
plot(melanoma$time, melanoma$age, xlab = "Time", ylab = "Age", main = "Scatter Plot: Time vs Age")
fit_age <- lm(melanoma$age ~ melanoma$time)  # Fit linear model
abline(fit_age, col = "blue")  # Plot line of best fit

# Scatter plot for 'thickness' and 'age' with line of best fit
plot(melanoma$thickness, melanoma$age, xlab = "Thickness", ylab = "Age", main = "Scatter Plot: Thickness vs Age")
fit_thickness_age <- lm(melanoma$age ~ melanoma$thickness)  # Fit linear model
abline(fit_thickness_age, col = "green")  # Plot line of best fit


########################
###Section 4: T-Test####
########################

##THICKNESS
# Subset data by gender
thickness_female <- melanoma$thickness[melanoma$sex == "Female"]
thickness_male <- melanoma$thickness[melanoma$sex == "Male"]
# Perform t-test
t_test_thickness_gender <- t.test(thickness_female, thickness_male)
# Print test results
print(t_test_thickness_gender)
# Create a boxplot with color-friendly palette
ggplot(melanoma, aes(x = sex, y = thickness, fill = sex)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Thickness by Gender", x = "Gender", y = "Thickness") +
  theme_minimal()

#AGE
# Subset data by gender
age_female <- melanoma$age[melanoma$sex == "Female"]
age_male <- melanoma$age[melanoma$sex == "Male"]
# Perform t-test
t_test_age_gender <- t.test(age_female, age_male)
# Print test results
print(t_test_age_gender)
ggplot(melanoma, aes(x = sex, y = age, fill = sex)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Thickness by Gender", x = "Gender", y = "Thickness") +
  theme_minimal()

#TIME

# Subset data by gender
time_female <- melanoma$time[melanoma$sex == "Female"]
time_male <- melanoma$time[melanoma$sex == "Male"]
# Perform t-test
t_test_time_gender <- t.test(time_female, time_male)
# Print test results
print(t_test_time_gender)
# Create a boxplot for Time by Gender
ggplot(melanoma, aes(x = sex, y = time, fill = sex)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Time by Gender", x = "Gender", y = "Time") +
  theme_minimal()

##QQ Plot for Thickness

# Set up the plotting environment with two plots side by side
par(mfrow = c(1, 2))
# Create QQ plot for 'thickness' by gender (Male)
qqnorm(male_thick_data$thickness,
       main = "QQ Plot for Thickness (Male)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col = viridis(1))  # Using the first color from the viridis palette
qqline(male_thick_data$thickness, col = viridis(2))  # Add QQ line
grid(col = "gray")  # Add gridlines
# Create QQ plot for 'thickness' by gender (Female)
qqnorm(female_thick_data$thickness,
       main = "QQ Plot for Thickness (Female)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col = viridis(3))  # Using the third color from the viridis palette
qqline(female_thick_data$thickness, col = viridis(4))  # Add QQ line
grid(col = "gray")  # Add gridlines
# Reset the plotting environment
par(mfrow = c(1, 1))

##QQ Plot for Age
# Set up the plotting environment with two plots side by side
par(mfrow = c(1, 2))
# Create QQ plot for 'age' by gender (Male)
qqnorm(male_age_data$age,
       main = "QQ Plot for Age (Male)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col = viridis(1))  # Using the first color from the viridis palette
qqline(male_age_data$age, col = viridis(2))  # Add QQ line
grid(col = "gray")  # Add gridlines
# Create QQ plot for 'age' by gender (Female)
qqnorm(female_age_data$age,
       main = "QQ Plot for Age (Female)",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles",
       col = viridis(3))  # Using the third color from the viridis palette
qqline(female_age_data$age, col = viridis(4))  # Add QQ line
grid(col = "gray")  # Add gridlines
# Reset the plotting environment
par(mfrow = c(1, 1))






