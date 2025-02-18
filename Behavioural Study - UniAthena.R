#####----------201486788-----------------#######
install.packages("readxl")  # Install the readxl package
library(readxl) 
# Import the entire Excel file
# Main Data File
data <- read_excel("Main File.xlsx", sheet = "data")
# Data file with just Values
new_data <- read_excel("Main File.xlsx", sheet = "new_data")

####-------------------- Reliability Analysis ---------------#### Section 4.1
# Load necessary libraries
library(psych)
library(knitr)
library(kableExtra)
library(dplyr)

# Calculate Cronbach's alpha for each scale and store the results
alpha_rb <- alpha(RB[, c("RB1", "RB2", "RB3", "RB4", "RB5")])
alpha_dp <- alpha(DPS[, c("DP1", "DP2", "DP3", "DP4", "DP5")])
alpha_ta <- alpha(TA[, c("TA1", "TA2", "TA3", "TA4", "TA5", "TA6")])
alpha_oc <- alpha(OC[, c("OC1", "OC2", "OC3", "OC4", "OC5", "OC6", 
                         "OC7", "OC8", "OC9", "OC10", "OC11")])

# Combine alpha values in a single data frame for printing
alpha_values <- data.frame(
  Scale = c("Risk Behaviour (RB)", "Decisional Procrastination (DP)", 
            "Task Avoidance (TA)", "Organizational Constraints (OC)"),
  Cronbach_Alpha = c(alpha_rb$total$raw_alpha, 
                     alpha_dp$total$raw_alpha, 
                     alpha_ta$total$raw_alpha, 
                     alpha_oc$total$raw_alpha))

# Create the table
kable(alpha_values, col.names = c("Scale", "Cronbach's Alpha"), 
      caption = "Cronbach's Alpha for Each Scale") %>%
  kable_styling(full_width = FALSE, 
                position = "center", 
                font_size = 20) %>%
  row_spec(0, bold = TRUE) %>%  # Bold the header
  column_spec(2, width = "5cm", bold = TRUE) %>%
  add_header_above(c(" " = 1, "Reliability Statistics" = 1)) %>%
  kableExtra::kable_styling(html_font = "Times New Roman") %>%  # Set font to Times New Roman   # For clear APA style
  footnote(general = "Cronbach's Alpha values above 0.70 indicate acceptable internal consistency.")


####----------- - DATA Screening Analysis----------------- - #### Section 4.2
##################### - Normality Testing - ################### Section 4.2.1
# Perform the Shapiro-Wilk normality test for each variable
shapiro_rb <- shapiro.test(data$RB)
shapiro_dp <- shapiro.test(data$DP)
shapiro_ta <- shapiro.test(data$TA)
shapiro_oc <- shapiro.test(data$OC)

#  data frame to hold the results
shapiro_results <- data.frame(
  Variable = c("Risk Behaviour (RB)", "Decisional Procrastination (DP)", 
               "Task Avoidance (TA)", "Organizational Constraints (OC)"),
  W_Statistic = round(c(shapiro_rb$statistic, shapiro_dp$statistic, shapiro_ta$statistic, shapiro_oc$statistic), 4),
  p_Value = round(c(shapiro_rb$p.value, shapiro_dp$p.value, shapiro_ta$p.value, shapiro_oc$p.value), 4)
)

# Format the table in APA style with Times New Roman font
library(knitr)
library(kableExtra)

# Print the table
shapiro_results %>%
  kable(format = "html", col.names = c("Variable", "W Statistic", "p-Value"), 
        caption = "Shapiro-Wilk Test Results") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
  row_spec(0, bold = T) %>%
  add_header_above(c("Shapiro-Wilk Test Results" = 3)) %>%
  kable_styling(html_font = "Times New Roman")


# QQ Plots
# Set up Times New Roman font for APA style
par(family = "serif")  # Use "serif", which is often mapped to Times New Roman

# Set up the layout for 2x2 plots and adjust margins for cleaner presentation
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Q-Q Plot for Risk Behaviour (RB) in APA style
qqnorm(data$RB, main = "Q-Q Plot for Risk Behaviour", 
       cex.main = 1.2,  # Larger title font
       cex.lab = 1,  # Standard label size
       cex.axis = 0.9)  # Smaller axis text
qqline(data$RB, col = "black", lwd = 2)  # Black line with thicker width

# Q-Q Plot for Decisional Procrastination (DPS)
qqnorm(data$DPS, main = "Q-Q Plot for Decisional Procrastination", 
       cex.main = 1.2, 
       cex.lab = 1, 
       cex.axis = 0.9)
qqline(data$DPS, col = "black", lwd = 2)

# Q-Q Plot for Task Avoidance (TA)
qqnorm(data$TA, main = "Q-Q Plot for Task Avoidance", 
       cex.main = 1.2, 
       cex.lab = 1, 
       cex.axis = 0.9)
qqline(data$TA, col = "black", lwd = 2)

# Q-Q Plot for Organizational Constraints (OC)
qqnorm(data$OC, main = "Q-Q Plot for Organizational Constraints", 
       cex.main = 1.2, 
       cex.lab = 1, 
       cex.axis = 0.9)
qqline(data$OC, col = "black", lwd = 2)

# Histograms for checking normality
par(family = "serif")  # "serif" is mapped to Times New Roman on most systems

# Set up the plotting area for 2 rows and 2 columns
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Adjust margins for cleaner plots

# Plot the histogram for Risk Behaviour (RB) in APA style
hist(data$RB, 
     main = "Histogram for Risk Behaviour", 
     breaks = 10, 
     col = "gray90",  # Subtle color
     border = "black",  # Black borders for bars
     xlab = "Risk Behaviour", 
     ylab = "Frequency", 
     cex.main = 1.2,  # APA-style larger title
     cex.lab = 1,  # Labels slightly smaller
     cex.axis = 0.9)  # Axis numbers smaller

# Plot the histogram for Decisional Procrastination (DPS)
hist(data$DPS, 
     main = "Histogram for Decisional Procrastination", 
     breaks = 10, 
     col = "gray90",  # Subtle color
     border = "black", 
     xlab = "Decisional Procrastination", 
     ylab = "Frequency", 
     cex.main = 1.2, 
     cex.lab = 1, 
     cex.axis = 0.9)

# Plot the histogram for Task Avoidance (TA)
hist(data$TA, 
     main = "Histogram for Task Avoidance", 
     breaks = 10, 
     col = "gray90", 
     border = "black", 
     xlab = "Task Avoidance", 
     ylab = "Frequency", 
     cex.main = 1.2, 
     cex.lab = 1, 
     cex.axis = 0.9)

# Plot the histogram for Organizational Constraints (OC)
hist(data$OC, 
     main = "Histogram for Organizational Constraints", 
     breaks = 10, 
     col = "gray90", 
     border = "black", 
     xlab = "Organizational Constraints", 
     ylab = "Frequency", 
     cex.main = 1.2, 
     cex.lab = 1, 
     cex.axis = 0.9)

# Reset the plotting area and font to default (optional)
par(mfrow = c(1, 1)) 
par(family = "default")  # Reset font

# Reset font after plotting if needed
par(family = "default")  # Optional: reset back to default font family


####----------- - Multicollinearity - -----------------#### Section 4.2.2

library(car)
# Fit a model with only direct predictors
model_direct <- lm(DP ~ RB + OC + TA, data = data)  # Adjust this to include relevant direct predictors

# Calculate VIF values for the direct predictors model
vif_values_direct <- vif(model_direct)
print(vif_values_direct)

# Fit the model with interaction term
model_dp <- lm(DP ~ RB * OC, data = data)
# Calculate VIF
vif_values_dp <- vif(model_dp)
print(vif_values_dp)

# Fit the model with interaction term
model_ta <- lm(TA ~ RB * OC, data = data)
# Calculate VIF
vif_values_ta <- vif(model_ta)
print(vif_values_ta)
pairs(data[, c("RB", "DP", "TA", "OC")])

# Mean of Variables
mean_RB <- mean(data$RB, na.rm = TRUE)
mean_OC <- mean(data$OC, na.rm = TRUE)
mean_DP <- mean(data$DP, na.rm = TRUE)  # If needed for other models
mean_TA <- mean(data$TA, na.rm = TRUE)    # If needed for other models
# Center the predictors by subtracting their mean
data$RB_centered <- data$RB - mean_RB
data$OC_centered <- data$OC - mean_OC
data$DP_centered <- data$DP - mean_DP
data$TA_centered <- data$TA - mean_TA
# Create interaction terms for centered variables
data$RB_OC_interaction <- data$RB_centered * data$OC_centered
model_dp_centered <- lm(DP_centered ~ RB_centered * OC_centered, data = data)
summary(model_dp_centered)
model_ta_centered <- lm(TA_centered ~ RB_centered * OC_centered, data = data)
summary(model_ta_centered)
# Load the car package if not already loaded
library(car)

# Calculate VIF for the centered model with interaction
vif_dp_centered <- vif(model_dp_centered)
print(vif_dp_centered)

vif_ta_centered <- vif(model_ta_centered)
print(vif_ta_centered)


####------------------ - Outliers - ------------------#### Section 4.2.3
# Function to detect outliers using Z-scores
detect_outliers_z <- function(data, variable) {
  # Calculate Z-scores
  z_scores <- scale(data[[variable]])
  
  # Identify outliers (Z-score > 3 or Z-score < -3)
  outliers <- data %>%
    filter(abs(z_scores) > 3)
  
  return(outliers)
}

# Example usage for a variable named 'TA'
outliers_TA_z <- detect_outliers_z(data, 'TA')
print(outliers_TA_z)

outliers_RB_z <- detect_outliers_z(data, 'RB')
print(outliers_RB_z)

outliers_DP_z <- detect_outliers_z(data, 'DP')
print(outliers_DP_z)

outliers_OC_z <- detect_outliers_z(data, 'OC')
print(outliers_OC_z)
# Extract and print outlier details for each variable

# Risk Behaviour
rb_outliers <- boxplot.stats(data$RB)$out
cat("Outliers for Risk Behaviour:\n")
print(rb_outliers)

# Decisional Procrastination
dp_outliers <- boxplot.stats(data$DP)$out
cat("Outliers for Decisional Procrastination:\n")
print(dp_outliers)

# Task Avoidance
ta_outliers <- boxplot.stats(data$TA)$out
cat("Outliers for Task Avoidance:\n")
print(ta_outliers)

# Organizational Constraints
oc_outliers <- boxplot.stats(data$OC)$out
cat("Outliers for Organizational Constraints:\n")
print(oc_outliers)

# Identify and display detailed information about outliers for each variable

# Risk Behaviour
rb_outliers_index <- which(data$RB %in% rb_outliers)
cat("Detailed information for outliers in Risk Behaviour:\n")
print(data[rb_outliers_index, ])

# Decisional Procrastination
dp_outliers_index <- which(data$DP %in% dp_outliers)
cat("Detailed information for outliers in Decisional Procrastination:\n")
print(data[dp_outliers_index, ])

# Task Avoidance
ta_outliers_index <- which(data$TA %in% ta_outliers)
cat("Detailed information for outliers in Task Avoidance:\n")
print(data[ta_outliers_index, ])

# Organizational Constraints
oc_outliers_index <- which(data$OC %in% oc_outliers)
cat("Detailed information for outliers in Organizational Constraints:\n")
print(data[oc_outliers_index, ])


#### ------------- Preliminary Analysis------------ - #### Section 4.3
# Load necessary library
library(psych)  # Provides the describe() function for descriptive statistics

# Calculate descriptive statistics for continuous variables
continuous_vars <- data[, c("TA", "OC", "DP", "RB")]
descriptive_stats_cont <- describe(continuous_vars)
print(descriptive_stats_cont)

# Convert to a dataframe for easier manipulation
descriptive_stats_df <- as.data.frame(descriptive_stats_cont)

# Print the table using kable
kable(descriptive_stats_df, caption = "Descriptive Statistics for TA, OC, DP, and RB") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Calculate descriptive statistics for categorical variables
# Calculate frequency distribution for the Gender variable
gender_freq <- table(data$Gender)
gender_prop <- prop.table(gender_freq)

# Combine frequency and proportion into a data frame
gender_stats <- data.frame(Frequency = gender_freq, Proportion = gender_prop)
print(gender_stats)


# Calculate frequency distribution for the Gender variable
Age_freq <- table(data$Age)
Age_prop <- prop.table(Age_freq)

# Combine frequency and proportion into a data frame
Age_stats <- data.frame(Frequency = Age_freq, Proportion = Age_prop)
print(Age_stats)

# Calculate frequency distribution for the Gender variable
Exp_freq <- table(data$Experience)
Exp_prop <- prop.table(Exp_freq)

# Combine frequency and proportion into a data frame
Exp_stats <- data.frame(Frequency = Exp_freq, Proportion = Exp_prop)
print(Exp_stats)

# Calculate frequency distribution for the Gender variable
Job_freq <- table(data$`Job Level`)
Job_prop <- prop.table(Job_freq)

# Combine frequency and proportion into a data frame
Job_stats <- data.frame(Frequency = Job_freq, Proportion = Job_prop)
print(Job_stats)


# Combine continuous and categorical summaries into a list 
summary_list <- list(
  Continuous_Variables = descriptive_stats_cont,
  Gender_Distribution = gender_stats,
  Age_Distribution = Age_stats,
  Job_Distribution = Job_stats,
  Exp_Distribution = Exp_stats
)

# Print the combined summary
print(summary_list)

#### - Hypothesis Testing - #### Section 4.4
# Load necessary library
library(psych)

# Hypothesis 1: Decisional Procrastination and Task Avoidance are positively correlated
correlation_1 <- cor.test(data$DP, data$TA, method = "spearman")
print(correlation_1)

# Hypothesis 2: Risk Perception and Task Avoidance are positively correlated
correlation_2 <- cor.test(data$RB, data$TA, method = "spearman")
print(correlation_2)

# Hypothesis 3: Risk Perception and Decisional Procrastination are positively correlated
correlation_3 <- cor.test(data$RB, data$DP, method = "spearman")
print(correlation_3)

# Centering the variables
data$RB_centered <- scale(data$RB, center = TRUE, scale = FALSE)
data$OC_centered <- scale(data$OC, center = TRUE, scale = FALSE)
data$DP_centered <- scale(data$DP, center = TRUE, scale = FALSE)
data$TA_centered <- scale(data$TA, center = TRUE, scale = FALSE)

# Interaction term for Hypothesis 4
data$RB_OC_interaction_1 <- data$RB_centered * data$OC_centered

# Fit the moderation model
model_h4 <- rlm(DP_centered ~ RB_centered * OC_centered, data = data)
summary(model_h4)

# Check interaction term significance

# Interaction term for Hypothesis 5
data$RB_OC_interaction_2 <- data$RB_centered * data$OC_centered

# Fit the moderation model
model_h5 <- rlm(TA_centered ~ RB_centered * OC_centered, data = data)
summary(model_h5)

# Load necessary libraries
library(knitr)
library(kableExtra)

# Create a data frame for Decisional Procrastination (DP) model summary
dp_summary <- data.frame(
  Term = c("Intercept", "RB_centered", "OC_centered", "RB_centered × OC_centered"),
  B = c(-0.093, 0.390, 0.354, 0.138),
  SE = c(0.077, 0.091, 0.096, 0.089),
  t = c(-1.21, 4.27, 3.68, 1.55),
  p = c(NA, "< .001", "< .001", ".13")
)

# Print Decisional Procrastination (DP) model summary in APA format
dp_summary %>%
  kable(col.names = c("Term", "B", "SE", "t", "p"), format = "html", digits = 3, caption = "Regression Summary for Decisional Procrastination (DP)") %>%
  kable_styling(full_width = F) %>%
  row_spec(0, bold = TRUE)

# Create a data frame for Task Avoidance (TA) model summary
ta_summary <- data.frame(
  Term = c("Intercept", "RB_centered", "OC_centered", "RB_centered × OC_centered"),
  B = c(-0.056, 0.254, 0.067, 0.075),
  SE = c(0.082, 0.098, 0.103, 0.095),
  t = c(-0.68, 2.59, 0.65, 0.79),
  p = c(NA, "< .05", ".52", ".43")
)

# Print Task Avoidance (TA) model summary in APA format
ta_summary %>%
  kable(col.names = c("Term", "B", "SE", "t", "p"), format = "html", digits = 3, caption = "Regression Summary for Task Avoidance (TA)") %>%
  kable_styling(full_width = F) %>%
  row_spec(0, bold = TRUE)



#### - Correlation Analysis - #### Section 4.5.1 - Additional Analysis
# Install packages if not already installed
install.packages("corrplot")
install.packages("gt")

# Load libraries
library(corrplot)
library(gt)
library(ggplot2)
library(MASS)
library(gridExtra)  # For combining multiple plots
library(corrplot)
library(knitr)
library(kableExtra)

# Calculate the Spearman correlation matrix for non-parametric data
spearman_corr_matrix <- cor(new_data, use = "complete.obs", method = "spearman")


# Set the font to Times New Roman
par(family = "serif")  # "serif" is mapped to Times New Roman on most systems

# Generate the Spearman correlation matrix (for non-parametric data)
corr_matrix <- cor(new_data, use = "complete.obs", method = "spearman")

# Create a corrplot with Times New Roman font and minimal APA style elements
corrplot(corr_matrix, 
         method = "color",  # Use colors to indicate strength
         col = colorRampPalette(c("white", "white", "grey"))(200),  # Subtle color palette
         type = "upper",  # Display only the upper triangle
         addCoef.col = "black",  # Correlation coefficients in black for visibility
         tl.col = "black",  # Text label color
         tl.srt = 45,  # Rotate text labels for readability
         diag = FALSE,  # Remove the diagonal to focus on correlations
         cl.pos = "n",  # Remove the color legend (optional)
         number.cex = 0.8,  # Adjust size of correlation values
         main = "Spearman Correlation Matrix (APA Style)",  # Title
         mar = c(0, 0, 2, 0),  # Minimal margins
         bg = "white",  # Background color set to white
         outline = "black"  # Outline color for the matrix
)

# Reset font after plotting if needed
par(family = "default")  # Optional: reset back to default font family


# Install necessary packages if not already installed
install.packages("kableExtra")
install.packages("knitr")

# Load the necessary libraries

library(knitr)
library(kableExtra)

# Generate a Spearman correlation matrix (for non-parametric data)
spearman_corr_matrix <- cor(new_data, use = "complete.obs", method = "spearman")
spearman_corr_df <- as.data.frame(spearman_corr_matrix)

# Create the APA-style table with minimal horizontal lines
apa_style_table <- kable(spearman_corr_df, 
                         format = "html", 
                         caption = "Spearman Correlation Matrix of Continuous Variables", 
                         align = "c") %>%
  kable_styling(bootstrap_options = c("condensed", "responsive"),  
                full_width = F, 
                position = "center") %>%
  row_spec(0, bold = T, font_size = 12) %>%  # Bold header row
  row_spec(1:nrow(spearman_corr_df), font_size = 11) %>%  # Data rows font size
  column_spec(1, bold = T) %>%  # First column bold (variable names)
  kable_classic(full_width = F, 
                html_font = "Times New Roman") %>%  # Set font to Times New Roman
  # Remove all unnecessary lines and only keep the top, bottom, and header separator lines
  kable_styling(latex_options = c("hold_position", "scale_down", "striped"), 
                position = "center", 
                full_width = F) %>%
  # Add lines only above the header and below the last row
  add_header_above(c(" " = 1, "Correlation Matrix" = ncol(spearman_corr_df)), bold = TRUE) %>%
  # Remove all borders except top, bottom, and above the header
  kable_styling(latex_options = c("hold_position", "striped"), 
                position = "center", 
                full_width = F, 
                latex_hline = c("top", "bottom", "header"))

# Print the table
print(apa_style_table)


#### - Categorical Variables - #### Section 4.5.x - Additional Analysis
# Run the Wilcoxon test again with exact p-value calculation - GENDER
wilcox_test_ta_gender_exact <- wilcox.test(TA ~ Gender, data = data, exact = TRUE)
print(wilcox_test_ta_gender_exact)

wilcox_test_DP_gender_exact <- wilcox.test(DP ~ Gender, data = data, exact = TRUE)
print(wilcox_test_DP_gender_exact)

wilcox_test_RB_gender_exact <- wilcox.test(RB ~ Gender, data = data, exact = TRUE)
print(wilcox_test_RB_gender_exact)

wilcox_test_OC_gender_exact <- wilcox.test(OC ~ Gender, data = data, exact = TRUE)
print(wilcox_test_OC_gender_exact)

# Effect of Job.Level on Task Avoidance (TA)
kruskal_test_ta_job <- kruskal.test(TA ~ Job.Level, data = data)
print(kruskal_test_ta_job)

# Effect of Job.Level on Decisional Procrastination (DP)
kruskal_test_DP_job <- kruskal.test(DP ~ Job.Level, data = data)
print(kruskal_test_DP_job)

# Effect of Job.Level on Risk Perception (RB)
kruskal_test_rb_job <- kruskal.test(RB ~ Job.Level, data = data)
print(kruskal_test_rb_job)

# Effect of Job.Level on Organizational Constraints (OC)
kruskal_test_oc_job <- kruskal.test(OC ~ Job.Level, data = data)
print(kruskal_test_oc_job)

# Effect of Experience on Task Avoidance (TA)
kruskal_test_ta_exp <- kruskal.test(TA ~ Experience, data = data)
print(kruskal_test_ta_exp)

# Effect of Experience on Decisional Procrastination (DP)
kruskal_test_DP_exp <- kruskal.test(DP ~ Experience, data = data)
print(kruskal_test_DP_exp)

# Effect of Experience on Risk Perception (RB)
kruskal_test_rb_exp <- kruskal.test(RB ~ Experience, data = data)
print(kruskal_test_rb_exp)

# Effect of Experience on Organizational Constraints (OC)
kruskal_test_oc_exp <- kruskal.test(OC ~ Experience, data = data)
print(kruskal_test_oc_exp)

# Effect of Age on Task Avoidance (TA)
kruskal_test_ta_age <- kruskal.test(TA ~ Age, data = data)
print(kruskal_test_ta_age)

# Effect of Age on Decisional Procrastination (DP)
kruskal_test_DP_age <- kruskal.test(DP ~ Age, data = data)
print(kruskal_test_DP_age)

# Effect of Age on Risk Perception (RB)
kruskal_test_rb_age <- kruskal.test(RB ~ Age, data = data)
print(kruskal_test_rb_age)

# Effect of Age on Organizational Constraints (OC)
kruskal_test_oc_age <- kruskal.test(OC ~ Age, data = data)
print(kruskal_test_oc_age)


#### - Additional Insights on Categorical Variables - #### Section 4.5.x - Additional Analysis
# Calculate descriptive statistics by Gender
gender_stats <- data %>%
  group_by(Gender) %>%
  summarise(
    TA_median = median(TA),
    TA_IQR = IQR(TA),
    DP_median = median(DP),
    DP_IQR = IQR(DP),
    RB_median = median(RB),
    RB_IQR = IQR(RB),
    OC_median = median(OC),
    OC_IQR = IQR(OC)
  )

print(gender_stats)

# Calculate descriptive statistics by Gender
Job_stats <- data %>%
  group_by(Job.Level) %>%
  summarise(
    TA_median = median(TA),
    TA_IQR = IQR(TA),
    DP_median = median(DP),
    DP_IQR = IQR(DP),
    RB_median = median(RB),
    RB_IQR = IQR(RB),
    OC_median = median(OC),
    OC_IQR = IQR(OC)
  )

print(Job_stats)

# Calculate descriptive statistics by Experience
Exp_stats <- data %>%
  group_by(Experience) %>%
  summarise(
    TA_median = median(TA),
    TA_IQR = IQR(TA),
    DP_median = median(DP),
    DP_IQR = IQR(DP),
    RB_median = median(RB),
    RB_IQR = IQR(RB),
    OC_median = median(OC),
    OC_IQR = IQR(OC)
  )

print(Exp_stats)

# Calculate descriptive statistics by Age
Age_stats <- data %>%
  group_by(Age) %>%
  summarise(
    TA_median = median(TA),
    TA_IQR = IQR(TA),
    DP_median = median(DP),
    DP_IQR = IQR(DP),
    RB_median = median(RB),
    RB_IQR = IQR(RB),
    OC_median = median(OC),
    OC_IQR = IQR(OC)
  )
#-----------Combined Frequency Table-----------#
# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)

# Function to calculate frequency of categorical variables
calculate_frequency <- function(data, variable) {
  data %>%
    count({{ variable }}) %>%
    rename(Category = {{ variable }}, Frequency = n) %>%
    mutate(Proportion = Frequency / sum(Frequency)) %>%
    arrange(desc(Frequency))
}

# Calculate frequencies for each categorical variable
gender_freq <- calculate_frequency(data, Gender)
job_level_freq <- calculate_frequency(data, Job.Level)
experience_freq <- calculate_frequency(data, Experience)
age_freq <- calculate_frequency(data, Age)

# Combine all frequency tables into one data frame
combined_freq <- bind_rows(
  gender_freq %>% mutate(Variable = "Gender"),
  job_level_freq %>% mutate(Variable = "Job Level"),
  experience_freq %>% mutate(Variable = "Experience"),
  age_freq %>% mutate(Variable = "Age")
)

# Create a gt table
combined_table <- combined_freq %>%
  gt() %>%
  tab_header(
    title = "Frequency and Proportion of Categorical Variables"
  ) %>%
  fmt_number(
    columns = vars(Frequency, Proportion),
    decimals = 2
  ) %>%
  cols_label(
    Category = "Category",
    Frequency = "Frequency",
    Proportion = "Proportion",
    Variable = "Variable"
  ) %>%
  fmt_number(
    columns = vars(Proportion),
    scale = 100,
    suffixing = TRUE
  )

# Print the gt table
print(combined_table)

print(Age_stats)

#### - Surprising Discovery - #### Section 4.5.x - Additional Analysis


#### - Correlation Analysis - #### Section 4.5.1 - Additional Analysis
# Install packages if not already installed
install.packages("corrplot")
install.packages("gt")

# Load libraries
library(corrplot)
library(gt)

# Load the necessary libraries
library(corrplot)
library(knitr)
library(kableExtra)

# Calculate the Spearman correlation matrix for non-parametric data
spearman_corr_matrix <- cor(new_data, use = "complete.obs", method = "spearman")

# Load library
library(corrplot)

# Set the font to Times New Roman
par(family = "serif")  # "serif" is mapped to Times New Roman on most systems

# Generate the Spearman correlation matrix (for non-parametric data)
corr_matrix <- cor(new_data, use = "complete.obs", method = "spearman")

# Create a corrplot with Times New Roman font and minimal APA style elements
corrplot(corr_matrix, 
         method = "color",  # Use colors to indicate strength
         col = colorRampPalette(c("white", "white", "grey"))(200),  # Subtle color palette
         type = "upper",  # Display only the upper triangle
         addCoef.col = "black",  # Correlation coefficients in black for visibility
         tl.col = "black",  # Text label color
         tl.srt = 45,  # Rotate text labels for readability
         diag = FALSE,  # Remove the diagonal to focus on correlations
         cl.pos = "n",  # Remove the color legend (optional)
         number.cex = 0.8,  # Adjust size of correlation values
         main = "Spearman Correlation Matrix (APA Style)",  # Title
         mar = c(0, 0, 2, 0),  # Minimal margins
         bg = "white",  # Background color set to white
         outline = "black"  # Outline color for the matrix
)
# Install necessary packages if not already installed
install.packages("kableExtra")
install.packages("knitr")

# Load the necessary libraries
library(knitr)
library(kableExtra)
library(dplyr)

# Generate a Spearman correlation matrix (for non-parametric data)
spearman_corr_matrix <- cor(new_data, use = "complete.obs", method = "spearman")
spearman_corr_df <- as.data.frame(spearman_corr_matrix)

# Create the APA-style table with minimal horizontal lines
correlation_table <- kable(spearman_corr_df, 
                         format = "html", 
                         caption = "Spearman Correlation Matrix of Continuous Variables", 
                         align = "c") %>%
  kable_styling(bootstrap_options = c("condensed", "responsive"),  
                full_width = F, 
                position = "center") %>%
  row_spec(0, bold = T, font_size = 12) %>%  # Bold header row
  row_spec(1:nrow(spearman_corr_df), font_size = 11) %>%  # Data rows font size
  column_spec(1, bold = T) %>%  # First column bold (variable names)
  kable_classic(full_width = F, 
                html_font = "Times New Roman") %>%  # Set font to Times New Roman
  # Remove all unnecessary lines and only keep the top, bottom, and header separator lines
  kable_styling(latex_options = c("hold_position", "scale_down", "striped"), 
                position = "center", 
                full_width = F) %>%
  # Add lines only above the header and below the last row
  add_header_above(c(" " = 1, "Correlation Matrix" = ncol(spearman_corr_df)), bold = TRUE) 

# Print the table
print(correlation_table)


# Load necessary libraries
library(ggplot2)
library(MASS)
library(gridExtra)  # For combining multiple plots

# Set base theme for APA style
base_theme <- theme_minimal() +
  theme(text = element_text(family = "serif"),  # Times New Roman
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Title centered and bold
        plot.caption = element_text(hjust = 0.5),  # Center caption
        axis.title = element_text(size = 12),  # Axis titles
        axis.text = element_text(size = 10),  # Axis text
        panel.grid = element_blank())  # Remove gridlines for a clean look

# Scatter plot of TA vs. DP with regression line
plot1 <- ggplot(data, aes(x = TA, y = DP)) +
  geom_point() +
  geom_smooth(method = "rlm", se = FALSE, color = "blue") +
  labs(title = "TA vs. DP with Regression Line", x = "Task Avoidance (TA)", y = "Decisional Procrastination (DP)", 
       caption = "Hypothesis 1") +
  base_theme

# Scatter plot of RB vs. TA with regression line
plot2 <- ggplot(data, aes(x = RB, y = TA)) +
  geom_point() +
  geom_smooth(method = "rlm", se = FALSE, color = "blue") +
  labs(title = "RB vs. TA with Regression Line", x = "Risk Behaviour (RB)", y = "Task Avoidance (TA)", 
       caption = "Hypothesis 2") +
  base_theme

# Scatter plot of RB vs. DP with regression line
plot3 <- ggplot(data, aes(x = RB, y = DP)) +
  geom_point() +
  geom_smooth(method = "rlm", se = FALSE, color = "blue") +
  labs(title = "RB vs. DP with Regression Line", x = "Risk Behaviour (RB)", y = "Decisional Procrastination (DP)", 
       caption = "Hypothesis 3") +
  base_theme

# Combine all three plots into one figure (2 columns)
grid.arrange(plot1, plot2, plot3, ncol = 2)

# Optional: reset default theme if needed
# Load necessary libraries
library(ggplot2)
library(MASS)
library(gridExtra)  # For combining multiple plots

# Set base theme for APA style
base_theme <- theme_minimal() +
  theme(text = element_text(family = "serif"),  # Times New Roman
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Title centered and bold
        plot.caption = element_text(hjust = 0.5),  # Center caption
        axis.title = element_text(size = 12),  # Axis titles
        axis.text = element_text(size = 10),  # Axis text
        panel.grid = element_blank())  # Remove gridlines for a clean look

# Scatter plot of OC vs. DP with regression line
plot4 <- ggplot(data, aes(x = OC, y = DP)) +
  geom_point() +
  geom_smooth(method = "rlm", se = FALSE, color = "orange") +
  labs(title = "OC vs. DP with Regression Line", 
       x = "Organizational Constraints (OC)", 
       y = "Decisional Procrastination (DP)", 
       caption = "Surprising Discovery 1") +
  base_theme

# Scatter plot of OC vs. TA with regression line
plot5 <- ggplot(data, aes(x = OC, y = TA)) +
  geom_point() +
  geom_smooth(method = "rlm", se = FALSE, color = "orange") +
  labs(title = "OC vs. TA with Regression Line", 
       x = "Organizational Constraints (OC)", 
       y = "Task Avoidance (TA)", 
       caption = "Surprising Discovery 2") +
  base_theme

# Scatter plot of OC vs. RB with regression line
plot6 <- ggplot(data, aes(x = OC, y = RB)) +
  geom_point() +
  geom_smooth(method = "rlm", se = FALSE, color = "orange") +
  labs(title = "OC vs. RB with Regression Line", 
       x = "Organizational Constraints (OC)", 
       y = "Risk Behaviour (RB)", 
       caption = "Surprising Discovery 3") +
  base_theme

# Combine all three plots into one figure (2 columns)
grid.arrange(plot4, plot5, plot6, ncol = 2)

# Optional: reset default theme if needed

# Load necessary libraries
library(psych)

# Create a scatterplot matrix using the pairs.panels function from the psych package
# Ensure data is loaded into 'data'
pairs.panels(data[, c("DP", "RB", "TA", "OC")], 
             method = "spearman",   # Correlation method (Pearson correlation)
             hist.col = "lightblue",  # Histogram color
             density = TRUE,  # Add density plots
             ellipses = FALSE,  # Disable confidence ellipses
             smooth = FALSE,  # Disable smoothing
             lm = TRUE,  # Add regression lines
             main = "Scatterplot Matrix",
             cex.main = 1.5,  # Main title font size
             font.main = 2,  # Bold main title
             font.labels = 2,  # Bold variable names
             cex.axis = 0.8,  # Axis font size
             cex.labels = 1.2,  # Labels font size
             pch = 21,  # Set point type to filled circles
             bg = "lightgray",  # Background color for points
             cex = 0.8,  # Point size
             col = "black")  # Point color

# Optional: Reset graphic parameters to default after plotting
par(mfrow = c(1, 1))

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Create consistent theme for APA-style formatting
apa_theme <- theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),  # Centered and bold title
        axis.title = element_text(size = 10),  # Axis title font size
        axis.text = element_text(size = 10),  # Axis label font size
        panel.grid.major = element_line(color = "grey80"),  # Light grid lines
        panel.grid.minor = element_blank(),  # No minor grid lines
        legend.position = "none"  # No legend for APA style
  )
###### BOXPLOTS - GENDER #####
# Boxplot for Task Avoidance (TA) by Gender
plot1 <- ggplot(data, aes(x = Gender, y = TA, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title = "Task Avoidance by Gender", y = "Task Avoidance", x = "Gender") +
  apa_theme

# Boxplot for Decisional Procrastination (DP) by Gender
plot2 <- ggplot(data, aes(x = Gender, y = DP, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title = "Decisional Procrastination by Gender", y = "Decisional Procrastination", x = "Gender") +
  apa_theme

# Boxplot for Risk Behaviour (RB) by Gender
plot3 <- ggplot(data, aes(x = Gender, y = RB, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title = "Risk Behaviour by Gender", y = "Risk Behaviour", x = "Gender") +
  apa_theme

# Boxplot for Organizational Constraints (OC) by Gender
plot4 <- ggplot(data, aes(x = Gender, y = OC, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey")) +
  labs(title = "Organizational Constraints by Gender", y = "Organizational Constraints", x = "Gender") +
  apa_theme

# Combine all four plots in a grid layout (2 rows, 2 columns)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)


# Load necessary library
library(ggplot2)

# Create consistent APA-style theme
apa_theme <- theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),  # Centered and bold title
        axis.title = element_text(size = 10),  # Axis title font size
        axis.text = element_text(size = 10),  # Axis label font size
        panel.grid.major = element_line(color = "grey80"),  # Light grey grid lines
        panel.grid.minor = element_blank(),  # No minor grid lines
        panel.background = element_rect(fill = "white"),  # White background for the plot
        legend.position = "none"  # No legend
  )

# Filter out the 'Other' category from Job Level
data_filtered <- data[data$Job.Level != "Other", ]

### Job Level Boxplots
# Boxplot for Task Avoidance (TA) by Job Level (excluding 'Other')
plot_job_ta <- ggplot(data_filtered, aes(x =Job.Level, y = TA, fill = Job.Level)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Task Avoidance by Job Level", y = "Task Avoidance", x = "Job Level") +
  apa_theme

# Boxplot for Decisional Procrastination (DP) by Job Level (excluding 'Other')
plot_job_DP <- ggplot(data_filtered, aes(x = Job.Level, y = DP, fill = Job.Level)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Decisional Procrastination by Job Level", y = "Decisional Procrastination", x = "Job Level") +
  apa_theme

# Boxplot for Risk Behaviour (RB) by Job Level (excluding 'Other')
plot_job_rb <- ggplot(data_filtered, aes(x = Job.Level, y = RB, fill = Job.Level)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Risk Behaviour by Job Level", y = "Risk Behaviour", x = "Job Level") +
  apa_theme

# Boxplot for Organizational Constraints (OC) by Job Level (excluding 'Other')
plot_job_oc <- ggplot(data_filtered, aes(x = Job.Level, y = OC, fill = Job.Level)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Organizational Constraints by Job Level", y = "Organizational Constraints", x = "Job Level") +
  apa_theme

# Arrange Job Level plots in a grid
grid.arrange(plot_job_ta, plot_job_DP, plot_job_rb, plot_job_oc, ncol = 2)


# Use grid.arrange to plot all four boxplots together
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Load necessary library
library(ggplot2)

# APA theme
apa_theme <- theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),  # Centered and bold title
        axis.title = element_text(size = 10),  # Axis title font size
        axis.text = element_text(size = 10),  # Axis label font size
        panel.grid.major = element_line(color = "grey80"),  # Light grey grid lines
        panel.grid.minor = element_blank(),  # No minor grid lines
        panel.background = element_rect(fill = "white"),  # White background for the plot
        legend.position = "none"  # No legend
  )

### Age Boxplots
# Boxplot for Task Avoidance (TA) by Age
plot_age_ta <- ggplot(data, aes(x = Age, y = TA, fill = Age)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Task Avoidance by Age", y = "Task Avoidance", x = "Age") +
  apa_theme

# Boxplot for Decisional Procrastination (DP) by Age
plot_age_DP <- ggplot(data, aes(x = Age, y = DP, fill = Age)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Decisional Procrastination by Age", y = "Decisional Procrastination", x = "Age") +
  apa_theme

# Boxplot for Risk Behaviour (RB) by Age
plot_age_rb <- ggplot(data, aes(x = Age, y = RB, fill = Age)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Risk Behaviour by Age", y = "Risk Behaviour", x = "Age") +
  apa_theme

# Boxplot for Organizational Constraints (OC) by Age
plot_age_oc <- ggplot(data, aes(x = Age, y = OC, fill = Age)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Organizational Constraints by Age", y = "Organizational Constraints", x = "Age") +
  apa_theme

# Arrange Age plots in a grid
grid.arrange(plot_age_ta, plot_age_DP, plot_age_rb, plot_age_oc, ncol = 2)


### Experience Boxplots
# Boxplot for Task Avoidance (TA) by Experience
plot_exp_ta <- ggplot(data, aes(x = Experience, y = TA, fill = Experience)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Task Avoidance by Experience", y = "Task Avoidance", x = "Experience") +
  apa_theme

# Boxplot for Decisional Procrastination (DP) by Experience
plot_exp_DP <- ggplot(data, aes(x = Experience, y = DP, fill = Experience)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Decisional Procrastination by Experience", y = "Decisional Procrastination", x = "Experience") +
  apa_theme

# Boxplot for Risk Behaviour (RB) by Experience
plot_exp_rb <- ggplot(data, aes(x = Experience, y = RB, fill = Experience)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Risk Behaviour by Experience", y = "Risk Behaviour", x = "Experience") +
  apa_theme

# Boxplot for Organizational Constraints (OC) by Experience
plot_exp_oc <- ggplot(data, aes(x = Experience, y = OC, fill = Experience)) +
  geom_boxplot(outlier.color = "red") +
  scale_fill_manual(values = c("white", "grey90", "grey70", "grey50", "grey30")) +
  labs(title = "Organizational Constraints by Experience", y = "Organizational Constraints", x = "Experience") +
  apa_theme

# Arrange Experience plots in a grid
grid.arrange(plot_exp_ta, plot_exp_DP, plot_exp_rb, plot_exp_oc, ncol = 2)


# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Set APA style theme (minimal with clear labels)
apa_theme <- theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Gender Frequency Plot
plot_gender <- ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "grey70", color = "black") +
  labs(title = "Gender Frequency", y = "Frequency", x = "Gender") +
  apa_theme

# Age Frequency Plot
plot_age <- ggplot(data, aes(x = Age)) +
  geom_bar(fill = "grey70", color = "black") +
  labs(title = "Age Frequency", y = "Frequency", x = "Age") +
  apa_theme

# Experience Frequency Plot
plot_experience <- ggplot(data, aes(x = Experience)) +
  geom_bar(fill = "grey70", color = "black") +
  labs(title = "Experience Frequency", y = "Frequency", x = "Experience") +
  apa_theme

# Job Level Frequency Plot (with 'Other' filtered out)
data_filtered <- data[data$Job.Level != "Other", ]

plot_job_level <- ggplot(data_filtered, aes(x = Job.Level)) +
  geom_bar(fill = "grey70", color = "black") +
  labs(title = "Job Level Frequency", y = "Frequency", x = "Job Level") +
  apa_theme

# Arrange the plots into a grid
grid.arrange(plot_gender, plot_age, ncol = 2)
grid.arrange(plot_experience, plot_job_level, ncol = 2)

# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Set APA style theme (minimal with clear labels and Times New Roman font)
apa_theme <- theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# Boxplot for Task Avoidance (TA)
plot_ta <- ggplot(data, aes(x = "", y = TA)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(title = "Task Avoidance (TA)", x = "", y = "TA") +
  apa_theme

# Boxplot for Risk Behaviour (RB)
plot_rb <- ggplot(data, aes(x = "", y = RB)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(title = "Risk Behaviour (RB)", x = "", y = "RB") +
  apa_theme

# Boxplot for Decisional Procrastination (DP)
plot_dp <- ggplot(data, aes(x = "", y = DP)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(title = "Decisional Procrastination (DP)", x = "", y = "DP") +
  apa_theme

# Boxplot for Organizational Constraints (OC)
plot_oc <- ggplot(data, aes(x = "", y = OC)) +
  geom_boxplot(fill = "lightgray", color = "black") +
  labs(title = "Organizational Constraints (OC)", x = "", y = "OC") +
  apa_theme

# Arrange all four plots into one figure
grid.arrange(plot_ta, plot_rb, plot_dp, plot_oc, ncol = 2)
