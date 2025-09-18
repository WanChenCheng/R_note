# Load the data
library(readr)
ecommerce <- read_csv("C:/Users/Ava/Desktop/R/HW6/ecommerce.csv")
View(ecommerce)

# Split the data 
control_data <- ecommerce[ecommerce$group == "control", ]
treatment_data <- ecommerce[ecommerce$group == "treatment", ]

# Make a function
analyze_ab_test <- function(control_data, treatment_data, alpha = 0.05) {
  
  control_conversion_rate <- mean(control_data$converted)
  treatment_conversion_rate <- mean(treatment_data$converted)
  
  combined_data <- rbind(
    data.frame(group = "control", converted = control_data$converted),
    data.frame(group = "treatment", converted = treatment_data$converted)
  )
  
  # Perform a two-sample z-test for proportions
  # Calculate proportions and sample sizes
  n_control <- nrow(control_data)
  n_treatment <- nrow(treatment_data)
  p_control <- control_conversion_rate
  p_treatment <- treatment_conversion_rate
  
  # Pooled proportion
  p_pool <- (sum(control_data$converted) + sum(treatment_data$converted)) / (n_control + n_treatment)
  
  # Standard error 
  se_pool <- sqrt(p_pool * (1 - p_pool) * (1/n_control + 1/n_treatment))
  
  # Z-score 
  z_score <- (p_treatment - p_control) / se_pool
  
  # P-value (two-tailed test)
  p_value <- 2 * pnorm(-abs(z_score))
  
  result <- ifelse(p_value < alpha, 
                   paste("There is a significant difference (p-value =", round(p_value, 4), ")"),
                   paste("There is no significant difference (p-value =", round(p_value, 4), ")"))
  
  return(list(
    p_value = p_value,
    result = result,
    control_conversion_rate = control_conversion_rate,
    treatment_conversion_rate = treatment_conversion_rate
  ))
}

# Applied the function
result <- analyze_ab_test(control_data, treatment_data, alpha = 0.05)
print(result)

#Suggestions
Based on the results:
P-value: The p-value is 0.2151, which is greater than the typical significance level of 0.05. 
This means there is no significant difference between the conversion rates of the old webpage (control group) and the new webpage (treatment group).
Conversion Rates: The conversion rates for both groups are very close: (Control: 12.04%
Treatment: 11.89%)
Given these results, the company should maintain the old webpage since there is no evidence to suggest that the new webpage leads to a significantly higher conversion rate. The conversion rates are almost identical, and the difference observed is not statistically significant.