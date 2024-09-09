# Objective:
Developed a regression model to predict the factors affecting fuel economy using variables such as engine horsepower (hp) and rear axle ratio (drat). The goal was to identify the key variables most strongly correlated with better fuel efficiency.

# Load and preprocess the mtcars dataset
      mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")
      mtcars2 <- within(mtcars, {
      vs <- factor(vs)
      am <- factor(am)
      cyl <- factor(cyl)
      gear <- factor(gear)
      carb <- factor(carb) 
      })

# Scatterplot of Fuel Economy against Horse Power


     plot(mtcars2$hp, mtcars$mpg,
         main = "Scatterplot of Fuel Economy against Horse Power",
        ylab = "Fuel Economy", xlab = "Horse Power",
        ylim=c(0, 50),
        xlim=c(0, 500),
        col="blue",
        pch = 19, frame = FALSE)

# Correlation between horsepower (hp) and fuel economy (mpg)
         correlation_coefficient <- cor(mtcars2$hp, mtcars2$mpg, method = 
        "pearson")
      print(correlation_coefficient)

# Subset dataset and correlation matrix between mpg, drat, and hp

      myvars <- c("mpg", "drat", "hp")
      mtcars_subset <- mtcars[myvars]
      corr_matrix <- cor(mtcars_subset, method = "pearson")
      round(corr_matrix, 4)

# Fit a multiple regression model with mpg as the response variable
      model <- lm(mpg ~ drat + hp, data = mtcars_subset)
      summary(model)

# Fitted values and residuals from the model
      fitted_values <- fitted.values(model)
      residuals <- residuals(model)

# Plot residuals against fitted values
      plot(fitted_values, residuals, 
          main="Residuals against Fitted Values",
          xlab = "Fitted Values", ylab = "Residuals", 
          col="red",
          pch = 19)

# Normal Q-Q plot for residuals
      qqnorm(residuals, pch = 19, col="red")
      qqline(residuals, col = "blue", lwd = 2)

# Confidence intervals for the model coefficients
      conf_90_int <- confint(model, level = 0.95)
      round(conf_90_int, 4)

# Predict mpg for new data
      new_data <- data.frame(drat = 3.15, hp = 120)
      predict(model, new_data, interval = "confidence", level = 0.95)
      predict(model, new_data, interval = "prediction", level = 0.95)

# Key Findings:

Horsepower (hp) had a significant negative correlation with fuel economy (mpg), with higher horsepower resulting in lower fuel efficiency.
Rear axle ratio (drat) had a positive correlation with fuel economy, indicating that vehicles with a higher rear axle ratio tend to have better fuel economy.

# Model Performance 

- Adjusted R-squared: 0.723

- Significant Predictors:

- hp (p-value: 5.17e-06)

- drat (p-value: 0.000467)

  
# Full report here
[Cars-multiple-regression .pdf](https://github.com/user-attachments/files/16926595/Cars-multiple-regression.pdf)


  
