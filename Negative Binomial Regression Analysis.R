#=========================================================================
#     Importing Dataset
#=========================================================================

df = read.csv(choose.files(),header = TRUE)
df



df$date <- as.Date(df$date)
Revenue = df$Revenue
price_unit = df$price_unit
promotion_flag = df$promotion_flag
stock_available = df$stock_available
delivery_days = df$delivery_days
delivered_qty = df$delivered_qty
units_sold = df$units_sold

#=========================================================================
#     Importing Libraries
#=========================================================================

library(car)
library(MASS)
library(dplyr)


#=========================================================================
#         Justification for Model Selection
#=========================================================================

#Histogram of Units_sold

hist(units_sold,col = 'blue')


# 1. Count data
summary(df$units_sold)

# 2. Overdispersion
mean(df$units_sold)
var(df$units_sold)

# 3. Fit Poisson first
model_pois <- glm(units_sold ~ promotion_flag + stock_available + delivered_qty + Revenue, family = poisson, data = df)

#---> Check dispersion
sum(residuals(model_pois, type="pearson")^2) / model_pois$df.residual

##------------------------------------
##     Negative Binomial Model
##------------------------------------

model_nb <- glm.nb(units_sold ~ promotion_flag + stock_available + delivered_qty + Revenue, data = df)

# View summary
summary(model_nb)


vif(model_nb)

#==========================================================================
#                             Final Model Results
#==========================================================================

model_final <- glm.nb(
  units_sold ~ promotion_flag + stock_available ,
  data = df
)
summary(model_final)

#==========================================================================
#                   Multicollinearity Check
#==========================================================================

vif(model_final)

#=======================================================================
#                      Model Performance Evaluation
#=======================================================================



ts_weekly <- df %>%
  mutate(week = as.Date(cut(date, breaks = "week"))) %>%
  group_by(week) %>%
  summarise(
    units_sold = sum(units_sold),
    promotion_flag = mean(promotion_flag),
    stock_available = mean(stock_available)
  ) %>%
  arrange(week)


ts_weekly <- ts_weekly %>%
  mutate(
    lag_1 = lag(units_sold, 1)
  ) %>%
  na.omit()


model_weekly <- glm.nb(
  units_sold ~ promotion_flag + stock_available + lag_1,
  data = ts_weekly
)

# Weekly Model Negative Binomial Model with lag_1
summary(model_weekly)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weekly Actual vs Predicted Sales plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ts_weekly$predicted <- predict(model_weekly, type = "response")

plot(ts_weekly$week, ts_weekly$units_sold,
     type = "l", col = "blue", lwd = 2,
     xlab = "Week", ylab = "Units Sold",
     main = "Weekly Actual vs Predicted Sales")

lines(ts_weekly$week, ts_weekly$predicted,
      col = "red", lwd = 2)

legend("topleft",
       legend = c("Actual", "Predicted"),
       col = c("blue", "red"),
       lwd = 2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model Performance Metrics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rmse <- sqrt(mean((ts_weekly$units_sold - ts_weekly$predicted)^2))

mae <- mean(abs(ts_weekly$units_sold - ts_weekly$predicted))

mape <- mean(abs((ts_weekly$units_sold - ts_weekly$predicted) / ts_weekly$units_sold)) * 100

ss_res <- sum((ts_weekly$units_sold - ts_weekly$predicted)^2)
ss_tot <- sum((ts_weekly$units_sold - mean(ts_weekly$units_sold))^2)

r_squared <- 1 - (ss_res / ss_tot)

n <- nrow(ts_weekly)
p <- 3  # number of predictors (promotion_flag, stock_available, lag_1)

adj_r2 <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)

pseudo_r2 <- 1 - (model_weekly$deviance / model_weekly$null.deviance)

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("MAPE:", mape, "%\n")
cat("R-squared:", r_squared, "\n")
cat("Pseudo R²:", pseudo_r2, "\n")


# Maximum weekly sales value
max(ts_weekly$units_sold)
# Minimum weekly sales value
min(ts_weekly$units_sold)
# Average Weekly sales value
mean(ts_weekly$units_sold)
