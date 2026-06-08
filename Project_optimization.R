# ==========================================
# 0. LIBRARIES
# ==========================================
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stats)
install.packages("reshape2")
# ==========================================
# 1. DATA LOADING AND MERGING (Section 3)
# ==========================================
cat("Loading datasets...\n")
setwd("C:/Users/kaust/Studies/Sem 2/Optimization and Numericals Methods/Project")
train <- read.csv("train.csv")
features <- read.csv("features.csv")
stores <- read.csv("stores.csv")

# Merge datasets
df <- train %>%
  left_join(stores, by = "Store") %>%
  left_join(features, by = c("Store", "Date", "IsHoliday"))
View(df)
# Convert Date
df$Date <- as.Date(df$Date)
?as.Date
# ==========================================
# 2. DATA CLEANING
# ==========================================

# Fill MarkDown NA with 0
markdown_cols <- c("MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5")
df[markdown_cols] <- lapply(df[markdown_cols], function(x) replace(x, is.na(x), 0))

# Fill CPI and Unemployment (forward + backward fill)
df <- df %>%
  arrange(Store, Date) %>%
  group_by(Store) %>%
  fill(CPI, Unemployment, .direction = "downup") %>%
  ungroup()

# Remove negative sales
df <- df %>% filter(Weekly_Sales > 0)

cat("Data shape after cleaning:", dim(df), "\n")

# ==========================================
# 3. DESCRIPTIVE STATISTICS
# ==========================================

# Chart 1: Time Series
weekly_sales <- df %>%
  group_by(Date) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales))

holiday_sales <- df %>%
  filter(IsHoliday == TRUE) %>%
  group_by(Date) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales))

ggplot() +
  geom_line(data = weekly_sales, aes(x = Date, y = Weekly_Sales), color = "blue") +
  geom_point(data = holiday_sales, aes(x = Date, y = Weekly_Sales), color = "red") +
  ggtitle("Total Walmart Weekly Sales (2010–2012)") +
  xlab("Date") + ylab("Total Sales ($)") +
  theme_minimal()

ggsave("Chart_1_Sales_TimeSeries.png")

# Chart 2: Correlation Heatmap
library(reshape2)

corr_cols <- df %>%
  select(Weekly_Sales, IsHoliday, Temperature, Fuel_Price, MarkDown1, MarkDown3)

corr_matrix <- cor(corr_cols, use = "complete.obs")

melted_corr <- melt(corr_matrix)

ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  ggtitle("Correlation Matrix of Demand Variables") +
  theme_minimal()

ggsave("Chart_2_Correlation_Matrix.png")

# ==========================================
# 4. OPTIMIZATION (Newsvendor Model)
# ==========================================

# Filter Store 1, Dept 1
store1_dept1 <- df %>%
  filter(Store == 1, Dept == 1)

# Estimate demand distribution
mu <- mean(store1_dept1$Weekly_Sales)
sigma <- sd(store1_dept1$Weekly_Sales)

cat("\nEstimated Demand Distribution:\n")
cat(paste("Normal(mu =", round(mu,2), ", sigma =", round(sigma,2), ")\n"))

# Costs
cost_per_unit <- 50
selling_price <- 80
salvage_value <- 20

c_h <- cost_per_unit - salvage_value
c_s <- selling_price - cost_per_unit

# ==========================================
# Expected Cost Function
# ==========================================
expected_cost <- function(Q, mu, sigma, c_h, c_s) {
  z <- (Q - mu) / sigma
  
  expected_overage <- (Q - mu) * pnorm(z) + sigma * dnorm(z)
  expected_shortage <- (mu - Q) * (1 - pnorm(z)) + sigma * dnorm(z)
  
  return(c_h * expected_overage + c_s * expected_shortage)
}

# ==========================================
# Optimization using optim()
# ==========================================
result <- optim(
  par = mu,
  fn = expected_cost,
  mu = mu,
  sigma = sigma,
  c_h = c_h,
  c_s = c_s,
  method = "L-BFGS-B",
  lower = 0
)

optimal_Q <- result$par
min_cost <- result$value

cat("\n--- OPTIMIZATION RESULTS ---\n")
cat("Optimal Inventory Q*:", round(optimal_Q, 2), "\n")
cat("Minimized Expected Cost:", round(min_cost, 2), "\n")

# ==========================================
# Analytical Solution (Newsvendor Formula)
# ==========================================
critical_ratio <- c_s / (c_s + c_h)

analytical_Q <- qnorm(critical_ratio, mean = mu, sd = sigma)

cat("Analytical Q*:", round(analytical_Q, 2), "\n")

