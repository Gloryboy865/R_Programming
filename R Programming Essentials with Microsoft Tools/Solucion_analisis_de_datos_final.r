#Solución de análisis de datos final en R
# ===============================================================
# FitLife Fitness Club Loyalty Program Analysis - Solution Code
# ===============================================================

library(dplyr)

# === Step 1: Data Import & Cleaning ===

# Import customer_data.csv and loyalty_data.csv
customer_data <- read.csv("data/customer_data.csv")
loyalty_data <- read.csv("data/loyalty_data.csv")

# Check initial data structures
print("Initial customer data structure:")
str(customer_data)
print("Initial loyalty data structure:")
str(loyalty_data)


# Handle missing values in Age and PurchaseAmount
# Remove rows with PurchaseAmountQuantity = NA
new_set_data <- filter(customer_data, !is.na("PurchaseAmount"))


# Verify data types and check for missing values
str(customer_data)
str(loyalty_data)


# === Step 2: Customer Analysis ===

# Filter data by Age > 30
Above_30 <- subset(customer_data, Age > 30)
print("Customers with Age > 30:")
print(head(Above_30))

# Filter data for Female customers who bought Yoga Class
df_filtered <- subset(customer_data, Product == "Yoga Class" & Gender == "Female")
print("Female customers who bought Yoga Class:")
print(head(df_filtered))

# Select the Age and Product columns
df_selected <- customer_data[, c("Age", "Product")]
print("Age and Product columns selected:")
print(head(df_selected))

# === Step 3: Loyalty Program Analysis ===

# Create a new column in loyalty_data called Tier

loyalty_data <- loyalty_data %>% mutate(Tier = case_when(
    LoyaltyPoints <= 200 ~ "Bronze",
    LoyaltyPoints < 500 ~ "Silver",
    TRUE ~ "Gold"
))

#Create new column called PurchaseAmount_EUR
customer_data <- customer_data %>% mutate(PurchaseAmount_EUR = PurchaseAmount * 0.92)

# === Step 4: Report Generation ===
# Export the two dataset (customer_data & loyalty_data) as csv files and save the report as `revised_customer_analysis_report.csv` and revised_loyaltyfitlife_analysis_report.csv.
# Export customer_data as CSV
write.csv(customer_data, "revised_customer_analysis_report.csv", row.names = FALSE)

# Export loyalty_data as CSV
write.csv(loyalty_data, "revised_loyaltyfitlife_analysis_report.csv", row.names = FALSE)