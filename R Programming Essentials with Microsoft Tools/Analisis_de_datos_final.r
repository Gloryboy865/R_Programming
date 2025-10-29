#Análisis de datos final
customer_data <- read.csv("customer_data.csv")
loyalty_data <- read.csv("loyalty_data.csv")

#Check initial data structure
print("Initial customers data structure:")
str(customer_data)
print("Initial loyalty data structure:")    
str(loyalty_data)

#Convert PurchaseDate to Date format
customer_data$PurchaseDate <- as.Date(customer_data$PurchaseDate, format="%Y-%m-%d")

#Handle missing values in Age and PurchaseAmount
customer_data$Age[is.na(customer_data$Age)] <- median(customer_data$Age, na.rm=TRUE)
customer_data$PurchaseAmount[is.na(customer_data$PurchaseAmount)] <- mean(customer_data$PurchaseAmount, na.rm=TRUE)

#Verify data types and check for missing values
cat("Data types and missing values after cleaning:\n")
str(customer_data)
str(loyalty_data)
cat("Total missing values in customer_data:\n", sum(is.na(customer_data)), "\n")
cat("Total missing values in loyalty_data:\n", sum(is.na(loyalty_data)), "\n")

#Step 2: Customer Analysis
#Filter data by Age > 30
Above_30 <- subset(customer_data, Age > 30)
print("Customers with Age > 30:")
print(head(Above_30))

#Filter data for Female customers who bought Yoga Class
df_filtered <- subset(customer_data, product == "Yoga Class" & Gender == "Female")
print("Female customers who bought Yoga Class:")
print(head(df_filtered))

#Select the Age and Product columns
df_selected <- customer_data[, c("Age", "product")]
print("Selected Age and Product columns:")
print(head(df_selected))

#Step 3: Loyalty Program Analysis
# Create a new column in loyalty_data called Tier
loyalty_data$Tier <- NA
loyalty_data$Tier <- ifelse(loyalty_data$points >= 500, "Gold",
                      ifelse(loyalty_data$points >= 200, "Silver", "Bronze"))
print("Loyalty data with Tier column added:")
print(head(loyalty_data))

# Create new column called PurchaseAmount_EUR
customer_data$PurchaseAmount_EUR <- customer_data$PurchaseAmount * 0.92
print("Customer data with PurchaseAmount_EUR column added:")
print(head(customer_data))

#Function to categorize customers into tiers
categorize_tier <- function(points) {
  if (points >= 500) {
    return("Gold")
  } else if (points >= 200) {
    return("Silver")
  } else {
    return("Bronze")
  }
}

#Add tiers to loyalty_data
loyalty_data$Tier <- sapply(loyalty_data$LoyaltyPoints, categorize_tier)
print("Loyalty data with tiers:")

#Merge with customer_data
merged_data <- merge(customer_data, loyalty_data, by="CustomerID")
print("Merged customer and loyalty data:")
print(head(merged_data))

#Summarize tier statistics
library(dplyr)
tier_summary <- merged_data %>%
  group_by(Tier) %>%
  summarise(
    Customers = n(),
    Avg_PurchaseAmount = mean(PurchaseAmount, na.rm=TRUE),
    Top_Product = names(sort(table(product), decreasing=TRUE)[1])
  )
print("Tier summary statistics:")
print(tier_summary)

#Step 4: Report generation
#Export the two dataset (customer_data & loyalty_data) as csv files and save the report as `revised_customer_analysis_report.csv` and revised_loyaltyfitlife_analysis_report.csv.
write.csv(customer_data, "data/revised_customer_analysis_report.csv", row.names=FALSE)
write.csv(loyalty_data, "data/revised_loyaltyfitlife_analysis_report.csv", row.names=FALSE)




#Save the report
write.csv(merged_data, "data/fitlife_analysis_report.csv", row.names=FALSE)