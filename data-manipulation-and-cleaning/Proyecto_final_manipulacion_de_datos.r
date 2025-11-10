#Proyecto_final_manipulacion_de_datos
# ================================================
# Final Project Implemenation
# Module 5 - Final Project
# ================================================

# ================================================
# Graded Challenge 1: Handle missing values using specified rules (20 points)
# ================================================

# Please do not change the variable names in this lab, as this could result in
# the autograder not registering your code correctly.

# Missing values (20 points)
# First, import all dependencies and your dataset into a new df called retail_df
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

retail_df <- read_csv("retail_set12.csv")

# Next: 
#1. Identify missing values: Use functions like is.na() and sum() to assess the extent of missing data in each column.

# Check for missing values
first_missing_values <- colSums(is.na(retail_df))

#2. Impute missing values: For numerical columns, use the mean or median to replace NAs. For categorical columns, use the mode or a placeholder like "Unknown."

# Impute missing 'UnitPrice' with the median
retail_df$UnitPrice <- ifelse(is.na(retail_df$UnitPrice), median(retail_df$UnitPrice, na.rm = TRUE), retail_df$UnitPrice)

#3. Impute missing values in the UnitPrice with median and Quantity column with the mean.

# Impute missing 'Quantity' with the mean
retail_df$Quantity <- ifelse(is.na(retail_df$Quantity), mean(retail_df$Quantity, na.rm = TRUE), retail_df$Quantity)

# Final Missing Values Check 
final_missing_values <- colSums(is.na(retail_df))

# ================================================
# Graded Challenge 2: Customer Analysis
# (25 points)
# ================================================

# Duplicates and outliers (20 points)

#1. Remove duplicates based on 'CustomerID', 'OrderDate', and 'ProductDescription' columns.
#2. Check for duplicates.
#3. Treat outliers: For the purposes of this lab, treat values outside 1.5 times the interquartile range as outliers and remove the corresponding rows. 

#1. Remove duplicates based on 'CustomerID', 'OrderDate', and 'ProductDescription' columns.
retail_df <- distinct(retail_df, CustomerID, OrderDate, ProductDescription, .keep_all = TRUE)

#2. Check for duplicates.
duplicates <- retail_df[duplicated(retail_df), ]
print(duplicates)

#3. Treat outliers: For the purposes of this lab, treat values outside 1.5 times the interquartile range as outliers and remove the corresponding rows. The solutions file will show two approaches for doing this.

#Check before removing outliers
print(summary(retail_df))

# Identify and Remove Outliers using mutate, group_by, and filter()
numeric_cols <- c("Quantity", "UnitPrice")
retail_df <- retail_df %>%
  group_by(Category) %>%
  mutate(across(all_of(numeric_cols), ~ ifelse(. < quantile(., 0.25) - 1.5 * IQR(.) | . > quantile(., 0.75) + 1.5 * IQR(.), NA, .))) %>%
  ungroup() %>%
  filter(if_all(everything(), ~!is.na(.)))

# Print a summary of retail_df after removing outliers
removed_outliers <- summary(retail_df)
print(removed_outliers)

# ================================================
# Graded Challenge 3: Standardize Date/Time Formats (20 points)
# ================================================

# String Replacement (20 points)
# Use str_replace_all to expand street abbreviations

#1. Replace "Ln" with "Lane" in the Address column
retail_df <- retail_df %>%
  mutate(Address = str_replace_all(Address, "Ln", "Lane"))

#2. Replace "Blvd" with "Boulevard" in the Address column
retail_df <- retail_df %>%
  mutate(Address = str_replace_all(Address, "Blvd", "Boulevard"))


# ================================================
# Graded Challenge 4: Transform combined fields into structured components (20 points)
# ================================================

#1.Separate the 'Address' column into StreetNumber','StreetName','StreetType', 'City', 'State', and 'Zip'.'Street', 'City', 'State', and 'Zip'.

# Separate the address components
retail_df <- retail_df %>%
  separate(Address, into = c("StreetNumber","StreetName","StreetType", "City", "State", "Zip"), sep = " ", extra = "merge")

#2.Split the FullName column into FirstName and LastName using a space as the separator, taking into account edge cases with middle names.

# Separate the name components
retail_df <- retail_df %>%
    separate(FullName, into = c("FirstName", "LastName"),
    sep = " ", extra = "merge")

print(retail_df)

# ================================================
# Graded Challenge 5: Create required calculated fields (20 points)
# ================================================

#Create Calculated Fields(20 points)
## Calculate TotalRevenue
retail_df <- retail_df %>%
  mutate(TotalRevenue = UnitPrice * Quantity)

## Add a column for UnitPrice in Euros. Call it EuroPrice (assuming 1 USD = 0.92 EUR)
retail_df <- retail_df %>%
  mutate(EuroPrice = UnitPrice * 0.92)