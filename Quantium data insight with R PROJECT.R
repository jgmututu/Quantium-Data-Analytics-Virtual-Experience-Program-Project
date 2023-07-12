install.packages("readxl")
library(readxl)
# Set the working directory to the folder containing the Excel file
setwd("D:/Data Analysis/Datasets")

# Load the Excel file
data <- read_excel("QVI_transaction_data.xlsx")
install.packages("readr")
library(readr)
transaction <- read_csv("QVI_purchase_behaviour.csv")
str(data)
colnames(data)
# Rename the column names
colnames(data) <- c("Date", "Store_Number", "Loyalty_Card_Number", "Transaction_ID", "Product_Number", "Product_Name", "Product_Quantity", "Total_Sales")

View(data)
# Convert the date variable to the appropriate format
data$Date <- as.Date(data$Date, origin = "1899-12-30")
# Check for duplicates in the dataset
duplicates <- data[duplicated(data), ]
# Remove duplicates from the dataset
data <- unique(data)
# NO duplicates
# Loop through all columns to check unique values
for (col in colnames(data)) {
  print(unique(data[[col]]))
}

#explore outliers in product quantity
summary(data)
# Filter dataset for Product_Quantity greater than 150
filtered_data <- data[data$Product_Quantity > 150, ]

# we drop these two obs, seem the customer was buying for an organisation or event
# Drop rows where Loyalty_Card_Number is equal to 226000
data <- data[data$Loyalty_Card_Number != 226000, ]
#Outliers fixed

#We explore products column

library(dplyr)
library(stringr)
data <- data %>%
  filter(!str_detect(Product_Name, "Salsa"))
View(data)
#verify if salsa was removed
salsa_removed <- all(!str_detect(data$Product_Name, "salsa"))

if (salsa_removed) {
  print("Salsa has been successfully removed from all products.")
} else {
  print("Salsa still exists in some products.")
}
dim(data)
unique_products <- unique(data$Product_Name)
print(unique_products)

#extract pack size
data$Pack_Size <- str_extract(data$Product_Name, "\\d+g")

unique_pack_sizes <- unique(data$Pack_Size)
print(unique_pack_sizes)
View(data)
#remove "g"
data$Pack_Size <- as.integer(gsub("g", "", data$Pack_Size))

# remove NA on pack size
data <- data[complete.cases(data$Pack_Size), ]

#extract brand name from product name

library(stringr)

data <- data %>%
  mutate(Brand_Name = str_extract(Product_Name, "^[^ ]+"))
#check brand names extracted 

unique_brands <- data %>% select(Brand_Name) %>% distinct() %>% pull(Brand_Name)
unique_brands
#cleaning brand names
data_copy <- data
View(data_copy)
data_copy <- data_copy %>% mutate(
  Brand_Name = ifelse(Brand_Name == "Red", "RRD", Brand_Name),
  Brand_Name = ifelse(Brand_Name == "Smith", "Smiths", Brand_Name),
  Brand_Name = ifelse(Brand_Name == "NCC", "Natural", Brand_Name),
  Brand_Name = ifelse(Brand_Name == "Grain", "GrnWves", Brand_Name),
  Brand_Name = ifelse(Brand_Name == "Infzns", "Infuzions", Brand_Name),
  Brand_Name = ifelse(Brand_Name == "Dorito", "Doritos", Brand_Name),
  Brand_Name = ifelse(Brand_Name == "WW", "Woolworths", Brand_Name),
  Brand_Name = ifelse(Brand_Name == "Snbts", "Sunbites", Brand_Name)
)

#check output
data_copy %>% filter(Brand_Name == "RRD" | Brand_Name == "Smiths" | Brand_Name == "Natural" | Brand_Name == "GrnWves" | Brand_Name == "Infuzions" | Brand_Name == "Doritos" | Brand_Name == "Woolworths" | Brand_Name == "Sunbites")

#Creating sequence
install.packages("ggplot2")
library(ggplot2)
install.packages("scales")
library(scales)
transactions_by_day <- data_copy %>%
  group_by(Date) %>%
  summarise(n = n())

ggplot(transactions_by_day, aes(x = Date, y = n)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b %Y")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# filter December to see individual days
library(lubridate)

library(lubridate)

transactions_by_day <- transactions_by_day %>%
  mutate(Date = as.POSIXlt(Date))

ggplot(transactions_by_day[lubridate::month(Date) == 12,], aes(x = Date, y = n)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions in December") +
  scale_x_date(breaks = "1 day") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#code error to fix later

summary(data_copy) 
is.numeric(data_copy$Pack_Size)


help("hist")

#Histogram of Pack Size
Pack_Size <- data_copy$Pack_Size #create a new object called Pack_Size and assign it

hist(Pack_Size, breaks = 10, xlab = "Pack Size", ylab = " Frequency ")

# extract month and dayof the week

library(lubridate)

data_copy$Day_of_Week <- weekdays(data_copy$Date)
data_copy$Month <- months(data_copy$Date)

#PLOT
library(ggplot2)
library(RColorBrewer)


colors <- brewer.pal(9, "Blues")[4]

ggplot(total_sales_by_day_of_week, aes(x = Day_of_Week, y = Total_sales)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(x = "Day of the Week", y = "Total Sales")


#plot top 5 most orderd brands

colors <- brewer.pal(5, "Set2")

top_5_brands <- data_copy %>%
  group_by(Brand_Name) %>%
  summarise(total_sales = sum(Product_Quantity)) %>%
  arrange(desc(total_sales)) %>%
  head(5)

ggplot(top_5_brands, aes(x = reorder(Brand_Name, total_sales), y = total_sales)) +
  geom_bar(stat = "identity", fill = colors) +
  labs(x = "Brand", y = "Total Sales") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot by month

# Generate color palette with appropriate length
n_levels <- length(unique(data_copy$Month))
colors <- brewer.pal(n_levels, "Set3")

total_sales_by_month <- data_copy %>%
  group_by(Month) %>%
  summarise(Total_sales = sum(Product_Quantity))

# Convert Month to factor with desired order
total_sales_by_month$Month <- factor(total_sales_by_month$Month, levels = month.name)

ggplot(total_sales_by_month, aes(x = Month, y = Total_sales, fill = Month)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colors) +
  labs(x = "Month", y = "Total Sales")

#check missing value and duplicate transaction dataset

# Check for missing values
missing_values <- sum(is.na(transaction))
missing_values # no missing value

#check duplicates
# Check for duplicate transactions
duplicate_rows <- transaction[duplicated(transaction), ]
duplicate_rows
transaction
#rename columns
new_names <- c("Loyalty_Card_Number", "Life_Stage", "Premium_Customer")

colnames(transaction) <- new_names

#plots
# Create a new dataframe with the life stage and premium customer information
life_stage_premium <- transaction %>%
  select(Life_Stage, Premium_Customer) %>%
  group_by(Life_Stage, Premium_Customer) %>%
  summarise(count = n())


# Plot the life stage by premium customer
ggplot(life_stage_premium, aes(y = Life_Stage, x = count, fill = Premium_Customer)) +
  geom_bar(stat = "identity") +
  labs(y = "Life Stage", x = "Frequency", fill = "Premium Customer")

transaction_copy <- transaction

#merge customer and transaction data
# Merge data frames based on customer ID
tran_custo_data <- merge(data_copy, transaction_copy, by = "Loyalty_Card_Number")
View(tran_custo_data)

#Total sales by Lifestage
ggplot(total_sales_by_life_stage, aes(y = Life_Stage, x = Total_sales, fill = Life_Stage)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Life Stage", x = "Total Sales")

# Plot the total sales by premium customers

total_sales_by_life_stage <- total_sales_by_life_stage %>%
  merge(transaction_copy[, c("Life_Stage", "Premium_Customer")], by = "Life_Stage")

# Plot the total sales by premium customers
ggplot(total_sales_by_life_stage, aes(y = Life_Stage, x = Total_sales, fill = Premium_Customer)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red", "green")) +
  labs(y = "Life Stage", x = "Total Sales")

#Brand Popularity by Life stage
library(forcats)

brand_popularity_by_life_stage <- tran_custo_data %>%
  group_by(Life_Stage, Brand_Name) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(Brand_Name = fct_reorder(Brand_Name, n))

ggplot(brand_popularity_by_life_stage, aes(x = Life_Stage, y = n, group = Brand_Name)) +
  geom_line(aes(color = Brand_Name)) +
  geom_point(aes(color = Brand_Name), size = 2) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Life Stage", y = "Number of Transactions", title = "Popularity of Brands by Life Stage")

#Top 5 popular brands by nu
brand_popularity_by_life_stage <- tran_custo_data %>%
  group_by(Life_Stage, Brand_Name) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(Life_Stage, desc(n)) %>%
  group_by(Life_Stage) %>%
  top_n(5, n) %>%
  mutate(Brand_Name = fct_reorder(Brand_Name, n))

ggplot(brand_popularity_by_life_stage, aes(y = Life_Stage, x = n, fill = Brand_Name)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "Life Stage", x = "Number of Transactions", title = "Top 5 Popular Brands by Life Stage")
