#GOAL Determine which campaigns had a positive impact on food purchases (Excluding wine)
# Then Determine which campaigns were effective at targeting "Parents" specifically
# We used chatGPT to assist, as this is my one of my first dives into R


#Read in csv
data <- read.csv("marketing_campaign.csv", header= TRUE, sep=';')

#display first 5 rows
head(data, n = 5)


# Calculate the percentage of rows with missing values
missing_percent <- mean(rowSums(is.na(data)) > 0) * 100

# Print the result
cat("Percentage of rows with missing values:", missing_percent, "%\n")


# Drop rows with missing values
data_without_nas <- na.omit(data)


# Create a new column 'MntFood' with the sum of food purchases
data_without_nas$MntFood <- rowSums(data_without_nas[, c("MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts")])

# Create a new column "Parental Status" which is 1 if teens, children in home, and 0 if there are no children or teens
data_without_nas$IsParent <- ifelse(data_without_nas$Kidhome > 0 | data_without_nas$Teenhome > 0, 1, 0)

data_without_nas$CountPurchases <- rowSums(data_without_nas[, c("NumCatalogPurchases", "NumWebPurchases", 'NumStorePurchases' )])


# Check what percent of records are parents, and what percent of total customers are parents
parental_percent <- mean(data_without_nas$IsParent > 0) * 100


#Select the relevant fields for my analysis
selected_columns <-  c("ID", "IsParent", "MntFood", "AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "Response", "CountPurchases")
parental_food_campgains  <- data_without_nas[selected_columns]

#Rename Response to Campaign 6
names(parental_food_campgains)[names(parental_food_campgains) == "Response"] <- "AcceptedCmp6"

#plotting the various campaigns, comparing those who accepted with those who didn't
library(ggplot2)
# Melt the data for easier plotting
library(reshape2)
melted_data <- melt(parental_food_campgains, id.vars = c("ID", "MntFood", 'IsParent', "CountPurchases"),
                    measure.vars = c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "AcceptedCmp6"))

# Create a violin plot
ggplot(melted_data, aes(x = factor(value), y = CountPurchases, fill = factor(value))) +
  geom_violin(scale = "count", position = "dodge") +
  facet_wrap(~ variable, ncol = 2) +  # One subplot for each campaign
  labs(x = "", y = "Purchase Count", title = "Distribution of Food Purchase Counts by Campaign Response") +
  scale_fill_discrete(name = "Campaign Accepted", labels = c("Not Accepted", "Accepted")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())  # Hide x-axis labels and title

library(dplyr)
library(stringr)


# Convert the "variable" column to character
melted_data <- melted_data %>%
  mutate(variable = as.character(variable))

# Filter out the "Not Accepted" group before plotting
accepted_data <- melted_data %>%
  filter(value == 1)

# Calculate total purchase count for each campaign
total_purchase_data <- accepted_data %>%
  group_by(variable, IsParent) %>%
  summarize(total_purchase = sum(MntFood))

# Create a bar chart
ggplot(total_purchase_data, aes(x = variable, y = total_purchase, fill = factor(IsParent))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Campaigns", y = "Sum of Food Purchased in Dollars",
       title = "Campaigns by Amount Spent on Food (Excluding Wine)") +
  scale_fill_discrete(name = "Parental Status", labels = c("Not Parent", "Parent")) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## In this above graph, it looks like campaign 6 was most effective in terms of sales,
## and campaign 3 was particularly effective among parents, compared to the entire population of customers
## who accepted campaign 3
## Campaign 2 was clearly an underperformer when it comes to food sales. 


# when we add in the violin plots from before we can see campaign 6 and 3 both contained a wide range of purchase amounts. 
# More analysis is needed to determine what other factors led to campaign 3 and 6's success among parents. 