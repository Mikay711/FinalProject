
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

filename="NY-House-Dataset.csv"
mydata=read.csv(filename)


# see data ----------------------------------------------------------


head(mydata)


# see data types ----------------------------------------------------------

str(mydata)

# Consolidate similar categories
# For example, let's combine "Condo for sale" and "Condo for Sale" into a single category called "Condo for sale"

# Replace inconsistent representations
mydata$TYPE <- gsub(" for Sale", " for sale", mydata$TYPE, ignore.case = TRUE)
# Remove specified categories from the "TYPE" variable in the dataset
mydata <- mydata[!(mydata$TYPE %in% c("Coming Soon", "For sale", "Pending", "Contingent")), ]
mydata <- mydata%>%filter(PRICE <= 50000000)



# load libraries ----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(reshape2)


# deliverable 1 ----------------------------------------------------------

str(mydata,width = 70,strict.width='cut')

# absolute values
absoluteT=table(mydata$TYPE,
                exclude = 'nothing') #include all values!
#here you are:
absoluteT
prop.table(absoluteT)

# Relative values
propT <- prop.table(absoluteT) * 100


# Convert frequency table to a data frame
tableFreq <- as.data.frame(propT)
names(tableFreq) <- c("Type", "Percent")


# Add percentages
tableFreq$Percent <- as.vector(propT)

# Add Labels



base1 <- ggplot(data = tableFreq, aes(x = reorder(Type,Percent), y = Percent))
# Add bar plot layer
plot1 <- base1 + 
  geom_bar(fill = "red", stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  labs(x = "Type of Realty", 
       y = "Proportion", 
       title = "New York Housing Market Offer",
       caption = "Source: Kaggle.com",
       subtitle = "February 2024") +
  scale_y_continuous(breaks=c(0,10, 25,40),
                     limits = c(0, 40), 
                     labels=unit_format(suffix = '%')) +
  geom_text(vjust=0,
            size = 3.1,
            aes(y = Percent,
                label = ("")))

# Display the plot
plot1

# save del1Draft ----------------------------------------------------------
saveRDS(plot1, file = "categorical_draft.rds")


# deliverable 2 ----------------------------------------------------------

# Set a reasonable upper limit for the y-axis range
upper_limit <- 5e6  # Adjust this value based on your preference and the distribution of your data

# Calculate the mean price
mean_price <- mean(mydata$PRICE)

# Create a box plot for housing prices with adjusted y-axis range
base2 <- ggplot(mydata, aes(y = PRICE))
plot2 <- base2 +
  geom_boxplot() +
  ylim(0, upper_limit) +  # Set the y-axis range
  labs(y = "Price (USD)", title = "Distribution of Housing Prices in New York") +
  # Add annotations with adjusted x coordinates
  annotate("text", x = 0.5, y = upper_limit * 0.95, label = paste("Mean Price:", scales::dollar(mean_price)), hjust = 1, size = 3) +
  # Add y-intercept line at the mean price
  geom_hline(yintercept = mean_price, linetype = "dashed", color = "red") +
  # Remove x-axis label
  labs(x = NULL)

plot2 <- plot2 + coord_flip()


plot2

# save del2Draft ----------------------------------------------------------
saveRDS(plot2, file = "numerical_draft.rds")


# deliverable 3 ----------------------------------------------------------

# Step 1: Create a contingency table
contingency_table <- mydata %>%
  group_by(TYPE) %>%
  summarize(mean = mean(PRICE), 
            median = median(PRICE))

# Step 2: Reshape the data for plotting
contingency_df <- melt(contingency_table, id.vars = "TYPE")

# Calculate the maximum value in the 'value' column
max_value <- max(contingency_df$value)

# Add a buffer for better visualization
y_limit <- max_value * 1.2  # Adjust the factor as needed

# Plot the bivariate relationship as a segment point plot with facet grid


base3 <- ggplot(data = contingency_df, aes(x = TYPE, y = value, fill = variable))
plot_bivariate <- base3 +
  geom_bar(stat = "identity", position = "dodge", show.legend = F) +
  geom_text(size = 3,
            aes(label = paste0(format(round(value, 1), scientific = TRUE, digits = 2))),
            vjust = .3, hjust = .3) +  # Adjust horizontal position of labels
  ylim(0, y_limit) +  # Set y-axis limits
  labs(title = "Summary of Housing Prices by Type",
       subtitle = "New York Housing Market Feb 2024",
       x = "Type of Realty",
       y = "Price (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~ variable)+
  coord_flip()

# Display the plot
plot3 <- plot_bivariate
plot3













# Create the bivariate plot with logarithmic scale for y-axis and disabling scientific notation
plot_not_using <- ggplot(mydata, aes(x = TYPE, y = PRICE, fill = TYPE)) +
  geom_boxplot() +
  labs(title = "Distribution of Housing Prices by Type",
       subtitle = "New York Housing Market Offer",
       x = "Type of Realty",
       y = "Price (USD)",
       caption = "Source: Kaggle.com") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") +  # Adjust color palette as per your preference
  scale_y_continuous(trans = "log10", labels = scales::comma_format())  # Use logarithmic scale for y-axis and disable scientific notation

# Display the plot
plot_not_using


# save del3Draft ----------------------------------------------------------
saveRDS(plot3, file = "cat-num_draft.rds")


# deliverable 4  ----------------------------------------------------------

library(sf)
county_map=sf::read_sf("WA_County_Boundaries.geojson")
head(county_map)
head(mydata)

# merge data into map ----------------------------------------------------------
mydataCounty=aggregate(data=mydata,Free.Lunch~County,FUN = mean)
myMapLunch=merge(county_map,mydataCounty,by.x='JURISDIC_2',"County")

# prepare plot

base=ggplot(myMapLunch)
del4Draft=base + geom_sf(aes(fill=Free.Lunch))
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")
