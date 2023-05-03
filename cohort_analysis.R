if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}

pacman::p_load(stringr, readxl, openxlsx, tidyverse, lubridate, 
               reshape, reshape2, plotly)

retail <- read_excel("Online Retail.xlsx")

# exclude duplicates
retail_wd <- retail %>% group_by(across(everything())) %>%
  ungroup() %>%
  distinct()

# create a function that counts unique values in each column
count_r<-function(x){length(unique(x))}
apply(retail_wd,2,count_r)

# calculate the amount of transactions per country

# create a list of unique transactions with the corresponding country data
country_counts <- retail_wd %>%
  group_by(Country, InvoiceNo) %>%
  summarize(total_orders = n())
# count the number of transactions per each country
country_counts2<-country_counts%>%group_by(Country)%>%summarize(total_orders = n())

# calculate the shares of transactions belonging to each country

country_percentages <- country_counts2 %>%
  mutate(percent_orders = total_orders / sum(total_orders) * 100) %>%
  arrange(desc(percent_orders)) 

# select top 10 countries by the total number of transactions
country_percentages_10<-country_percentages%>%slice(1:10)

# Create a bar chart that shows the percentage of orders from each country
ggplot(country_percentages_10, aes(x = reorder(Country, -percent_orders), y = percent_orders, fill = Country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue", "green", "orange", "purple", "gray", "yellow", "cyan", "pink", "brown")) +
  geom_text(aes(label = paste0(round(percent_orders,1),"%")), 
            vjust = -0.5, size = 4) +
  ggtitle("Top 10 Countries by Percentage of Orders") +
  xlab("Country") +
  ylab("Percentage of Orders") +
  guides(fill = "none")

# create a column indicating whether the order was cancelled

retail_wd$cancelled<-ifelse(grepl("c|C",retail_wd$InvoiceNo),1,0)

# 9251 out of 536641 orders were cancelled
sum(retail_wd$cancelled)

# exclude cancelled orders
retail_<-retail_wd[retail_wd$cancelled==0,]

# create a column indicating transactions with the negative amount of items

retail_$negative<-ifelse(grepl("-",retail_$Quantity),1,0)
# 1336 orders have negative quantity
sum(retail_$negative)
# exclude them
retail_cl<-retail_[retail_$negative==0,]

# filter for only transactions from UK

retail_uk <- retail_cl[retail_cl$Country=="United Kingdom",]

str(retail_uk)

apply(retail_uk,2,count_r)

# exclude transactions that have missing data in any of the column

retail_uk_noNA<-na.omit(retail_uk)
apply(retail_uk_noNA,2,count_r)

##### 1) Time cohorts analysis

summary(retail_uk_noNA$InvoiceDate)

# separate Date from Time stamp

retail_uk_noNA$InvoiceMonth <- as.Date(retail_uk_noNA$InvoiceDate)

# find a first day for each transaction

first_day_of_transac <- retail_uk_noNA %>%
  group_by(CustomerID) %>%
  summarize(CohortMonth = min(InvoiceMonth)) %>%
  ungroup()

# replace actual day of the first purchase with the first day of the corresponding month
first_day_of_transac$year <- format(first_day_of_transac$CohortMonth, "%Y")
first_day_of_transac$month <- format(first_day_of_transac$CohortMonth, "%m")
first_day_of_transac$CohortMonth <- as.Date(paste(first_day_of_transac$year, 
                                                  first_day_of_transac$month, 
                                                  "01", sep = "-"))

retail_uk_noNA <- left_join(retail_uk_noNA, first_day_of_transac, by = "CustomerID")
head(retail_uk_noNA)

# calculate in months the difference between the first and the next time the customer made a purchase

retail_uk_noNA$CohortIndex <- as.numeric(difftime(retail_uk_noNA$InvoiceMonth,
                                                  retail_uk_noNA$CohortMonth,
                                                  units = "days")) / 30.44

# Replace the numbers with digits after decimal point (e.g 4.65 -> 4)
retail_uk_noNA$CohortIndex <- floor(retail_uk_noNA$CohortIndex)

# calculate how many customers made a purchase in the following months after the 1st purchase

df_count <- aggregate(CustomerID ~ CohortMonth + CohortIndex, data = retail_uk_noNA, FUN = length)

# create a matrix
# rows: the date of the first purchase
# columns: the ordinal number of the months after the first purchase was done

df_matrix <- dcast(df_count, CohortMonth ~ CohortIndex, 
                   value.var = "CustomerID")



# calculate Retention rate
# column "0" - amount of transactions made at a CohortMonth

# Define a modified function to divide each row by its second value, excluding the first element
divide_by_second_excluding_id <- function(x) {
  x <- round((x / x[1])*100, 0)
  return(x)
}

# Apply the modified function to each row of the data frame, excluding the ID column
df_matrix_divided <- apply(df_matrix[, -1], 1, divide_by_second_excluding_id)

df_matrix_divided <- as.data.frame(t(df_matrix_divided))
  
# Combine the result with the ID column
df_matrix_divided_back <- cbind(df_matrix[, 1], df_matrix_divided)
rownames(df_matrix_divided_back ) <- df_matrix_divided_back [, 1]
df_matrix_divided_back <- df_matrix_divided_back[, -1]

heatmap_mx <- as.matrix(df_matrix_divided_back)




# create a heatmap with with ggplot

heatmap_mx_melt <- melt(heatmap_mx)
head(heatmap_mx_melt)

ggplot(heatmap_mx_melt, aes(Var2, Var1)) +
  geom_tile(aes(fill = value)) +
  labs(x = "CohortMonth", y = "CohortIndex", fill = "value") +
  scale_fill_gradient(low = "green", high = "black", na.value = "lightgray") +
  theme_classic() +
  scale_y_discrete(limits = rev(levels(factor(heatmap_mx_melt$Var1)))) +
  geom_text(aes(label = value), color = "white")


# with plotly

heatmap_plot <- plot_ly(z = heatmap_mx, type = "heatmap")

# modify the layout to show row names on y-axis
heatmap_plot <- layout(heatmap_plot, yaxis = list(title = "", tickmode = "array",
                                                  tickvals = 0:nrow(heatmap_mx),
                                                  ticktext = rownames(heatmap_mx)))

# display the plot
heatmap_plot


