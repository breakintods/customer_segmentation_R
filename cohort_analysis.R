if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}

pacman::p_load(stringr, readxl, openxlsx, tidyverse, lubridate, 
               reshape, reshape2, plotly, psych, gridExtra, 
               cluster, factoextra)

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
  summarize(total_orders = n(), .groups = "drop")
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
  guides(fill = "none") +
  theme_classic()

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
check <- retail_[retail_$negative==1,] #CustomerID is NA here
sum(is.na(check$CustomerID))
# exclude cases when CustomerID is NA
retail_cl<-retail_[!is.na(retail_$CustomerID),]

# filter for only transactions from UK

retail_uk_noNA <- retail_cl[retail_cl$Country=="United Kingdom",]

str(retail_uk_noNA)

apply(retail_uk_noNA,2,count_r)

##### 1) Time cohorts analysis

summary(retail_uk_noNA$InvoiceDate)

# separate Date from Time stamp

retail_uk_noNA$InvoiceMonth <- as.Date(retail_uk_noNA$InvoiceDate)

# when each customer made a purchase for the first time?

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
# This step might affect the difference in my results and in the tutorial!
# Though the trends are the same as in the tutorial
retail_uk_noNA$CohortIndex <- floor(retail_uk_noNA$CohortIndex)

# calculate how many customers made a purchase in the following months after the 1st purchase

df_count <- aggregate(CustomerID ~ CohortMonth + CohortIndex, data = retail_uk_noNA, FUN = length)

# create a matrix
# rows: the date of the first purchase (CohortMonth)
# columns: the ordinal number of the months after the first purchase was done (CohortIndex)

df_matrix <- dcast(df_count, CohortMonth ~ CohortIndex, 
                   value.var = "CustomerID")



# calculate Retention rate
# column "0" - amount of transactions made at a CohortMonth

# Define a function to divide the values in each row by 
# the first element in the row
divide_by_second_excluding_id <- function(x) {
  x <- round((x / x[1])*100, 0)
  return(x)
}

# Apply the function to each row of the data frame, excluding the first ID column
df_matrix_divided <- apply(df_matrix[, -1], 1, divide_by_second_excluding_id)
# transpose a matrix
df_matrix_divided <- as.data.frame(t(df_matrix_divided))

# Combine the result with the ID column
df_matrix_divided_back <- cbind(df_matrix[, 1], df_matrix_divided)
rownames(df_matrix_divided_back ) <- df_matrix_divided_back [, 1]
df_matrix_divided_back <- df_matrix_divided_back[, -1]


# create a heatmap with with ggplot

heatmap_mx <- as.matrix(df_matrix_divided_back)
heatmap_mx_melt <- melt(heatmap_mx)

ggplot(heatmap_mx_melt, aes(Var2, Var1)) +
  geom_tile(aes(fill = value)) +
  labs(x = "CohortMonth", y = "CohortIndex", fill = "value") +
  scale_fill_gradient(low = "#FF8C00", high = "#8B4500", na.value = "#FFFFFF") +
  theme_classic() +
  scale_y_discrete(limits = rev(levels(factor(heatmap_mx_melt$Var1)))) +
  geom_text(aes(label = ifelse(is.na(value), "", paste0(value, "%"))), color = "white") +
  ggtitle("Retention rates") +
  theme(plot.title = element_text(hjust = 0.5))



##### 2) RFM (Recency, Frequency, Monetary) analysis

# Calculate a monetary value of each transaction

retail_uk_noNA$TotalSum <- retail_uk_noNA$Quantity*retail_uk_noNA$UnitPrice
summary(retail_uk_noNA$TotalSum)
describe(retail_uk_noNA$TotalSum)

# Set a snapshot date - the day on which we do the analysis

snapshot <- max(retail_uk_noNA$InvoiceMonth)+1
limit <- snapshot - 365

# confine the data to a period of one year
retail_rfm <- retail_uk_noNA[retail_uk_noNA$InvoiceMonth>=limit,]

# add a column with the snapshot date
retail_rfm$snapshot <- snapshot 


# calculate RFM metrics for each customer


RFM <- retail_rfm%>%group_by(
  CustomerID
) %>% summarise(
  Recency = snapshot - max(InvoiceMonth),
  Frequency = n_distinct(InvoiceNo),
  MonetaryValue = sum(TotalSum)
) 

# Add RFM scores

# convert difftime to numeric
RFM$Recency <- as.numeric(RFM$Recency)
RFM$Frequency <- as.numeric(RFM$Frequency)

# create quartiles
# commented script doesn't work, however explains the logic

# RFM <- RFM %>% mutate(R = case_when(
#   Recency <= quantile(Recency,0.25) ~ 4
#   ,Recency > quantile(Recency,0.25) & Recency <= quantile(Recency,0.5) ~ 3
#   ,Recency > quantile(Recency,0.5) & Recency <= quantile(Recency,0.75) ~ 2
#   ,TRUE ~ 1
# )
# ,"F" = case_when(
#   Frequency <= quantile(Frequency,0.25) ~ 1
#   ,Frequency > quantile(Frequency,0.25) & Frequency <= quantile(Frequency,0.5) ~ 2
#   ,Frequency > quantile(Frequency,0.5) & Frequency <= quantile(Frequency,0.75) ~ 3
#   ,TRUE ~ 4
#   )
# ,M = case_when(
#   MonetaryValue <= quantile(MonetaryValue,0.25) ~ 1
#   ,MonetaryValue > quantile(MonetaryValue,0.25) & MonetaryValue <= quantile(MonetaryValue,0.5) ~ 2
#   ,MonetaryValue > quantile(MonetaryValue,0.5) & MonetaryValue <= quantile(MonetaryValue,0.75) ~ 3
#   ,TRUE ~ 4
# ))

quantiles_R <- quantile(RFM$Recency, probs = c(0, 0.25, 0.5, 0.75, 1))
quantiles_F <- quantile(RFM$Frequency, probs = c(0, 0.25, 0.5, 0.75, 1))
quantiles_M <- quantile(RFM$MonetaryValue, probs = c(0, 0.25, 0.5, 0.75, 1))

RFM$R <- cut(RFM$Recency, 
             breaks = quantiles_R, 
             labels = c("4", "3", "2", "1"), include.lowest = TRUE)

RFM$"F" <- cut(RFM$Frequency, 
               breaks = quantiles_F, 
               labels = c("1", "2", "3", "4"), include.lowest = TRUE)

RFM$M <- cut(RFM$MonetaryValue, 
             breaks = quantiles_M, 
             labels = c("1", "2", "3", "4"), include.lowest = TRUE)

RFM$RFM_Segment <- paste0(RFM$R,RFM$"F",RFM$M)

# first convert factor to character and then to numeric
RFM$R <- as.numeric(as.character(RFM$R))
RFM$"F" <- as.numeric(as.character(RFM$"F"))
RFM$M <- as.numeric(as.character(RFM$M))

RFM$RFM_Score <- RFM$R + RFM$"F" + RFM$M

RFM_Score_groups <- RFM %>% group_by(RFM_Score) %>%
  summarise(Recency_av = mean(Recency)
            ,Frequency_av = mean(Frequency)
            ,MonetaryValue_av = mean(MonetaryValue)
            ,count = n())

RFM <- RFM %>% mutate(
  General_Segment = case_when(
    RFM_Score >= 9 ~ "Top"
    ,RFM_Score >=5 & RFM_Score < 9 ~ "Middle"
    ,TRUE ~ "Low"
  )
)

General_Segment_groups <- RFM %>% group_by(General_Segment) %>%
  summarise(
    Recency_av = mean(Recency)
    ,Frequency_av = mean(Frequency)
    ,MonetaryValue_av = mean(MonetaryValue)
    ,count = n()
  )

# Processing data for K-Means clustering

# assumptions of the algorithm

# 1) the variables distributed symmetrically
# 2) variables have similar average values and standard deviations

grid.arrange(
  
  ggplot(RFM, aes(Recency)) +
    geom_histogram(aes(y = ..density..), binwidth = 10, fill = "lightblue", color = "black") +
  geom_density() +
  labs(y = "") +
  theme_classic()

,ggplot(RFM, aes(Frequency)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightblue", color = "black") +
  geom_density() +
  labs(y = "") +
  theme_classic()

,ggplot(RFM, aes(MonetaryValue)) +
  geom_histogram(aes(y = ..density..), binwidth = 1000, fill = "lightblue", color = "black") +
  geom_density() +
  labs(y = "") +
  theme_classic()

,ncol = 1, nrow = 3)

# check the data

describe(RFM)
RFM[RFM$MonetaryValue==0,]
RFM <- RFM[!(RFM$MonetaryValue==0),]

# apply log transformation

RFM$ln_Recency <- log(RFM$Recency)
RFM$ln_Frequency <- log(RFM$Frequency)
RFM$ln_MonetaryValue <- log(RFM$MonetaryValue)

grid.arrange(
  
  ggplot(RFM, aes(ln_Recency)) +
    geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black") +
    geom_density() +
    labs(y = "") +
    theme_classic()
  
  ,ggplot(RFM, aes(ln_Frequency)) +
    geom_histogram(aes(y = ..density..),  fill = "lightblue", color = "black") +
    geom_density() +
    labs(y = "") +
    theme_classic()
  
  ,ggplot(RFM, aes(ln_MonetaryValue)) +
    geom_histogram(aes(y = ..density..),  fill = "lightblue", color = "black") +
    geom_density() +
    labs(y = "") +
    theme_classic()
  
  ,ncol = 1, nrow = 3)

describe(RFM)

# standardize the data

RFM$sc_Recency <- scale(RFM$ln_Recency)
RFM$sc_Frequency <- scale(RFM$ln_Frequency)
RFM$sc_MonetaryValue <- scale(RFM$ln_MonetaryValue)


grid.arrange(

  ggplot(RFM, aes(sc_Recency)) +
    geom_histogram(aes(y = ..density..),  fill = "lightblue", color = "black") +
    geom_density() +
    labs(y = "") +
    theme_classic()

  ,ggplot(RFM, aes(sc_Frequency)) +
    geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black") +
    geom_density() +
    labs(y = "") +
    theme_classic()

  ,ggplot(RFM, aes(sc_MonetaryValue)) +
    geom_histogram(aes(y = ..density..), fill = "lightblue", color = "black") +
    geom_density() +
    labs(y = "") +
    theme_classic()

  ,ncol = 1, nrow = 3)

describe(RFM)

# Clustering with K-Means

rfm_norm <- RFM%>%ungroup()%>%select(sc_Recency,sc_Frequency,sc_MonetaryValue)

# Define optimal number of clusters using elbow method

# max_clusters is the maximum number of clusters to test
max_clusters <- 20
wcss <- vector("numeric", max_clusters)

for (i in 1:max_clusters) {
  kmeans_model <- kmeans(rfm_norm, centers = i, nstart = 25)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot the elbow curve
par(mar = c(2, 2, 2, 2)) #adjust margines of the plot
plot(1:max_clusters, wcss, type = "b", xlab = "Number of Clusters",
     ylab = "Within-Cluster Sum of Squares (WCSS)")

# the plot suggests 3 clusters ("elbow" point)

num_clusters <- 4  

kmeans_model <- kmeans(rfm_norm, centers = num_clusters, nstart = 25)

# Add cluster labels to the original RFM data
rfm_data_clustered <- cbind(RFM, Cluster = kmeans_model$cluster)

# View cluster centers
kmeans_model$centers

# Visualize the clusters using a scatter plot (using factoextra package)
fviz_cluster(kmeans_model, data = rfm_norm, stand = FALSE,
             geom = "point", ellipse.type = "convex", ggtheme = theme_minimal())

# Compare the values mean RFM values in "manual" and K-Means clusters

RFM %>% group_by(General_Segment) %>% 
  summarise (R_mean = mean(Recency)
             ,F_mean = mean(Frequency)
             ,M_mean = mean(MonetaryValue))

rfm_data_clustered %>% group_by(Cluster) %>%
  summarise (R_mean = mean(Recency)
             ,F_mean = mean(Frequency)
             ,M_mean = mean(MonetaryValue))

# create snake plot
data_norm_k4 <- rfm_data_clustered %>%
  select(CustomerID, Cluster, sc_Recency, sc_Frequency, sc_MonetaryValue)
data_melt <- melt(data_norm_k4,
                  id.vars = c("CustomerID", "Cluster"),
                  measure.vars = c("sc_Frequency", "sc_MonetaryValue", "sc_Recency"),
                  variable.name = "Attribute",
                  value.name = "Value") 

agg_data_melt <- data_melt %>% group_by(Cluster, Attribute) %>%
  summarise(mean_Value = mean(Value)) %>% ungroup()

ggplot(data = agg_data_melt, aes(x = Attribute, y = mean_Value, 
                                 color = factor(Cluster), group = Cluster)) +
  geom_line() +
  labs(title = "Snake plot of standardized variables") +
  theme_minimal()
