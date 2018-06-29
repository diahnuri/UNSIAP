# Daejeon, June 2018
# Behavior Based Customer Segmentation using E-commerce
# Data from http://archive.ics.uci.edu/ml/datasets/online+retail#
setwd("D:/R Online Course/code on the spot")
library(dplyr)
#readxl::read_excel("Online Retail.xlsx")
data <- readxl::read_excel("Online Retail.xlsx")
str(data)
head(data, 10)
summary(data)

# The data contains:
# InvoiceNo, StockCode, Description, Quantity,
# InvoiceData, UnitPrice, CustomerID, Country
# Dalam customer behavior segmentation kita butuh tiga aspek yaitu:
# a. recency: how recently did customer purchase?
# b. frequency: how often did our customer purchase our porducts?
# c. monetary: how is the total amount the customer has purchased?


# WRANGLING DATA:
library(dplyr)
# 2.1. Select appropriate variables
dataset <- data[c("InvoiceNo", "StockCode", "Quantity", "UnitPrice", "InvoiceDate", "CustomerID", "Description", "Country")]
# 2.2 Delete Quantity and Unit Price < 0
dataset %>% filter(Quantity > 0 & UnitPrice > 0)
# 2.3 Create Amount Variable
dataset["Amount"] <- dataset["Quantity"] * dataset["UnitPrice"]
head(dataset, 3)
x11()
boxplot(data$Quantity~data$UnitPrice, data = data, main="Quantity",
        xlab="Unit Price", ylab="Quantity")

# 2.4 Split InvoiceDate into Date and Time
dataset["Date"] <- format(as.POSIXct(strptime(dataset$InvoiceDate, "%Y-%m-%d %H:%M", tz="")), format="%Y-%m-%d")
dataset["Time"]  <- format(as.POSIXct(strptime(dataset$InvoiceDate, "%Y-%m-%d %H:%M", tz="")), format="%H:%M")
# 2.5 Contry == United Kingdom
dataset %>% filter(Country == "United Kingdom")
head(dataset, 3)


#Create RFM Variables
# 2.7 Create New Dataset
?arrange
?summarise
df_1 <- dataset %>% arrange(CustomerID) %>% group_by(InvoiceNo) %>% summarise(TotalAmount = sum(Amount))
head(df_1, 5)
df_2 <- dataset[c("InvoiceNo", "CustomerID", "Date", "Time")]
head(df_2, 5)
total <- merge(df_1, df_2, by="InvoiceNo")
head(total, 5)
total <- total[!duplicated(total),]
head(total, 5)
rownames(total) <- 1:nrow(total)

total %>% arrange(CustomerID)
total$CustomerID <- as.factor(total$CustomerID)

Monetary <- total %>% group_by(CustomerID) %>% summarise(Monetary = mean(TotalAmount))
head(Monetary, 5)
Frequency <- total %>% group_by(CustomerID) %>% summarise(Frequency = n())
head(Frequency, 5)

total$TimeDiff <- round((as.numeric(difftime(strptime('2011-12-09', format = "%Y-%m-%d", tz = ""),
                                            strptime(total$Date, format = "%Y-%m-%d", tz = "")))/(60*60*24*30)),1)
head(total, 3)

Recency <- total %>% group_by(CustomerID) %>% summarise(Recency = max(TimeDiff))
# we already have Monetary, Recency, and Frequency data
RFM_data <- merge(merge(Recency, Frequency, by='CustomerID'), Monetary, by='CustomerID')
head(RFM_data, 3)
summary(RFM_data)

# NORMALIZING DATA
# 2.8.1 Delete Outlier
sort(RFM_data$Monetary, decreasing = T)
sort(RFM_data$Frequency, decreasing = T)

boxplot(RFM_data$Monetary)$stats
boxplot(RFM_data$Frequency)$stats

RFM_data$Monetary <- ifelse(RFM_data$Monetary >  761.3750, NA, RFM_data$Monetary)
RFM_data$Frequency <- ifelse(RFM_data$Frequency > 11, NA, RFM_data$Frequency)
RFM_data <- na.omit(RFM_data)

# 2.8.2 Normalization
dataResult <- data.frame(RFM_data[,c(2,3,4)], row.names = RFM_data$CustomerID)
head(dataResult, 3)
dataResult <- scale(dataResult)

## K-MEANS CLUSTERING
# 3.1 K-Means
set.seed(6024)
k.max <- 15
wss <- sapply(1:k.max, function(k){kmeans(dataResult, k, nstart = 50, iter.max = 15)$tot.withinss})
plot(1:k.max, wss, type = 'b', pch = 19, frame=F, col = 'red', xlab = "Number of Clusters K", ylab = "Total Within clusters sum of squares")

set.seed(6024)
k = kmeans(dataResult, 5)
k$size #number of member of each cluster

dataCluster <- data.frame(dataResult)
dataCluster$clus <- k$cluster
head(dataCluster, 3)
clusters <- data.frame(k$cluster)
head(clusters,3)

## K-Means Plotting in 3D Graph
colors <- c('red', 'blue', 'yellow', 'orange', 'green')
colors <- colors[as.numeric(dataCluster$clus)]
colors
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(dataCluster[, 1:3], xlim = -2:1.5, ylim = -1:3, zlim = -2:4, color = colors)
scatterplot3d(dataCluster[dataCluster$clus==1, 1:3], xlim = -2:1.5, ylim = -1:3, zlim = -2:4, color = "red", main = "Cluster 1" )
scatterplot3d(dataCluster[dataCluster$clus==2, 1:3], xlim = -2:1.5, ylim = -1:3, zlim = -2:4, color = "blue", main = "Cluster 2" )
scatterplot3d(dataCluster[dataCluster$clus==3, 1:3], xlim = -2:1.5, ylim = -1:3, zlim = -2:4, color = "yellow", main = "Cluster 3" )
scatterplot3d(dataCluster[dataCluster$clus==4, 1:3], xlim = -2:1.5, ylim = -1:3, zlim = -2:4, color = "orange", main = "Cluster 4" )
scatterplot3d(dataCluster[dataCluster$clus==5, 1:3], xlim = -2:1.5, ylim = -1:3, zlim = -2:4, color = "green", main = "Cluster 5" )
