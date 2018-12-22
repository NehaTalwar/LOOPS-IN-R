#CREATING LOOPS IN R USING THE WEATHER DATA OF 4 U.S CITIES
#read the file in my R studio and name it chicago
chicago <- read.csv(file.choose(), row.names = 1)

# Task1: To Find the Average Row Wise
chicago[1,]  # Give me the data of the 1st row and all columns

# Convert the Chicago DF into a matrix
chicago.matx <- as.matrix(chicago)
mean(chicago.matx[1,])

# Create a Blank variable and then a for loop 
output <- NULL
for(rowno in 1:nrow(chicago.matx)){
  output[rowno] <-  mean(chicago.matx[rowno,])
  
}
names(output) <-  rownames(chicago.matx)

#R Functions meant for Loop
# apply(X, MARGIN, FUN, ...)
apply(chicago,1, mean)

#task 2
#Take all the Dataframes and make a list of it...

Houston <- read.csv(file.choose(),row.names=1)
NewYork <- read.csv(file.choose(),row.names=1)
San.Francisco <-read.csv(file.choose(),row.names=1)

weather <-  list("chicago" = chicago, "Houston" = Houston , "NewYork" = NewYork,
                 "San.Francisco" = San.Francisco)

#Indexing through lists
weather[[4]][1,]
weather$Houston[2,]

# Transpose the Weather list, t stands for transpose here
lapply(weather,t)

# Task1: Find the Average of all the metrics of all cities
apply(chicago,1,mean)
apply(Houston,1,mean)
apply(San.Francisco,1,mean)
apply(NewYork,1,mean)

lapply(weather, rowMeans)
sapply(weather, rowMeans)

# Functions that are user based/customized.
missu <- function(x){
  return(sum(is.na(x)))
  }
sapply(chicago , missu)
# or we could check our NA values 
sapply(chicago,function(x)sum(is.na(x)))

# Task02: Find the Temp Fluctuation from min and Max(). Take Min Temp as BASE
# Max-min/min
lapply(weather, "[" , 1,)#gives the values of the first row of each city in weather

lapply(weather, function(i) round((i[1,]-i[2,])/i[2,],2))

# Deliverable #3 & 4: Find Annual Max/Min for all the cities in the list...
# lapply(x, func..)

findmax <-  function(i){
  return(apply(i,1,max))
  }
lapply(weather,findmax)
#OR
lapply(weather, function(i)apply(i,1,max))
lapply(weather, function(i) apply(i,1,min))
sapply(weather, function(i) apply(i,1,min))

# Find the Month that has max value row wise...
# First go in every city and then go in every row.... two functions

names(which.max(Houston[1,]))

apply(Houston,1, function(x)names(which.max(x[])))

lapply(weather, function(cities) apply(cities,1,function(x)
  names(which.max(x[]))))

sapply(weather, function(cities) apply(cities,1,function(x)
  names(which.max(x[]))))












