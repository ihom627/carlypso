# carlypso


#load data
train <- read.csv("./Master_Quiz_wo_Answers.csv", stringsAsFactors = FALSE)

#look at summary
summary(train)

#remove useless columns
train_v2 <- subset(train, select = -c(X, X.1, X.2))

#get percentage of na entries
length(train[is.na(train)])/(ncol(train)*nrow(train))

#count number of rows before removing na entries
nrow(train_v2)

#remove rows with na entries
train_v3 <- na.omit(train_v2)

#count number of rows before removing na entries
nrow(train_v3)

#look at summary
summary(train_v3)

#list range of values for columns
col_ct = sapply(train_v2, function(x) length(unique(x)))

#count numerical and character columns
train_numr = train_v3[, sapply(train_v3, is.numeric)]
train_char = train_v3[, sapply(train_v3, is.character)]
cat("Numerical column count : ", dim(train_numr)[2], 
+     "; Character column count : ", dim(train_char)[2])


#examine first 100 entries of each character columns
str(lapply(train_char, unique), vec.len = 100)

#look for na values, and convert them to NA for more predictable processing
#state na is ""
#color na is "" and "-"
train_char[train_char==""] = NA
train_char[train_char=="—"] = NA


#separate out the dates to study further
#format the column train_v3$Date.Sold
#2015-01-14T10:15:00.000Z
x <- train_v3$Date.Sold
date_sold <- strptime(x, '%Y-%m-%dT%H:%M:%S')

#plot histogram
hist(date_sold, breaks = 100)
notice dates are highly skewed, only a few cars sold at the beginning of 2014, but then nothing till the end of 2014 and all of 2015

#hist(train_v3$year, 
#     main="Histogram for Car Model Years", 
#     xlab="Passengers", 
#     border="blue", 
#     col="green",
#     xlim=c(100,700),
#     las=1, 
#     breaks=5)

#plot states
#get the number of unique states
states <- train_v3$state
states[states==""] = NA
length(unique(states))

#create a vector of unique states
states_count <- aggregate(data.frame(count = states), list(value = states), length)


#create a vector of unique states
#states_unique = unique(states)

#create a dataframe with the states and their count
#states_count = cbind(count = 1, states)
#states_count_df = as.data.frame(states_count)
names(states_count)[1] <-'state.abb'

states_count_upper = as.data.frame(sapply(states_count, toupper))

states_count_upper$states <- tolower(state.name[match(states_count_upper$state.abb, state.abb)])

#NOTE there is Puerto Rico and Canadian Provinces too

#remove na
states_count_upper_clean <- na.omit(states_count_upper)

#prepare map
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
#mapUSA <- map('world2', 'USA', fill=TRUE, plot=FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))

#idx <- match(unique(nms),  states_count_upper_clean$states)
#dat2 <- data.frame(value = states_count_upper_clean$value[idx], state = unique(nms))

all_states_names <- as.data.frame(nms)
all_states_names_unique = unique(all_states_names)
all_states_names_unique <- rename(all_states_names_unique, c(nms="states"))

#outer join
states_join = merge(x=all_states_names_unique, y=states_count_upper_clean, by = "states", all= TRUE)


states_join_clean <- subset(states_join, select = -c(state.abb))

states_join_clean <- rename(states_join_clean, c(states="state"))
states_join_clean <- rename(states_join_clean, c(count="value"))

#HACK to remove hawaii from entry
states_join_clean <- states_join_clean[-50,]


row.names(states_join_clean) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = states_join_clean)
spplot(USAsp['value'])

==> plot does not scale color map correctly, really over complicated


########################## another approach ######################

#example of other approach

tf= structure(list(state = structure(1:14, .Label = c("AK", "AL", 
"AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "IA", "IL", "IN", 
"KS"), class = "factor"), num = c(21L, 0L, 12L, 56L, 0L, 53L, 
31L, 7L, 335L, 63L, 42L, 73L, 40L, 2L), region = structure(c(2L, 
1L, 4L, 3L, 5L, 6L, 7L, 8L, 9L, 10L, 13L, 11L, 12L, 14L), .Label = c("alabama", 
"alaska", "arizona", "arkansas", "california", "colorado", "connecticut", 
"delaware", "florida", "georgia", "illinois", "indiana", "iowa", 
"kansas"), class = "factor")), .Names = c("state", "num", "region"
), class = "data.frame", row.names = c(NA, -14L))


re(maps);require(ggplot2)

states <- map_data("state")
tfmerged <- merge(states, tf, sort = FALSE, by = "region")
tfmerged <- tfmerged[order(tfmerged$order), ]
qplot(long, lat, data = tfmerged, group = group, fill = num, geom="polygon")


### Ok so try other approach with my data

#HACK in missing states
states_count_upper_clean <- na.omit(states_count_upper)

states_join = merge(x=all_states_names_unique, y=states_count_upper_clean, by = "states", all= TRUE)

states_count_upper_clean = states_join

states_count_upper_clean <- rename(states_count_upper_clean, c(states="region"))
states_count_upper_clean <- rename(states_count_upper_clean, c(count="num"))
states_count_upper_clean <- rename(states_count_upper_clean, c(state.abb="state"))

tf = states_count_upper_clean

#remove <NA> from tf$num
df1 = as.matrix(tf)
indx <- which(is.na(df1[, 3])==TRUE) 
df1[indx, 3] = 0 
final_tf = data.frame(df1)
summary(final_tf)

sapply(final_tf, mode)

#convert factor to numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
tf_foo <- transform(final_tf, num = as.numeric.factor(num))

tf = tf_foo


states <- map_data("state")
tfmerged <- merge(states, tf, sort = FALSE, by = "region")
tfmerged <- tfmerged[order(tfmerged$order), ]
qplot(long, lat, data = tfmerged, group = group, fill = num, geom="polygon")

==> heat map works!


################################# Now do analysis ####################################

#load data
train <- read.csv("./Master_Quiz_wo_Answers.csv", stringsAsFactors = TRUE)

#look at summary
summary(train)

#remove useless columns
train_v2 <- subset(train, select = -c(X, X.1, X.2))

#remove date sold (doesn't make sense it predicts the sale price)
train_v3 <- subset(train_v2, select = -c(Date.Sold))

#get percentage of na entries
length(train_v3[is.na(train_v3)])/(ncol(train_v3)*nrow(train_v3))
==> 1.1% are na

#count number of rows before removing na entries
nrow(train_v3)
==>5870

#remove rows with na entries
train_v4 <- na.omit(train_v3)

#count number of rows before removing na entries
nrow(train_v4)
==>5197

#look at summary
summary(train_v4)


#NOTE: sometimes a car sells for below its estimated cost, hmmmm
difference <- c(train_v4$Actual.Selling.Price - train_v4$Ave.Price..for.that.make.model.trim..year.)
difference


#split into training and testing
set.seed(12345)
train_rand <- train_v4[order(runif(5197)), ]

train_set <- train_rand[1:4600, ]
test_set <- train_rand[4600:5197, ]

write.table(train_set, file="./train_set", row.names = FALSE)
write.table(test_set, file="./test_set", row.names = FALSE)

####################### use Regression Trees ########################

install.packages("rpart")
library(rpart)
summary(train_v4)
model_rpart <- rpart(Actual.Selling.Price ~ ., data = train_v4)
model_rpart
summary(model_rpart)

predict_rpart <- predict(model_rpart, train_v4)
summary(predict_rpart)
summary(train_v4$Actual.Selling.Price )

cor(predict_rpart, train_v4$Actual.Selling.Price)

#plot regression tree
library(rpart.plot)
install.packages("rpart plot")
version
rpart.plot(model_rpart, digits = 3)

#calc MAE
MAE(predict_rpart, train_v4$Actual.Selling.Price)
install.packages("hydroGOF")
library(hydroGOF)
MAE(predict_rpart, train_v4$Actual.Selling.Price)
mae(predict_rpart, train_v4$Actual.Selling.Price)



###### import into h20 ###########
tried h20 but was not able to generate ROC (not sure if that is possible) 



######## attempt to use decision tree, but need to convert Avg Sales Price to Factors rather than numeric

#convert numeric actual selling prices as factors for sales price 
train_v4$actual_selling_price_as_factors <- cut(train_v4$Actual.Selling.Price,breaks=0:100000)

#remove numeric actual selling prices
train_v5 <- subset(train_v4, select = -c(Actual.Selling.Price))
summary(train_v5)

#randomize rows of data
set.seed(12345)
train_rand <- train_v5[order(runif(5197)), ]

#split into training and testing
train_set <- train_rand[1:4600, ]
test_set <- train_rand[4600:5197, ]

#run C5.0 decision tree
library(C50)

model <- C5.0(train_set[-10], train_set$actual_selling_price_as_factors)

==> not creating model due to an error with input dataframe









####### misc ########

write.table(train_v5, file="./train_v5", row.names = FALSE)





