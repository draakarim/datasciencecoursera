@@ -0,0 +1,64 @@
# Read and bind X training and testing

trainX <- read.table("/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/train/X_train.txt")
testX  <- read.table("/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/train/X_train.txt")
dataX  <- rbind(trainX, testX)

# Read and bind S training and testing

trainS <- read.table("/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/train/subject_train.txt")
testS  <- read.table("/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/train/subject_train.txt")
dataS  <- rbind(trainS, testS)

# Read and bind Y training and testing

trainY <- read.table("/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/train/y_train.txt")
testY  <- read.table("/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/train/y_train.txt")
dataY  <- rbind(trainY, testY)

# Read features and assign names to X data

features <- read.table("/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/features.txt")
meanIndex=grep("*mean\\(\\)", features[, 2])
stdIndex=grep("*std\\(\\)", features[, 2])
meanStdIndexs= c(meanIndex,stdIndex)
dataX <- dataX[, meanStdIndexs]
namesVect = features[meanStdIndexs, 2]
namesVect = tolower(gsub("\\(|\\)", "", namesVect))
names(dataX) = namesVect

# Read activities and assign names to Y and S data 
# labels the data set with descriptive activity names

activities <- read.table("/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
dataY[,1] = activities[dataY[,1], 2]
names(dataY) <- "activity"
names(dataS) <- "subject"
allData <- cbind(dataS, dataY, dataX)
write.table(allData, "/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/allData.txt")

# Generate the average of each variable for each activity and each subject
# Create the required tidy data

uniqueSub = unique(dataS)[,1]
numSub    = length(uniqueSub)
numAct    = length(activities[,1])
colNums   = dim(allData)[2]
dataResult    = allData[1:(numSub*numAct), ]

r = 1
for (s in 1:numSub) {
  for (a in 1:numAct) {
    dataResult[r, 1] = uniqueSub[s]
    dataResult[r, 2] = activities[a, 2]
    z <- allData[allData$subject==s & allData$activity==activities[a, 2], ]
    dataResult[r, 3:colNums] <- colMeans(z[, 3:colNums])
    r = r+1
  }
}


# Write the required tidy data

write.table(dataResult, "/Users/arsalankarim/Desktop/datasciencecoursera/UCI HAR Dataset/tidy.txt")
