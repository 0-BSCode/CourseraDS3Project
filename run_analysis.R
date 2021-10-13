library(dplyr)

# Load in data sets
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
subjects_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subjects_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Place in labels for each x data set
headers <- features[,2]
colnames(x_test) <- headers
colnames(x_train) <- headers

# Label columns in y data sets
y_label <- c("Activity")
colnames(y_test) <- y_label
colnames(y_train) <- y_label

# Merge training data set with test data set and create final merged data set
x_tt <- rbind(x_train, x_test)
y_tt <- rbind(y_train, y_test)
merged_data <- cbind(x_tt, y_tt)

# Create function to extract standard deviation and mean
extractData <- function(data) {
        reg_ex <- paste(data, "\\(\\)", sep="")
        filter <- grep(reg_ex, colnames(merged_data))
        new_data <- merged_data[, filter]
}

# Extract mean and standard deviations of each measurement
mean_measures <- extractData("mean")
std_measures <- extractData("std")

# Create function to assign activity labels to numbers
assignCode <- function() {
        acts <- labels$V2
        code <- lapply(merged_data$Activity, function(i){acts[i]})
}

# Take out numeric Activity column for second data set
activities_num <- merged_data$Activity

# Replace numbers with actual activity labels
merged_data$Activity <- assignCode()

# Create function to merge training subjects and test subjects
assignSubjects <- function() {
        subjects <- rbind(subjects_train, subjects_test)
        colnames(subjects) <- c("Subjects")
        subjects
}

# Add subjects to data set
merged_data$Subjects <- assignSubjects()

manageDuplicates <- function() {
        
        # Store column names for data set
        data_columns <- colnames(merged_data)
        
        # Create X, Y, and Z characters to assign to duplicate column names
        dims <- c("X", "Y", "Z")
        
        # Create index to iterate through dims vector
        index <- 0
        
        # Locate names of duplicated columns
        duplicate_columns <- merged_data[,duplicated(data_columns)]
        
        # Go through each column name that's duplicated
        for (elem in colnames(duplicate_columns)) {
                
                # Find indices of duplicated column name in data_columns
                instances <- which(data_columns %in% elem)
                for (e in instances) {
                        
                        # Assign an X, Y, or Z character to the end of said
                        # column name
                        index <- (index %% 3) + 1
                        data_columns[e] <- paste(data_columns[e], dims[index]
                                                 , sep=" ")
                }
        }
        
        colnames(merged_data) <<- data_columns
}

# Manage duplicate columns before grouping them by activity and subject
manageDuplicates()

createSummary <- function() {
        grp <- merged_data %>% group_by(Activity, Subjects)
        summary <- summarise_all(grp, mean)
        summary
}

# Create second data set with average of each variable by activity and subject and write it to a file
s <- createSummary()
write.table(s, "./data2.txt")