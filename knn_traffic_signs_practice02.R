#  the k-NN algorithm can be implemented using the class package
library('class')

# ---------------- data preparation ----------------------------

# load the knn_traffic_signs.csv -> info about traffic signs, pixel color values
# id: unique identifier for each observation
# sample: (train/test) indicate whether the observation is part of the training/test set
# sign_type: the type of traffic sign (pedestrian, stop, speed) 
# r1, g1, b1,..., r16, g16, b16 -> RGB values for 16 different center pixels on the traffic sign image

traffic_signs <- read.csv('https://assets.datacamp.com/production/course_2906/datasets/knn_traffic_signs.csv')
# View(traffic_signs)
# str(traffic_signs)

# normalize numerical data
#  min-max normalization normalization
normalize <- function(vect_x) {
  return ((vect_x - min(vect_x)) / (max(vect_x) - min(vect_x)))
}

# the categorical cols & identifier col shouldn't be normalized
# Exclude the 'id', 'sample', and 'sign_type' columns
numeric_columns <- traffic_signs[-c(1, 2, 3)]   
# numeric_columns <- traffic_signs[, 4:ncol(traffic_signs)]  # Columns r1 to b16

# Check if they are indeed numeric (convert if needed)
numeric_columns <- data.frame(lapply(numeric_columns, 
                                     function(col) as.numeric(as.character(col))))

# Apply normalization to the numeric columns
# use 'lapply' and convert the result back to a data frame
normalized_columns <- as.data.frame(lapply(numeric_columns, normalize))

# Combine the normalized columns with the original categorical columns
normalized_traffic_signs <- cbind(traffic_signs[, c("id", "sample", "sign_type")], normalized_columns)

# Check the summary of the normalized dataset, particularly for 'r1'
# summary(normalized_traffic_signs$r1)   # min = 0, max = 1

# the 'sign_type' column from the traffic_signs dataset
# table(traffic_signs$sign_type)   # pedestrian: 65, speed: 70, stop: 71
# table(traffic_signs$sample)      # approx. 70% train data

# to handle text directly (prevent character data from being automatically converted to factors)
StringAsFactor = F     

# turn off warnings (i.e warning NAs introduced by coercion)
options(warn=-1)    # warn = negative int -> off, warn = 0 -> on
# bro P : error handling (try - except in Python), 
# as.integer(" "); 1 2__ (trailing spaces) -> NAs introduced by coercion

# get the training data
signs <- subset(normalized_traffic_signs, sample=="train")

# examine  the structure of the dataset signs with str() function
# str(signs)

# remove 2 un-useful columns (identifier & sample column)
# signs$id <- NULL
signs$sample <- NULL
# View(signs)

# (use all cols, except 1st col i.e the identifier col 
# & the 2nd col i.e the sample col = the classification labels col)
training_data = signs[-c(1, 2)] 
# View(training_data)

# the sign_type = a vector of labels (classifications) for the training data = train_labels 
sign_types <- signs$sign_type
# check r10's average red level by sign_type
aggregate(r10~sign_types, data=signs, mean)

# the test observation (i.e the data we want to classify)
next_sign <- subset(normalized_traffic_signs, sample == "example")     # the final row of traffic_signs
# remove cols id, sample, sign_type from next_sign
next_sign$id <- NULL
next_sign$sample <- NULL
# The sign_type is removed because it’s the label that we are trying to predict.
next_sign$sign_type <- NULL
# View(next_sign)

# E1: classify the next_ sign, baseline model k = 1 (default)
predicted_label <- knn(train=training_data, test=next_sign, cl=sign_types, k=5)
print(paste("predicted label:", predicted_label))

# inspect k nearest neighbors manually
# Calculate Euclidean distances between `next_sign` and `train_data`
num_data_points= nrow(training_data)
distances <- replicate(num_data_points, 0)
for (i in 1:num_data_points){
  distances[i] <- sqrt(sum((training_data[i,] - next_sign)^2))
}
# print(distances)

# add new column 'euclid_dist' to signs
signs$euclid_dist = distances
# View(signs)

# Sort and get the indices of the k nearest neighbors
# num_neighbors : odd, near sqrt(num_data_points), > length(table(traffic_signs$sign_type))
num_neighbors <- 10
k_nearest_distances <- sort(distances)[1:num_neighbors]
# print(k_nearest_distances)

result <- data.frame(result_id=signs$id, 
                     result_sign_type = signs$sign_type, 
                     dist = signs$euclid_dist)
# View(result)
# sort the data frame result by the result$dist col in ascending order
sorted_result <- result[order(result$dist), ]
# View(sorted_result)
k_nearest_neighbors <- sorted_result[1: num_neighbors, ]
print(k_nearest_neighbors)
# Majority Voting for Class Label:
# Convert col k_nearest_neighbors$result_sign_type to a single string separated by spaces
k_nearest_classes <- paste(k_nearest_neighbors$result_sign_type, collapse = " ")
# split the k_nearest_classes string into words
k_nearest_classes <- strsplit(k_nearest_classes, " ")[[1]]

# Convert string to tibble (modern version of data.frame)
df <- tibble(k_nearest_classes = k_nearest_classes)
# Count the frequency of each class
df_count <- df %>% count(k_nearest_classes)
majority_vote <- df_count %>% 
  filter(n==max(n)) %>%     # keep only the rows where the frequency (n) is equal to the maximum frequency
  pull(k_nearest_classes)   # pull(k_nearest_classes): extracts the class labels from the filtered result

majority_vote

# E2: Classify a collection of road signs
test_signs = subset(normalized_traffic_signs, sample =="test")
test_signs$sample <- NULL     # remove the sample col from test_signs

# to free up space:
# remove the entire traffic_signs data frame from memory since it's no longer needed
rm(traffic_signs)
rm(normalized_traffic_signs)

# use kNN to identify the test road signs
# remove cols: id, sign_type, euclid_dist from signs
signs_pred <- knn(train=signs[-c(1, 2, 49)], test=test_signs[-c(1, 2)], cl=sign_types)
# Error in knn(train = signs[-c(1, 2)], test = test_signs[-c(1, 2)], cl = sign_types) : 
# dims of 'test' and 'train' differ
# print(signs_pred)

# create a confusion matrix of the predicted versus actual values
signs_actual <- test_signs$sign_type
print(table(signs_pred, signs_actual))   # cross-tabulate signs_pred & signs_actual

# compute the accuracy
print(paste("overall accuracy =", mean(signs_pred == signs_actual)))   # 0.440678 - low accuracy

# bigger k doesn't mean better
# testing other k values
k_vals <- c(1, 3, 7, 15)
best_k <- 1
best_accuracy <- 0
for (num_k in k_vals){
  
  # use the "prob" parameter to get the proportion of votes for the winning class
  signs_pred <- knn(train=signs[-c(1, 2, 49)], 
                    test=test_signs[-c(1, 2)], 
                    cl=sign_types,
                    k = num_k,
                    prob = TRUE)      
  # Examine the first several predictions
  # head(sign_pred)
  
  # get the "prob" attribute from the predicted classes
  # View(sign_prob) -> Show attributes -> prob: double[59]   0.857 1...
  # => whether the voters were unanimous or widely separated
  # sign_prob <- attr(sign_pred, "prob")
  
  # Examine the proportion of votes for the winning class
  # head(sign_prob)
  
  # create a confusion matrix of the predicted values versus actual values
  signs_actual <- test_signs$sign_type
  print(table(signs_pred, signs_actual))
  
  # compute the accuracy
  overall_accuracy <- mean(signs_pred == signs_actual)
  print(paste("k = ", num_k, "overall accuracy =", overall_accuracy))
  if (overall_accuracy > best_accuracy){
    best_accuracy <- overall_accuracy
    best_k <- num_k
  }
}
print(paste("best_k = ", best_k, "best_accuracy = ", best_accuracy))
