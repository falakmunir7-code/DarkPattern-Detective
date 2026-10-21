
# DarkPattern Detective
# Step 1: Load the dataset

data <- read.delim(
  "dataset.tsv",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Check the dataset
head(data)
colnames(data)
dim(data)
colSums(is.na(data))



# Step 2: Count the different labels

table(data$label)

# Count each pattern category
table(data$`Pattern Category`)

# Display the results as percentages
round(prop.table(table(data$label)) * 100, 2)


# Step 3: Clean the text

# Convert text to lowercase
data$clean_text <- tolower(data$text)

# Remove extra spaces at the beginning and end
data$clean_text <- trimws(data$clean_text)

# Replace multiple spaces with a single space
data$clean_text <- gsub("\\s+", " ", data$clean_text)

# View original and cleaned text
head(data[, c("text", "clean_text")], 10)



# Step 4: Split data into training and testing sets

set.seed(123)

# Randomly shuffle the row numbers
total_rows <- nrow(data)
random_rows <- sample(total_rows)

# Calculate 80% of the data
train_size <- floor(0.8 * total_rows)

# Create training data (80%)
train_data <- data[random_rows[1:train_size], ]

# Create testing data (20%)
test_data <- data[random_rows[(train_size + 1):total_rows], ]

# Check the sizes
nrow(train_data)
nrow(test_data)


install.packages("tm")
install.packages("e1071")


# Step 5: Load the packages

library(tm)
library(e1071)

# Convert text into a document-term matrix
corpus <- VCorpus(VectorSource(data$clean_text))

# Convert text to lowercase and remove punctuation
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)

# Convert text into word-frequency features
dtm <- DocumentTermMatrix(corpus)

# Convert the matrix into a regular data frame
text_features <- as.data.frame(as.matrix(dtm))

# Add the target label
text_features$label <- factor(data$label)

# Create training and testing datasets
train_features <- text_features[random_rows[1:train_size], ]
test_features <- text_features[random_rows[(train_size + 1):total_rows], ]

# Train the Naive Bayes model
model <- naiveBayes(label ~ ., data = train_features)

# Display the model
print(model)



# Step 6: Predict using the test data

predictions <- predict(
  model,
  newdata = test_features[, names(test_features) != "label"],
  type = "class"
)

# Compare predictions with actual labels
confusion_matrix <- table(
  Actual = test_features$label,
  Predicted = predictions
)

print(confusion_matrix)

# Calculate accuracy
accuracy <- mean(predictions == test_features$label) * 100

cat("Model Accuracy:", round(accuracy, 2), "%\n")



# Step 7: Improve the text features

# Keep only words that appear in at least 5 documents
dtm_small <- DocumentTermMatrix(
  corpus,
  control = list(
    bounds = list(global = c(5, Inf))
  )
)

# Convert to a data frame
text_features2 <- as.data.frame(as.matrix(dtm_small))

# Add labels
text_features2$label <- factor(
  data$label,
  levels = c(0, 1)
)

# Create training and testing data
train_features2 <- text_features2[
  random_rows[1:train_size], ]

test_features2 <- text_features2[
  random_rows[(train_size + 1):total_rows], ]

# Train a new model
model2 <- naiveBayes(
  label ~ .,
  data = train_features2,
  laplace = 1
)

# Make predictions
predictions2 <- predict(
  model2,
  newdata = test_features2[
    , names(test_features2) != "label"
  ],
  type = "class"
)

# Check results
print(table(
  Actual = test_features2$label,
  Predicted = predictions2
))

accuracy2 <- mean(
  predictions2 == test_features2$label
) * 100

cat("Improved Model Accuracy:",
    round(accuracy2, 2), "%\n")




# Step 8: Create a prediction function

predict_dark_pattern <- function(user_text) {
  
  # Convert input to lowercase
  user_text <- tolower(user_text)
  
  # Remove punctuation
  user_text <- gsub("[[:punct:]]", " ", user_text)
  
  # Remove extra spaces
  user_text <- gsub("\\s+", " ", user_text)
  user_text <- trimws(user_text)
  
  # Find words from the training vocabulary
  vocabulary <- colnames(dtm_small)
  words <- unlist(strsplit(user_text, " "))
  
  # Create a row of word counts
  new_text <- matrix(
    0,
    nrow = 1,
    ncol = length(vocabulary),
    dimnames = list(NULL, vocabulary)
  )
  
  matched_words <- words[words %in% vocabulary]
  
  for (word in matched_words) {
    new_text[1, word] <- new_text[1, word] + 1
  }
  
  # Predict the class
  result <- predict(
    model2,
    newdata = as.data.frame(new_text),
    type = "class"
  )
  
  if (as.character(result) == "1") {
    return("Potential Dark Pattern")
  } else {
    return("Not Dark Pattern")
  }
}

# Test the function
predict_dark_pattern(
  "Hurry! Only 2 items left in stock"
)



# Step 9: Test different messages

test_messages <- c(
  "Hurry! Only 2 items left in stock",
  "FLASH SALE limited time only",
  "Pillowcases and shams",
  "Read our shipping policy",
  "Thousands of customers love this product",
  "Subscribe to our newsletter"
)

# Predict each message
for (msg in test_messages) {
  cat("\nMessage:", msg, "\n")
  cat("Prediction:", predict_dark_pattern(msg), "\n")
}

