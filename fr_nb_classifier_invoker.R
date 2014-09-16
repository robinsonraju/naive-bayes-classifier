library(caret)
VERBOSE = FALSE

## Read Data
# Read data from CSV
full_data <- read.csv("tax_ev.csv", header=TRUE)
num_rows <- nrow(full_data) 

# 70% is training, 30% is testing data
seventy_perct <- round(0.7 * num_rows , 0)
trng_data <- full_data[1:seventy_perct,]
testing_data <- full_data[seventy_perct:num_rows,]

# Set rownames as the Tid column
rownames(trng_data) <- trng_data[,1]
# Remove Tid column since it is not a data column
trng_data <- trng_data[,-1]

######## Training ############
if (VERBOSE)
    print("Training the classifier")
# Call the classifier to compute probabilities
classifier <- fr.classifier(trng_data)
if (VERBOSE)
    print(classifier)

######## Testing ############
if (VERBOSE)
    print("Testing the classifier")
# Use classifier to predict the class of input data
input_classes <- as.character(testing_data[, ncol(testing_data)])
output_classes <- c()
testing_data <- testing_data[,-c(1,ncol(testing_data))]

for(i in 1:nrow(testing_data)) {
    data_rec <- testing_data[i,]
    output_classes[i] <- fr.predictor(data_rec,classifier,trng_data)
}

# Confusion matrix
cm <- confusionMatrix(input_classes, output_classes)
if (VERBOSE)
    print(cm)



