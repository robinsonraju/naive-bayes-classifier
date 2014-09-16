VERBOSE = FALSE

## This function computes the following
## probability of the categorical class 
## conditional probabilities of all features with reference to the class 
## It takes a data frame with the assumption that the last column is the categorical class and 
## other columns are features. 
## The output is a list of tables(matrices) with probablity of the class and 
## conditional probabilities of all the features. 
fr.classifier <- function(trng_data) {
    if (VERBOSE) {
        print("Dimensions of input data")
        print(dim(trng_data))
    }
    
    if (VERBOSE)
        print("Calculating a priori")
    
    # Number of columns
    number_of_cols <- length(names(trng_data))
    
    # Class column is the last column 
    class_col = number_of_cols
    
    ## Distribution of the class
    class_data <- trng_data[,class_col]
    class_summary <- summary(class_data)
    class_names <- names(class_summary)
    no_of_classes <- length(class_names)
    
    a_priory_table <- matrix(NA, nrow=1, ncol=no_of_classes)
    colnames(a_priory_table) <- class_names
    
    for (i in 1:no_of_classes) {
        a_priory_table[i] <- class_summary[class_names[i]]/length(class_data)
    }
    
    if (VERBOSE) {
        print("Class distribution")
        print(a_priory_table)
    }
    
    if (VERBOSE) 
        print("Iterating through all features to create table for each")
    
    ## Iterate through all features and create table for each
    feature_data <- trng_data[,1:class_col-1]
    f_matrix_list <- list()
    
    feature_names <- names(feature_data)
    feature_type_table <- matrix(NA, nrow=length(feature_names), ncol=1)
    rownames(feature_type_table) <- feature_names
    for (i in 1:ncol(feature_data)){
        feature_type_table[i] <- class(feature_data[,i])
    }
    
    for (i in 1:ncol(feature_data)){
        col_data <- feature_data[,i]
        if (class(col_data) == "factor") {
            f_names <- names(summary(col_data))
            p_table <- matrix(NA, nrow=length(f_names), ncol=no_of_classes)
            colnames(p_table) <- class_names
            rownames(p_table) <- f_names
            f_matrix_list[[i]] <- p_table
        } else if (class(col_data) == "integer") {
            p_table <- matrix(NA, nrow=length(col_data), ncol=no_of_classes)
            colnames(p_table) <- class_names
            rownames(p_table) <- col_data
            f_matrix_list[[i]] <- p_table
        } else {
            # ignore for now
        }
    }
    
    if (VERBOSE) 
        print("Filling in the feature tables with conditional probabilities")
    ## Fill the feature tables with conditional probabilities
    ## Conditional Probabilities
    for (i in 1:no_of_classes) {
        # Featureset for each class
        features <- trng_data[class_data==class_names[i],]
        num_of_features = ncol(features) - 1
        size_of_class = nrow(features)
        
        # Iterate through features
        for (j in 1:num_of_features) {
            f_data <- features[,j]
            if (class(f_data) == "factor") {
                f_summary <- summary(f_data)
                f_names <- names(f_summary)
                no_of_values <- length(f_names)
                
                ## Get the table for this feature
                f_table <- f_matrix_list[[j]]
                for (k in 1:nrow(f_table)) {
                    f_table[k,i] = f_summary[f_names[k]]/size_of_class
                }
                f_matrix_list[[j]] <- f_table
            } else if (class(col_data) == "integer") {
                mean <- mean(f_data)
                variance <- sd(f_data)^2
                
                ## Get the table for this feature
                f_table <- f_matrix_list[[j]]
                f_names <- rownames(f_table)
                for (k in 1:nrow(f_table)) {
                    curr_val <- as.numeric(f_names[k])
                    f_table[k,i] = (1/sqrt(2*pi*variance))*exp(-(curr_val-mean)^2/(2*variance))
                }
                f_matrix_list[[j]] <- f_table
            } else {
                # ignore for now
            }
        }
    }
    
    # Set names of each of tables for access later
    names(f_matrix_list) <- names(feature_data)
    f_matrix_list$a_priori <- a_priory_table
    f_matrix_list$feature_type_table <- feature_type_table
    
    # Return the list of tables
    f_matrix_list
}

fr.predictor <- function(data_rec, classifier, trng_data){
    final_class <- ""
    highest_prob <- 0
    class_names <- colnames(classifier$a_priori)
    for (i in 1:length(class_names)) {
        class_name <- class_names[i]
        
        f_names <- names(data_rec)
        f_prob <- 1
        for (j in 1:ncol(data_rec)) {
            factor_name <- f_names[j]
            factor_value <- data_rec[[factor_name]]
            
            # Get the specific table from classifier
            f_table <- classifier[[factor_name]]
            
            # Get the type of feature
            f_type <- classifier$feature_type_table[factor_name,]
            if (f_type == "factor") {
                # Get the specific value.
                val <- f_table[factor_value, class_name]
                f_prob <- f_prob * val
            } else if (f_type == "integer") {
                f_values <- rownames(f_table)
                if (factor_value %in% f_values) {
                    factor_value <- as.character(factor_value)
                    val <- f_table[factor_value, class_name]
                    f_prob <- f_prob * val
                } else {
                    curr_val <- as.numeric(factor_value)
                    
                    # Trng data is passed only for this. Optimize later. 
                    class_data <- trng_data[,length(names(trng_data))]
                    f_data <- trng_data[class_data==class_names[i],j]
                    mean <- mean(f_data)
                    variance <- sd(f_data)^2
                    
                    val = (1/sqrt(2*pi*variance))*exp(-(curr_val-mean)^2/(2*variance))
                    f_prob <- f_prob * val
                }
            } else {
                # Do nothing
            }
        }
        
        if (f_prob > highest_prob) {
            final_class <- class_name
            highest_prob <- f_prob
        }
    }
    final_class
}