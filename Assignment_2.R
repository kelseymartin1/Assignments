data(iris)
head(iris)

sp_ids <- unique(iris$Species)

output <- matrix(0, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(output) <- sp_ids
colnames(output) <- names(iris[ , -ncol(iris)])

for(i in seq_along(sp_ids)) {
  iris_sp <- subset(iris, subset=Species == sp_ids[i], select=-Species)
  for(j in 1:(ncol(iris_sp))) {
    x <- 0
    y <- 0
    if (nrow(iris_sp) > 0) {
      for(k in 1:nrow(iris_sp)) {
        x <- x + iris_sp[k, j]
        y <- y + 1
      }
      output[i, j] <- x / y 
    }
  }
}
output

#' 1. The loops created a data set of the average of each row and column.

#' 2. Pseudo-code:

# 'unique' returns the three unique species in the data. This creates a species list. The list is named 'species_ids'
sp_ids <- unique(iris$Species)

# This sets up a table that has the same number of rows as species and the number of columns minus 1.
# This also adds row names and column names
# The name of the matrix is 'output'
output <- matrix(0, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(output) <- sp_ids
colnames(output) <- names(iris[ , -ncol(iris)])

# Sequence along counts the species (1 2 3). This loops through the 3 species
for(i in seq_along(sp_ids)) {
  # Subset the data down to specific species, drop species column. 
  # '-Species' selects all of the columns except the column Species
  iris_sp <- subset(iris, subset=Species == sp_ids[i], select=-Species)
  # Loop through column 1 to the last column of species
  for(j in 1:(ncol(iris_sp))) {
    # set x and y equal to 0 between traits
    x <- 0  
    y <- 0
    # If the number of rows of species is greater than 0...(which they are)
    if (nrow(iris_sp) > 0) {
      # ... Then loop through row 1 to the last row of species
      for(k in 1:nrow(iris_sp)) {
        # set x = the sum of iris species
        x <- x + iris_sp[k, j]
        x
        #set y = the number of rows
        y <- y + 1
        y
      }
      # output equals the sum of iris species divided by y (the number of rows).
      # This basically calculates the average.
      output[i, j] <- x / y 
    }
  }
}
# 'Output' is a matrix of the average of each trait for each species.
output

#' 3. Renaming variables

# To be clearer, 'Output' should be named 'trait_avg', 'x' should be named 'trait_sum', and 'y' should be named 'n_samples'.

sp_ids <- unique(iris$Species)

trait_avg <- matrix(0, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(trait_avg) <- sp_ids
colnames(trait_avg) <- names(iris[ , -ncol(iris)])

for(i in seq_along(sp_ids)) {
  iris_sp <- subset(iris, subset=Species == sp_ids[i], select=-Species)
  for(j in 1:(ncol(iris_sp))) {
    trait_sum <- 0
    n_samples <- 0
    if (nrow(iris_sp) > 0) {
      for(k in 1:nrow(iris_sp)) {
        trait_sum <- trait_sum + iris_sp[k, j]
        n_samples <- n_samples + 1
      }
      trait_avg[i, j] <- trait_sum / n_samples 
    }
  }
}
trait_avg

#' 4.

sp_ids <- unique(iris$Species)

trait_avg <- matrix(0, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(trait_avg) <- sp_ids
colnames(trait_avg) <- names(iris[ , -ncol(iris)])

for(i in seq_along(sp_ids)) {
  iris_sp <- subset(iris, subset=Species == sp_ids[i], select=-Species)
  for(j in 1:(ncol(iris_sp))) {
    # Remove the lines that assign x and y to 0, and delete the if loop
    for(k in 1:nrow(iris_sp)) {
      # Remove the lines that are looping through every values of x and y
      trait_avg[i, j] <- mean(iris_sp[ ,j]) 
    }
  }
}
trait_avg

#' Sum of a Sequence

#' 5. 
x_vec <- 1:10
y_vec <- (cumsum(x_vec))
y_vec


#' 6.
x_vec <- 1:10
y_vec <- cumsum(x_vec)
y_vec <- ifelse(y_vec > 10, NA, y_vec)
y_vec


#' 7.
x_vec <- 1:1
for (i in x_vec){
  y_vec[i] <- (cumsum(x_vec))
}
y_vec
### I can't seem to figure this one out since I didn't use a for loop in the previous questions.  

#' 8. Fibonacci numbers
Fibonacci <- numeric(15)
Fibonacci[1] <- Fibonacci[2] <- 1
for (i in 3:15) Fibonacci[i] <- Fibonacci[i - 2] + Fibonacci[i - 1]
print(Fibonacci)

