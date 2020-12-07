## In the first section of this script, we will define a function that contains
## 4 different functions to manipulate the data that we give to the main function.
## There 4 functions will be used for setting a matrix, getting a matrix, 
## setting a inverted version for matrix and getting that inverted version of that matrix. 

## We are first creating a dummy variable to use in various situations called *invert*
## Secondly, to store our matrix, we are creating a variable called *set_matrix*
## We are using *<<-" operator to assign our matrix (input) to another variable called *y*
## To ask for our matrix, we are creating a function just to call our matrix, named *get_matrix*
## To ask for our matrix's mean, we are creating a function called *get_matrix_mean*
## *get_matrix_solved* will be equal to our dummy variable because we don't want to calculate the inverted right now
## At the end, to store our looped functions, we are creating a list of them.


makeCacheMatrix <- function(x = matrix()) {
     invert <- NULL
     set_matrix <- function (x) {
          x <<- y
          invert <- NULL
     }
     get_matrix <- function() {x}
     set_matrix_inverted <- function(mean) {invert <<- mean}
     get_matrix_inverted <- function() {invert}
     list(set_matrix=set_matrix, get_matrix= get_matrix,
          set_matrix_inverted=set_matrix_inverted, get_matrix_inverted=get_matrix_inverted)
}

## To get the inverted version of our matrix, we will define a new function called *cacheSolve*
## One important point is, we will first check if the inverted version is already calculated.
## If not, our function will calculate and show the result by using *solved* function.

cacheSolve <- function(x, ...) {
     invert <- x$get_matrix_inverted()
     if(!is.null(invert)) {
          message("getting cached data")
          return(invert)
     }
     data <- x$get_matrix()
     invert <- solve(data, ...)
     x$set_matrix_inverted(invert)
     invert
}


## Try the following commands to check if your functions work.
deneme <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
cacheSolve(deneme)
