## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # This function takes a matrix as input and return a list of functions as output
  # set - sets the values of the matrix
  # get - gets the values of the matrix
  # setinv_mat - sets the value of the inverse
  # getinv_mat - gets the values of the inverse
  
  
  #We initiate the inverse
  Inv <- NULL
  
  #Define the function set
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  
  #Define function get that returns the input
  get <- function() x
  
  #Define function setinv_mat that sets the inverse
  setinv_mat <- function(inv_mat) Inv <<- inv_mat
  
  #Define function setinv_mat that returns the inverse
  getinv_mat <- function() Inv
  
  #return the list of fucntions
  list(set = set, get = get,
       setinv_mat = setinv_mat,
       getinv_mat = getinv_mat)
}



cacheSolve <- function(x, ...) {
  
  #The function computes the inverse of the matrix x and stores it in cache so it can be
  #return directly in the next calls to the function
  
  ## Return a matrix that is the inverse of 'x'
  
  Inv <- x$getinv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinv_mat(Inv)
  Inv
}

#Test data
#
#To test we choose an easily invertible matrix: The diagonal matrix 2I where I is the
#2x2 identity matrix
#
#x<- cbind(c(2,0),c(0,2))
#
#and thus its inverse will be (1/2)I
#
# test <- makeCacheMatrix(x)
# 
# test$get() # should return the original matrix
# 
# cacheSolve(test) # should not provide any message on the first run but should show the message
#                    getting cached data on all following runs
