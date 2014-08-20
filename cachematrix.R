## Put comments here that give an overall description of what your
## functions do
# The functions makeCacheMatrix() and cacheSolve() work together to first create a special 
# sort of matrix (actually a list of functions) to store a matrix and its inverse in cache.
# The first time cacheSolve() is called on a special matrix created by makeCacheMatrix
# the inverse matrix is calculated and stored. Susequent calls to cacheSolve() will retrieve 
# the value of the matrix inverse from cache.


## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix", which is actually a list containing
# functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve(x) is called to calculate the inverse matrix of a special "matrix"
# which was created by calling makeCacheMatrix(). cacheSolve first checks if the inverse
# has already been stored in the special matrix. If already cached it returns the 
# value of the inverse by calling the get() function of the special matrix.
# If not already cached, then the inverse is calculated using solve() and stored in
# the special matrix by using the special matrix' setinv() function. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
