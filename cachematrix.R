## Function to create a special matrix which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the inverse of a matrix
## 4. set the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL;
  set <- function(y){
    x <<- y;
    m <<- NULL;
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function calculates the inverse of a special matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the 
## cache via the setinverse function.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  } 
  data <- x$get()
  size <- ncol(data)
  identity <- diag(size)
  m <- solve(data, identity, ...)
  x$setinverse(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}
