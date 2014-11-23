## The following are a pair of functions that cache the inverse of a matrix. 

## The first function creates a special "matrix" object that can cache its inverse
## which is really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

## We use the `<<-` operator which can be used to assign a value to an object 
## in an environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  
  ## This will be used to store the cache of the inverse of the matrix
  
  inv_erse <- NULL
  
  ## This helps us set the value of the matrix
  
  set <- function(y) {
    x <<-y
    inv_erse <<- NULL
  }
  
  ## This helps us get the value of the matrix
  
  get <- function() x
  
  ## This helps us set the value of the inverse
  
  setinverse <- function(inverse) inv_erse <<- inverse
  
  ## This helps us get the value of the inverse
  
  getinverse <- function() inv_erse
  
  ## The list
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## The second function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated
## (and the matrix has not changed), then `cacheSolve` should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## This helps us check if the inverse has already been calculated. 
  ## If so, it `get`s the mean from the cache and skips the computation.
  
  inv_erse <- x$getinverse()
  if(!is.null(inv_erse)) {
    message("Getting cached data")
    return(inv_erse)
  }
  
  ## This helps us calculates the inverse of the data and sets the value
  ## of the inverse in the cache via the `setinverse` function.
  ## Computing the inverse of a square matrix can be done with the `solve`
  ## function in R. For example, if `X` is a square invertible matrix, then
  ## `solve(X)` returns its inverse.
  ## Here we assume that the matrix supplied is always invertible.
  
  data <- x$get()
  inv_erse <- solve(data,...)
  x$setinverse(inv_erse)
  inv_erse
  ## Return a matrix that is the inverse of 'x'
}

