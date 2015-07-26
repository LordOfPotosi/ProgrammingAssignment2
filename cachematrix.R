## Put comments here that give an overall description of what your
## functions do

## This function returns an object as a list which is the get,set,getmean and setmean functions for the passed data matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y                                     # assign x the data in the parent frame
    inv <<- NULL                                # assign null to the variable holding inverse
    
  }
  
  get <- function() x                           # get function to return the data x
  
  setinv <- function(inverse) inv <<- inverse   # setinv function to assign the value of inverse to inv
  
  getinv <- function() inv                      # return the value of inv, it will be null if not already cached
  
  list(set = set, get = get, setinv = setinv, getinv = getinv) # return list with functions as elements
   

}



cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)){
    
    message("Getting Cached Data")   # return cached value of inverse if already in cache
    return(inv)
    
  } else {                           # if value not cached, need to get data and assign value of inverse using solve()    
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    
  }
  
  inv
  
 
}





