#Created for Coursera R Programming Assignment 2: Lexical Scoping 1 Feb 2016
#These two functions store a matrix and calculate and cache its inverse.
#To demonstrate their use:
#m <- diag(c(2, 1), nrow = 2, ncol = 2) #create a simple matrix (2,0,0,1)
#x <- makeCacheMatrix() #instantiate a function that can set/get the matrix and its inverse.
#x$set(m) #populate the function with the simple matrix m
#cacheSolve(x) #looks for a saved inverse matrix in x. If there isn't one, it calculates it(0.5,0.0,0,1) and stores it in x. 
#cacheSolve(x) #if there is a saved inverse matrix in x, then display a message and return the cached inverse matrix


## This function provides set and get functions for a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){#accepts the matrix and stores it
      x <<- y
      inv <<- NULL #this ensures that the cached inverse matrix is reset when the values in the matrix are changed
  }
  get <- function(){x} #returns the matrix we stored earlier
  setinv <- function(inverse){inv <<-inverse} #accepts the inverse of the matrix and stores it
  getinv <- function(){inv} #returns the inverse matrix we stored earlier (or NULL if it has not been calcuated)
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function returns the inverse of a matrix stored in the makeCacheMatrix function
## If the inverse has already been calculated and cached then use that, otherwise calculate the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv() #get what was cached in x for the inverse
  if(!is.null(inv)){
    invisible("getting cached data") #we found a cached value so return it
    return(inv) #stop here and return the inverse value
  }
  data <- x$get() #we didn't find a cached value for the inverse matrix so get the original matrix
  inv <- solve(data,...) # and calculate the inverse matrix
  x$setinv(inv) #and cache it
  inv #and finally, return the inverse matrix
}
