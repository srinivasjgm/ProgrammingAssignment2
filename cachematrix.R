## Functions to depict the concept of caching. 
## To be used for time consuing computations. This is shown using the inverse 
## matrix computation. 
## It takes advantage of the scoping rules of R language and how they can be 
## manipulated to preserve state inside of an R object.


## makeCacheMatrix function creates "specail" matrix. This caches its inverse using
## the `<<-` operator. So the special matrix stores the matrix and its inverse once
## computed

makeCacheMatrix <- function(matrix = matrix()) {
      inverse <- NULL
      set <- function(y) {        ## set funcion - sets value of Matrix
            matrix <<- y
            inverse <<- NULL      ## initialise to null
      }
      
      get <- function() matrix    ## function to get/return matrix
      
      setinverse <- function(inv) inverse <<- inv ## function to set inverse
      
      getinverse <- function() inverse ## function to get inverse
      
      list(set = set,             ## return list with functions for the special matrix
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Return a matrix that is the inverse of matrix passed - x
## cacheSolve function returns the inverse of the special matrix
## created with the above function. It first checks to see if the
## inverse has already been calculated. If so, returns it from the
## cache, else it calculates the inverse of the matrix using Solve function
## and sets the value of the inverse in the cache using setinverse function.

cacheSolve <- function(x, ...) {     ## Assume only inversible matrcies are given
      inv <- x$getinverse()
      if(!is.null(inv)) {            ## check if inverse is cached                   
            message("getting cached inverse")
            return(inv)              ## return cached inverse 
      }
      
      m <- x$get()                   ## get the original matrix stored
      inv <- solve(m, ...)           ## compute inverse
      x$setinverse(inv)
      inv                            ## return computed inverse
}
