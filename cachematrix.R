## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ## Similar to makeCachevector, m is the inverse and  intially is nulll
      m <- NULL
      ## set function allows the makeCacheMatrix the ability to overwrite the initial matrix.
      set <- function(y) {
        ## Assign the new matrix to x
        x <<- y
        ## Revert the inverse variable to NULL since the Matrix has changed, a new inverse
        ## will have to be calculated.
        m <<- NULL
      }
      ## Returns the matrix. This is an interesting function and demonstrates an interesting
      ## property of R. The return list from  the function does not include the original 
      ## matrix. But R retains  "x", the original (or reassigned) argument even after the function  is called.
      get <- function() x
      ## This function lays the ground work for CacheSolve in order to set the calculated inverse.
      setinverse <- function(inv) m <<- inv
      ## This function returns the inverse (eitehr the value or NULL)
      getinverse <- function() m
      ## Export a list of  four functions assigned to the same variable as their function call
      ## In lexical scoping, this essentially "passes" the value of the function names to the
      ## parent environment and makes them accessibel as calls. Compared to other languages,
      ## this is similar to constructors on classes.
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Identical to vector edition, retrive the inverse value, either the inverse
    ## or the NULL
    m <- x$getinverse()
    ## If m is not NULL, it returns the stored value
    if(!is.null(m)) {
      message("getting cached data")
      ## At this point, the function will break and the value is returned
      return(m)
    }
    ## Retrieved the original matrix (or redefined)
    data <- x$get()
    ## Solve for the inverse and stores it as m
    m <- solve(data, ...)
    ## Sets the inverse within the makeCacheMatrix object, essentially caching it for later
    x$setinverse(m)
    ## Return the calculated inverse
    m
}



