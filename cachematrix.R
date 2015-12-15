## Coded by PBG

## Below are two functions that are used to create a special object 
## that stores a numeric matrix (assumed to be square and invertable)
## and cache's its inverse (or calculates and stores if it have not already ben done.
## These assume the matrix is square and invertable

##  The first function, makeCache Matrix creates a special "vector",
##  which is really a list containing a function to
##  - set the value of the matrix
##  - get the value of the matrix stored
##  - set the value of the inverse of the matrix
##  - get the value of the inverse of the matrix that has been stored


makeCacheMatrix <- function(x = matrix()) {
    
    ##Initialize spot for inverse
    im <- NULL  
    ## This function sets the matrix from the input and clears the inverse
    ## This is run on the initial call as well as subsequent calls to ..$set
    set <- function(y) {                    
      x <<- y                
      im <<- NULL
    }
    ## This returns the matrix
    get <- function() x
    
    ##This stores the inverse of the matrix after it have been calculated
    setinverse <- function(inverse) im <<- inverse
    
    ## This returns the current contennts of im.  
    ## This will be wither null or the inverse of the current matrix
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## Return a matrix that is the inverse of 'x'
## This function take an object created by the makeCacheMatrix function and
## a) returns a previously stored inverted matrix
## or
## b) calculates the inverse, stores it, and returns it

cacheSolve <- function(x, ...) {
  ## First, check for a stored inverted matrix and if not NULL, return it
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  ## If nothing is stored, grab the matrix, invert it, store it, and return it
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
