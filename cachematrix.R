## create a list representing a matrix and its inverse
## $set
## $get
## $setInverse
## $getInverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set=set, 
         get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


## compute calcution of inverse matrices (solve(x)) if not already cached
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
     if (is.null(x$getInverse())) {
         x$setInverse(solve(x$get()))   
     }   
     x$getInverse()
}
