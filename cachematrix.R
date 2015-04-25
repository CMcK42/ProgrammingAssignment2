
# makeCacheMatrix creates a function to set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
# get the value of the matrix   
 get <- function() x

# set the value of inverse of the matrix

    setinverse <- function(inverse) inv <<- inverse

# get the value of inverse of the matrix
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# return the inverse of the matrix. 
# It checks if the inverse has been found 
# If it hasn't it computes the inverse and sets the value in the cache with
# setinverse.

# Only for a matrix that is invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


## Run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## Solve cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieve cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.
