
## Function that creates the matrix and the vector of associated functions.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Checks to see if inverse has previously been computed for matrix value, 
## if it hasn't then it computes the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

##Value Checking
X <- matrix(c(2,5,7,3,6,8,1,11,8), nrow = 3, ncol = 3)
solve(X)
x <- makeCacheMatrix(X)
cacheSolve(x)
