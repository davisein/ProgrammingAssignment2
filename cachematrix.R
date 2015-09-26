## Use makeCacheMatrix and cacheSolve to reuse the computed inverse value of a matrix
## The matrix must be invertible
## Example:
## m <- rbind(c(1, -1/2), c(-1/2, 1))
## mCached <- makeCacheMatrix(m)
## mSolved <- cacheSolve(m)
## mCached$get()  # Get the original value `m`


## makeCacheMatrix returns a variable that can be used with cacheSolve
## The variable supports the following uses:
## m <- makeCacheMatrix(x)
## m$get() # Get the original matrix
## m$set(x2) # Save new matrix x2
## m$getinverse() # Get the cached value of the inverse. Use `cacheSolve` instead
## m$setinverse() # Set the cached value of the inverse. Use `cacheSolve` instead

makeCacheMatrix <- function(x = matrix()) {
    cached <- NULL

    # Getter and setter for x
    set <- function(y) {
        x <<- y
        cached <<- NULL
    }
    get <- function() x

    # Getter and setter for the cached value
    setinverse <- function(inverse) cached <<- inverse
    getinverse <- function() cached

    invisible(list(
        set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse))
}


## cacheSolve gets a variable produced by makeCacheMatrix and returns the inverse of the matrix, computing it if needed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached <- x$getinverse()
    if(!is.null(cached)) {
        message("getting cached data")
        return(cached)
    }
    data <- x$get()
    cached <- solve(data, ...)
    x$setinverse(cached)
    cached
}
