## Matrix inversion is usually a costly computation.The functions below could cache the inverse of a matrix in case it is computed repeatedly.
## makeCacheMatrix is a function that stores a list of functions, and they are set, get, setinverse, and getinverse. 
## set is a function that changes the matrix stored in the main function.
## get is a function that returns the vector x stored in the main function.
## setinverse stores the value of the input in a variable m into the main function makeCacheMatrix and getinverse returns the result.

makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){ 
        x <<- y
        m <<-NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cachesolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...){
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached matrix")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix,...)
    x$setinverse(m)
    m
}
