## returns a matrix wrapped with functions to access the matrix itself and it's inverse

makeCacheMatrix <- function(x = matrix()) {

    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }

    get <- function() x
    setinv <- function(inv) m_inv <<- inv
    getinv <- function() m_inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv
         )
}


## returns a matrix that is the inverse of 'x'
## If a cached inverse does not exist
##      the inverse is calculated and cached.
## else,
##      the cached inverse is returned.

cacheSolve <- function(x, ...) {
    m_inv <- x$getinv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }

    data <- x$get()
    m_inv <- solve(data, ...)
    x$setinv(m_inv)
    m_inv
}
