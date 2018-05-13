# makeCacheMatrix takes a matrix and places it in the makeCacheMatrix environment with 4 functions: get, set, setinv, and getinv. These functions allow us to store and retrieve the matrix inverse later through the cacheSolve function. The actual computation of the matrix inverse is done in cacheSolve if we don't already have the right matrix inverse stored in cache.

#makeCacheMatrix takes in a matrix
makeCacheMatrix <- function(x = matrix()) {
        #inv is the object we use to cache the computed matrix inverse
        inv <- NULL
        #set allows us to input a new matrix to makeCacheMatrix by clearing the cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get is used by cacheSolve to retrieve the matrix
        get <- function() x
        #setinv is used to take store the computed matrix inverse in our cache
        setinv <- function(inverse) inv <<- inverse
        #getinv is used to retrieve the computed matrix inverse from cache
        getinv <- function() inv
        #the list allows us to call the makeCacheMatrix internal functions using the $ operator
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
#cacheSolve checks if we have the desired matrix inverse in cache, and if not it computes the matrix inverse of the matrix supplied to makeCacheMatrix
cacheSolve <- function(x, ...) {
        #retrieving cache to check if it is null, if not then cacheSolve returns the cached matrix inverse
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #retrieving matrix then setting inv to the computed matrix inverse
        data <- x$get()
        inv <- solve(data, ...)
        #stores the matrix inverse into cache
        x$setinv(inv)
        #returns the matrix inverse
        inv
}