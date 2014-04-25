## E. Lonvick; programming Assignment 2
## This funcation stores a copy of the object in memory
## The second time it is reference without changes; it will return the value(s)
##  This is an abstract object with accessor and get function.
##
##
makeCacheMatrix <- function(x = matrix()) {
        CacheMatrix <- NULL

        set <- function(y) {
                x <<- y
                CacheMatrix <<- NULL
        }
        get <- function() x
        setMatrix <- function(CacheMatrix)  CacheMatrix <<- x 
        getMatrix <- function()  CacheMatrix
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)

}
##
## This fuction will access the object help in memory
## and return the inverse matrix and no processing is needed
## becuase it was cached in memory.
cacheSolve <- function(x, ...) {
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
