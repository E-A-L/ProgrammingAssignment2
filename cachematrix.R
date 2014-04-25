## E. Lonvick; programming Assignment 2
## This funcation stores a copy of the object in memory
## The second time it is reference without changes; it will return the value(s)
##  This is an abstract object with accessor (set) and get functions.
##
##
makeCacheMatrix <- function(x = matrix()) {
#       CacheMatrix is a holder in memory
        CacheMatrix <- NULL
#        If the value is not null the update memory otherwise save it for later use.
        set <- function(y) {
                x <<- y
                CacheMatrix <<- NULL
        }
        get <- function() x
        # setmatrix will store the 'matrix' in memory for the next function call.
        setMatrix <- function(CacheMatrix)  CacheMatrix <<- x 
        # getmatrict will retrieve teh value if is not null; no need to recommpute if not null.
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
        # get obejct from memory and compute the values if different
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
