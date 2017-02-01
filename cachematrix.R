## This function creates a special "matrix" object that can cache its inverse.

## This function will set the matrix, get the matrix, set the inverse,
## and get the inverse 

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set = function (y) {
        x <<- y
        inv <<- NULL
}
get = function() x
setinv = function(inverse) inv <<- inverse
getinv = function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This will return the inverse of the orginal matrix input to makeCacheMatrix()
## If the inverse has been computed it skips the computation and get the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
if (!is.null(inv)) {
        return(inv)
}
mat.data <- x$get()
inv <- solve(mat.data, ...)
x$setinv(inv)
return(inv)
}

## Test
## x = rbind(c(1, -1/2), c(-1/2, 1))
## m = makeCacheMatrix(x)
## m$get()
## cacheSolve(m)
