## Functions that cache the inverse of a matrix
#Creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function (y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function () x
        setInvrs <- function(solveMat) invrs <<- solveMat
        getInvrs <- function () invrs
        list(set = set, get = get, setInvrs = setInvrs, getInvrs = getInvrs)
}


## Modified from the README.md file from GitHub. 
#Calculates the inverse of the matrix created with the above function. It first checks to see if the inverse has already been calculated. If it has, then it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInvrs function.

cacheSolve <- function(x, ...) {
        invrs <- x$getInvrs
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get
        invrs <- solve(data)
        x$setInvrs(invrs)
        invrs
}
