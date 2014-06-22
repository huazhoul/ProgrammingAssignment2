## The functions can save a matrix and its inverse value. If the inverse has been calcuated before
## The saved value will be return in a new calculation

##  This function will make a special matrix which can get and set matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    getInverse <- function() m
    setInverse <- function(inv) m <<- inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function take special matrix created in above function as input and return inverse of the matrix
## If the inverse been calculated before, return invserse from saved value. Otherwise, do the invers cacluation, save and return result

cacheSolve <- function(x, ...) {
    ## Get saved inverse result
    inv <- x$getInverse()
    if (!is.null(inv))
    {
        message("Get the cached inverse matrix")
        return(inv)
    }
    ## Do reserve calcuation 
    inv <- solve(x$get())
    ## Save the result for future usage
    x$setInverse(inv)
    ## Return result
    inv    
}
