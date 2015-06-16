## These two functions are used to calculate the inverse of a square matrix
## And if it has been calculated before, the value can be cached 

## this function create a list that can set/get the value of the matrix/inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## this function will check if the inverse has been calculated before
## If not, it will calculate the invese and store it inside the special
## list created above, so that we can cached the value next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
