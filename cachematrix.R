## The following function returns a list of 4 functions:
## 1. set a value for the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<- function(y) {
                x<<-y
                inv<<- NULL
        }
        get <- function() x
        setinv<- function(inverse) inverse<<- solve
        getinv<- function() m
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
        

}


## The following function calculate the inverse of a matrix but only if it is not
## already calculated before (cached) 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverted matrix")
                return(inv)
        }
        data<- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

