## Below are two functions that are used to create a special object
##that stores a matrix and cache's its inverse.


##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinv <- function(solve) i<<- solve
        getinv <- function() i
        list(set=set,get=get,setinv=setinv,getinv=getinv)

}


##The following calculates the inverse of the matrix created with the above function.
##However it first checks to see if the inverse has already been calculated.
##If so it gets the mean from the cache and skips computation. otherwise it calculates the inverse of the data
## and sets the value of inverse in cache via setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setinv(i)
        i
}
