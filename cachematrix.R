## Put comments here that give an overall description of what your
## functions do
#Matrix inversion is usually a costly computation and their may be some 
#benefit to caching the inverse of a matrix rather than compute it repeatedly
#(there are also alternatives to matrix inversion that we will not discuss here).
#Your assignment is to write a pair of functions that cache the inverse of a 
#matrix.
#When executing program: 
                ##  mtrx <-c(1,2,3,4); mtrx <-matrix(mtrx,2,2)
                ##  inv<-makeCacheMatrix(mtrx)
                ##  inv$get()
                ##  inv$set(matrix(3:6,2))
                ##  inv$get()
                ##  cacheSolve(inv)
                ##  cacheSove(inv)
#Write the following functions:
#1)makeCacheMatrix: This function creates a special "matrix" #object that can cache its inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(mtrx = matrix()) {
#make sure an actual matrix is passed, else stop
        if(!is.matrix(mtrx)) stop("mtrx isn't a matrix")
        m <- NULL
        set <- function(y) {
           mtrx <<- y
           m <<- NULL
        }
        get <- function() mtrx
        #inverse matrix stored in m
        setinv <- function(solve) m <<- solve
        getinv <- function() m 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

#2) cacheSolve: This function computes the inverse of the special #"matrix" returned by makeCacheMatrix above. 
##If the inverse has #already been calculated (and the matrix has not changed), then #the cachesolve should 
##retrieve the inverse from the cache.


cacheSolve <- function(mtrx, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- mtrx$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- mtrx$get()
        m <- solve(data, ...)
        mtrx$setinv(m)
        m
}

