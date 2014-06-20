## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. Below are 3 functions, matrixInv() which will find the inverse of non-square matrices, makeCacheMatrix() which will create a special "matrix" object that can cache its inverse, and cacheSolve(), which computes the inverse of the special "matrix" returned by makeCacheMatrix()
## Note guidance for these functions obtained from the coursera r programming discussion forum link https://class.coursera.org/rprog-004/forum/thread?thread_id=333

## this function to finds the inverse of a matrix, to use in subsequent makeCacheMatrix and cacheSolve
matrixInv <- function(m){
    d <- svd(m)$d
    u <- svd(m)$u
    v <- svd(m)$v
    m.inv <- v%*%diag(1/d)%*%t(u)
    m.inv
}

## This function creates a special "matrix" object that can cache its inverse
# create a special "matrix" object x that can cache its inverse m
makeCacheMatrix <- function(x = matrix()) {
    ## set the matrix inverse to NULL for starters
    m <- NULL
    ## create a setter that caches x and m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## create a getter that returns the cached x
    get <- function() {
        x
    }
    ## create a setmatrixInv function 
    setmatrixInv <- function(matrixInv) {
        m <<- matrixInv
    }
    ## create a getmatrixInv function that returns the matrix inverse, only calculating it if necessary
    getmatrixInv <- function() {
        if (is.null(m)) {
            m <<- matrixInv(x)
        }
        m
    }
    ## return the makeCacheMatrix object as a list of 4 functions
    list(set = set, get = get, setmatrixInv = setmatrixInv, getmatrixInv = getmatrixInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getmatrixInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- matrixInv(data, ...)
    x$setmatrixInv(m)
    m
}