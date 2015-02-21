## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is the function that set and returns a list of functions
## It stores a martix and a caches the value of the inverse of the matrix. 
## This function contains the following subfunctions:
## *-- set                 :sets the value of a matrix
## *-- get                 :gets the value of a matrix
## *-- setMatrixCached     :sets the cached value. This is the inverse of the matrix.
## *-- getMatrixCached     :gets the cached value. This is the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix<-NULL
    
    set<-function(newValue){
        x<<-newValue
        cachedMatrix<<-NULL
    }
    
    get<-function() x
    
    setMatrixCached<-function(solve) cachedMatrix<<- solve
    
    getMatrixCached<-function() cachedMatrix
    
    list(set=set, get=get,
         setMatrixCached=setMatrixCached,
         getMatrixCached=getMatrixCached)
}
}


## The function below creates the inverse of the matrix(setMatrixCached) 
## and puts it cache, OR, if already cached, read it from the matrix cache
## (getMatrixCached)
## The cacheMatrix variable is used to hold the results of this inverse(solve)
## function


cacheSolve <- function(x=matrix(), ...) {
    cachedMatrix<-x$getMatrixCached()
    ## Check it the matrix is already cached
    if(!is.null(cachedMatrix)){
        message("getting cached data")
        return(cachedMatrix)
    }
    matrix<-x$get()
    
    cachedMatrix<-solve(matrix, ...)
    
    ## if not cached, put it in the cache here
    x$setMatrixCached(cachedMatrix)
    
    ## cacheMatrix returns a matrix that is the inverse of the input 'x'
    cachedMatrix
}

