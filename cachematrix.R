## The following two functions manage the computation of the inverse 
## of a given matrix using a cache.

## makeCacheNatrix creates a special "matrix", that is a list containing a function to
##      1) set the value of the matrix
##      2) get the value of the matrix
##      3) set the value of the inverse of the matrix
##      4) get the value of the inverse of the matrix
## 

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL;
        
        set <- function(y) {
                x <<- y
                inverse <<-NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse
        )
        
}

## CacheSolve compute the inverse of the matrix list given as a parameter 
## and that was created by the function makeCacheMatrix
## It retreives the value of the inverse if it was already computed and the matrix value didn't change  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ivMat <- x$getinverse()
        if(!is.null(ivMat)) {
                message("getting cached data")
                return(ivMat)
        }
        Mat <- x$get()
        ivMat <- solve(Mat)
        x$setinverse(ivMat)
        ivMat
        
}

# runing trace example: 
# > source("cachematrix.R")
# > ls()
# [1] "cacheSolve"      "makeCacheMatrix"
# > matm<-makeCacheMatrix()
# > matm
# $set
# function (y) 
# {
#         x <<- y
#         inverse <<- NULL
# }
# <environment: 0x10b362a78>
#         
#         $get
# function () 
#         x
# <environment: 0x10b362a78>
#         
#         $setinverse
# function (inv) 
#         inverse <<- inv
# <environment: 0x10b362a78>
#         
#         $getinverse
# function () 
#         inverse
# <environment: 0x10b362a78>
#         
#         > matm$set(matrix(1:4,2,2))
# > matm$getinverse()
# NULL
# > cacheSolve(matm)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > matm$getinverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > matm$set(matrix(9:12,2,2))
# > matm$get()
# [,1] [,2]
# [1,]    9   11
# [2,]   10   12
# > matm$getinverse()
# NULL
# > cacheSolve(matm)
# [,1] [,2]
# [1,]   -6  5.5
# [2,]    5 -4.5
# > cacheSolve(matm)
# getting cached data
# [,1] [,2]
# [1,]   -6  5.5
# [2,]    5 -4.5
# > 