## Created by: Antonije Tadic
## Date: 2018-12-01
## Solving inverse matrix and storing once generated result in special 'vector'.


## makeCacheMatrix function is having 'get' and 'set' functions for storing 
## and getting input matrix. Functions 'setinverse' and 'getinverse' used to 
## storeing and gett inverse matrix. 
## No special checking for input types are made.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function calculates inverse of matrix for 'vector' input type.
## First checks if inverse is already calculated and stored in 'vector' object.
## If cache is available it returns cached data.
## If cache is not available it calculates inverse but first checking if inverse 
## is possible to calculate at all.
## Stackoverflow reference: 
## https://stackoverflow.com/questions/24961983/how-to-check-if-a-matrix-has-an-inverse-in-the-r-language
## In case that input is not matrix or matrix is not sqaure or matrix is
## non-singular, following message is sent back:
## 'no inverse: input not square non-singular matrix'.

cacheSolve <- function(x) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$get()
        if (class(try(solve(matrix),silent=T))=="matrix") {
                inverse <- solve(matrix)
        } else {
                message('no inverse: input not square non-singular matrix')
        }
        x$setinverse(inverse)
        inverse
}

################################################################################

# testing square non-singular matrix
A <- makeCacheMatrix(matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), 3))
A$get()
class(A$get())
det(A$get())
det(A$get())
A$getinverse()
cacheSolve(A)
A$getinverse()
cacheSolve(A)

# testing square singular matrix
B <- makeCacheMatrix(matrix(rep(1,100), 10))
B$get()
class(B$get())
det(B$get())
B$getinverse()
cacheSolve(B)
B$getinverse()
cacheSolve(B)

# testing non square matrix
C <- makeCacheMatrix(matrix( c(5, 1, 0, 3,-1, 2), 3))
C$get()
class(C$get())
dim(C$get())
C$getinverse()
cacheSolve(C)
C$getinverse()
cacheSolve(C)

# testing non matrix - vector
D <- makeCacheMatrix(c(5, 1, 0, 3,-1, 2, 4, 0,-1))
D$get()
class(D$get())
D$getinverse()
cacheSolve(D)
D$getinverse()
cacheSolve(D)

# testing non matrix - character
E <- makeCacheMatrix('test this')
E$get()
class(E$get())
E$getinverse()
cacheSolve(E)
E$getinverse()
cacheSolve(E)
