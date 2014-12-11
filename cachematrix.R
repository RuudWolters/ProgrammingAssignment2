## Introduction
##
## This is a Peer Assessments/Programming Assignment 2: Lexical Scoping 
## For  the course from coursera "R Programming"
## by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
## look at https://class.coursera.org/

## This programming assignment will require to write an R function that
## is able to cache potentially time-consuming computations. For example,
## taking the mean of a numeric vector is typically a fast operation. 
## However, for a very long vector, it may take too long to compute the mean,
## especially if it has to be computed repeatedly (e.g. in a loop).
## If the contents of a vector are not changing, it may make sense to cache
## the value of the mean so that when we need it again, 
## it can be looked up in the cache rather than recomputed.
## In this Programming Assignment will take advantage of the scoping rules
## of the R language and how they can be manipulated to preserve state inside
## of an R object.
##
## For this assigment we have to write a function for:
## Caching the Inverse of a Matrix
## usage:
##
## step 1 # create a matrix 
## matrix  <- matrix(1:4, nrow=2, ncol=2)
## creates the following matrix
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## step 2 # create the function for managing the cache
## cache  <- makeCacheMatrix(matrix)
##
## step 3 # call the cachesolve function to get the inversedMatrix
##    First time it will calculate the inverse.
##    Every next call it will reuse the cached inversedMatrix.
##
## inverseMatrix <- cacheSolve(cache)
##
## result:
## inverseMatrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## makeCacheMatrix
## This function creates a function with 4 methods
## set, get
## for getinng and setting the value for the Matrix
## in the calling environment
## setInverse, getInverse
## for getinng and setting the value for the inverseMatrix
## in the calling environment

makeCacheMatrix <- function(x = matrix())
{
      #An object for the inverse matrix
      inverseMatrix <- NULL
      
      #Set method for the original matrix
      set <- function(y) 
      {
            x <<- y
            inverseMatrix <<- NULL
      }
      
      # Get method for the original matrix
      get <- function(){ x }
      
      # Set method for the inverse matrix
      setInverse <- function(inverse) { inverseMatrix <<- inverse }
      
      # Get method for the inverse matrix
      getInverse <- function() { inverseMatrix }
      
      #list with methods
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve will call the function and methods created by
## "makeCacheMatrix" If the inverseMatrix hasn't been calculated yet
## it will solve it first and then cache it 
## via (makeCacheMatrix) setInverse. 

cacheSolve <- function(x, ...)
{
      #use makeCacheMatrix.getInverse
      inverseMatrix <- x$getInverse()
      
      if(!is.null(inverseMatrix)) 
      {
            message("getting cached inverse matrix")
            return(inverseMatrix)
      }
      
      data <- x$get()
      inverseMatrix <- solve(data, ...)
      x$setInverse(inverseMatrix)
      
      ## Return a matrix that is the inverse of 'x'
      inverseMatrix
}
