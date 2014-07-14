
## Below are two functions that are used to create a
##special object that stores a matrix and caches its inverse.

#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list of four functions

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

#The list containing the four functions above is specific to a particular matrix 
#which was the input into makeCacheMatrix. Although the functions are not 
#unique to the matrix input, within the environment of the list is stored the cached matrix
#and the cached value of the inverse for that matrix

makeCacheMatrix <- function(x = matrix()) {     #Accepts a matrix and names it x. 
                                                #If no matrix is supplied, an empty matrix is the default
    i<-NULL                                     #i is a placeholder for the inverse matrix, it is initialized to NULL    
    
    set <- function(y) {                        #1st component function 'set' accepts a matrix y and replaces 
        x <<- y                                 #matrix x in the parent environment with y
        i <<- NULL                              #'set' also initializes i in parent environment to NULL
    }
    
    get <- function() x                         #2nd component function 'get' returns the current 
                                                #value of x in the parent environment
    
    setinv <- function(inv) i <<- inv           #3rd component function 'setinv' accepts a matrix 'inv'
                                                #and replaces `i' in the parent environment with 'inv'
    
    getinv <- function() i                      #4th component function 'getinv' returns the current value of i
                                                #in the parent environment
    
    list(set = set, get = get,                  #In the end, makeVector returns a named list of the four 
         setinv = setinv,                       #component functions along with the stored matrices 'x'
         getinv = getinv)                       #and 'i' in the environment of the list
}


## The following function calculates the inverse of the special "matrix"
#created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data and sets the value of the inverse in the cache via the `setinv`
#function.

cacheSolve <- function(w, ...) {                #cacheSolve takes in a special object w, which is composed of
                                                #1) a list of component functions created by makeVector for a given matrix x
                                                #2) the environment of the list which includes the stored matrix and inverse

    i <- w$getinv()                             #this line retrieves the "getinv" function 
                                                #from the list of functions in 'w' and runs it.
                                                #In effect, this line retrieves the stored value of i in 
                                                #the environment of list w and assigns it to the local i.
    
    if(!is.null(i)) {                           #the current value of i in the environment of list w is NULL unless
                                                #the setinv command had been previously called to set i to something else
        message("getting cached data")          #If i was already calculated and set previously, then return message, 
        return(i)                               #return the cached value of i and exit out of cacheSolve.
    }
    
    data <- w$get()                             #If however, setinv command has never been called to calculate/set i, 
                                                #then i is still NULL.
                                                #in this case, the 'get' command is executed which retrieves 
                                                #stored matrix 'x' from the environment of list 'w'
                                                #and assigns it to 'data.' `x' is the matrix input 
                                                #given to the makeVector() function when it was called.
    
    i <- solve(data, ...)                       #Calculates the inverse of matrix x
                                                #and assigns it to local i
    w$setinv(i)                                 #Runs setinv function which takes the freshly calculated inverse 
                                                #(local i) and caches it as 'i' in the environment of list `w'
    
    i                                           #returns the inverse (local i)
}

#testing if it works

z<-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(z)

