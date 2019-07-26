#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

DEBUG <- TRUE

#* @apiTitle hourlyCost API

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Return the sum of five numbers
#* @param a The first number to add
#* @param b The second number to add
#* @param c The third number to add
#* @param d The fourth number to add
#* @param e The fifth number to add
#* @get /sumfive
function(a, b, c, d, e) {
    print(paste('Called sum with', a,b,c,d,e))
    as.numeric(a) + 
    as.numeric(b) + 
    as.numeric(c) + 
    as.numeric(d) + 
    as.numeric(e)  
}

#* Return sum of numbers
#* @post /sum
function(req, d) {
    # if (DEBUG) {
    #     print("Called sum")
    #     # browser()
    #     print(req$postBody)
    #     print(d)
    # }
    resp <- sum(as.numeric(d))
    if (DEBUG) print(resp)
    resp
}


#* Return length of list numbers
#* @post /count
function(req, d) {
    resp <- length(d)
    # browser()
    if (DEBUG) print(resp)
    resp
}

#* Return OK
#* @get /nodename
function() {
    print('Called nodename')
    Sys.info()["nodename"]
}

