#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

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
#* @get /sum
function(a, b, c, d, e) {
    print(paste('Called sum with', a,b,c,d,e))
    as.numeric(a) + 
    as.numeric(b) + 
    as.numeric(c) + 
    as.numeric(d) + 
    as.numeric(e)  
}

#* Return OK
#* @get /nodename
function() {
    print('Called nodename')
    Sys.info()["nodename"]
}

