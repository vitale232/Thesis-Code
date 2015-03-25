#--------------------------------
# Name:         lapse_function.R
# Purpose:      function to calculate lapse rates
# Author:       Andrew Vitale  vitale232@gmail.com
# Created       2014/11/20
# R:            3.1.2
#--------------------------------

############################################
#### Take a look at lapse rates

### Create a lapse function that will calculate lapse
### rates with temperature datasets that are structured
### such that time is rows, space is columns.  it will
### systematically run a linear model of elevation predicting
### each rows temperature data, thus the columns must be 
### in the same order as the elev input to make sure each
### elevation is aligned with the proper measurement.

### INPUTS:
### df          = the temperature data.frame structured in the proper
###               format, space as columns, rows as time
### elev        = elevation for each site in the same order as the
###               columns, such that each elev measurement is in the
###               same position as the associated column (not paying
###               attention to date)
### rm_date     = TRUE/FALSE, should a date column be removed? Defaults
###               TRUE
### date_column = if rm_date is TRUE, which column (by number) are the
###               dates in? Defaults 1
###
### OUTPUTS:
### a vector of lapse rates, one for each day (row of df)


lapse = function(df, elev, rm_date=TRUE, date_column=1){
  # create an empty list to store model summaries
  l = list()
  
  # linear model of each day's temperature (row in df) minus
  # the first element which is the date
  for(i in 1:nrow(df)){
    if(rm_date){
      v = as.vector(as.matrix(df[i, ][-date_column]))
    } else{
      v = as.vector(as.matrix(df[i, ]))
    }
    l[[i]] = summary(lm(v ~ elev))
  }
  
  return(sapply(l, function(x) coef(x)[2]))
  
}