#business
# row-operations
# (DONE)     - select only rows where atleast one of the categories is Restaurant
# (DONE)     - select only rows where city="Phoenix"
#
# col-operations
# (DONE)  - there are duplicate columns with slight spelling difference, merge them
#TODO   - these columns are mostly not required: full_address, hours.*, 
#TODO   - normalize the categories into 1/0 factors
#TODO   - normalize neighborhoods into 1/0 factors
#TODO   - convert latitude, longitude to a meter based projection (see Gerber's code)
#TODO   - remove columns that contain only NA's (may occur after row subset)
#   
#
####################
#
#reviews
#
# row-operations
# (DONE)   - select only those rows where business_id in business table after preprocessing above
#
#
##################
#
#users
#
# row-operations
# (DONE)  - select only those users where user_id is in above reviews table
#   
# column-operations
# (DONE)  - remove elite columns, they do not seem to be useful
#
###################

library(sqldf)

preProcess <- function(){
  
  load(file="../data/rdata/business_raw_flatfile.RData")
  load(file="../data/rdata/reviews_raw_flatfile.RData")
  load(file="../data/rdata/users_raw_flatfile.RData")
  
  business$attributes.Good.For.Kids[is.na(business$attributes.Good.For.Kids)] = business$attributes.Good.for.Kids[is.na(business$attributes.Good.For.Kids)]
  drops <- c("attributes.Good.for.Kids")
  business <- business[,!(names(business) %in% drops)]
  
  business$categories <- as.character(business$categories)
  business$categories1 <- as.character(business$categories1)
  business$categories2 <- as.character(business$categories2)
  business$categories3 <- as.character(business$categories3)
  business$categories4 <- as.character(business$categories4)
  business$categories5 <- as.character(business$categories5)
  business$categories6 <- as.character(business$categories6)
  business$categories7 <- as.character(business$categories7)
  business$categories8 <- as.character(business$categories8)
  business$categories9 <- as.character(business$categories9)
  business$categories10 <- as.character(business$categories10)
  
  business_clean <- sqldf(
      "SELECT *
      FROM business
      WHERE city='Phoenix'
      AND (
        categories = 'Restaurants'
        OR categories1 = 'Restaurants'
        OR categories2 = 'Restaurants'
        OR categories3 = 'Restaurants'
        OR categories4 = 'Restaurants'
        OR categories5 = 'Restaurants'
        OR categories6 = 'Restaurants'
        OR categories7 = 'Restaurants'
        OR categories8 = 'Restaurants'
        OR categories9 = 'Restaurants'
        OR categories10  = 'Restaurants')"
  )
  
  
#-----------------------------------------------------------------
# reviews
  
  reviews_clean <- sqldf(
        "SELECT reviews.*
         FROM reviews, business_clean
         WHERE reviews.business_id = business_clean.business_id
        "
    )
  
#------------------------------------------------------------------  
# users
  users_clean <- sqldf(
    "SELECT *
    FROM users
    WHERE user_id IN (SELECT DISTINCT user_id FROM reviews_clean)"
  )
  
  drops <- c("elite","elite1","elite2","elite3","elite4","elite5","elite6","elite7","elite8","elite9","elite10","elite11")
  users_clean <- users_clean[,!(names(users_clean) %in% drops)]
  
}
