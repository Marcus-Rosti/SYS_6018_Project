#business
# row-operations
# (DONE)     - select only rows where atleast one of the categories is Restaurant
# (DONE)     - select only rows where city="Phoenix"
#
# col-operations
# (DONE)  - there are duplicate columns with slight spelling difference, merge them
# (DONE)   - these columns are mostly not required: full_address, hours.*,
# (DONE)   - normalize the categories into 1/0 factors
#TODO   - convert latitude, longitude to a meter based projection (see Gerber's code)
# (DONE)   - remove columns that contain only NA's (may occur after row subset)
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

preProcess <- function(){
  require(sqldf)

  load(file="../data/rdata/business_raw_flatfile.RData")
  load(file="../data/rdata/reviews_raw_flatfile.RData")
  load(file="../data/rdata/users_raw_flatfile.RData")

  business$attributes.Good.For.Kids[is.na(business$attributes.Good.For.Kids)] = business$attributes.Good.for.Kids[is.na(business$attributes.Good.For.Kids)]
  drops <- c("attributes.Good.for.Kids","full_address","hours.Monday.open","hours.Monday.close","hours.Tuesday.open","hours.Tuesday.close",
             "hours.Wednesday.open","hours.Wednesday.close","hours.Thursday.open","hours.Thursday.close","hours.Friday.open",
             "hours.Friday.close","hours.Saturday.open","hours.Saturday.close","hours.Sunday.open","hours.Sunday.close")
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

  categories <- unique(c(as.character(business_clean$categories),
                         as.character(business_clean$categories1),
                         as.character(business_clean$categories2),
                         as.character(business_clean$categories3),
                         as.character(business_clean$categories4),
                         as.character(business_clean$categories5),
                         as.character(business_clean$categories6),
                         as.character(business_clean$categories7),
                         as.character(business_clean$categories8),
                         as.character(business_clean$categories9),
                         as.character(business_clean$categories10)))

  m <- matrix(0,nrow = nrow(business_clean), ncol = length(categories))
  categories_df <- data.frame(m)
  names(categories_df) = categories

  for( i  in 1:nrow(business_clean)){

    ct <- as.character(business_clean$categories[i])
    ct1 <- as.character(business_clean$categories1[i])
    ct2 <- as.character(business_clean$categories2[i])
    ct3 <- as.character(business_clean$categories3[i])
    ct4 <- as.character(business_clean$categories4[i])
    ct5 <- as.character(business_clean$categories5[i])
    ct6 <- as.character(business_clean$categories6[i])
    ct7 <- as.character(business_clean$categories7[i])
    ct8 <- as.character(business_clean$categories8[i])
    ct9 <- as.character(business_clean$categories9[i])
    ct10 <- as.character(business_clean$categories10[i])

    if(!is.na(ct))  categories_df[i,ct] <-  1
    if(!is.na(ct1))  categories_df[i,ct1] <-  1
    if(!is.na(ct2))  categories_df[i,ct2] <-  1
    if(!is.na(ct3))  categories_df[i,ct3] <-  1
    if(!is.na(ct4))  categories_df[i,ct4] <-  1
    if(!is.na(ct5))  categories_df[i,ct5] <-  1
    if(!is.na(ct6))  categories_df[i,ct6] <-  1
    if(!is.na(ct7))  categories_df[i,ct7] <-  1
    if(!is.na(ct8))  categories_df[i,ct8] <-  1
    if(!is.na(ct9))  categories_df[i,ct9] <-  1
    if(!is.na(ct10))  categories_df[i,ct10] <-  1
  }
  names(categories_df) <- paste0("category.",names(categories_df))
  business_clean <- cbind(business_clean,categories_df)

  drops <- c("categories","categories1","categories2","categories3","categories4","categories5","categories6","categories7",
             "categories8","categories9","categories10")
  business_clean <- business_clean[,!(names(business_clean) %in% drops)]

  #NA cols
  drops <- c()
  for(i in names(business_clean)){
    if(sum(is.na(business_clean[,i]))==nrow(business_clean))
      drops <- c(drops,i)
  }
  business_clean <- business_clean[,!(names(business_clean) %in% drops)]

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

  save(business_clean, file="../data/rdata/business_clean.RData")
  save(reviews_clean, file="../data/rdata/reviews_clean.RData")
  save(users_clean, file="../data/rdata/users_clean.RData")

}
