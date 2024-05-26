design_DF <- read.csv("survey_design_2.csv") 
design_DF

responses_DF <- read.csv("respondent_data_2.csv") 
N <- nrow(responses_DF)
summary(responses_DF)




lm_res <- vector(mode="list", length=nrow(responses_DF))
for (i in 1:nrow(responses_DF)) {
   response = as.numeric(responses_DF[i,2:ncol(responses_DF)])
   est_DF = cbind(design_DF, response=response)

   lm_res[[i]] = lm(response ~ (Screen) + factor(Cell) + (Price) + 
                      (Battery) + factor(OS),
                    data=est_DF)
 }
summary(lm_res[[50]])



##Q3
res_list<-lm_res

prods_DF <- data.frame(Screen = c(10,10),
                     Cell = c("Y","N"),
                     Price = c(500,300),
                     Battery = c(8,8),
                     OS = c("iOS","Android"))

rownames(prods_DF) = c("iPad","Google_A")
prods_DF


##Q3

comp_demand <- function(res_list, prods_DF) {
  # initialize
  N = length(res_list) # number subjects
  choices = rep(0,N)
  # loop over subjects: predict ratings/utilities, determine expected choices
  for (i in 1:N) {
    ratings = predict(res_list[[i]], newdata = prods_DF)
    choices[i] = which.max(ratings)
  }
  # calculate demand for each product (rows in prods_DF)
  N_prod = nrow(prods_DF) # number products
  demand = rep(0,N_prod)
  # loop over products: calculate aggregate demand
  for (i in 1:N_prod) {
    demand[i] = sum(choices==i)
  }
  # label the output
  names(demand) = rownames(prods_DF)
  # return values
  return(demand)
}

prods_DF.results <- comp_demand(res_list, prods_DF)
print(prods_DF.results)


##Q4

comp_cost <- function(prods_DF) {
  N_prod = nrow(prods_DF) # number products
  cost = rep(0, N_prod)
  for (i in 1:N_prod) {
    cost[i] = 80 + 5*(prods_DF$Screen[i]==10) + 15*(prods_DF$Cell[i]=="Y") + 10*(prods_DF$Battery[i]==8) + 20*(prods_DF$Battery[i]==12)
    print(cost[i])
  }
  names(cost) = rownames(prods_DF)
  return(cost)
}

comp_cost(prods_DF)
  
  
##Q5

profit1 <- function(res_list, prods_DF, sum_ndx) {
  # Calculate demand for all products
  demand <- comp_demand(res_list, prods_DF)
  
  cost <- comp_cost(prods_DF)
  
  # Initialize total profit
  total_profit <- 0
  
  for (i in length(sum_ndx)) {
    ndx = sum_ndx[i]
    Q <- demand[ndx]
    P <- prods_DF[ndx, "Price"]
    MC <- cost[ndx]
    profit <- Q * (P - MC)
    total_profit <- total_profit + profit
    
   
  }
  
  return(total_profit)
}
expected_profit <- profit1(res_list, prods_DF, 2)
print(expected_profit)



##Q6
profit2 <- function(lm_res, prods_DF, sum_ndx, price) {
  prods_DF[sum_ndx,"Price"] = price
  pft = profit1(lm_res, prods_DF, sum_ndx)
  return(pft)
}

expected_profit2 <- profit2(res_list, prods_DF, 2, 450)




##Q7
pxs <- seq(100, 500, by = 10)
pft <- rep(0, length(pxs))

# Iterate over each price and calculate profit
for (i in seq_along(pxs)) {
  pft[i] <- profit2(lm_res, prods_DF, 2, pxs[i])
}

# Find the index of the maximum profit
max_profit_index <- which.max(pft)

# Get the profit-maximizing price
profit_maximizing_price <- pxs[max_profit_index]

# Print the profit-maximizing price
print(profit_maximizing_price)


##8

price_points <- c(100, 300, 500)

# Create a dataframe with all feasible designs that Google can produce
allprods_DF <- expand.grid(Screen = unique(design_DF$Screen),
                           Cell = unique(design_DF$Cell),
                           Price = price_points,
                           Battery = unique(design_DF$Battery),
                           OS = 'Android')
# Filter to include only designs with the Android OS

nrow(allprods_DF)



Np <- nrow(allprods_DF)
pft <- rep(0,Np)

for (i in 1:Np) {
  prods <- data.frame(Screen = c(10, allprods_DF[i,"Screen"]), 
                      Cell = c("Y", as.character(allprods_DF[i,"Cell"])), 
                      Price =  c(500, allprods_DF[i,"Price"]), 
                      Battery = c(8, allprods_DF[i,"Battery"]),
                      OS =  c("iOS", "Android"))
  
  pft[i] <- profit1(lm_res, prods, 2)
}

max_profit <- max(pft)
max_index <- which.max(pft)
print(allprods_DF[max_index,])
