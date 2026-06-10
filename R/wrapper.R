#' Checking indifference of commodity baskets
#'
#' This function verifies whether two consumption baskets have the same level of utility to the consumer.
#'
#' @param basket_A A numeric vector representing the quantities of goods in basket A.
#' @param basket_B A numeric vector representing the quantities of goods in basket B.
#' @param utility_f An R function that accepts a vector of goods and returns the utility level.
#' @param rho The evaluation environment for the utility function.
#'
#' @return A logical value (0 or 1).
#'
#' @useDynLib ConsumerTheory are_indifferent
#' @export
are_indifferent_r <- function(basket_A, basket_B, utility_f, rho = parent.frame()) {
  output <- .Call("are_indifferent",
                  basket_A,
                  basket_B,
                  utility_f,
                  rho)
  return(output)
}

#' Check budget concurrency of a basket
#'
#' This function checks whether the utility level of given basket is within given utility budget limit.
#'
#' @param basket A numeric vector representing the quantities of goods in the basket.
#' @param utility_f An R function representing the utility function.
#' @param budget A numeric value specifying the maximum allowable utility limit.
#' @param rho The evaluation environment.
#'
#' @return A logical value (0 or 1).
#'
#' @useDynLib ConsumerTheory are_concurrent
#' @export
are_concurrent_r <- function(basket, utility_f, budget, rho = parent.frame()) {
  output <- .Call("are_concurrent",
                  basket,
                  utility_f,
                  as.numeric(budget),
                  rho)
  return(output)
}

#' Price indexes (Laspeyres and Paasche)
#'
#' Calculates aggregate price change indices: the Laspeyres price index and the Paasche price index, that
#' indicate the percentage change in the cost of living. A price index value greater than 1 indicates an increase in 
#' the cost of living, while a value less than 1 indicates that the cost of living has fallen. The Laspeyres index measures
#' the cost of living based on a basket from the previous period, while the Paasche index measures it based on a basket from the current period.
#'
#' @param base_basket A vector of quantities in the base period (q_0).
#' @param base_price A vector of prices in the base period (p_0).
#' @param today_basket A vector of quantities in the current period (q_t).
#' @param today_price A vector of prices in the current period (p_t).
#'
#' @return A numeric vector of length 2: c(Laspeyres, Paasche).
#'
#' @useDynLib ConsumerTheory price_index
#' @export
price_index_r <- function(base_basket, base_price, today_basket, today_price) {
  output <- .Call("price_index",
                  base_basket,
                  base_price,
                  today_basket,
                  today_price)
  return(output)
}

#' Quantity indexes (Laspeyres and Paasche)
#'
#' Calculates aggregate quantity change indices: the Laspeyres quantity index and the Paasche quantity index.
#' If index > 1, one can say that “average” consumption increased between time b and time t. It indicates an increase in consumption or an improvement in welfare.
#' If index < 1, one can say that “average” consumption decreased between periods b and t. It indicates a decline in welfare due to a decrease in consumption.
#' Laspeyres index < 1 indicates that the consumer prefers the basket from the previous period over the basket from the current period; Their position was better in the base period than in period t
#' Paasche index > 1 indicates that the consumer prefers the current period basket to the previous-period basket; Their position in period t is better than it was in the base period.
#' 
#' @inheritParams price_index_r
#'
#' @return A numeric vector of length 2: c(Laspeyres, Paasche).
#'
#' @useDynLib ConsumerTheory quantity_index
#' @export
quantity_index_r <- function(base_basket, base_price, today_basket, today_price) {
  output <- .Call("quantity_index",
                  base_basket,
                  base_price,
                  today_basket,
                  today_price)
  return(output)
}

#' Total value changes index
#'
#' Calculates the ratio of total expenses in the current period to the base period.
#' If Paasche price index > index then the consumer prefers basket from period b to basket x from current period
#' If Laspeyres price index < index then the consumer prefers basket from current period to basket x from period b
#'
#' @inheritParams price_index_r
#'
#' @return A numeric value representing the value index.
#'
#' @useDynLib ConsumerTheory total_changes_index
#' @export
total_changes_index_r <- function(base_basket, base_price, today_basket, today_price) {
  output <- .Call("total_changes_index",
                  base_basket,
                  base_price,
                  today_basket,
                  today_price)
  return(output)
}

#' Universal consumer utility maximization
#'
#' Solves the consumer utility optimization problem using the Nelder-Mead algorithm.
#' Calculated consumer demand is being called Marshall's / Walras demand function
#' 
#' @param prices A numeric vector of commodity prices.
#' @param income_or_non_wage Nominal income or non-wage income.
#' @param wage Given period wage rate.
#' @param max_time Maximum time endowment.
#' @param utility_f A utility function implemented in R.
#' @param rho The evaluation environment.
#' @param model_type Model type: 1 for classical model, 2 for labor market model.
#'
#' @return A vector of commodity quantities representing the optimal demand basket.
#'
#' @useDynLib ConsumerTheory maximize_utility_universal
#' @export
maximize_utility_universal_r <- function(prices, income_or_non_wage, wage, max_time, utility_f, rho = parent.frame(), model_type = 1) {
  output <- .Call("maximize_utility_universal",
                  prices,
                  as.numeric(income_or_non_wage),
                  as.numeric(wage),
                  as.numeric(max_time),
                  utility_f,
                  rho,
                  as.integer(model_type))
  return(output)
}

#' Marginal utility
#'
#' Calculates the vector of marginal utilities for all goods in the basket.
#' The marginal utility of the i-th good indicates by how much the utility of the basket will change if
#' the quantity of the i-th good increases by one unit, while the quantities of the other goods remain unchanged.
#' If the consumption of the i-th good or service increases while the consumption of the other goods
#' and services remains constant, then the total utility also increases.
#' 
#' @param utility_f A utility function implemented in R.
#' @param basket A vector of commodity quantities.
#' @param rho Numerical differentiation step size (h_val).
#'
#' @return A numeric vector containing MU values for each good.
#'
#' @useDynLib ConsumerTheory marginal_utility
#' @export
marginal_utility_r <- function(utility_f, basket, rho = 1e-4) {
  output <- .Call("marginal_utility",
                  utility_f,
                  basket,
                  as.numeric(rho))
  return(output)
}

#' Marginal rate of substitution (MRS)
#'
#' Calculates the Marginal Rate of Substitution of good i for good j.
#' It indicates by how much (approximately) the quantity of the jth good in basket should be increased when the
#' quantity of the ith good is reduced by one unit, so that the utility of the basket remains unchanged.
#'
#' @param utility_f A utility function implemented in R.
#' @param basket A vector of commodity quantities.
#' @param i Index of the first good.
#' @param j Index of the second good.
#' @param rho Numerical differentiation step size.
#'
#' @return A numeric value specifying the MRS.
#'
#' @useDynLib ConsumerTheory marginal_utility_i_by_j
#' @export
marginal_utility_i_by_j_r <- function(utility_f, basket, i, j, rho = 1e-4) {
  output <- .Call("marginal_utility_i_by_j",
                  utility_f,
                  basket,
                  as.integer(i),
                  as.integer(j),
                  as.numeric(rho))
  return(output)
}

#' Verification of Gossen's first law
#' 
#' Verifies a ceteris paribus - an increase in the consumption of the i-th good or service, while the consumption of other goods
#' and services remains constant, leads to increasingly smaller increases in total utility, 
#' accompanied by decreases in the marginal utility of the i-th good
#' 
#' @param utility_f A utility function implemented in R.
#' @param variable A single-element vector (point x0) for derivative evaluation.
#' @param rho Numerical differentiation step size.
#'
#' @return A logical value (0 or 1).
#'
#' @useDynLib ConsumerTheory first_Gossen_law
#' @export
first_Gossen_law_r <- function(utility_f, variable, rho = 1e-4) {
  output <- .Call("first_Gossen_law",
                  utility_f,
                  as.numeric(variable),
                  as.numeric(rho))
  return(output)
}

#' Verification of Gossen's second law
#'
#' Verifies the consumer equilibrium condition – in order to maximize utility, the consumer chooses a basket
#' of goods in which the ratios of the marginal utilities of these goods are equal to the ratio of their prices.
#' The higher the price of the substitute good , the higher the marginal rate of substitution (assuming that the price pj remains constant).
#'
#' @param utility_f A utility function implemented in R.
#' @param basket The evaluated basket of goods.
#' @param i Index of good i.
#' @param j Index of good j.
#' @param prices A vector of market prices.
#' @param rho Numerical differentiation step size.
#'
#' @return A logical value (0 or 1).
#'
#' @useDynLib ConsumerTheory second_Gossen_law
#' @export
second_Gossen_law_r <- function(utility_f, basket, i, j, prices, rho = 1e-4) {
  output <- .Call("second_Gossen_law",
                  utility_f,
                  basket,
                  as.integer(i),
                  as.integer(j),
                  prices,
                  as.numeric(rho))
  return(output)
}

#' Elasticity of utility with respect to good consumption
#'
#' Calculates the partial elasticity of the utility function with respect to the i-th good.
#' Indicates the approximate percentage change in the utility of basket if the quantity of the i-th good
#' increases by one percent, while the quantities of the other goods remain unchanged. 
#' 
#' @inheritParams marginal_utility_i_by_j_r
#'
#' @return A numeric value representing the elasticity.
#'
#' @useDynLib ConsumerTheory usage_flexibility
#' @export
usage_flexibility_r <- function(utility_f, basket, i, rho = 1e-4) {
  output <- .Call("usage_flexibility",
                  utility_f,
                  basket,
                  as.integer(i),
                  as.numeric(rho))
  return(output)
}

#' Relative elasticity of utility substitution
#'
#' Calculates the ratio of utility elasticity of good i to good j.
#' Indicates the approximate percentage by which the quantity of the jth good in basket must be increased
#' while reducing the quantity of the ith good by one percent, so that the utility of that basket remains unchanged.
#'
#' @inheritParams marginal_utility_i_by_j_r
#'
#' @return A numeric value.
#'
#' @useDynLib ConsumerTheory substitution_flexibility_i_by_j
#' @export
substitution_flexibility_i_by_j_r <- function(utility_f, basket, i, j, rho = 1e-4) {
  output <- .Call("substitution_flexibility_i_by_j",
                  utility_f,
                  basket,
                  as.integer(i),
                  as.integer(j),
                  as.numeric(rho))
  return(output)
}

#' Income elasticity of demand
#'
#' Calculates the vector of Marshall's income elasticities of demand for each good.
#' Measures what percentage the demand for good i will change when income changes by one percent.
#' 
#' @inheritParams maximize_utility_universal_r
#' @param rho Numerical differentiation step size.
#'
#' @return A vector of income elasticities.
#'
#' @useDynLib ConsumerTheory income_flexibility_of_demand
#' @export
income_flexibility_of_demand_r <- function(prices, income_or_non_wage, wage, max_time, utility_f, rho = 1e-4, model_type = 1) {
  output <- .Call("income_flexibility_of_demand",
                  prices,
                  as.numeric(income_or_non_wage),
                  as.numeric(wage),
                  as.numeric(max_time),
                  utility_f,
                  as.numeric(rho),
                  as.integer(model_type))
  return(output)
}

#' Price elasticity of demand
#'
#' Calculates the own-price elasticity of demand for individual goods in the basket.
#' Measures what percentage the demand for good i will change when its price changes by one percent.
#' 
#' @inheritParams income_flexibility_of_demand_r
#'
#' @return A vector of price elasticities.
#'
#' @useDynLib ConsumerTheory price_flexibility_of_demand
#' @export
price_flexibility_of_demand_r <- function(prices, income_or_non_wage, wage, max_time, utility_f, rho = 1e-4, model_type = 1) {
  output <- .Call("price_flexibility_of_demand",
                  prices,
                  as.numeric(income_or_non_wage),
                  as.numeric(wage),
                  as.numeric(max_time),
                  utility_f,
                  as.numeric(rho),
                  as.integer(model_type))
  return(output)
}

#' Cross-Price elasticity of demand
#'
#' Calculates the elasticity of demand for good i with respect to a price change of good j.
#' How the change of good j price changes how much good i a customer buys.
#' 
#' @inheritParams income_flexibility_of_demand_r
#' @param i_good Index of good i.
#' @param j_price Index of the price of good j.
#'
#' @return A numeric value representing the cross-price elasticity.
#'
#' @useDynLib ConsumerTheory price_flexibility_of_demand_i_by_j
#' @export
price_flexibility_of_demand_i_by_j_r <- function(prices, i_good, j_price, income_or_non_wage, wage, max_time, utility_f, rho = 1e-4, model_type = 1) {
  output <- .Call("price_flexibility_of_demand_i_by_j",
                  prices,
                  as.integer(i_good),
                  as.integer(j_price),
                  as.numeric(income_or_non_wage),
                  as.numeric(wage),
                  as.numeric(max_time),
                  utility_f,
                  as.numeric(rho),
                  as.integer(model_type))
  return(output)
}

#' Economic classification of a good
#'
#' Automatically classifies the i-th good based on its income and price elasticity of demand.
#' Possible categories are:
#' * normal goods (an increase in consumer income leads to an increase in demand for the i-th good). They divide into:
#'      * basic (essential) - income elasticity in [0,1]
#'      * luxury - income elasticity greater than 1
#' * inferior goods (an increase in consumer income causes a decrease in demand for the i-th good) - income elasticity lesser than 0
#' There are also specific kind of goods, that behave unexpectedly:
#' * Giffen's good - type of inferior good, as the price of this good rises, so does the demand for it.
#' * Veblen's good - type of luxury good, as the price of this good rises, so does the demand for it. Indicator of a 'snob effect'.
#' 
#' @inheritParams income_flexibility_of_demand_r
#' @param good Index of the good being classified.
#'
#' @return A character string (the economic classification of the good).
#'
#' @useDynLib ConsumerTheory classify_good
#' @export
classify_good_r <- function(good, prices, income_or_non_wage, wage, max_time, utility_f, rho = 1e-4, model_type = 1) {
  output <- .Call("classify_good",
                  as.integer(good),
                  prices,
                  as.numeric(income_or_non_wage),
                  as.numeric(wage),
                  as.numeric(max_time),
                  utility_f,
                  as.numeric(rho),
                  as.integer(model_type))
  return(output)
}

#' Analysis of economic relationships between goods
#'
#' Determines the economic relationship between a pair of goods using cross-price elasticities.
#' Possible categories are:
#' * substitues - up to a factor of scale are interchangeable, they satisfy the same needs
#' * complementary - they are always consumed in equal proportions, any excess is wasted
#' * neutral - a change in the price of one of them does not affect the demand for the other 
#' * none of the above
#' 
#' @inheritParams income_flexibility_of_demand_r
#' @param i_good Index of the first good.
#' @param j_good Index of the second good.
#' @param i_price Index of the price of the first good.
#' @param j_price Index of the price of the second good.
#'
#' @return A character string indicating the relationship (e.g., substitutes).
#'
#' @useDynLib ConsumerTheory relationship_between_goods
#' @export
relationship_between_goods_r <- function(prices, i_good, j_good, i_price, j_price, income_or_non_wage, wage, max_time, utility_f, rho = 1e-4, model_type = 1) {
  output <- .Call("relationship_between_goods",
                  prices,
                  as.integer(i_good),
                  as.integer(j_good),
                  as.integer(i_price),
                  as.integer(j_price),
                  as.numeric(income_or_non_wage),
                  as.numeric(wage),
                  as.numeric(max_time),
                  utility_f,
                  as.numeric(rho),
                  as.integer(model_type))
  return(output)
}

#' Reservation wage
#'
#' Determines the reservation wage in the labor supply model.
#' It is the minimal wage above which a worker is willing to accept a job.
#'
#' @param prices A vector of consumer commodity prices.
#' @param income_non_wage Non-wage income.
#' @param max_time Maximum available time.
#' @param utility_f The utility function.
#' @param rho Numerical differentiation step size.
#'
#' @return A numeric value representing the reservation wage.
#'
#' @useDynLib ConsumerTheory reservation_wage
#' @export
reservation_wage_r <- function(prices, income_non_wage, max_time, utility_f, rho = 1e-4) {
  output <- .Call("reservation_wage",
                  prices,
                  as.numeric(income_non_wage),
                  as.numeric(max_time),
                  utility_f,
                  as.numeric(rho))
  return(output)
}

#' Consumer expenditure minimization (Hicksian demand)
#'
#' Finds the basket of goods that minimizes the costs required to achieve a target utility level.
#' 
#' @param prices A numeric vector of commodity prices.
#' @param target_utility The target utility level.
#' @param wage The wage rate.
#' @param max_time Maximum time.
#' @param utility_f The utility function.
#' @param rho The evaluation environment.
#' @param model_type Model type (1 or 2).
#'
#' @return A vector of commodity quantities (Hicksian demand basket).
#'
#' @useDynLib ConsumerTheory minimize_expenses
#' @export
minimize_expenses_r <- function(prices, target_utility, wage, max_time, utility_f, rho = parent.frame(), model_type = 1) {
  output <- .Call("minimize_expenses",
                  prices,
                  as.numeric(target_utility),
                  as.numeric(wage),
                  as.numeric(max_time),
                  utility_f,
                  rho,
                  as.integer(model_type))
  return(output)
}

#' Hicksian Price Elasticity of Demand
#'
#' Calculates the vector of compensated (Hicksian) price elasticities of demand.
#' Properties:
#' * is continuous and increasing
#' * we pay nothing for a zero basket
#' *  if everything becomes x times more expensive, we will pay x times more.
#'
#'
#' @inheritParams minimize_expenses_r
#' @param rho GSL machine numerical differentiation step size.
#'
#' @return A vector of compensated price elasticities of demand.
#'
#' @useDynLib ConsumerTheory Hicks_price_flexibility_of_demand
#' @export
Hicks_price_flexibility_of_demand_r <- function(prices, target_utility, wage, max_time, utility_f, rho = 1e-3, model_type = 1) {
  output <- .Call("Hicks_price_flexibility_of_demand",
                  prices,
                  as.numeric(target_utility),
                  as.numeric(wage),
                  as.numeric(max_time),
                  utility_f,
                  as.numeric(rho),
                  as.integer(model_type))
  return(output)
}