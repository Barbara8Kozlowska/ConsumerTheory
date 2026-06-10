library(tinytest)

u_cobb_douglas <- function(basket) {
  if (any(basket <= 0)) return(0)
  sqrt(basket[1] * basket[2])
}

prosta_uzytecznosc <- function(basket) {
  sum(basket)
}

u_ces <- function(basket) {
  if (any(basket <= 0)) return(0)
  (sqrt(basket[1]) + sqrt(basket[2]))^2
}

u_luxury_mock <- function(x) {
    if (any(x <= 0)) return(0)
    return((x[1] - 2)^0.8 * x[2]^0.2)
}

u_labor_leisure <- function(basket) {
  L <- basket[1]
  X <- basket[length(basket)]

  if (L <= 0 || X <= 0) return(0)

  sqrt(L * X)
}

market_prices <- c(2, 4)
consumer_income <- 100
target_utility_level <- 15.0

## 1. are_indifferent


expect_true(.Call("are_indifferent",c(4,9),c(6,6),u_cobb_douglas,.GlobalEnv))
expect_false(.Call("are_indifferent",c(4,9),c(5,5),u_cobb_douglas,.GlobalEnv))
expect_warning(.Call("are_indifferent",c(4,9),NULL,u_cobb_douglas,.GlobalEnv))


## 2. are_concurrent
 

expect_true(.Call("are_concurrent",c(3,5),prosta_uzytecznosc,10,.GlobalEnv))
expect_false(  .Call("are_concurrent",c(7,8),prosta_uzytecznosc,10,.GlobalEnv))
expect_warning(.Call("are_concurrent",c(3,5),prosta_uzytecznosc,-5,.GlobalEnv))


 
## 3. price_index
 

koszyk_0 <- c(10,50)
ceny_0   <- c(5,6)

koszyk_t <- c(12,40)
ceny_t   <- c(6,7.5)

ind <- .Call("price_index",koszyk_0,ceny_0,koszyk_t,ceny_t)
expect_equal(ind[1], 1.242857, tolerance = 1e-4)
expect_equal(ind[2], 1.24,     tolerance = 1e-4)

expect_warning(.Call("price_index",koszyk_0,c(5,-2),koszyk_t,ceny_t))


 
## 4. quantity_index
 

ind <- .Call("quantity_index",koszyk_0,ceny_0,koszyk_t,ceny_t)

expect_equal(ind[1], 0.857143, tolerance = 1e-4)
expect_equal(ind[2], 0.855172, tolerance = 1e-4)
expect_warning(.Call("quantity_index",koszyk_0,ceny_0,c(25,8,15),ceny_t))


## 5. total_changes_index
 

expect_equal(  .Call("total_changes_index",c(5,2),c(10,20),c(6,3),c(12,22)),  1.533333,tolerance = 1e-4)
 
## 6. maximize_utility


expect_equal(.Call("maximize_utility_universal",c(2,4),100,0,0,u_cobb_douglas,.GlobalEnv,1L),  c(25,12.5),  tolerance = 1e-3)
res_praca <- .Call("maximize_utility_universal",c(2),20,10,16,u_labor_leisure,.GlobalEnv,2L)

expect_true(is.numeric(res_praca))
expect_equal(length(res_praca), 2)
expect_true(all(res_praca >= 0))
expect_true(res_praca[1] <= 16)

 
## 7. marginal_utility
 

f_kwadratowa <- function(x) x^2
f_logarytm   <- function(x) log(x)

expect_equal(  .Call("marginal_utility",f_kwadratowa,c(5),1e-4),  c(10),tolerance = 1e-4)
expect_equal(  .Call("marginal_utility",f_logarytm,c(4),1e-4),  c(0.25),tolerance = 1e-4)

 
## 8. MRS
 

expect_equal(.Call("marginal_utility",u_cobb_douglas,c(4,9),1e-4),c(0.75,0.333333),tolerance = 1e-4)
expect_equal(.Call("marginal_utility_i_by_j",u_cobb_douglas,c(4,9),1L,2L,1e-4),-2.25,  tolerance = 1e-4)
 

## 9. Gossen
 

expect_true(  .Call("first_Gossen_law",u_cobb_douglas,5,1e-4))
expect_true(  .Call("second_Gossen_law",u_cobb_douglas,c(25,12.5),1L,2L,c(2,4),1e-4))
expect_false(  .Call("second_Gossen_law",u_cobb_douglas,c(10,20),1L,2L,c(2,4),1e-4))

 
## 10. usage/substitution
 

expect_equal(.Call("usage_flexibility",u_cobb_douglas,c(5,20),1L,1e-4),0.5,tolerance = 1e-4)
expect_equal(.Call("substitution_flexibility_i_by_j",u_cobb_douglas,c(5,20),1L,2L,1e-4),1,tolerance = 1e-4)

 
## 13. reservation_wage


expect_equal(.Call("reservation_wage",c(2),50,16,u_labor_leisure,1e-3),3.125,tolerance = 1e-3)


## 14. classify_good


res_luxury <- classify_good_r(good = 1,prices = c(5, 5),income_or_non_wage = 200,wage = 0, max_time = 0,utility_f = u_luxury_mock,rho = 1e-4, model_type = 1)
expect_true(res_luxury %in% c("Luxury good", "Unknown good"))

res_classify <- classify_good_r(good = 1,prices = market_prices,income_or_non_wage = consumer_income,wage = 0,max_time = 0,utility_f = u_cd,rho = 1e-4,model_type = 1)
expect_true(is.character(res_classify))
expect_equal(res_classify, "Basic good")

## 15. minimize_expenses


res_minimize <- minimize_expenses_r(prices = market_prices,target_utility = target_utility_level,wage = 0,max_time = 0,utility_f = u_cd,model_type = 1)

expect_true(is.numeric(res_minimize))
expect_equal(length(res_minimize), 2)
expect_equal(res_minimize[1], 21.2132, tolerance = 1e-3)
expect_equal(res_minimize[2], 10.6066, tolerance = 1e-3)


## 16. Hicks_price_flexibiliry_of_demand


res_hicks_flex <- Hicks_price_flexibility_of_demand_r(prices = market_prices,target_utility = target_utility_level,wage = 0,max_time = 0,utility_f = u_cd,rho = 1e-3,model_type = 1)

expect_true(is.numeric(res_hicks_flex))
expect_equal(length(res_hicks_flex), 2)
expect_equal(res_hicks_flex[1], -0.5, tolerance = 1e-2)
expect_equal(res_hicks_flex[2], 0.5, tolerance = 1e-2)
