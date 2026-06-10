#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

#define INF R_PosInf

typedef struct {
    SEXP r_func;
    SEXP env;
    SEXP prices;
    double wage;
    double total_income;
    double max_time;
    int n_goods;
    int mode;
} OptData;

typedef struct {
    SEXP f;
    int rzad;
    double h;
    SEXP r_function;

    SEXP prices;
    int idx_i;
    int idx_j;
    SEXP income;
    SEXP wage;
    SEXP max_time;
    SEXP utility_f;

    SEXP env;
    SEXP model_type;
} derivative_info;

typedef struct {
    double (*function)(double, void *);
    void *params;
} gsl_function;

int deriv_central(const gsl_function *F, double x, double h, double *result, double *abserr) {
    double fp = F->function(x + h, F->params);
    double fm = F->function(x - h, F->params);

    *result = (fp - fm) / (2.0 * h);

    if (abserr != NULL)
        *abserr = 0.0;

    return 0;
}

SEXP minimize_expenses(SEXP prices, SEXP target_utility, SEXP wage, SEXP max_time, SEXP utility_f, SEXP rho, SEXP model_type);
SEXP maximize_utility_universal(SEXP prices, SEXP income_or_non_wage, SEXP wage, SEXP max_time, SEXP utility_f, SEXP env, SEXP model_type);

static double prepare_f(double x, void *params) {

    derivative_info *ctx = (derivative_info *) params;
    if (ctx->rzad == 0) {
        if (ctx->prices == R_NilValue) {

            SEXP val = PROTECT(ScalarReal(x));
            SEXP call = PROTECT(lang2(ctx->r_function, val));
            SEXP res = PROTECT(eval(call, R_GlobalEnv));

            double out = REAL(res)[0];

            UNPROTECT(3);

            return out;
        }

        SEXP basket;

        if (ctx->idx_j == -1) {

            SEXP mod_income = PROTECT(ScalarReal(x));
            basket = PROTECT(maximize_utility_universal(ctx->prices,mod_income,ctx->wage,ctx->max_time,ctx->utility_f,ctx->env,ctx->model_type));

            if (ctx->idx_i < 0 || ctx->idx_i >= LENGTH(basket))
                error("idx_i out of bounds");

            double out = REAL(basket)[ctx->idx_i];
            UNPROTECT(2);
            return out;
        }

        int n = LENGTH(ctx->prices);

        SEXP mod_prices = PROTECT(allocVector(REALSXP, n));

        for (int k = 0; k < n; k++)
            REAL(mod_prices)[k] = REAL(ctx->prices)[k];

        if (ctx->idx_j < 0 || ctx->idx_j >= n)
            error("idx_j out of bounds");

        REAL(mod_prices)[ctx->idx_j] = x;

        ctx->prices = mod_prices;
        if (ctx->f != R_NilValue) {
            basket = PROTECT(minimize_expenses(mod_prices,ctx->income,ctx->wage,ctx->max_time,ctx->utility_f,ctx->env,ctx->f));

        } else {
            ctx->prices = mod_prices;
            basket = PROTECT(maximize_utility_universal(mod_prices,ctx->income,ctx->wage,ctx->max_time,ctx->utility_f,ctx->env,ctx->model_type));
        }

        if (ctx->idx_i < 0 || ctx->idx_i >= LENGTH(basket))
            error("idx_i out of bounds");

        double out = REAL(basket)[ctx->idx_i];

        UNPROTECT(2);
        return out;
    }

    derivative_info lower = *ctx;
    lower.rzad = ctx->rzad - 1;

    gsl_function F = {
        .function = &prepare_f,
        .params = &lower
    };

    double wynik, blad;

    deriv_central(&F, x, ctx->h, &wynik, &blad);

    return wynik;
}

double target_function(int n_params, double *x_params, void *ex) {
    OptData *data = (OptData *)ex;

    int vector_len = (data->mode == 2)? (data->n_goods + 1) : data->n_goods;

    SEXP x_sexp = PROTECT(allocVector(REALSXP, vector_len));

    double spent = 0.0;
    double *p_array = REAL(data->prices);

    if (data->mode == 2) {

        //TRYB RYNKU PRACY

        double leisure = x_params[0];

        if (leisure < 0.0 || leisure > data->max_time) {
            UNPROTECT(1);
            return INF;
        }

        REAL(x_sexp)[0] = leisure;

        for (int i = 0; i < data->n_goods; i++) {
            double good_val = x_params[i + 1];
            if (good_val < 0.0) {
                UNPROTECT(1);
                return INF;
            }

            REAL(x_sexp)[i + 1] = good_val;
            spent += p_array[i] * good_val;
        }

    } else {

    //TRYB KLASYCZNY

        for (int i = 0; i < data->n_goods; i++) {

            double good_val = x_params[i];
            if (good_val < 0.0) {
                UNPROTECT(1);
                return INF;
            }

            REAL(x_sexp)[i] = good_val;
            spent += p_array[i] * good_val;
        }
    }

    SEXP R_call = PROTECT(lang2(data->r_func, x_sexp));
    SEXP res    = PROTECT(eval(R_call, data->env));

    double u_val = REAL(res)[0];
    double penalty = 0.0;

    if (data->mode == 1) {
        double budget_error = spent - data->total_income;
        penalty = 1e6 * budget_error * budget_error;
    } else {

        double budget_error =
            spent +
            data->wage * REAL(x_sexp)[0] -
            (data->wage * data->max_time +
             data->total_income);

        penalty =
            1e6 * budget_error * budget_error;
    }

    UNPROTECT(3);
    return -u_val + penalty;
}

SEXP derivative_nth(SEXP utility_f, SEXP variable, SEXP order, SEXP h_step, SEXP rho)
{
    int n = INTEGER(order)[0];
    double x0 = REAL(variable)[0];
    double h_val = REAL(h_step)[0];

    if (n < 1) {
        error("Order of the derivative must be a positive number!");
    }

    derivative_info start = {0};

    start.f = R_NilValue;
    start.r_function = utility_f;
    start.rzad = n;
    start.h = h_val;
    start.prices = R_NilValue;
    start.idx_i = 0;
    start.idx_j = 0;
    start.income = R_NilValue;
    start.wage = R_NilValue;
    start.max_time = R_NilValue;
    start.utility_f = utility_f;
    start.env = rho;

    SEXP mode = PROTECT(ScalarInteger(1));
    start.model_type = mode;

    double end = prepare_f(x0, &start);
    UNPROTECT(1);
    return ScalarReal(end);
}

SEXP are_indifferent(SEXP basket_A, SEXP basket_B, SEXP utility_f, SEXP rho){
    if(basket_A== R_NilValue || basket_B== R_NilValue) {
        SEXP error_res = PROTECT(allocVector(LGLSXP, 1));
        LOGICAL(error_res)[0] = NA_LOGICAL;
        warning("No R_NilValue values allowed");
        UNPROTECT(1);
        return error_res;
    };

    SEXP call_A = PROTECT(lang2(utility_f, basket_A));
    SEXP call_B = PROTECT(lang2(utility_f, basket_B));

    SEXP res_A = PROTECT(eval(call_A,rho));
    SEXP res_B = PROTECT(eval(call_B, rho));

    double u_A = REAL(res_A)[0];
    double u_B = REAL(res_B)[0];

    SEXP result = PROTECT(allocVector(LGLSXP, 1));

    if(fabs(u_A - u_B) < 1e-6) {
        LOGICAL(result)[0] = 1; }
    else {
        LOGICAL(result)[0] = 0;
    }

    UNPROTECT(5);
    return result;
}

SEXP are_concurrent(SEXP basket, SEXP utility_f, SEXP budget, SEXP rho){
    if(basket == R_NilValue || LENGTH(basket)==0 || utility_f== R_NilValue || budget== R_NilValue || asReal(budget) < 0) {
        SEXP error_res = PROTECT(allocVector(REALSXP, 1));
        REAL(error_res)[0] = NA_REAL;
        warning("No R_NilValue values allowed");
        UNPROTECT(1);
        return error_res;
    };

    SEXP call = PROTECT(lang2(utility_f, basket));
    SEXP res = PROTECT(eval(call,rho));

    double u = REAL(res)[0];
    SEXP result = PROTECT(allocVector(LGLSXP, 1));

    if(u < asReal(budget)) {
        LOGICAL(result)[0] = 1; }
    else {
        LOGICAL(result)[0] = 0;
    }

    UNPROTECT(3);
    return result;
}

SEXP price_index(SEXP base_basket, SEXP base_price, SEXP today_basket, SEXP today_price){
    if(base_basket==R_NilValue || base_price==R_NilValue || today_basket==R_NilValue || today_price==R_NilValue) {
        SEXP error_res = PROTECT(allocVector(REALSXP, 2));
        REAL(error_res)[0] = NA_REAL;
        REAL(error_res)[1] = NA_REAL;
        warning("No NULL values allowed");
        UNPROTECT(1);
        return error_res;
    };

    int n = LENGTH(base_basket); //Zakladam, ze dlugosci rowne - jesli nie ustawione wartosci 0

    if (n == 0) {
    SEXP error_res = PROTECT(allocVector(REALSXP, 2));
        REAL(error_res)[0] = NA_REAL;
        REAL(error_res)[1] = NA_REAL;
        warning("Lengths of baskets have to be positive");
        UNPROTECT(1);
        return error_res;
    }

    if(LENGTH(today_basket) != n){
        SEXP error_res = PROTECT(allocVector(REALSXP, 2));
        REAL(error_res)[0] = NA_REAL;
        REAL(error_res)[1] = NA_REAL;
        warning("Lengths of today and base baskets have to be equal. If they are not, fill missing values with zeroes");
        UNPROTECT(1);
        return error_res;
    }

    double* q_0 = REAL(base_basket);
    double* p_0 = REAL(base_price);
    double* q_t = REAL(today_basket);
    double* p_t = REAL(today_price);

    double up_L=0.0, up_P = 0.0;
    double down_L=0.0, down_P = 0.0;

    for (int i=0; i<n; i++ ){
        if (p_0[i] < 0.0 || p_t[i] < 0.0 || q_0[i] < 0.0 || q_t[i] < 0.0) {
            SEXP error_res = PROTECT(allocVector(REALSXP, 2));
            REAL(error_res)[0] = NA_REAL;
            REAL(error_res)[1] = NA_REAL;
            warning("Prices and quantities have to be positive");
            
            UNPROTECT(1);
            return error_res;
        }
            down_L += p_0[i] * q_0[i];
            up_L += p_t[i] * q_0[i];
            down_P += p_0[i] * q_t[i];
            up_P   += p_t[i] * q_t[i];
    }

    if (down_L < 1e-9 || down_P < 1e-9) {
        SEXP error_res = PROTECT(allocVector(REALSXP, 2));
        REAL(error_res)[0] = NA_REAL;
        REAL(error_res)[1] = NA_REAL;
        warning("Zero base values - division cannot be performed");
        UNPROTECT(1);
        return error_res;
    }

    SEXP result = PROTECT(allocVector(REALSXP, 2));
    REAL(result)[0] = up_L/down_L; // Laspeyres
    REAL(result)[1] = up_P/down_P; // Paasche

    UNPROTECT(1);
    return result;
}

SEXP quantity_index(SEXP base_basket, SEXP base_price, SEXP today_basket, SEXP today_price){
     if(base_basket==R_NilValue || base_price==R_NilValue || today_basket==R_NilValue || today_price==R_NilValue) {
        SEXP error_res = PROTECT(allocVector(REALSXP, 2));
        REAL(error_res)[0] = NA_REAL;
        REAL(error_res)[1] = NA_REAL;
        warning("No NULL values allowed");
        UNPROTECT(1);
        return error_res;
    };
    
    int n = LENGTH(base_basket);

    if (n == 0) {
    SEXP error_res = PROTECT(allocVector(REALSXP, 2));
        REAL(error_res)[0] = NA_REAL;
        warning("Lengths of baskets have to be positive");
        UNPROTECT(1);
        return error_res;
    }

    if(LENGTH(today_basket) != n){
        SEXP error_res = PROTECT(allocVector(REALSXP, 2));
        REAL(error_res)[0] = NA_REAL;
        REAL(error_res)[1] = NA_REAL;
        warning("Lengths of today and base baskets should be equal. If they are not, fill missing values with zeroes");
        UNPROTECT(1);
        return error_res;
    }

    double* q_0 = REAL(base_basket);
    double* p_0 = REAL(base_price);
    double* q_t = REAL(today_basket);
    double* p_t = REAL(today_price);

    double up_L =0.0,  up_P = 0.0;
    double down_L =0.0, down_P = 0.0;

for (int i = 0; i < n; i++) {
        if (p_0[i] < 0.0 || p_t[i] < 0.0 || q_0[i] < 0.0 || q_t[i] < 0.0) {
            SEXP error_res = PROTECT(allocVector(REALSXP, 2));
            REAL(error_res)[0] = NA_REAL;
            REAL(error_res)[1] = NA_REAL;
            warning("Prices and quantities have to be positive");
            
            UNPROTECT(1);
            return error_res;
        }

        down_L += p_0[i] * q_0[i];
        up_L   += p_0[i] * q_t[i];
        down_P += p_t[i] * q_0[i];
        up_P   += p_t[i] * q_t[i];
    }

    if (down_L < 1e-9 || down_P < 1e-9) {
        SEXP error_res = PROTECT(allocVector(REALSXP, 2));
        REAL(error_res)[0] = NA_REAL;
        REAL(error_res)[1] = NA_REAL;
        warning("Zero base values - division cannot be performed");
        UNPROTECT(1);
        return error_res;
    }

    SEXP result = PROTECT(allocVector(REALSXP, 2));
    REAL(result)[0] = up_L/down_L; //Laspeyres
    REAL(result)[1] = up_P/down_P; //Paasche

    UNPROTECT(1);
    return result;
}

SEXP total_changes_index(SEXP base_basket, SEXP base_price, SEXP today_basket, SEXP today_price){
     if(base_basket==R_NilValue || base_price==R_NilValue || today_basket==R_NilValue || today_price==R_NilValue) {
        SEXP error_res = PROTECT(allocVector(REALSXP, 1));
        REAL(error_res)[0] = NA_REAL;
        warning("No NULL values allowed");
        UNPROTECT(1);
        return error_res;
    };
    
    int n = LENGTH(base_basket);

    if (n == 0) {
    SEXP error_res = PROTECT(allocVector(REALSXP, 1));
        REAL(error_res)[0] = NA_REAL;
        warning("Lengths of baskets have to be positive");
        UNPROTECT(1);
        return error_res;
    }

    if(LENGTH(today_basket) != n){
        SEXP error_res = PROTECT(allocVector(REALSXP, 1));
        REAL(error_res)[0] = NA_REAL;
        warning("Lengths of today and base baskets should be equal. If they are not, fill missing values with zeroes");
        UNPROTECT(1);
        return error_res;
    }

    double* q_0 = REAL(base_basket);
    double* p_0 = REAL(base_price);
    double* q_t = REAL(today_basket);
    double* p_t = REAL(today_price);

    double up =0.0,  down=0.0;

for (int i = 0; i < n; i++) {
        if (p_0[i] < 0.0 || p_t[i] < 0.0 || q_0[i] < 0.0 || q_t[i] < 0.0) {
            SEXP error_res = PROTECT(allocVector(REALSXP, 1));
            REAL(error_res)[0] = NA_REAL;
            warning("Prices and quantities have to be positive");
            
            UNPROTECT(1);
            return error_res;
        }

        down += p_0[i] * q_0[i];
        up += p_t[i] * q_t[i];
    }

        if (down < 1e-9) {
        SEXP error_res = PROTECT(allocVector(REALSXP, 1));
        REAL(error_res)[0] = NA_REAL;
        warning("Zero base values - division cannot be performed");
        UNPROTECT(1);
        return error_res;
    }

    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = up/down;

    UNPROTECT(1);
    return result;
}

SEXP maximize_utility_universal(
    SEXP prices,
    SEXP income_or_non_wage,
    SEXP wage,
    SEXP max_time,
    SEXP utility_f,
    SEXP env,
    SEXP model_type) {

    int protect_count = 0;

    if (!isReal(prices)) {
        prices = PROTECT(coerceVector(prices, REALSXP));
    } else {
        PROTECT(prices);
    }
    protect_count++;

    if (!isFunction(utility_f)) {
        UNPROTECT(protect_count);
        error("Utility have to be a function");
    }

    int n_goods = LENGTH(prices);
    int mode = asInteger(model_type);

    OptData data;
    data.r_func = utility_f;
    data.env = env;
    data.prices = prices; 
    data.n_goods = n_goods;
    data.mode = mode;
    
    int n_params = (mode == 2) ? (n_goods + 1) : n_goods;
    double *xin = (double *)R_alloc(n_params + 1, sizeof(double));
    double *xout = (double *)R_alloc(n_params + 1, sizeof(double));

    if (mode == 2) {
        data.wage = asReal(wage);
        data.max_time = asReal(max_time);
        data.total_income = asReal(income_or_non_wage);
        
        xin[0] = data.max_time / 2.0;
        double full_budget = data.wage * data.max_time + data.total_income;
        for (int i = 0; i < n_goods; i++) {
            xin[i + 1] = full_budget / (n_goods * REAL(prices)[i]); 
        }
    } else {
        data.wage = 0.0;
        data.max_time = 0.0;
        data.total_income = asReal(income_or_non_wage);

        for (int i = 0; i < n_goods; i++) {
            xin[i] = data.total_income / (n_goods * REAL(prices)[i]);
        }
    }

    double ynew = 0.0;
    int fail = 0;
    int fncount = 0;
    int maxfeval = 5000;

    nmmin(n_params, xin, xout, &ynew, target_function, &fail, 1e-9, 1e-8, &data, 1.0, 0.5, 2.0, 0, &fncount, maxfeval);

    SEXP final_basket;
    double spent = 0.0;

    if (mode == 2) {
        final_basket = PROTECT(allocVector(REALSXP, n_goods + 1));
        protect_count++;
        REAL(final_basket)[0] = xout[0];
        for (int i = 0; i < n_goods; i++) {
            REAL(final_basket)[i + 1] = xout[i + 1];
            spent += REAL(prices)[i] * xout[i + 1];
        }
    } else {
        final_basket = PROTECT(allocVector(REALSXP, n_goods));
        protect_count++;
        for (int i = 0; i < n_goods - 1; i++) {
            REAL(final_basket)[i] = xout[i]; 
            spent += REAL(prices)[i] * xout[i];
        }
        double remaining = data.total_income - spent;
        REAL(final_basket)[n_goods - 1] = (remaining < 0.0 ? 0.0 : remaining) / REAL(prices)[n_goods - 1];
    }

    UNPROTECT(protect_count);
    return final_basket;
}

double eval_f(double x, void *params) {
    SEXP call = PROTECT(lang2((SEXP)params, ScalarReal(x)));
    SEXP res  = PROTECT(eval(call, R_GlobalEnv));
    double out = REAL(res)[0];
    UNPROTECT(2);
    return out;
} 

typedef struct {
    SEXP r_func;
    SEXP mod_basket;
    int target_idx;
} MarginalData;

static double eval_marginal_f(double x, void *params) {
    MarginalData *data = (MarginalData *)params;
    REAL(data->mod_basket)[data->target_idx] = x;
    
    SEXP R_call = PROTECT(lang2(data->r_func, data->mod_basket));
    SEXP res = PROTECT(eval(R_call, R_GlobalEnv));
    double u_val = REAL(res)[0];
    
    UNPROTECT(2);
    return u_val;
}

SEXP marginal_utility(SEXP utility_f, SEXP basket, SEXP rho) {
    if (!isReal(basket)) basket = PROTECT(coerceVector(basket, REALSXP));
    else PROTECT(basket);

    int n_goods = LENGTH(basket);
    double h_val = REAL(rho)[0];

    SEXP mod_basket = PROTECT(allocVector(REALSXP, n_goods));
    SEXP mu_vector = PROTECT(allocVector(REALSXP, n_goods));
    
    MarginalData data;
    data.r_func = utility_f;
    data.mod_basket = mod_basket;

    for (int i = 0; i < n_goods; i++) {
        data.target_idx = i;
        for (int k = 0; k < n_goods; k++) {
            REAL(mod_basket)[k] = REAL(basket)[k];
        }
        
         gsl_function F;
        F.function = &eval_marginal_f;
        F.params = &data;
        
        double wynik, blad;
        deriv_central(&F, REAL(basket)[i], h_val, &wynik, &blad);
        
        REAL(mu_vector)[i] = wynik;
    }

    UNPROTECT(3); 
    return mu_vector;
}

SEXP marginal_utility_i_by_j(SEXP utility_f, SEXP basket, SEXP i, SEXP j, SEXP rho) {
    SEXP mu_vector = PROTECT(marginal_utility(utility_f, basket, rho));
    
    int idx_i = asInteger(i) - 1;
    int idx_j = asInteger(j) - 1;
    int n_goods = LENGTH(mu_vector);
    
    if (idx_i < 0 || idx_i >= n_goods || idx_j < 0 || idx_j >= n_goods) {
        UNPROTECT(1);
        error("Index i is outside of the basket reach");
    }

    double mu_i = REAL(mu_vector)[idx_i];
    double mu_j = REAL(mu_vector)[idx_j];
    
    if (mu_j == 0.0) {
        UNPROTECT(1);
        error("Utility equals zero - cannot perform a division");
    }
    
    double mrs_value = (-1.0) * mu_i / mu_j;
    UNPROTECT(1);
    return ScalarReal(mrs_value);
}

SEXP first_Gossen_law(SEXP utility_f, SEXP variable, SEXP rho) {
    SEXP order_1 = PROTECT(ScalarInteger(1));
    SEXP order_2 = PROTECT(ScalarInteger(2));
    
    SEXP first = derivative_nth(utility_f, variable, order_1, rho, R_GlobalEnv);
    SEXP second = derivative_nth(utility_f, variable, order_2, rho, R_GlobalEnv);

    SEXP result = PROTECT(allocVector(LGLSXP, 1));
    double mu = REAL(first)[0];
    double d_mu = REAL(second)[0];

    if(mu< 0.0 || d_mu > 0.0) {
        LOGICAL(result)[0] = 0; }
    else {
        LOGICAL(result)[0] = 1;
    }

    UNPROTECT(3);
    return result;
}

SEXP second_Gossen_law(SEXP utility_f, SEXP basket, SEXP i, SEXP j, SEXP prices, SEXP rho){
    SEXP utility_mrs = PROTECT(marginal_utility_i_by_j(utility_f, basket, i, j, rho));
    
    int idx_i = asInteger(i) - 1;
    int idx_j = asInteger(j) - 1;
    double p_i = REAL(prices)[idx_i];
    double p_j = REAL(prices)[idx_j];
    
    if (p_j == 0.0) {
        UNPROTECT(1);
        error("Goods price equals zero!");
    }
    
    double price_ratio = (-1.0) * p_i / p_j;
    double mrs_value = REAL(utility_mrs)[0];
    SEXP result = PROTECT(allocVector(LGLSXP, 1));

    if (fabs(price_ratio - mrs_value) < 1e-4) {
        LOGICAL(result)[0] = 1; }
    else {
        LOGICAL(result)[0] = 0;
    }

    UNPROTECT(2);
    return result;
}

SEXP usage_flexibility(SEXP utility_f, SEXP basket, SEXP i, SEXP rho) {
    SEXP mu_vector = PROTECT(marginal_utility(utility_f, basket, rho));
    
    int idx_i = asInteger(i) - 1; 
    int n_goods = LENGTH(mu_vector);

    if (idx_i < 0 || idx_i >= n_goods) {
        UNPROTECT(1);
        error("Index i is outside of the basket reach");
    }

    SEXP R_call = PROTECT(lang2(utility_f, basket));
    SEXP res_u = PROTECT(eval(R_call, R_GlobalEnv));
    
    double u_val = REAL(res_u)[0];
    if (u_val == 0.0) {
        UNPROTECT(3);
        error("Utility equals zero - cannot perform a division");
    }

    double mu_i = REAL(mu_vector)[idx_i];
    double x_i = REAL(basket)[idx_i];
    double flexibility = mu_i * x_i / u_val;

    UNPROTECT(3);
    return ScalarReal(flexibility);
}

SEXP substitution_flexibility_i_by_j(SEXP utility_f, SEXP basket, SEXP i, SEXP j, SEXP rho) {
    SEXP up = PROTECT(usage_flexibility(utility_f, basket, i, rho));
    SEXP down = PROTECT(usage_flexibility(utility_f, basket, j, rho));

    double e_i = REAL(up)[0];
    double e_j = REAL(down)[0];

    if (e_j == 0.0) {
        UNPROTECT(2);
        error("Zero values - division cannot be performed");
    }

    double sub_flex = e_i / e_j;
    UNPROTECT(2);
    return ScalarReal(sub_flex);
}


SEXP income_flexibility_of_demand(SEXP prices, SEXP income_or_non_wage, SEXP wage, SEXP max_time, SEXP utility_f, SEXP rho, SEXP model_type) {     
    int protect_count = 0;
    double m0 = asReal(income_or_non_wage);

    PROTECT(prices); protect_count++;
    PROTECT(income_or_non_wage); protect_count++;
    PROTECT(wage); protect_count++;
    PROTECT(max_time); protect_count++;
    PROTECT(utility_f); protect_count++;
    PROTECT(model_type); protect_count++;
    if (rho != R_NilValue) { PROTECT(rho); protect_count++; }

    SEXP basket_center = PROTECT(maximize_utility_universal(prices, income_or_non_wage, wage, max_time, utility_f, R_GlobalEnv, model_type));
    protect_count++;
    
    int n_elements = LENGTH(basket_center);
    SEXP elasticities = PROTECT(allocVector(REALSXP, n_elements));
    protect_count++;

    derivative_info start = {0};
    start.f = R_NilValue;
    start.rzad = 1;
    start.h = isReal(rho) ? REAL(rho)[0] : 1e-4;
    start.r_function = R_NilValue;
    start.prices = prices;
    start.income = income_or_non_wage;
    start.idx_j = -1;
    start.wage = wage;
    start.max_time = max_time;
    start.utility_f = utility_f;
    start.env = R_GlobalEnv; 
    start.model_type = model_type;

    for (int i = 0; i < n_elements; i++) {
        double x_center = REAL(basket_center)[i];
        if (x_center < 1e-9) {
            REAL(elasticities)[i] = NA_REAL;
            continue;
        }

        start.idx_i = i;
        double derivative = prepare_f(m0, &start); 
        REAL(elasticities)[i] = derivative * (m0 / x_center);
    }

    UNPROTECT(protect_count);     
    return elasticities;
}

SEXP price_flexibility_of_demand(SEXP prices, SEXP income_or_non_wage, SEXP wage, SEXP max_time, SEXP utility_f, SEXP rho, SEXP model_type) {    
    int protect_count = 0;

    if (!isReal(prices)) {
        prices = PROTECT(coerceVector(prices, REALSXP));
    } else {
        PROTECT(prices);
    }
    protect_count++;

    PROTECT(income_or_non_wage); protect_count++;
    PROTECT(wage); protect_count++;
    PROTECT(max_time); protect_count++;
    PROTECT(utility_f); protect_count++;
    PROTECT(model_type); protect_count++;
    if (rho != R_NilValue) { PROTECT(rho); protect_count++; }


    SEXP basket_center = PROTECT(maximize_utility_universal(prices, income_or_non_wage, wage, max_time, utility_f, R_GlobalEnv, model_type));
    protect_count++;
    
    int n_elements = LENGTH(basket_center);
    int n_goods = LENGTH(prices);

    SEXP elasticities = PROTECT(allocVector(REALSXP, n_elements));
    protect_count++;

    derivative_info start = {0};
    start.f = R_NilValue;
    start.rzad = 1;
    start.h = fmax(isReal(rho) ? REAL(rho)[0] : 1e-4, 1e-6);
    start.r_function = R_NilValue;
    start.prices = prices;
    start.income = income_or_non_wage;
    start.wage = wage;
    start.max_time = max_time;
    start.utility_f = utility_f;
    start.model_type = model_type;
    start.env = R_GlobalEnv; 

    for (int i = 0; i < n_elements; i++) {
        double x_center = REAL(basket_center)[i];
        int p_idx = (i >= n_goods) ? (n_goods - 1) : i; 
        double p0 = REAL(prices)[p_idx];

        if (x_center < 1e-9 || p0 <= 0.0) {
            REAL(elasticities)[i] = NA_REAL;
            continue;
        }

        start.idx_i = i;
        start.idx_j = p_idx;
        double derivative = prepare_f(p0, &start); 
        REAL(elasticities)[i] = derivative * (p0 / x_center);
    }

    UNPROTECT(protect_count); 
    return elasticities;
}

SEXP price_flexibility_of_demand_i_by_j(SEXP prices, SEXP i_good, SEXP j_price, SEXP income_or_non_wage, SEXP wage, SEXP max_time, SEXP utility_f, SEXP rho, SEXP model_type) {
    int idxi = asInteger(i_good)-1;
    int idxj = asInteger(j_price)-1;

    if (!isReal(prices)) prices = PROTECT(coerceVector(prices, REALSXP));
    else PROTECT(prices);

    double p0j = REAL(prices)[idxj];
    
    SEXP basket_center = PROTECT(maximize_utility_universal(prices, income_or_non_wage, wage, max_time, utility_f, R_GlobalEnv, model_type)); // Stos = 2
    int n_elements = LENGTH(basket_center);

    if (idxi < 0 || idxi >= n_elements || idxj < 0 || idxj >= LENGTH(prices)) {
        UNPROTECT(2);
        error("Goods indexes or prices get out of the basket range");
    }

    double x_center = REAL(basket_center)[idxi]; 
    
    if (x_center < 1e-9) {
        UNPROTECT(2);
        return ScalarReal(NA_REAL);
    }

    derivative_info start = {0};

    start.f = R_NilValue;
    start.r_function = R_NilValue;
    start.rzad = 1;
    start.h = isReal(rho) ? REAL(rho)[0] : 1e-4;
    start.prices = prices;
    start.idx_i = idxi;
    start.idx_j = idxj;
    start.income = income_or_non_wage;
    start.wage = wage;
    start.max_time = max_time;
    start.utility_f = utility_f;
    start.env = R_GlobalEnv;
    start.model_type = model_type;

    double derivative = prepare_f(p0j, &start); 
    UNPROTECT(2); 
    return ScalarReal(derivative * (p0j / x_center));
}

SEXP classify_good(SEXP good, SEXP prices, SEXP income_or_non_wage, SEXP wage, SEXP max_time, SEXP utility_f, SEXP rho, SEXP model_type) {
    SEXP income_flex = PROTECT(income_flexibility_of_demand(prices, income_or_non_wage, wage, max_time, utility_f, rho, model_type));
    SEXP demand_flex = PROTECT(price_flexibility_of_demand(prices, income_or_non_wage, wage, max_time, utility_f, rho, model_type));

    int idx = asInteger(good) - 1; 
    int n_elements = LENGTH(income_flex);

    if (idx < 0 || idx >= n_elements) {
        UNPROTECT(2);
        error("Wrong index or too much indexes in the basket");
    }

    double inf = REAL(income_flex)[idx];
    double df  = REAL(demand_flex)[idx];
    const char* classification = "Unknown good";

    if (ISNA(inf) || ISNA(df)) {
        classification = "NA";
    }

    else if (inf < 0.0) {
        if (df > 0.0) {
            classification = "Giffen good"; 
        } else {
            classification = "Inferior good";
        }
    } 
    else if (inf >= 0.0 && inf < 1.0) {
        classification = "Basic good";
    } 
    else {
        if (df > 0.0) {
            classification = "Veblen good"; 
         } else {
            classification = "Luxury good";
        }
    }

    SEXP wynik = PROTECT(mkString(classification));
    UNPROTECT(3);
    return wynik;
}

SEXP relationship_between_goods(SEXP prices, SEXP i_good, SEXP j_good, SEXP i_price, SEXP j_price, SEXP income_or_non_wage, SEXP wage, SEXP max_time, SEXP utility_f, SEXP rho, SEXP model_type) {
    SEXP i_j = PROTECT(price_flexibility_of_demand_i_by_j(prices, i_good, j_price, income_or_non_wage, wage, max_time, utility_f, rho, model_type));
    SEXP j_i = PROTECT(price_flexibility_of_demand_i_by_j(prices,j_good, i_price, income_or_non_wage, wage, max_time, utility_f,rho, model_type));

    double e_ij = REAL(i_j)[0];
    double e_ji = REAL(j_i)[0];
    const char* relation = "Goods are neither substitutes, nor complementary or neutral";

    if (ISNA(e_ij) || ISNA(e_ji)) {
        relation = "NA";
    } else if (e_ij > 0.0 && e_ji > 0.0) {
        relation = "Goods are substitutes";
    } else if (e_ij < 0.0 && e_ji < 0.0) {
        relation = "Goods are complementary";
    } 
    else if (e_ij == 0.0 && e_ji == 0.0) {
        relation = "Goods are neutral"; }

    SEXP wynik = PROTECT(mkString(relation));
    UNPROTECT(3); 
    return wynik;
}

SEXP reservation_wage(SEXP prices, SEXP income_non_wage, SEXP max_time, SEXP utility_f, SEXP rho) {
    double L_max = asReal(max_time);
    int n_goods = LENGTH(prices);

    SEXP zero_wage = PROTECT(ScalarReal(1e-6));
    SEXP model_mode = PROTECT(ScalarInteger(2)); 
    SEXP basket_zero = PROTECT(maximize_utility_universal(prices, income_non_wage, zero_wage, max_time, utility_f, R_GlobalEnv, model_mode));
    REAL(basket_zero)[0] = L_max; 

    SEXP mu_vector = PROTECT(marginal_utility(utility_f, basket_zero, rho));
    double mu_leisure = REAL(mu_vector)[0];
    double mu_last_good = REAL(mu_vector)[n_goods];


    double p_last = REAL(prices)[n_goods - 1];
    double mu_income = mu_last_good / p_last;
    UNPROTECT(4);

    if (mu_income < 1e-9) {
        return ScalarReal(mu_leisure / 1e-9);
    }
    
    return ScalarReal(mu_leisure / mu_income);
}

double expenditure_search_func(int n, double *m_param, void *ex) {
    SEXP *args = (SEXP *)ex;
    
    double m_current = m_param[0];
    if (m_current < 0.0) return R_PosInf;
    SEXP m_scaled = PROTECT(ScalarReal(m_current));
    SEXP basket = PROTECT(maximize_utility_universal(args[0], m_scaled, args[1], args[2], args[3], args[4], args[5]));
    SEXP R_call = PROTECT(lang2(args[3], basket));
    SEXP res = PROTECT(eval(R_call, args[4]));

    double current_utility = REAL(res)[0];
    double target_utility = asReal(args[6]);
    double diff = current_utility - target_utility;

    UNPROTECT(4); 
    return diff * diff;
}

SEXP minimize_expenses(SEXP prices,SEXP target_utility,SEXP wage,SEXP max_time,SEXP utility_f,SEXP env,SEXP model_type) {
    SEXP context[7] = {
    prices,
    wage,
    max_time,
    utility_f,
    env,
    model_type,
    target_utility
};
    double xin[1] = {50.0};
    double xout[1] = {0.0};
    double ynew = 0.0;
    int fail = 0; 
    int fncount = 0;
    int maxfeval = 1000;
    nmmin(1, xin, xout, &ynew, expenditure_search_func, &fail, 1e-10, 1e-8, context, 1.0, 0.5, 2.0, 0, &fncount, maxfeval);

    if (fail != 0) {
        warning("nmin haven't found an optimal value");
    }

    SEXP final_income = PROTECT(ScalarReal(xout[0]));
    SEXP hicksian_basket = PROTECT(maximize_utility_universal(prices, final_income, wage, max_time, utility_f, R_GlobalEnv, model_type));

    UNPROTECT(2);
    return hicksian_basket;
}

static double prepare_hicks_f(double x, void *params) {
    derivative_info *ctx = (derivative_info*)params;

    if (ctx->rzad == 0) {
        int n = LENGTH(ctx->prices);
        SEXP mod_prices = PROTECT(allocVector(REALSXP, n));
        for (int k = 0; k < n; k++) {
            REAL(mod_prices)[k] = REAL(ctx->prices)[k];
        }
        
        REAL(mod_prices)[ctx->idx_j] = x;
        SEXP basket = PROTECT(minimize_expenses(
            mod_prices, ctx->income, ctx->wage, ctx->max_time, 
            ctx->utility_f, R_GlobalEnv, ctx->model_type
        ));
        
        double x_i = REAL(basket)[ctx->idx_i];
        
        UNPROTECT(2);
        return x_i;
    }

    derivative_info nizej = *ctx;
    nizej.rzad = ctx->rzad - 1;

    gsl_function F = {
        .function = &prepare_hicks_f,
        .params = &nizej
    };

    double wynik, blad;
    deriv_central(&F, x, ctx->h, &wynik, &blad);
    return wynik;
}


SEXP Hicks_price_flexibility_of_demand(SEXP prices, SEXP target_utility, SEXP wage, SEXP max_time, SEXP utility_f, SEXP rho, SEXP model_type) {    
    int n_goods = LENGTH(prices);
    int mode = asInteger(model_type);
    int n_elements = (mode == 2) ? (n_goods + 1) : n_goods;

    SEXP basket_center = PROTECT(minimize_expenses(prices, target_utility, wage, max_time, utility_f, rho, model_type));
    SEXP elasticities = PROTECT(allocVector(REALSXP, n_elements));
    derivative_info start = {0};

    start.r_function = R_NilValue;
    start.env = rho;
    start.rzad = 1; 
    start.h = isReal(rho) ? (REAL(rho)[0] < 1e-3 ? 1e-3 : REAL(rho)[0]) : 1e-3;
    start.prices = prices;
    start.income = target_utility;
    start.wage = wage;
    start.max_time = max_time;
    start.utility_f = utility_f;
    start.model_type = model_type;
    start.f = R_NilValue; 
    start.idx_j = -2;

    for (int i = 0; i < n_elements; i++) {
        double x_center = REAL(basket_center)[i];
        if (x_center < 1e-9) {
            REAL(elasticities)[i] = NA_REAL;
            continue;
        }

        int j_price_idx = (mode == 2) ? (i - 1) : i;
        double p0_j;

        if (mode == 2 && i == 0) {
            p0_j = asReal(wage);
            continue;
        } else {
            p0_j = REAL(prices)[j_price_idx];
        }

        start.idx_i = i;
        start.idx_j = j_price_idx;


        gsl_function F;
        F.function = &prepare_hicks_f;
        F.params = &start;

        double derivative, blad;
        deriv_central(&F, p0_j, start.h, &derivative, &blad);
        REAL(elasticities)[i] = derivative * (p0_j / x_center);
    }

    UNPROTECT(2);
    return elasticities;
}


int main(){
    return 0;
}
