ConsumerTheory
==============

ConsumerTheory is an R package that provides computational tools for solving and analysing consumer choice problems. The package combines user-friendly R functions with implementations written in C.
The main objective of the project is to allow user to easily compute and analyse concepts known from microeconomics consumer theory.

Features:
~~~~~~~~~

The package currently supports:

* Checking indifference of commodity baskets
* Checking budget concurrency of a basket
* Computing price indexes (Laspeyres and Paasche)
* Computing quantity indexes (Laspeyres and Paasche)
* Computing total value changes index
* Marshall's demand function optimization
* Computing marginal utillity
* Computing marginal rate of substitution (MRS)
* Veryfying first and second Gossen's laws
* Computing elasticity of utility
* Computing relative elasticity of utility substitution
* Computing reservation wage
* Classifying good
* Hick's demand function optimization
* Computing Hicksian Price Elasticity of Demand

In the future I'd like to add:
* Computing single and mixed flexibilities of demand and price
* Intertemporal choice model and laws regarding it
* Macroeconomics functions 

Installation
~~~~~~~~~~~~~

You can install the development version of the package directly from GitHub.

.. code-block:: r

   install.packages("remotes")

   remotes::install_github("Barbara8Kozlowska/ConsumerTheory")

After installation, load the package:

.. code-block:: r

   library(ConsumerTheory)

Examples
~~~~~~~~


Utility maximization
~~~~~~~~~~~~~~~~~~~~

The following example computes the optimal consumption bundle for a Constant Elasticity of Substitution (CES) utility function

.. math::

   U(x_1,x_2)=\left(x_1^{\rho}+x_2^{\rho}\right)^{1/\rho},

where :math:`\rho = 0.5`.

.. code-block:: r

   u_ces <- function(x) {
       (x[1]^0.5 + x[2]^0.5)^2
   }

   maximize_utility(
       prices = c(1, 3),
       income = 120,
       utility = u_ces
   )

Example output:

.. code-block:: text

   [1] 60 20

The result indicates that the consumer maximizes utility by consuming 60 units of the first good and 20 units of the second good.
```

   

Verification of Gossen's First Law
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following example computes the marginal utility of consuming successive units of a good.

.. code-block:: r

   utility <- function(x) sqrt(x)

   x <- 1:5
   marginal_utility <- diff(utility(c(0, x)))

   marginal_utility

Example output:

.. code-block:: text

   [1] 1.0000000 0.4142136 0.3178372 0.2679492 0.2360680

The decreasing sequence of marginal utilities confirms Gossen's First Law: each additional unit of consumption provides a smaller increase in utility.


Verification of Gossen's Second Law
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The package can determine the optimal consumption bundle and verify the equimarginal principle.

.. code-block:: r

   prices <- c(2, 4)
   income <- 100

   maximize_utility(
       prices = prices,
       income = income,
       utility = "cobb_douglas"
   )

Example output:

.. code-block:: text

   [1] 25 12.5

At the optimum, the ratio of marginal utility to price is equal across all goods, which is consistent with Gossen's Second Law.


Project structure
~~~~~~~~~~~~~~~~~

The package is written primarily in C, and then the functions were being exported to R.


License
~~~~~~~

This project is distributed under the terms specified in the LICENSE file included in the repository.
