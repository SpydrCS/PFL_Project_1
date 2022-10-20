# PFL_TP1
## Polynomial Manipulation
Developed by:
- Rui Pires up202008252@up.pt
- Guilherme Moreira up202007036@up.pt

### Description
This project consists of a program that manipulates polynomials. The program is able to read polynomials via user input and perform operations on them. The operations that can be performed are:
- Normalization
- Addition
- Multiplication
- Derivation

## Internal Representation
We chose a representation of polynomials as a list of tuples, where each tuple is a pair of the form (coefficient,list of exponents). The list of exponents is a list of tuples, where each tuple is a pair of the form (variable,exponent). So to sum it up the internal representation is a list of tuples of the form (coefficient,list of tuples of the form (variable,exponent)). For example, the polynomial 2xy^2 + 3x^2y^3 would be represented as:
```
[(2,[(x,1),(y,2)]),(3,[(x,2),(y,3)])]
```
We chose this representation because it was the most intuitive and easy to implement. It also allows for easy manipulation of the polynomials, since we can easily add, multiply, normalize and derive them.

## Normalization

## Addition

## Multiplication

The multiplication of two polynomials is done by 3 important steps:
First, we create a function capable of multiplying a list of variables by a list of variables, which is done by appending the variables of the second list to the first one. This function is called multiplyVars.
Second, we use a function capable of multiplying a monomial by another monomial which is done by multiplying the coefficients and multiplying the variables, using the multiplyVars function. This function is called multiplyOne.
Finally, we use a function capable of multiplying a polynomial by another polynomial, which is done by multiplying each monomial of the first polynomial by each monomial of the second polynomial, using the multiplyOne function. This function is called multiply.
After that we simply transform the internal representation of the polynomial into a string int the function multiplication.

## Derivation

## Examples of use