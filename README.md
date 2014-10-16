symDiff
=======

An implementation of symbolic differentiation using Ocaml.

Currently, this project includes:
  * type declarations for reprenting expressions in a single variable
  * functions for printing expressions
  * and differentiation functions

But I plan to implement many more features, including:
  * a parser for reading symbolic expressions
  * simplification functions (as a side note, these functions will probably
    focus on ameliorating the lengthy expresssion produced by the current
    differentiation functions, as opposed to being general functions intended
    for simplifying any expression)
  * explanation functions--since the rules used to differentiate functions all
    can be derived from the chain and product rules, the linearity of
    differentation, and a few basic identities, it should be easy to extend
    `diff` so that it returns an explanation of how it found the derivative.
    E.g. it could explain which rules were applied when, and be able tell you
    that sin(x)cos(x) = -(sin(x)^2) + cos(x)^2 because the derivative of sin(x)
    is cos(x) (by the differentiation rule of cosine), the derivative of cos(x)
    is -sin(x) (by the differentiation rule of sine), and the derivative of
    f(x)g(x) = f(x)g'(x) + f'(x)g(x) (by the product rule).
