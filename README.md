Symbolic Expression Formatting in ASCII
=======================================

Symbolic Expression Formatting in ASCII written in Haskell.
Output is simlar to the one produced by the Matlab
[pretty](http://www.mathworks.com/help/symbolic/pretty.html)
function.

Example:
--------

> #### **$** ./pretty '(a/(b+2))/3*(x^3^a-y^b^(1+a/b))+10'
>
>          a              
>        -----            
>        b + 2            
> ------------------- + 10
>   /        /    a\\     
>   |        |1 + -||     
> 3 |  a     \    b/|     
>   | 3     b       |     
>   \x   - y        /     

