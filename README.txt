Apoorva Anand - 2037610
a.apoorvaanand@students.uu.nl

I've attempted the first 10

10) 10 doesn't work. I ran out of time. Here's a textual solution:

Whenever you encounter a "return" for function with identifier "ident", go through the environment and remove all the "MEMB *" entries. This way, for example, on a recursive call, we can rename the temp variables as necessary.

11) Add all of the variables currently in scope to the environment. This would be done in such a way that, when we leave a block, only necessary "MEMB variables that might be used later are present (and variables from the outer block that hasn't yet closed.

12) The environment also contains return types, along with that, we probably need to save the types of methods as well. With this, we can check whether something type-checks.

13) When replacing the "MEMB *" variables in a classMethod, if the numbers don't line up, throw an error.

14) A combination of the approach of 12 and 13 will work.

15) Add the membVars to the environment and they only leave the environment when we are done with the class.
