# Written: Expression Compilation

Treat this assignment as a take-home test: it is to be done on your own (no
discussion, please), and I'm looking for thoughtful, precise answers.
Conciseness is a true virtue in answering these questions – none of these
require more than a few hundred words, or about half a page, to answer in
full.  If you look up external sources to help you answer these questions,
cite them.

Hand it in electronically by pushing to this repository some obviously-named
file, like "answers.txt" or "answers.pdf".  Feel free to put separate answers
in separate files, as well.  All answers are due by Wednesday, March 16, at
11:59pm.


## 1. ANF and If

Consider the alternate version of the Cobra compiler provided in
`if-alternate-anf.ml`.  The following changes were made:

1.  Moved `CIf` to the `aexpr` datatype:

    ```
    type aexpr =
      | AIf of immexpr * aexpr * aexpr
      | ALet of string * cexpr * aexpr
      | ACExpr of cexpr
    ```

3.  Changed the `EIf` case of `anf` to produce `AIf` as follows:
 
    ```
    | EIf(cond, thn, els) ->
      anf cond (fun immcond -> AIf(immcond, anf thn k, anf els k))
    ```

2.  Changed `acompile_expr`, `count_vars`, etc. in obvious ways

Answer the following questions:

- Give an English description of no more than two sentences characterizing the
  difference between this ANF strategy and the original.
- Can the two strategies ever produce programs that give different answers?
  That is, is the alternate strategy _correct_?  Give an example if there is a
  difference, and argue why not if not.  To be clear, this would mean that a
  test with `t` or `terr` would succeed on one but fail on the other.
- What impact does the given transformation have on generated assembly code
  size, compared to the original you were given?  Try to be as precise as
  possible in describing the size increase or decrease.
- Will `count_vars` always return the same answer for a program ANFed by one
  strategy versus the other, or could the answer differ?  If it can differ,
  give an example.  If it cannot, explain why.


## 2. Compiler Runtime Complexity

Our compiler generates instruction lists mainly via appending (the `@`
operator), which has made the implementation quite simple.  This append
operator takes _O(n)_ time, where `n` is the size of the left-hand operand.

- What does this mean for the complexity of our compiler overall?  For example,
  (roughly) how many new list links are created if we compile a let-expression
  with 1000 bindings?
- Is this acceptable performance?  What are some implementation options
  (changing function signatures, changing data structures, etc) for alleviating
  this issue if not?


## 3. Tagged Representations and If

Many languages do not check for boolean-typed values in conditional position,
and values act as either true or false in that position depending on
language-specific rules.  For example, consider the examples in `truth.py`.

Suppose we wanted to support this in a language we were designing that uses
tagged representations for values.  What impact might that have on our
representation choices if we wanted to make checking conditionals easy?  What
if we added more types, for example a special `None` value (like in Python),
that makes if take the false branch if it appears in conditional position?  Say
we used the _two_ least significant bits to represent that tag?  How would
these choices affect performance relative to C, which treats everything as an
integer?  Discuss this design space a little bit; there is no right answer to
this question, and it is intentionally open-ended.


