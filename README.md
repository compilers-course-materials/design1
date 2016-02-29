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

Consider the following changes to ANF:

1.  Move `CIf` to the `aexpr` datatype:

    ```
    type aexpr =
      | AIf of immexpr * aexpr * aexpr
      | ALet of string * cexpr * aexpr
      | ACExpr of cexpr
    ```

    And change `acompile_expr`, `count_vars`, etc. in the obvious ways

2.  Change the `EIf` case of `anf` to produce `AIf` as follows:
 
    ```
    | EIf(cond, thn, els) ->
      anf cond (fun immcond -> AIf(immcond, anf thn k, anf els k))
    ```

You can copy and paste it into your Boa or Cobra compiler to try it out.
Answer the following questions:

- Give an English description of no more than two sentences characterizing the
  difference between the two strategies.
- Can the two strategies ever produce programs that give different answers?
  That is, is the alternate strategy _correct_?  Give an example if there is a
  difference, and argue why not if not.
- What impact does the given transformation have on generated assembly code
  size, compared to the original you were given?  Try to be as precise as
  possible in describing the size increase or decrease.
- Will `count_vars` always return the same answer for a program ANFed by one
  strategy versus the other, or could the answer differ?  If it can differ,
  give an example.  If it cannot, explain why.


## 2. If and Effects

Consider this implementation of compiling `CIf`:

```
    | CIf(cond, thn, els) ->
      let cond_as_arg = acompile_imm_arg cond si env in
      let thn_instrs = acompile_expr thn si env in
      let els_instrs = acompile_expr els si env in
      let done_label = gen_temp "done" in
      thn_instrs @ [
        IMov(Reg(EAX), cond_as_arg);
        ICmp(Reg(EAX), const_false);
        IJne(done_label);
      ] @
      els_instrs @ [
        ILabel(done_label)
      ]
    | CImmExpr(i) -> acompile_imm i si env
```

In English, it runs all the instructions for the `then` branch, then checks
the conditional and skips the instructions for the `else` branch if the
conditional is not false:

```
instructions for then
...
mov eax, <conditional-value>
cmp eax, <false>
jne done
instructions for else
...
done:
```

You should be able to paste the code above directly into your Cobra compiler
to try it out.

- Give an example of a Cobra program that behaves differently under the
  correct version and this version, in a way _other than_ throwing an error
  because the conditional check is omitted.
- If we used an analogous strategy for Boa, could we still write a test to
  distinguish the two implementations?  Why or why not?


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


