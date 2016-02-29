def f(val):
  if val:
    print("Was truthy")
  else:
    print("Was falsy")

f(0)    # prints "Was falsy"
f(54)   # prints "Was truthy"
f(true) # prints "Was truthy"
f(false)# prints "Was falsy"
f(None) # prints "Was falsy"

