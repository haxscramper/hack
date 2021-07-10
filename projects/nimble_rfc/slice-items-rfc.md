# Allow ``iterator `..`*(a, b: SomeInteger): SomeInteger`` for decreasing ranges

Very minor RFC. tl;dr - make `for i in 2 .. 0` yield all values in range, even if it is decreasing

It is very easy to iterate over increasing integer range with just `0 .. 10`, but at the same time decreasing ranges require much more verbose `countdown`. Would it be possible to improve current implementation of `..` and `..<` (for integers) with check start/end element?

```nim
iterator `..`*(a, b: int): int {.inline.} =
  if a < b:  
    for i in countup(a, b): yield i
    
  else:      
    for i in countdown(a, b): yield i
    
iterator `..<`*(a, b: int): int {.inline.} =
  if a < b: 
    for i in countup(a, b - 1): yield i
    
  elif a == b:
    yield a
    
  else:      
    for i in countdown(a - 1, b): yield i
        
for i in 0 .. 2: echo i # 0 1 2
for i in 0 .. 0: echo i # 0
for i in 2 .. 0: echo i # 2 1 0

for i in 0 ..< 2: echo i # 0 1
for i in 0 ..< 0: echo i # 0
for i in 2 ..< 0: echo i # 2 1
```

Otherwise, if `..` is not *supposed* to do countdown raise an exception instead of silently doing wrong thing (not yielding anyting).

Otherwise, if the main intention was to make `0 .. len - 3` result in zero iterations introduce `iterator` overload that accepts `static[int]`:

```nim
iterator `..`(a, b: static[int]): int =
  when a > b:
    for i in countdown(a, b): yield i
    
  else:
    for i in countup(a, b): yield i
    
    
for i in 0 .. 2: echo i # 0 1 2
for i in 0 .. 0: echo i # 0
for i in 2 .. 0: echo i # 2 1 0
```

