How I thought it was done initially:
g: 1*f(1-1)
g: 1*1
g: 1 ??????

How to actually do it:
g = lamb fact lamb n if n = 0 1 else n*fact(n-1)
fix g = lamb f. (lamb x. f (lamb y. x x y)) (lamb x. f (lamb y. x x y))
      = lamb x. g (lamb y. x x y)   lamb x. g (lamb y. x x y)
      = g (lamb y. h h y) h
      = lamb fact. lamb n. (lamb fact lamb n if n = 0 1 else n*fact(n-1))  (lamb y. h h y)
      = 1 * ((lamb y. h h y) 0)
      = 1* (h h 0)