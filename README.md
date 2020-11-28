# naive-proof

a naive proof assistant that does nothing :)

## example

to infer:

``` txt
p' ^ q
r -> p
r' -> t
-------
t
```

code:

``` scheme
(do-proof '(
  (proof (and (not p) q))
  (infer r p)
  (infer (not r) t)
))
;; get t
```
