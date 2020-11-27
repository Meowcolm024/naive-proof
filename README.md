# naive-proof

a naive proof assistant that does nothing :)

## example

to infer:

``` txt
a
a -> b
(a ^ b) -> c
------------
c
```

code:

``` scheme
(do-proof '((proof a) (infer a b) (infer (both a b) c)))
;; get c
```
