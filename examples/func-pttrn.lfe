(defmodule func-pttrn
  (export (safety-check 2)))

(defun safety-check
  (('ok msg)
    (: io format '"~s seems good.~n" (list msg)))
  (('warn msg)
    (: io format '"There's a problem with ~s.~n" (list msg)))
  (('crit msg)
    (: io format '"Be careful of ~s.~n" (list msg))))
