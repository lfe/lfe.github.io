(defmodule queues4
  (export
    (add 2)
    (fetch 1)
    (len 1)
    (new 0)))

(defun new ()
  #(() ()))

(defun add
  "Faster addition of elements that using lists:append."
  ((item (tuple x y))
    (tuple (cons item x) y)))

(defun fetch
  (((tuple x (cons head tail)))
    (tuple 'ok head (tuple x tail)))
  (((tuple '() '()))
    'empty)
  (((tuple x '()))
    ;; perform this heavy computation only sometimes
    (fetch (tuple '() (lists:reverse x)))))

(defun len
  (((tuple x y))
    (+ (length x) (length y))))
