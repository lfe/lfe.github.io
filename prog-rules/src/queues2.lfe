(defmodule queues2
  (export
    (add 2)
    (fetch 1)
    (new 0)))

(defun new ()
  '())

(defun add (item queue)
  (lists:append queue (list item)))

(defun fetch
  (((cons head tail))
    (tuple 'ok head tail))
  (('())
    'empty))
