#|
@doc
  lhome public API
@end
|#

(defmodule lhome-app
  (behaviour application)

  ;; Application callbacks
  (export (start 2)
          (stop 1)))

;;; API

(defun start (_type _args)
  (lhome-sup:start_link))

(defun stop (_state)
  'ok)

;;; Internal functions
