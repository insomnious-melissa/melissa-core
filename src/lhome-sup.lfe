#|
@doc
  lhome top level supervisor
@end
|#

(defmodule lhome-sup
  (behaviour supervisor)

  ;; API
  (export (start_link 0))

  ;; Supervisor callbacks
  (export (init 1)))

;;; API functions

(defun server-name ()
  'lhome-sup)

(defun start_link ()
  (supervisor:start_link
    `#(local ,(server-name)) (MODULE) '()))

;;; Supervisor callbacks

(defun init (_args)
  (let* ((start-order (list (child 'lhome-arp 'worker ())
                            (child 'lhome-reactor 'worker ()))))
    `#(ok #(#(one_for_one 5 10)
            ,start-order))))

;;; Internal functions

(defun child
  "Helper function for declaring children of supervisor."
  ((mod 'supervisor '())
    `#(,mod #(,mod start_link ()) permanent 5000 supervisor (,mod)))
  ((mod type '())
   `#(,mod #(,mod start_link ()) permanent infinity ,type (,mod)))
  ((mod type args)
   `#(,mod #(,mod start_link (,args)) permanent 5000 ,type (,mod))))
