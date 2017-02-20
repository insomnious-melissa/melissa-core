(defmodule lhome-reactor
  (behaviour gen_server)

  ;; API
  (export
   (start_link 0))

  ;; gen_server exports
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3)))

;;; API functions

(defun server-name ()
  'lhome-reactor)

(defun start_link ()
  (gen_server:start_link
   `#(local ,(server-name)) (MODULE) '() '()))

;; gen_server implementation

(defun init (args)
  (let* ((`#(ok ,timer) (timer:send_interval 1000
                                             'timer-tick)))
    `#(ok #(,timer))))

(defun handle_call
  (('current _ state)
   `#(reply #(ok ,state) ())))

(defun handle_cast (req state)
  (reactor-logic)
  `#(noreply ,state))

(defun handle_info
  (('timer-tick state)
   (react)
   `#(noreply ,state)))

(defun terminate (reason state)
  (let* ((`#(,timer) state))
    (time:cancel timer))
  'ok)

(defun code_change (oldvsn state extra)
  `#(ok ,state))

;; private API

(defun react ()
  (gen_server:cast (server-name) 'react))

(defun reactor-logic ()
  (let* ((`#(ok ,current-arps) (lhome-arp:current))
         (filtered-arps (filter-arps current-arps)))
    (if (> (length filtered-arps) 0)
      (progn (io:format "Found ARP: ~p~n" (list filtered-arps))
             (make-request)))))

(defun filter-arps (current-arps)
  (lists:append
   (lists:map (lambda (arp-pair)
                (lists:filter (lambda (arp)
                                (string:equal "50:f5:da"  ;; Dash button
                                              (string:left arp 8)))
                              arp-pair))
              current-arps)))

(defun make-request ()
  (io:format "Some magic here!"))
