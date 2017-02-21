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
  (let* ((`#(ok ,polling-interval) (lhome-config:get 'reactor
                                                     'polling-interval
                                                     'true))
         (`#(ok ,timer) (timer:send_interval polling-interval
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
   (timer-tick-reaction)
   `#(noreply ,state)))

(defun terminate (reason state)
  (let* ((`#(,timer) state))
    (time:cancel timer))
  'ok)

(defun code_change (oldvsn state extra)
  `#(ok ,state))

;; private API

(defun timer-tick-reaction ()
  (gen_server:cast (server-name) 'react))

(defun reactor-logic ()
  (let* ((`#(ok ,current-arps) (lhome-arp:current))
         (filtered-arps (filter-arps current-arps)))
    (if (> (length filtered-arps) 0)
      (progn (io:format "Found ARPs: ~p~n" (list filtered-arps))
             (make-request)))))

(defun filter-arps (current-arps)
  (let* ((`#(ok ,arp-family) (lhome-config:get 'arp
                                               'arp-family
                                               'true)))
    (lists:append
     (lists:map (lambda (arp-pair)
                  (lists:filter (lambda (arp)
                                  (string:equal arp-family
                                                (string:left arp 8)))
                                arp-pair))
                current-arps))))

(defun make-request ()
  (let* ((`#(ok ,make-request) (lhome-config:get 'reactor 'make-request 'true)))
    (if make-request
      (let* ((`#(ok ,ifttt-maker-event) (lhome-config:get 'ifttt-maker 'event))
             (`#(ok ,ifttt-maker-token) (lhome-config:get 'ifttt-maker 'token))
             (request-url (++ "https://maker.ifttt.com/trigger/"
                              ifttt-maker-event
                              "/with/key/"
                              ifttt-maker-token)))
        (io:format "Requesting ~p~n" (list request-url))
        (httpc:request request-url)))))
