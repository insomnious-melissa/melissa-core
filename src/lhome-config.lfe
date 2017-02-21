(defmodule lhome-config
  (behaviour gen_server)

  ;; API
  (export
   (start_link 0)
   (get 2)
   (get 3))

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
  'lhome-config)

(defun start_link ()
  (gen_server:start_link
   `#(local ,(server-name)) (MODULE) '() '()))

(defun get (group key)
  (gen_server:call (server-name) `#(get ,group ,key false)))

(defun get (group key usedefault)
  (gen_server:call (server-name) `#(get ,group ,key ,usedefault)))

;; gen_server implementation

(defun init (args)
  `#(ok state))

(defun handle_call
  (((tuple 'get group key usedefault) message state)
   (let* ((env (application:get_env 'lhome group))
          (reply (case env
                   ('undefined (case usedefault
                                 ('false `#(error nogroup ,group))
                                 ('true (default group key))))
                   ((tuple 'ok found) (case usedefault
                                        ('false (proplists:get_value key found))
                                        ('true (proplists:get_value key
                                                                    found
                                                                    (default group key))))))))
     `#(reply #(ok ,reply) ,state)))
  ((req message state)
   `#(reply #(ok ,state) ,state)))

(defun handle_cast (req state)
  `#(noreply ,state))

(defun handle_info (req state)
  `#(noreply ,state))

(defun terminate (reason state)
  'ok)

(defun code_change (oldvsn state extra)
  `#(ok ,state))

;; private API

(defun default
  (('arp 'bpf-filter) "arp")
  (('arp 'arp-family) "50:f5:da")
  (('reactor 'polling-interval) 1000)
  (('reactor 'make-request) 'false)
  (('ifttt-maker 'event) "")
  (('ifttt-maker 'token) ""))
