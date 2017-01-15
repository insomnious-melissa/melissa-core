(defmodule lhome-arp
  (behaviour gen_server)

  ;; API
  (export 
   (start_link 0)
   (current 0))

  ;; gen_server exports
  (export 
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3)))

(include-lib "pkt/include/pkt.hrl")

;;; API functions

(defun server-name ()
  'lhome-arp)

(defun start_link ()
  (gen_server:start_link
   `#(local ,(server-name)) (MODULE) '() '())) 

(defun current ()
  (gen_server:call (server-name) 'current))

;; gen_server implementation

(defun init (args)
  (epcap:start_link '(#(filter "arp")
                      #(chroot "priv/tmp")))
  #(ok ()))

(defun handle_call 
  (('current _ state)
   `#(reply #(ok ,state) ()))) 

(defun handle_cast (req state)
  `#(stop #(error not_implemented) ,state))

(defun handle_info
  (((tuple 'packet dlt _time _len data) state)
   `#(noreply
     ,(cons (decode-packet dlt data) state))))

(defun terminate (reason state)
  'ok)

(defun code_change (oldvsn state extra)
  `#(ok ,state))

;; private API

(defun decode-packet (dlt data)
  (let (((tuple 'ok (tuple headers payload))
         (pkt:decode (pkt:dlt dlt) data)))
    (collect-addresses headers ())))

(defun collect-addresses
  ((() addresses) addresses)
  (((cons header headers) addresses)
   (collect-addresses headers
                      (++ (if (is-ether header)
                            (list (mac-address (ether-dhost header))
                                  (mac-address (ether-shost header)))
                            ())
                          addresses)))
  (((cons _header headers) addresses)
   (collect-addresses headers addresses)))

(defun mac-address (ether)
  (string:join (list-comp
                   ((<= (n (size 8)) ether))
                 (string:to_lower
                  (string:right (integer_to_list n 16) 2
                                (car "0"))))
               ":"))
