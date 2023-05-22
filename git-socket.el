(defconst git_socket_directory nil)
(defconst git_socket_binary nil)
(defvar git_socket_hash_table (make-hash-table :test `equal))

(defun init/git/socket ()
  (if (get-process "git.socket.server")
      (delete-process (get-process "git.socket.server"))
    )
  (make-process
   :name "git.socket.server"
   :buffer nil
   :command git_socket_binary
   :connection-type `pipe
   )
  (message (format "git socket: initialized"))
  )

(defun connect/git/process ()
  (if (get-process "git.socket.client")
      (delete-process (get-process "git.socket.client"))
    )
  (make-network-process
   :name "git.socket.client"
   :filter (lambda (process string)
	     (let ((arr (split-string string "[\n]+" t)))
	       (dolist (x arr)
		 (let ((xspl (split-string x)))
		   (puthash (car xspl) (concat "git:" (car (cdr xspl))) git_socket_hash_table)
		   )
		 )
	       )
	     )
   :sentinel (lambda (process event)		   
	       (message (format "git socket: %s" (string-trim-right event)))
	       (clrhash git_socket_hash_table)
	       )
   :remote (concat git_socket_directory ".socket"))
  )

(provide 'git-socket)
