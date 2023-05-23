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
		 (message x)
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
   :remote (concat git_socket_directory ".tracked.socket")
   )
  )

(defun write_to/git/process (str)
  (if (not (get-process "git.socket.client"))
      (return nil)
    (if (not (stringp str))
	(return nil)
      )
    )
  (let (
	(p (get-process "git.socket.client"))
	(s (concat (expand-file-name (string-trim-right str)) "\n"))
	)
    (send-string p s)
    )
  )

(defun add_dir/to/tracked ()
  (interactive)
  (if (not (get-process "git.socket.client"))
      (return nil)
    )
  (write_to/git/process (read-directory-name "Add directory to tracker: "))
  )

(provide 'git-socket)
