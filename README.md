# git socket server for branch info in emacs

To load in emacs:
```lisp
(require 'git-socket)
(csetq git_socket_directory "$HOME/github/")
(csetq git_socket_binary (list "PATH_TO_EXECUTABLE" "-f" (concat git_socket_directory ".tracked")))
(progn
  (init/git/socket)
  (sleep-for 0 250)
  (connect/git/process)
)
```

To use in modeline, use something like:
```lisp
(csetq mode-line-format
 (:eval
  (let
   ((x (gethash (expand-file-name default-directory) git_socket_hash_table)))
   (if (stringp x)
    (propertize x (quote face) (quote bold))
   )
  )
 )
)
```
