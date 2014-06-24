
(defun twisted-dev-runtests (&optional debug noprompt)
  (interactive)
  (with-cd twisted-dev-scratch-directory
           (hack-local-variables)
           (let* ((bfn (buffer-file-name))
                  ;; this whole pile of crap is to compensate for the fact that

                  ;;   a. debian glibc is buggy with 2.6.5+ kernels and sends
                  ;;   SIGHUP to processes run in the way that emacs
                  ;;   asynchronous process runner runs them when threads
                  ;;   terminate, which certain unit tests don't

                  ;;   b. gud randomly corrupts the commandline in such a way
                  ;;   that it is impossible to pass commandlines around - it
                  ;;   considers anything that doesn't start with a "-" to be a
                  ;;   filename, and (incorrectly, due to the fact that we're
                  ;;   running in a different directory than it expects)
                  ;;   expands it to be an absolute filename

                  ;;   c. windows doesn't have a shell we can invoke, and Trial
                  ;;   won't run by itself on Windows; we have to use Python or
                  ;;   Combinator (the current hack here is to use Combinator,
                  ;;   but that could be changed)

                  (shell-script-name
                   (format "%s/trialscript" twisted-dev-scratch-directory))
                  (full-trial-command-line
                   (format "trial --rterrors --reporter=bwverbose --tbformat=%s %s --testmodule=%s"
                           twisted-dev-tbformat
                           (if debug "--debug" "")
                           bfn))
                  (full-command-line (if twisted-dev-isnt-windows
                                         (progn
                                           (shell-command
                                            (format "mkdir -p %s; echo '%s/bin/%s' > %s"
                                                    twisted-dev-scratch-directory
                                                    twisted-dev-directory
                                                    full-trial-command-line
                                                    shell-script-name
                                                    ))
                                           (format "sh %s" shell-script-name))
                                       full-trial-command-line))
                  )
             (if bfn
                 (funcall (if debug 'better-pdb 'compile)
                          full-command-line))
             full-command-line)))
