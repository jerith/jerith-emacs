
(setq erlang-root-dir "/usr/local/lib/erlang")

(setq load-path (cons (concat erlang-root-dir "/lib/tools-2.6.7/emacs") load-path))
(setq exec-path (cons (concat erlang-root-dir "/bin") exec-path))
(require 'erlang-start)
