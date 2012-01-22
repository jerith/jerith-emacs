
(setq erlang-root-dir "/opt/local/lib/erlang")

(setq load-path (cons (concat erlang-root-dir "/lib/tools-2.6.6.5/emacs") load-path))
(setq exec-path (cons (concat erlang-root-dir "/bin") exec-path))
(require 'erlang-start)