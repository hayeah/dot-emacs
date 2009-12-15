;; clojure-mode

(add-to-list 'load-path "~/el/lib/clojure-mode")
(require 'clojure-mode)

;; swank-clojure
(add-to-list 'load-path "~/el/lib/swank-clojure/src/emacs")

(setq swank-clojure-jar-path
      "~/build/clojure/clojure.jar"
      ;; swank-clojure-extra-classpaths
;;       (list
;;        "~/opt/swank-clojure/src/main/clojure"
;;        "~/.clojure/clojure-contrib.jar")
      )

(require 'swank-clojure-autoload)

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path "~/el/lib/slime")
(require 'slime)
(slime-setup)
