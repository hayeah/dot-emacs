
(add-to-list 'load-path "~/el/lib/scala-mode")

(require 'scala-mode-auto)

(push 'my-use-camelcase scala-mode-hook)

(defkeys scala-mode-map
    ([f1] nil))