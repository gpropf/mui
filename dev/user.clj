;; user.clj: This file is required to get a figwheel REPL working in IntelliJ
;; with Cursive. The Emacs + Cider setup does not require it
;;
(require '[figwheel.main.api :as fig])
(fig/start "dev")
