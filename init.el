;; init.el
;; =======


;; load-path
;; ---------

(setq init-dir (expand-file-name (concat "~" init-file-user "/.emacs.d")))

(setq load-path (append
                 (list (concat init-dir "/lisp"))
                 load-path))

;; melpa.org packages
;; ------------------
(require 'package)

(if t
    (add-to-list 'package-archives
                 '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; enable or disable some options
;; ------------------------------

(setq *local-tabbar-enabled* nil)


;; my font size
;; ------------

(defun my-font-height ()
  "compute font height based on screeen height"
  (let* ((h (display-pixel-height))
         (selected (cond
                    ((>= h 1000) 160)
                    ((>= h  900) 160)
                    (t           120))))
    (message "display height = %d => %d" h selected)
    selected))


;; emacs customisation
;; -------------------

(message "init.el: Customisation")
(require 'ps-mule)


;; menubar/toolbar
;; ---------------

(tool-bar-mode 0)
(menu-bar-mode 0)


;; adjust window size
;; ------------------

(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))

(when (eq system-type 'windows-nt)
    (progn
      (add-hook 'window-setup-hook 'w32-maximize-frame t)))


;; special options for X11
;; -----------------------

(when window-system
  (set-frame-position (selected-frame) 0 0)
  (set-frame-parameter (selected-frame) 'mouse-color "red")
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized))


;; special options for non-X11
;; ---------------------------

(unless window-system
  (setq inhibit-splash-screen t)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  )




;; function keys
;; -------------

(message "init.el: Function keys")

;;(global-set-key (kbd "<f1>") 'save-buffers-kill-emacs)
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f3>") 'match-paren)
(global-set-key (kbd "<f4>") (lambda () "toggle between buffers" (interactive) (switch-to-buffer nil)))

(global-set-key (kbd "<f5>") 'toggle-case-char-at-point)

(global-set-key (kbd "S-<f5>") (lambda (arg)
                                 "add a fix this note"
                                 (interactive "p")
                                 (let ((tag "***** FIX THIS: "))
                                   (comment-indent)
                                   (unless (looking-at tag) (insert tag)))))

(global-set-key (kbd "<f6>") (lambda (arg)
                       "set up flyspell"
                       (interactive "p")
                       (flyspell-mode 1)
                       (flyspell-buffer)))


(global-set-key (kbd "<f7>") 'set-mark-command)
(global-set-key (kbd "S-SPC") 'set-mark-command)
(global-set-key (kbd "<f8>") 'call-last-kbd-macro)

(global-set-key (kbd "<f9>") 'delete-trailing-whitespace)

(load-library "mml")
(global-set-key (kbd "C-<f9>") (lambda () "mime to mml"
                                 (interactive)
                                 (toggle-read-only 0)
                                 (mime-to-mml)
                                 (not-modified)
                                 (toggle-read-only 1)))

(global-set-key (kbd "S-<f9>") (lambda (arg) "git blame via (vc-annotate)"
                                 (interactive "p")
                                 (vc-annotate (buffer-file-name) (vc-working-revision buffer-file-name))
                                 (vc-annotate-toggle-annotation-visibility)))

(global-set-key (kbd "<f10>") 'bury-buffer)
(global-set-key (kbd "<f11>") 'my-server-edit)
(global-set-key (kbd "<f12>") 'delete-other-windows)


;; miscellaneous keys
;; ------------------

(message "init.el: Global keys")

(global-set-key (kbd "M-z") 'save-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<kp_5>") 'goto-line)   ; 5
(global-set-key (kbd "<home>") 'goto-line)   ; 5
(global-set-key (kbd "C-\\") 'undo)
(global-set-key (kbd "M-ESC") 'keyboard-quit) ; Esc-Esc
(global-set-key (kbd "<pause>") 'eval-region) ; Pause

(global-set-key (kbd "C-<prior>") 'beginning-of-buffer) ; CTRL-Page Up
(global-set-key (kbd "C-<next>") 'end-of-buffer) ; CTRL-Page Down

(global-set-key (kbd "<insert>") 'comment-region) ; Insert comment markers
(global-set-key (kbd "C-<insert>") 'uncomment-region) ; Remove comment markers

(global-set-key (kbd "s-r") #'(lambda () (interactive) (revert-buffer t t nil))) ; Win-r

(global-set-key (kbd "s-h") 'hoogle) ; Win-h

(when window-system
  (global-unset-key (kbd "C-z")) ; iconify-or-deiconify-frame (C-x C-z)
  )


;; compilation keys
;; ----------------

(message "init.el: compilation setup")

;; Makefile detect in current or higher directory
;; then recompile.
;; no prefix -> make
;; -         -> make test
;; zero      -> make all
;; positive  -> make emacs-N
;; negative  -> make test-N


(require 'ansi-color)
(defun my-colour-compilation-buffer ()
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-truncate-lines 1))
(add-hook 'compilation-filter-hook 'my-colour-compilation-buffer)

(defun my-recompile (arg)
  "Search for Makefile and recompile"
  (interactive "P")
  (let*
      ((base-directory-for-compile ".")
       (target
         (cond
          ((null arg)      "")
          ((equal '- arg)  "test")
          ((zerop arg)     "all")
          ((> arg 0)       (format "emacs-%d" arg))
          ((< arg 0)       (format "test-%d" (- arg)))
          (t "qqq"))))

    (message "recompile target = %s" target)

    (if
        (or (file-exists-p "Makefile") (file-exists-p "BSDmakefile"))
        (set (make-local-variable 'compile-command)
             (concat "make -k " target))
      (loop for the-dir = ".."
            then (concat the-dir "/..")
            until (string-equal "/" (file-truename the-dir))
            when (or (file-exists-p (concat the-dir "/Makefile"))
                     (file-exists-p (concat the-dir "/BSDmakefile")))
            do (progn
                 (message "dir = %s" the-dir)
                 (set 'base-directory-for-compile (directory-file-name the-dir))
                 (set (make-local-variable 'compile-command)
                      (concat "make -k -C " base-directory-for-compile " THIS_DIR=\'" default-directory "' " target))
                 (return))))
    (message "recompile = %s" compile-command)
    (recompile)
    (with-current-buffer (get-buffer-create "*compilation*")
      (cd base-directory-for-compile))))


(global-set-key (kbd "C-<menu>") 'my-recompile)  ; CTRL-Menu
(global-set-key (kbd "M-<menu>") 'next-error) ; ALT-Menu

;; compilation window

(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; on compile scroll the split window
;;(setq compilation-scroll-output t)
;; on compile scroll the split window and scroll to first error
(setq compilation-scroll-output 'first-error)


;; mouse wheel
;; -----------

(message "init.el: Mouse wheel")

(defun my-mouse-wheel-up (arg)
  "Scroll display"
  (interactive "p")
  (scroll-up 1)
  )
(defun my-mouse-wheel-down (arg)
  "Scroll display"
  (interactive "p")
  (scroll-down 1)
  )

(global-set-key (kbd "<mouse-4>") 'my-mouse-wheel-down)
(global-set-key (kbd "<mouse-5>") 'my-mouse-wheel-up)


;; tabbar
;; ------

;; from: http://www.emacswiki.org/emacs/TabBarMode

(when *local-tabbar-enabled*
  (require 'tabbar)
  (tabbar-mode)

  (setq tabbar-buffer-groups-function
        (lambda ()
          (list "All Buffers")))

  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda(buffer)
             (let ((name (buffer-name buffer)))
               (or (string-match "\\.html-template-indent-buffer$" name)
                   (and (find (aref name 0) " *")
                        (not (string= "*scratch*" name))))))
           (buffer-list))))
  )

;; w3m
;; ---

;;* (message "init.el: w3m")
;;*
;;* ;; load the interface
;;* (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
;;* (setq w3m-home-page "http://127.0.0.1/")

;; To use emacs-w3m on Wanderlust:
;; (require 'mime-w3m)

;; To use octet stream viewer:

;; (require 'w3m)
;; (require 'octet)
;; (octet-mime-setup)


;; Unicode
;; -------

(message "init.el: unicode characters")

(require 'cl)

(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (case name
                      ;; arrows
                      ('left-arrow 8592)
                      ('up-arrow 8593)
                      ('right-arrow 8594)
                      ('down-arrow 8595)

                      ('leftwards-double-arrow #X21D0)
                      ('upwards-double-arrow #X21D1)
                      ('rightwards-double-arrow #X21D2)
                      ('downwards-double-arrow #X21D3)

                      ;; boxes
                      ('double-vertical-bar #X2551)

                      ;; relational operators
                      ('equal #X003d)
                      ('not-equal #X2260)
                      ('identical #X2261)
                      ('not-identical #X2262)
                      ('less-than #X003c)
                      ('greater-than #X003e)
                      ('less-than-or-equal-to #X2264)
                      ('greater-than-or-equal-to #X2265)

                      ('equivalent-to #X224D)
                      ('strictly-equivalent-to #X2263)

                      ;; logical operators
                      ('logical-and #X2227)
                      ('logical-or #X2228)
                      ('logical-neg #X00AC)

                      ;; misc
                      ('nil #X2205)
                      ('horizontal-ellipsis #X2026)
                      ('double-exclamation #X203C)
                      ('prime #X2032)
                      ('double-prime #X2033)
                      ('for-all #X2200)
                      ('there-exists #X2203)
                      ('element-of #X2208)

                      ;; mathematical operators
                      ('square-root #X221A)
                      ('squared #X00B2)
                      ('cubed #X00B3)

                      ;; letters
                      ('lambda #X03BB)
                      ('alpha #X03B1)
                      ('beta #X03B2)
                      ('gamma #X03B3)
                      ('delta #X03B4))))

(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (interactive)
  (font-lock-add-keywords
   nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                             ,(unicode-symbol symbol))
                             nil))))))

(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))


;; Ocaml mode
;; ----------

(when (require 'caml-font "caml-font" t)
  (message "init.el: ocaml")

  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))

  (defun ocaml-unicode ()
    (interactive)
    (substitute-patterns-with-unicode
     (list (cons "\\(<-\\)" 'left-arrow)
           (cons "\\(->\\)" 'right-arrow)
           (cons "[^=]\\(=\\)[^=]" 'equal)
           (cons "\\(==\\)" 'identical)
           (cons "\\(\\!=\\)" 'not-identical)
           (cons "\\(<>\\)" 'not-equal)
           (cons "\\(()\\)" 'nil)
           (cons "\\<\\(sqrt\\)\\>" 'square-root)
           (cons "\\(&&\\)" 'logical-and)
           (cons "\\(||\\)" 'logical-or)
           (cons "\\<\\(not\\)\\>" 'logical-neg)
           (cons "\\(>\\)[^=]" 'greater-than)
           (cons "\\(<\\)[^=]" 'less-than)
           (cons "\\(>=\\)" 'greater-than-or-equal-to)
           (cons "\\(<=\\)" 'less-than-or-equal-to)
           (cons "\\<\\(alpha\\)\\>" 'alpha)
           (cons "\\<\\(beta\\)\\>" 'beta)
           (cons "\\<\\(gamma\\)\\>" 'gamma)
           (cons "\\<\\(delta\\)\\>" 'delta)
           (cons "\\(''\\)" 'double-prime)
           (cons "\\('\\)" 'prime)
           (cons "\\<\\(List.for_all\\)\\>" 'for-all)
           (cons "\\<\\(List.exists\\)\\>" 'there-exists)
           (cons "\\<\\(List.mem\\)\\>" 'element-of)
           (cons "^ +\\(|\\)" 'double-vertical-bar))))

  (add-hook 'caml-mode-hook 'ocaml-unicode)
  (add-hook 'tuareg-mode-hook 'ocaml-unicode)

  (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
  ;;(autoload 'caml-hilit "caml-hilight" "Hilit19 patterns used for Caml mode" t)
  )


;; Haskell mode
;; ------------

(message "init.el: Haskell")


(add-to-list 'auto-mode-alist '("\\.[hg]s$"  . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hi$"     . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[hg]s$" . literate-haskell-mode))

(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)

;; Add the following lines according to which modules you want to use:

(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; Note that the two indentation modules are mutually exclusive:
;; add at most one.

(if t
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  )

;; from mrd's blog (sequence.complete.org)
;; (add-hook 'haskell-mode-hook 'turn-on-font-lock)
;; (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

;; I wrote my own "newline and indent" function which brings any
;; code you split onto the newline back up to the same indentation
;; level it was at previously.

(when nil
  (remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (remove-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

  ;; Just use tab-stop indentation, 2-space tabs

  (add-hook 'haskell-mode-hook
            (lambda ()
              (turn-on-haskell-doc-mode)
              (turn-on-haskell-simple-indent)
              (setq indent-line-function 'tab-to-tab-stop)
              (setq tab-stop-list
                    (loop for i from 2 upto 120 by 2 collect i))
              (local-set-key (kbd "RET") 'newline-and-indent-relative))
            )
  )

(defun newline-and-indent-relative ()
  (interactive)
  (newline)
  (indent-to-column (save-excursion
                      (forward-line -1)
                      (back-to-indentation)
                      (current-column))))


;; change the display of some characters

;; commented out items are missing from the fonts
(defun haskell-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "[^<]\\(<-\\)" 'left-arrow)
         (cons "\\(->\\)[^>]" 'right-arrow)
         (cons "\\(==\\)" 'identical)
         (cons "\\(/=\\)" 'not-identical)
         (cons "\\(()\\)" 'nil)
         (cons "\\<\\(sqrt\\)\\>" 'square-root)
         (cons "\\(&&\\)" 'logical-and)
         (cons "\\(||\\)" 'logical-or)
         (cons "\\<\\(not\\)\\>" 'logical-neg)
         (cons "\\(>\\)[^=]" 'greater-than)
         (cons "\\(<\\)[^=]" 'less-than)
         (cons "[^>]\\(>=\\)" 'greater-than-or-equal-to)
         (cons "[^<]\\(<=\\)" 'less-than-or-equal-to)
         (cons "\\<\\(alpha\\)\\>" 'alpha)
         (cons "\\<\\(beta\\)\\>" 'beta)
         (cons "\\<\\(gamma\\)\\>" 'gamma)
         (cons "\\<\\(delta\\)\\>" 'delta)
         (cons "\\(''\\)" 'double-prime)
         (cons "\\('\\)" 'prime)
         (cons "\\(!!\\)" 'double-exclamation)
         (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))

(add-hook 'haskell-mode-hook 'haskell-unicode)


;; CoffeScript mode
;; ----------------

(defun coffee-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "[^<]\\(<-\\)" 'left-arrow)
         (cons "\\(->\\)[^>]" 'right-arrow)
         (cons "\\(=>\\)[^>]" 'rightwards-double-arrow)
         (cons "\\(\\.\\.\\.\\)" 'horizontal-ellipsis))))

(add-hook 'coffee-mode-hook 'coffee-unicode)


;; Erlang mode
;; -----------

(defun erlang-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "[^<]\\(<-\\)" 'left-arrow)
         (cons "\\(->\\)[^>]" 'right-arrow)
         (cons "[^<]\\(<=\\)" 'leftwards-double-arrow)
         (cons "\\(=>\\)[^>]" 'rightwards-double-arrow)
         (cons "\\(=:=\\)" 'identical)
         (cons "\\(=/=\\)" 'not-identical)
         (cons "[^=]\\(==\\)[^=]" 'equal)
         (cons "[^=]\\(/=\\)" 'not-equal)
         (cons "[^=/<>]\\(=\\)[^=/<>]" 'equivalent-to)
         ;;(cons "\\(\\[\\]\\)" 'nil)
         (cons "\\<\\(sqrt\\)\\>" 'square-root)
         ;;(cons "\\(&&\\)" 'logical-and)
         ;;(cons "\\(||\\)" 'logical-or)
         ;;(cons "\\<\\(not\\)\\>" 'logical-neg)
         (cons "\\(>\\)[^=]" 'greater-than)
         (cons "[^=]\\(<\\)" 'less-than)
         (cons "\\(>=\\)" 'greater-than-or-equal-to)
         (cons "\\(=<\\)" 'less-than-or-equal-to)
         (cons "\\(!!\\)" 'double-exclamation)
         )))

(add-hook 'erlang-mode-hook 'erlang-unicode)

;; FreeBSD EMACS mode is installed in a special place
(if (eq system-type 'berkeley-unix)
    (let*
        ((lib-prefix "/usr/local/lib")
         (erlang-libs (directory-files lib-prefix t "^erlang")))
      (when erlang-libs
        (let*
            ((erlang-local-dir (car (nreverse erlang-libs)))
             (erlang-lib-dir (concat erlang-local-dir "/lib"))
             (erlang-bin-dir (concat erlang-local-dir "/bin"))
             (erlang-tools-dir (car (nreverse
                                     (directory-files erlang-lib-dir t "^tools-"))))
             (erlang-emacs-dir (concat erlang-tools-dir "/emacs")))
          (setq load-path (cons erlang-emacs-dir load-path))
          (setq erlang-root-dir erlang-local-dir)
          (setq exec-path (cons erlang-bin-dir exec-path))
      (require 'erlang-start)))))


;; Go mode
;; -------

;; see: https://code.google.com/p/go-wiki/wiki/IDEsAndTextEditorPlugins
(let ((go-emacs-directory (expand-file-name (concat "~" init-file-user "/gocode/emacs/go-mode.el"))))
  (when (file-directory-p go-emacs-directory)
    (message "init.el: go mode")
    (add-to-list 'load-path go-emacs-directory t)
    (require 'go-mode)
    (add-hook 'before-save-hook #'gofmt-before-save)))


;; Fundamental mode
;; ----------------

(setq flyspell-issue-welcome-flag nil) ;; workaround bug #619015 in ubuntu 10.10

(defun do-commit-header ()
  "move to line end and turn on spell check"
  (move-end-of-line nil)
  (flyspell-mode))


(defun activate-flyspell ()
  "activate commit mode"
  (let ((fn (file-name-nondirectory buffer-file-name)))
    (cond
     ((string-equal fn "COMMIT_EDITMSG")
      (do-commit-header))
     ((string-equal fn "svn-commit.tmp")
      (do-commit-header)))))

(add-hook 'find-file-hook 'activate-flyspell)


;; Lisp mode
;; ---------

(message "init.el: lisp greek text")

(defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
    (loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code)))
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))
(add-hook 'lisp-mode-hook 'pretty-greek)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)


;; enable slime
;; ------------

(add-to-list 'load-path "/usr/share/common-lisp/source/slime/")

(when (require 'slime "slime" t)
  (message "init.el: SLIME")

  (setq slime-startup-animation nil)

  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
                                        ;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

  (setq inferior-lisp-program "sbcl")

  ;; set up to use inferior lisp buffer to auto-load swank server on M-x slime
  ;; uses a custom core file (see comments below)
  (setq slime-lisp-implementations
        ;;      '((cmucl ("cmucl" "-quiet"))
        ;;        (sbcl ("sbcl") :coding-system utf-8-unix)))
        ;;        (sbcl ("sbcl" "--core" "~/sbcl.core-for-slime"))))
        (list (list 'sbcl (list "sbcl" "--core" (expand-file-name "~/sbcl.core-with-swank"))
                    :init #'(lambda (port-file _)
                              (format "(swank:start-server %S)\n" port-file))
                    :coding-system 'utf-8-unix)))

  (slime-setup '(slime-fancy slime-asdf))


  ;; creating the above custom core images
  ;; $ sbcl
  ;; * (mapc ’require ’(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
  ;; * (save-lisp-and-die "sbcl.core-for-slime")
  ;; $ sbcl
  ;; * (mapc ’require ’(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf swank))
  ;; * (swank-loader:dump-image "sbcl.core-with-swank")
  ;; * CTRL-D
  ;; Running an external sblc wath the swank server (for M-x slime-connect)
  ;; $ sbcl --core /path/to/sbcl.core-with-swank
  ;; * (swank:create-server :port 4005 :style :spawn :dont-close t)
)

;; Lua mode
;; --------

(when (require 'lua-mode "lua-mode" t)
  (message "init.el: lua-mode available"))


;; Ruby mode
;; ---------

(message "init.el: ruby")

(setq ruby-program-name "/usr/local/bin/ruby")
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook #'(lambda () (inf-ruby-keys)))


;; C mode
;; ------

(message "init.el: C styles")

;; A partial list of the better known styles:
;;   “gnu”         The default style for GNU projects
;;   “k&r”         What Kernighan and Ritchie, the authors of C used in their book
;;   “bsd”         What BSD developers use, aka “Allman style” after Eric Allman.
;;   “stroustrup”  What Stroustrup, the author of C++ used in his book
;;   “linux”       What the Linux developers use for kernel development
;;   “python”      What Python developers use for extension modules
;;   “java”        The default style for java-mode (see below)
;;   “user”        When you want to define your own style

(setq c-default-style "bsd")


(defun my-tabify ()
  "tabify the buffer for certain file types"
  (interactive)
  (when (stringp mode-name)
    (cond
     ((or (string= (substring mode-name 0 (min 2 (length mode-name))) "C/")
          (string= (substring mode-name 0 (min 4 (length mode-name))) "C++/"))
      (message "tabifying buffer before save")
      (save-excursion
        (tabify (point-min) (point-max)))
      )
     ((or (string= (substring mode-name 0 (min 7 (length mode-name))) "Haskell")
          (string= (substring mode-name 0 (min 10 (length mode-name))) "Emacs-Lisp"))
      (message "untabifying buffer before save")
      (save-excursion
        (untabify (point-min) (point-max)))
      )
     )
    ))

(setq tabify-regexp "^\t* [ \t]+")

(add-hook 'before-save-hook 'my-tabify)


;; Java mode
;; ---------

(message "init.el: Java styles")


(add-hook 'java-mode-hook #'(lambda ()
                              (setq c-basic-offset 4)))


;; awk mode
;; --------

(message "init.el: awk styles")

(add-hook 'awk-mode-hook
          #'(lambda ()
             (setq c-basic-offset 4)))


;; Python and Django
;; -----------------

;; see this: http://stackoverflow.com/questions/1257236/django-emacs-as-textmate-replacement
;; look at the answer by: http://stackoverflow.com/users/2354/benjamin-pollack


(defun my-set-flymake-mode ()
  (flymake-mode 1))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))

  (add-hook 'python-mode-hook 'my-set-flymake-mode))


;; nXhtml mode, from: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
;; ----------------------------------------------------------------------

(when (load "~/nxhtml/autostart.el" t)
  (message "init.el: nXhtml")
  (setq mumamo-background-colors nil)
  (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))
  ;; Workaround the annoying warnings:
  ;; Warning (mumamo-per-buffer-local-vars):
  ;; Already 'permanent-local t: buffer-file-name
  (when (string< "24.1" (format "%d.%d" emacs-major-version emacs-minor-version))
    (eval-after-load "mumamo"
      '(setq mumamo-per-buffer-local-vars
             (delq 'buffer-file-name mumamo-per-buffer-local-vars)))))

;; miscellaneous items
;; -------------------

(message "init.el: miscellaneous")


(line-number-mode 1)

(setq display-time-24hr-format 'T)
(display-time)

(setq vc-initial-comment 'T)

(mouse-avoidance-mode 'animate)

(put 'overwrite-mode 'disabled t)  ; disble nasty overstrike mode
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)



(setq abbrev-file-name             ; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ; definitions from...
(setq save-abbrevs t)              ; save abbrevs when files are saved


(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)

(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)


;; Tramp
;; -----

(message "init.el: Tramp set up")

(require 'tramp)
(setq tramp-debug-buffer t)
(setq tramp-verbose 10)
(setq tramp-default-method "ssh")


;; Markdown mode
;; -------------

(when (require 'markdown-mode "markdown-mode" t)
  (message "init.el: markdown-mode available"))


;; YAML mode
;; ---------

(when (require 'yaml-mode "yaml-mode" t)
  (message "init.el: YAML-mode available"))


;; Modes
;; -----

(message "init.el: mode alist changes")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.a77\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.4th\\'" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.app.src\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(when (require 'aggressive-indent-mode "aggressive-indent-mode" t)
  (message "init.el: aggressive-indent-mode available")
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode)
  (add-hook 'asm-mode-hook #'aggressive-indent-mode)
  (add-hook 'c-mode-hook #'aggressive-indent-mode)
  (add-hook 'c++-mode-hook #'aggressive-indent-mode))

(add-hook 'asm-mode-hook
          #'(lambda ()
              (setq comment-column '54)
              )
          )

;;(add-hook 'asm-mode-set-comment-hook
;;          #'(lambda ()
;;             (setq asm-comment-char ?@)
;;           )
;;        )


;; removing spaces at end-of-line
;; ------------------------------

(message "init.el: Remove spaces")


(defun my-delete-trailing-whitespace ()
  "remove trailing whilespace from certain file types"
  (interactive)
  (when (stringp mode-name)
    (when (not (string= mode-name "Hexl"))
      (message "removing trailing whitespace before save")
      (delete-trailing-whitespace)
      )
    )
  )

(add-hook 'before-save-hook 'my-delete-trailing-whitespace)


;; matching parentheses
;; --------------------

(message "init.el: Match parentheses")

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise do nothing"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))


;; spreadsheet calculation
;; -----------------------

(message "init.el: Spreadsheet")

(global-set-key (kbd "C-<kp-multiply>") 'sc-calc)
(global-set-key (kbd "C-<kp-add>") 'sc-total)

(defun sc-calc (arg)
  "multiply quantity by unit price"
  (interactive "p")
  (let (
        (qty 0)
        (unit-price 0)
        (total-price 0)
        )
    (if (search-forward-regexp "|[[:space:]]*\\([0-9.]+\\)[[:space:]]*|" nil t)
        (let (
              (start-pos (point))
              (end-pos (progn (end-of-line) (point)))
              )
          (goto-char start-pos)
          (backward-char 1)
          (setq qty (string-to-number (match-string 1)))
          (message "qty = %s" qty)
          (if (search-forward-regexp "|[[:space:]]*\\([0-9.]+\\)[[:space:]]*|" end-pos t)
              (progn
                (setq unit-price (string-to-number (match-string 1)))
                (message "unit price = %s" unit-price)
                (setq total-price (* unit-price qty))
                (backward-char 1)
                (message "total price = %s" total-price)
                (if (search-forward-regexp "|[^|]+|" end-pos t)
                    (progn
                      (replace-match (format "| %7.2f |" total-price))
                      )
                  )
                )
            )
          )
      )
    )
  )

(defun sc-replace-total (format-string total-value)
  "replace the current match with the new value"
  (message "current match  = %s" (match-string 0))
  (let* (
         (tot (format format-string total-value))
         (len (- (length (match-string 0)) (length tot) 2))
         )
    (replace-match (concat "|" tot))
    (dotimes (i len)
      (insert " ")
      )
    (insert "|")
    )
  )

(defun sc-total (arg)
  "sum a column of numbers"
  (interactive "p")
  (let (
        (current-value 0)
        (total-value 0)
        )
    (while
        (let (
              (start-pos (point))
              (end-pos (progn (end-of-line) (point)))
              )
          (goto-char start-pos)
          (cond
           ((looking-at "\\+") (next-line 1) t)
           ((looking-at "|[[:space:]]+|") (next-line 1) t)

           ((search-forward-regexp "|[[:space:]]*\\([0-9.]+\\)[[:space:]]*|" end-pos t)
            (setq current-value (string-to-number (match-string 1)))
            (message "value = %s" current-value)
            (setq total-value (+ total-value current-value))
            (backward-char (string-width (match-string 0)))
            (next-line 1)
            t
            )

           ((looking-at "|=[^|]+|")
            (sc-replace-total "=%7.2f" total-value)
            nil
            )

           ((looking-at "|#[^|]+|")
            (sc-replace-total "#%5d" total-value)
            nil
            )

           (t (message "total value = %s" total-value) nil)
           )
          )
      )
    )
  )


;; taskjuggler
;; -----------

;;* (message "init.el: taskjuggler")
;;*
;;* (load "taskjuggler-mode")
;;*
;;*
;;* ;; task name<digits> "desc"
;;* ;; replace the digits with tj-number and increment tj-number
;;*
;;* (global-set-key (kbd "C-<kp-multiply>") 'tj-increment)
;;* (global-set-key (kbd "C-<kp-subtract>") 'tj-reset)
;;*
;;* (setq tj-number 1)
;;*
;;* (defun tj-reset (arg)
;;*   "renumber tasks"
;;*   (interactive "p")
;;*   (setq tj-number 1)
;;* )
;;*
;;*
;;* (defun tj-increment (arg)
;;*   "renumber tasks"
;;*   (interactive "p")
;;*   (if (search-forward-regexp "[[:space:]]*task[[:space:]][[:alpha:]_-]*\\([[:digit:]]+\\)")
;;*       (progn
;;*         (backward-delete-char (string-width (match-string 1)))
;;*         (insert-string tj-number)
;;*         (setq tj-number (+ tj-number 1))
;;*         ))
;;*
;;* )


;; binary numbering
;; ----------------

(message "init.el: Binary numbering")

(global-set-key (kbd "C-<kp-divide>") 'binary-increment)


(defun binary-increment-string (bin-str)
  "increment a string looking like '  1111 0101 1110 '"
  (let ((carry 1))
    (concat
     (reverse
      (mapcar #'(lambda (x)
                 (cond
                  ((and (= carry 1) (= x ?1)) ?0)
                  ((and (= carry 1) (= x ?0)) (setq carry 0) ?1)
                  (t x)))
              (reverse (string-to-list bin-str))))))
  )


(defun binary-increment (arg)
  "read a binary string from the cursor and place the incremented value below it"
  (interactive "p")
  (beginning-of-line)
  (let ((re "[[:space:]]*\\([01]+[01[:space:]]*\\)+"))
    (if (looking-at re)
        (let ((start-pos (point)))
          (goto-char start-pos)
          (if (looking-at re)
              (let ((binary-string (match-string 0)))
                (goto-char start-pos)
                (forward-line)
                (if (looking-at re)
                    (delete-region (match-beginning 0) (match-end 0)))
                (insert-string (binary-increment-string binary-string))
                (goto-char start-pos)
                (forward-line)
                )
            )
          )
      )
    )
  )


;; Toggle case of letter at the cursor
;; -----------------------------------

(message "init.el: Toggle case")

(defun toggle-case-char-at-point (arg)
  "Convert the character at the cursor position to uppercase."
  (interactive "p")
  (let ((x case-fold-search))
    (setq case-fold-search nil)
    (if (looking-at "[a-z]")
        (upcase-region (point) (+ (point) 1))
      (downcase-region (point) (+ (point) 1))
      )
    (setq case-fold-search x)
    )
  (forward-char 1))


;; HTML timestamps (in ISO 8601 format)
;; ------------------------------------

(message "init.el: HTML timestamps")

(require 'time-stamp)

(add-hook 'before-save-hook 'time-stamp)

(defun insert-date-iso ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%R%z")))

(defun my-html-insert-timestamp ()
  "Customised timestamp insertion function."
  (insert "Last modified: "
          (format-time-string "%Y-%m-%dT%R%z")
          "\n"
          )
  )

(defun my-html-replace-timestamp ()
  "Customised timestamp insertion function."
  (interactive)
  (let ((cur (point)))
    (beginning-of-line)
    (goto-char (point-max))
    (if (search-backward "\n<!-- hhmts start -->\n" nil t)
        (progn
          (beginning-of-line)
          (next-line 2)
          (beginning-of-line)
          (let ((begin (point)))
            (if (search-forward "<!-- hhmts end -->\n" nil t)
                (progn
                  (previous-line 1)
                  (beginning-of-line)
                  (let ((end (point)))
                    (delete-region begin end)
                    (beginning-of-line)
                    (my-html-insert-timestamp)
                    )
                  )
              )
            )
          )
      )
    (goto-char cur)
    )
  nil
  )

(add-hook 'before-save-hook 'my-html-replace-timestamp)

;; for XEmacs html mode
;;(setq html-helper-timestamp-hook 'my-html-insert-timestamp)


;; email system
;; ------------

;;(message "init.el: loading Wanderlust Email Reader")

;;(require 'wanderlust-startup)


;; for the F11 key: if server edit dispatch the buffer, else just kill it
;; ----------------------------------------------------------------------

(message "init.el: server-edit")

(defun my-server-edit (arg)
  "Exit server buffer and kill buffer"
  (interactive "p")
  (unless (if (functionp 'server-edit)
              (if server-buffer-clients
                  (progn (server-edit) 't)
                nil)
            nil)
    (if window-system
        (kill-buffer (current-buffer))
      (save-buffers-kill-emacs))
    )
  )

;; setup server
;; ------------

;; setup the server socket to a different value dependant on the
;; current desktop number obtained from KDE dcop program.  Use a shell
;; script to determine the desktop number and call emacsclient with the
;; "-s <socket>" option.
;; The socket path looks like: /tmp/emacs<uid>-<desktop>

(if (eq system-type 'darwin)
    (message "OS: darwin")
  (progn
    (message "OS: other")

    (message "init.el: Starting server")

    (let ((qdesktop (executable-find "eie"))
          (work-buffer "*CurrentScreen*")
          )
      (if qdesktop
          (save-excursion
            (if (bufferp work-buffer) (kill-buffer work-buffer))
            (call-process qdesktop nil work-buffer nil "--desktop-number")
            (set-buffer work-buffer)
            (let ((screen-number (string-to-number (buffer-string))))
              (setq server-socket-dir (format "/tmp/emacs%d-%d" (user-uid) screen-number))
              )
            (kill-buffer work-buffer))))

    (when window-system
      (message (format "init.el: server-socket-dir = %s" server-socket-dir))
      ;;(if (and (boundp 'gnuserv-process) (not gnuserv-process)) (gnuserv-start))
      (server-start))))


;; git blame override
;; ------------------

(eval-after-load "vc-annotate"
  '(defun vc-annotate-get-time-set-line-props ()
    (let ((bol (point))
          (date (vc-call-backend vc-annotate-backend 'annotate-time))
          (inhibit-read-only t))
      (assert (>= (point) bol))
      (put-text-property bol (point) 'invisible 'vc-annotate-annotation)
      (when (string-equal "Git" vc-annotate-backend)
      (save-excursion
        (goto-char bol)
        (search-forward "(")
        (let ((p1 (point)))
          (re-search-forward " [0-9]")
          (remove-text-properties p1 (1- (point)) '(invisible nil))
          )))
    date)))


;; custom set variable at end to override any internal package defaults
;; --------------------------------------------------------------------

(message "init.el: Customising variables")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; fix printer command
;; -------------------

(if (eq system-type 'gnu/linux)
  (setq ps-lpr-command "/usr/bin/lpr"))


;; override certain of the above options on a machine-by-machine basis
;; -------------------------------------------------------------------

(load "local-config" t)


;; finished
;; --------

(if window-system
    (message "init.el: Initialisation complete (X11)")
  (message "init.el: Initialisation complete (command)")
  )
