;; init.el  -*- lexical-binding: t; -*-
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

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))


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
(global-set-key (kbd "<f2>") #'(lambda () "save buffer" (interactive) (delete-other-windows) (save-buffer)))
(global-set-key (kbd "<f3>") 'match-paren)
(global-set-key (kbd "<f4>") #'(lambda () "toggle between buffers" (interactive) (switch-to-buffer nil)))

(global-set-key (kbd "<f5>") 'toggle-case-char-at-point)

(global-set-key (kbd "S-<f5>") #'(lambda (arg)
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

(when (require 'mml "mml" t)
  (global-set-key (kbd "C-<f9>") (lambda () "mime to mml"
                                   (interactive)
                                   (toggle-read-only 0)
                                   (mime-to-mml)
                                   (not-modified)
                                   (toggle-read-only 1))))

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
(global-set-key (kbd "S-<pause>") 'eval-region) ; Shift Pause
(global-set-key (kbd "C-<pause>") 'eval-defun) ; Ctrl Pause

(global-set-key (kbd "C-<prior>") 'beginning-of-buffer) ; CTRL-Page Up
(global-set-key (kbd "C-<next>") 'end-of-buffer) ; CTRL-Page Down

(global-set-key (kbd "<insert>") 'comment-region) ; Insert comment markers
(global-set-key (kbd "C-<insert>") 'uncomment-region) ; Remove comment markers

(global-set-key (kbd "s-r") #'(lambda () (interactive) (revert-buffer t t nil))) ; Win-r

(global-set-key (kbd "s-h") 'hoogle) ; Win-h

(global-set-key (kbd "C-<print>") 'my-control-print-screen)


;; in cases where mode-name is a structure (e.g., in "Elisp" mode)
(defun mode-name-string ()
  "if list, return the car else return the string"
  (or (and (stringp mode-name) mode-name) (car mode-name)))


;; PrtScn key handler
(defun my-control-print-screen (arg)
  "Do something useful with the Ctrl-PrtScn"
  (interactive "P")
  (let ((mn (mode-name-string)))
    (cond ((string= mn "Markdown")
           (my-markdown-to-pdf arg))
          ((string= mn "Org")
           (my-org-to-html arg))
          (t
           (my-recompile arg) ; on t580 <print> is in the <menu> position
           ))))
           ;;(message "init.el: my-control-print-screen: unsupported mode: ~p", mode-name)))))


(when window-system
  (global-unset-key (kbd "C-z")) ; iconify-or-deiconify-frame (C-x C-z)
  )


;; ido mode
;; --------

(require 'ido)
(ido-mode t)
(global-set-key "\C-x\C-b" 'ibuffer)
(ido-toggle-prefix) ; ensure start in sub-string match


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

(defvar my-last-compilation-exit-status 0 "records the exit status of the last compile")

;; add a hook to remove compilation window if success
(defun my-compilation-finish (buffer status)
  (with-current-buffer buffer
    (save-excursion
      (let ((status (replace-regexp-in-string "\n?$" "" status)))

        (message "compilation finish: '%s'  '%s'" buffer status)
        (goto-char (point-min)) ; find compilation-dir: DIR-NAME in make output and override THIS_DIR as base
        (when (search-forward-regexp "compilation-dir:[[:space:]]+\\(.*\\)[[:space:]]" (point-max) t 1)
          (message "compilation-dir: '%s'" (match-string 1))
          (cd (match-string 1)))

        (if (and (= compilation-num-errors-found 0)
                 (= compilation-num-warnings-found 0)
                 (= compilation-num-infos-found 0)
                 (= my-last-compilation-exit-status 0))
            (let ((w (get-buffer-window)))
              (message "compilation success: '%s'  '%s'" buffer status)
              (when w
                (goto-char (point-min)) ; find close-compilation-window
                (when (not (search-forward-regexp "close-compilation-window:[[:space:]]+no[[:space:]]" (point-max) t 1))
                  (sleep-for 2) ; allow time to view success
                  (delete-window w)))))))))


(add-hook 'compilation-finish-functions 'my-compilation-finish)

(defun my-capture-exit-status (process-status exit-status msg)
  "save exit status to global variable"
  (setq my-last-compilation-exit-status exit-status)
  (cons msg exit-status))

(setq compilation-exit-message-function 'my-capture-exit-status)

(defun my-testcompile (arg)
  "run the test-1 target"
  (interactive "P")
  (my-recompile -1))

(defun my-recompile (arg)
  "Search for Makefile and recompile"
  (interactive "P")
  (let*
      ((base-directory-for-compile default-directory)
       (target
         (cond
          ((null arg)      "")
          ((equal '- arg)  "test")
          ((zerop arg)     "all")
          ((> arg 0)       (format "emacs-%d" arg))
          ((< arg 0)       (format "test-%d" (- arg)))
          (t "qqq")))
       (the-makefile "Emakefile"))

    (message "recompile target = %s" target)

    (if (file-exists-p the-makefile)
        (set (make-local-variable 'compile-command)
             (concat "make -f " the-makefile " THIS_DIR=. PROJECTS_DIR=. " target))
      (cl-loop for the-dir = default-directory
            then (file-name-directory (directory-file-name the-dir))
            until (or (null the-dir) (string-equal "/" the-dir))
            when (file-exists-p (concat the-dir "/../" the-makefile))
            do (progn
                 (message "dir = %s  makefile = %s" the-dir the-makefile)
                 (set 'base-directory-for-compile (directory-file-name the-dir))
                 (set (make-local-variable 'compile-command)
                      (concat "make -f '../" the-makefile
                              "' -C '" base-directory-for-compile
                              "' THIS_DIR='" default-directory
                              "' PROJECTS_DIR='" (file-truename (concat base-directory-for-compile "/.."))
                              "' " target))
                 (cl-return))))
    (message "recompile = %s" compile-command)
    (delete-other-windows)
    (recompile)
    (with-current-buffer (get-buffer-create "*compilation*") ; so find error can work
      (cd base-directory-for-compile))))


; standard Menu key
(global-set-key (kbd "C-<menu>") 'my-recompile)  ; CTRL-Menu
(global-set-key (kbd "ESC <menu>") 'my-testcompile)  ; Esc Menu
(global-set-key (kbd "M-<menu>") 'next-error) ; ALT-Menu
(global-set-key (kbd "S-<menu>") 'previous-error) ; Shift-Menu

; alternative Menu key
(global-set-key (kbd "C-<XF86MenuKB>") 'my-recompile)  ; CTRL-Menu
(global-set-key (kbd "ESC <XF86MenuKB>") 'my-testcompile)  ; Esc Menu
(global-set-key (kbd "M-<XF86MenuKB>") 'next-error) ; ALT-Menu
(global-set-key (kbd "S-<XF86MenuKB>") 'previous-error) ; Shift-Menu


;;*(global-set-key (kbd "C-<print>") 'my-recompile)  ; CTRL-Print
;;* ^ see above
(global-set-key (kbd "M-<print>") 'next-error) ; ALT-Print
(global-set-key (kbd "S-<print>") 'previous-error) ; Shift-Print

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


;; Unicode
;; -------

(message "init.el: unicode characters")

(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (cl-case name
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

                      ;; :=
                      ('colon-equals #X2254)
                      ('equals-colon #X2255)
                      ('function #X0192)

                      ;; misc
                      ('nil #X2205)
                      ('horizontal-ellipsis #X2026)
                      ('double-exclamation #X203C)
                      ('prime #X2032)
                      ('double-prime #X2033)
                      ('for-all #X2200)
                      ('there-exists #X2203)
                      ('element-of #X2208)
                      ('double-plus #X29FA)

                      ;; mathematical operators
                      ('square-root #X221A)
                      ('squared #X00B2)
                      ('cubed #X00B3)

                      )))

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


;; Lisp mode
;; ---------

(message "init.el: lisp greek text")

(defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
    (cl-loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code)))
                (font-lock-add-keywords nil
                                        `((,(cl-concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil
                                        `((,(cl-concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))
(add-hook 'lisp-mode-hook 'pretty-greek)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)

;; rainbow mode
;; ------------

(when (require 'rainbow-delimiters "rainbow-delimiters" t)
  (message "init.el: rainbow-delimiters")
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(when (require 'rainbow-identifiers "rainbow-identifiers" t)
  (message "init.el: rainbow-identifiers")
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))


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
           (cons "\\(''\\)" 'double-prime)
           (cons "\\('\\)" 'prime)
           (cons "\\<\\(List.for_all\\)\\>" 'for-all)
           (cons "\\<\\(List.exists\\)\\>" 'there-exists)
           (cons "\\<\\(List.mem\\)\\>" 'element-of)
           (cons "^ +\\(|\\)" 'double-vertical-bar))))

  (add-hook 'caml-mode-hook 'ocaml-unicode)
  (add-hook 'caml-mode-hook 'pretty-greek)
  (add-hook 'tuareg-mode-hook 'ocaml-unicode)
  (add-hook 'tuareg-mode-hook 'pretty-greek)

  (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
  ;;(autoload 'caml-hilit "caml-hilight" "Hilit19 patterns used for Caml mode" t)
  )

;; Merlin for Ocaml

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))


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
                    (cl-loop for i from 2 upto 120 by 2 collect i))
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
         (cons "[^-]\\(->\\)[^>]" 'right-arrow)
         (cons "\\(==\\)" 'identical)
         (cons "\\(/=\\)" 'not-identical)
         (cons "\\(()\\)" 'nil)
         (cons "\\<\\(sqrt\\)\\>" 'square-root)
         (cons "\\(&&\\)" 'logical-and)
         (cons "[^|]\\(||\\)[^|]" 'logical-or)
         (cons "\\<\\(not\\)\\>" 'logical-neg)
         (cons "\\(>\\)[^=]" 'greater-than)
         (cons "\\(<\\)[^=]" 'less-than)
         (cons "[^>]\\(>=\\)" 'greater-than-or-equal-to)
         (cons "[^<]\\(<=\\)" 'less-than-or-equal-to)
         (cons "\\(\\+\\+\\)" 'double-plus)
         (cons "\\(''\\)" 'double-prime)
         (cons "\\('\\)" 'prime)
         (cons "\\(!!\\)" 'double-exclamation)
         (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))

(add-hook 'haskell-mode-hook 'haskell-unicode)
(add-hook 'haskell-mode-hook 'pretty-greek)


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

(defun compact-go-imports ()
  "remove extraneous blank lines from go imports"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[[:space:]]*import[[:space:]]*[(]" nil t)
      (end-of-line)
      (let ((p1 (point)))
        (when (re-search-forward "^[[:space:]]*[)][[:space:]]*$")
          (let ((bound (point)))
            (goto-char p1)
            (while (re-search-forward "\n\\([[:space:]]*\n\\)\\{1,\\}" bound t)
              (replace-match "\n" nil nil))))))))

(defun fix-go-break-continue ()
  "ensure all break and continue have labels - to prevent break in case/select causing memory leaks"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^//[[:space:]]*Copyright[[:space:]]+(c)[[:space:]]+.*Bitmark[[:space:]]+Inc\\.[[:space:]]*$" nil t)
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*\\(break\\|continue\\)[[:space:]]*\\(//.*\\)?$" nil t)
        (beginning-of-line)
        (forward-word)
        (insert " \"***")
        (insert (upcase (match-string 1)))
        (insert " MISSING LABEL***\"")
        (end-of-line)))))

(defun go-tidy-up ()
  "run all go tidying processes"
  (let ((mn (mode-name-string)))
    (when (string= mn "Go")
      (compact-go-imports)
      (fix-go-break-continue)
      (setq gofmt-command "gofmt")
      (setq gofmt-args (list "-s" "-r=(a) -> a"))
      (gofmt-before-save)
      (setq gofmt-command "goimports")
      (setq gofmt-args (if (boundp 'goimports-local)
                           (list "-local" goimports-local)))
      (gofmt-before-save))))

(global-set-key (kbd "S-<f7>") (lambda (arg)
                                 "insert if err → return"
                                 (interactive "p")
                                 (mapc (lambda (s) (newline 1 t) (insert s))
                                       '("if err != nil {" "return err" "}"))
                                 (newline 1 t)))

(defun golang-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "[^<]\\(<-\\)" 'left-arrow)
         (cons "\\(->\\)[^>]" 'right-arrow)
         ;;(cons "[^<]\\(<=\\)" 'leftwards-double-arrow)
         ;;(cons "\\(=>\\)[^>]" 'rightwards-double-arrow)
         ;;(cons "\\(=:=\\)" 'identical)
         ;;(cons "\\(=/=\\)" 'not-identical)
         (cons "\\(:=\\)" 'colon-equals)
         (cons "[^=]\\(==\\)[^=]" 'identical)
         (cons "[^=]\\(!=\\)" 'not-equal)
         ;;(cons "[^=/<>]\\(=\\)[^=/<>]" 'equivalent-to)
         ;;(cons "\\(\\[\\]\\)" 'nil)
         (cons "\\<\\(func\\)\\>" 'function)
         (cons "\\<\\(nil\\)\\>" 'nil)
         (cons "\\<\\(sqrt\\)\\>" 'square-root)
         (cons "\\(&&\\)" 'logical-and)
         (cons "\\(||\\)" 'logical-or)
         ;;(cons "\\<\\(not\\)\\>" 'logical-neg)
         (cons "\\(>\\)[^=]" 'greater-than)
         (cons "\\(<\\)[^=]" 'less-than)
         (cons "\\(>=\\)" 'greater-than-or-equal-to)
         (cons "\\(<=\\)" 'less-than-or-equal-to)
         )))

(when (require 'go-mode "go-mode" t)
  (message "init.el: go mode")
  ;;(require 'auto-complete)
  ;;(require 'go-autocomplete)
  ;;(add-hook 'go-mode-hook #'go-eldoc-setup)
  (add-hook 'go-mode-hook #'golang-unicode)
  (add-hook 'before-save-hook #'go-tidy-up t))


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
      (do-commit-header))
     ((string-equal fn "changelog")
      (setq indent-tabs-mode nil
            tab-width 2
            left-margin 2)
      (do-commit-header))
     ((string-match "[.]\\(go\\|cpp\\|h\\|c\\|hs\\)$" fn)
      (flyspell-prog-mode)))))

(add-hook 'find-file-hook 'activate-flyspell)


;; Lua mode
;; --------

(when (require 'lua-mode "lua-mode" t)
  (message "init.el: lua-mode available"))


;; SQL mode
;; --------

(when (require 'sqlup-mode "sqlup-mode" t)
  ;; (modify-syntax-entry ?" "\"" sql-mode-syntax-table)
  (message "init.el: sqlup-mode available")
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))



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

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd")))


(defun my-tabify ()
  "tabify the buffer for certain file types"
  (interactive)
  (let ((mn (mode-name-string)))
    (cond
     ((or (string= (substring mn 0 (min 2 (length mn))) "C/")
          (string= (substring mn 0 (min 4 (length mn))) "C++/"))
      (message "tabifying buffer before save")
      (save-excursion (clang-format-buffer)))
     ((or (string= (substring mn 0 (min 7 (length mn))) "Haskell")
          (string= (substring mn 0 (min 10 (length mn))) "Emacs-Lisp"))
      (message "untabifying buffer before save")
      (save-excursion
        (untabify (point-min) (point-max)))
      )
     ((or (string= (substring mn 0 (min 3 (length mn))) "SQL")
          (string= (substring mn 0 (min 3 (length mn))) "sql"))
      (message "untabifying buffer before save")
      (save-excursion
        (untabify (point-min) (point-max))
        ;;;(sqlup-capitalize-keywords-in-region (point-min) (point-max))
        )
      )
     )
    ))

(setq tabify-regexp "^\t* [ \t]+")

(add-hook 'before-save-hook 'my-tabify)


;; for clang-format
;; ----------------

(require 'clang-format)
;;(fset 'c-indent-region 'clang-format-region)


(defun my-clang-setup ()
  "change tab to use clang-format"
  (define-key c++-mode-map (kbd "<tab>") #'clang-format)
  (define-key c-mode-map (kbd "<tab>") #'clang-format))

(add-hook 'c-mode-hook #'my-clang-setup)
(add-hook 'c++-mode-hook #'my-clang-setup)


;; exuberant ctags
;; ---------------

(message "init.el: ctags")

;;(require 'auto-complete-exuberant-ctags)
;;(ac-exuberant-ctags-setup)

(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
;;(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'go-mode-hook  'turn-on-ctags-auto-update-mode)

;; (autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
;; (global-set-key "\C-cE" 'ctags-update)

;; for QT pro files
;; ----------------

(require 'qt-pro-mode)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))


;; Java mode
;; ---------

(message "init.el: Java styles")

(add-hook 'java-mode-hook #'(lambda ()
                              (c-set-style "java")
                              (setq indent-tabs-mode t)
                              (setq c-basic-offset 2)))


;; awk mode
;; --------

(message "init.el: awk styles")

(add-hook 'awk-mode-hook
          #'(lambda ()
             (setq c-basic-offset 4)))


;; nim mode
;; --------

;; (add-hook 'nim-mode-hook 'nimsuggest-mode)
;;  (setq nimsuggest-path "path/to/nimsuggest")


;; rust mode
;; ---------

(message "init.el: rust setup")

(add-hook 'after-save-hook 'my-show-rustfmt)

(defun my-show-rustfmt ()
  "if rust format failed split window to show errors"
  (let ((mn (mode-name-string)))
    (when (string= mn "Rust")
      (delete-other-windows)
      (when (get-buffer "*rustfmt*")
        (set-window-buffer (split-window-sensibly) "*rustfmt*")))))

(defun rust-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "[^<]\\(<-\\)" 'left-arrow)
         (cons "\\(->\\)[^>]" 'right-arrow)
         (cons "\\(=>\\)[^>]" 'rightwards-double-arrow)
         (cons "\\(==\\)" 'identical)
         (cons "\\(/=\\)" 'not-identical)
         (cons "[^a-zA-Z0-9_>:]\\(()\\)" 'nil)
         (cons "\\<\\(sqrt\\)\\>" 'square-root)
         (cons "\\(&&\\)" 'logical-and)
         (cons "[^|]\\(||\\)[^|]" 'logical-or)
         (cons "\\<\\(not\\)\\>" 'logical-neg)
         (cons "\\(>\\)[^=]" 'greater-than)
         (cons "\\(<\\)[^=]" 'less-than)
         (cons "[^>]\\(>=\\)" 'greater-than-or-equal-to)
         (cons "[^<]\\(<=\\)" 'less-than-or-equal-to)
         (cons "\\(\\+\\+\\)" 'double-plus)
         (cons "\\(''\\)" 'double-prime)
         (cons "\\('\\)" 'prime)
         (cons "\\(!!\\)" 'double-exclamation)
         (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))

(add-hook 'rust-mode-hook 'rust-unicode)
(add-hook 'rust-mode-hook 'pretty-greek)



;; Python and Django
;; -----------------

;; see this: http://stackoverflow.com/questions/1257236/django-emacs-as-textmate-replacement
;; look at the answer by: http://stackoverflow.com/users/2354/benjamin-pollack


(defun my-set-flymake-mode ()
  (flymake-mode 1))

(when (require 'flymake "flymake" t)
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

(put 'overwrite-mode 'disabled t)  ; disable nasty overstrike mode
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


;; Org mode
;; --------

(message "init.el: org-mode available")

(defun my-org-to-html (arg)
  "convert the current markdown buffer to a pdf file"
  (interactive "P")
  (org-export-dispatch (universal-argument)))


;; Markdown mode
;; -------------

(when (require 'markdown-mode "markdown-mode" t)
  (message "init.el: markdown-mode available"))

(defun my-markdown-to-pdf (arg)
  "convert the current markdown buffer to a pdf file"
  (interactive "p")
  (let ((pdf-viewer "evince --presentation")
        (markdown-command (concat
                           "pandoc --from=markdown "
                           "--to=latex --pdf-engine=xelatex "
                           "--number-sections "
                           "--standalone --self-contained"))
        (output-file-name (concat (file-name-base) ".pdf"))
        (pandoc-errors (get-buffer-create "*Pandoc Errors*")))
    (message "creating PDF in: %s" output-file-name)
    (with-current-buffer pandoc-errors (erase-buffer))
    (save-window-excursion
      (let ((begin-region)
            (end-region))
        (if (markdown-use-region-p)
            (setq begin-region (region-beginning)
                  end-region (region-end))
          (setq begin-region (point-min)
                end-region (point-max)))
        ;; Pass region to `markdown-command' via stdin
        (call-process-region begin-region end-region
                             shell-file-name nil (list pandoc-errors t) nil
                             shell-command-switch
                             (concat markdown-command " -o "
                                     (shell-quote-argument output-file-name)))))
    (if (> (buffer-size pandoc-errors) 0)
        (progn
          (message "pandoc failed")
          (display-buffer pandoc-errors
                          '((display-buffer-reuse-window
                             display-buffer-pop-up-window
                             display-buffer-pop-up-frame)
                            (reusable-frames . 0)
                            (window-height . nil)
                            (window-width . 40))))
      (when (and (not (null arg)) (> arg 0))
        (shell-command (concat pdf-viewer " " (shell-quote-argument output-file-name)))))))


;; YAML mode
;; ---------

(when (require 'yaml-mode "yaml-mode" t)
  (message "init.el: YAML-mode available"))


;; Modes
;; -----

(message "init.el: mode alist changes")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json\\.sample\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jsonld\\'" . json-mode))
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


;; MMM
;; ---

(message "init.el: MMM-mode")

(require 'mmm-auto)

(mmm-add-classes
 '((shell-script-awk
    :submode awk-mode
    :delimiter-mode nil
    :face mmm-code-submode-face
    :front "awk.*'[[:space:]]*\n"
    :back "\n[[:space:]]*'")
   (nginx-embedded-lua
    :submode lua-mode
    :delimiter-mode nil
    :face mmm-code-submode-face
    :front "content_by_lua_block[[:space:]]*{[[:space:]]*\n"
    :back "\n[[:space:]]*}[[:space:]]*#[[:space:]]*lua_end")))


(setq mmm-global-mode 'maybe)
(mmm-add-mode-ext-class 'sh-mode nil 'shell-script-awk)
(mmm-add-mode-ext-class 'conf-mode nil 'nginx-embedded-lua)


;; highlighting and removing spaces at end-of-line
;; -----------------------------------------------

(message "init.el: Remove spaces")


(defun my-trailing-whitespace ()
  "highlight trailing whilespace"
    (setq show-trailing-whitespace t))

(add-hook 'find-file-hook 'my-trailing-whitespace)

(defun my-delete-trailing-whitespace ()
  "remove trailing whilespace from certain file types"
  (interactive)
  (let ((mn (mode-name-string)))
    (cond ((string= mn "Hexl") t)
          ((string= mn "Diff") t)
          (t
           (message "removing trailing whitespace before save")
           (delete-trailing-whitespace)))))

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
    (message "init.el: OS: darwin")
  (progn
    (message "init.el: OS: other")

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

(mapcar (lambda (file-name)
          (let ((file-name (concat init-dir "/" file-name)))
            (when (file-exists-p file-name)
              (message "init.el: Loading: %s" file-name)
              (load file-name nil 't))))
     (list
      "custom.el"
      "local.el"))


;; printer command
;; ---------------

(if (eq system-type 'gnu/linux)
  (setq ps-lpr-command "/usr/bin/lpr"))

;; detech default printer name
(when (file-exists-p "/usr/local/bin/lpstat")
  (let ((work-buffer "*CurrentPrinter*"))
    (save-excursion
      (if (bufferp work-buffer) (kill-buffer work-buffer))
      (call-process "/usr/local/bin/lpstat" nil work-buffer nil "-d")
      (set-buffer work-buffer)
      (when (string-match "^system[[:space:]]+default[[:space:]]+destination:[[:space:]]+\\(.+\\)$" (buffer-string))
        (setq ps-printer-name (match-string 1 (buffer-string)))
        (kill-buffer work-buffer)
        (message "init.el: printer is: '%s'" ps-printer-name)))))


;; override certain of the above options on a machine-by-machine basis
;; -------------------------------------------------------------------

(load "local-config" t)


;; finished
;; --------

(if window-system
    (message "init.el: Initialisation complete (X11)")
  (message "init.el: Initialisation complete (command)")
  )
