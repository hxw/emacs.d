;; init.el
;; =======

;; load-path
;; ---------

(setq init-dir (concat "~" init-file-user "/.emacs.d"))
;;(setq init-dir (concat "/home/" user-login-name "/.emacs.d"))

(setq load-path (append
                 (list (concat init-dir ""))
                 (list (concat init-dir "/lisp"))
                 load-path))


;; my screen size - so it is easy to change
;; ----------------------------------------

(setq my-lines 46)
(setq my-columns 123)


;; fonts
;; -----

(when (not (eq system-type 'darwin))
  (setq fs "fontset-default")

  ;; cannot change ascii
  ;;(set-fontset-font fs 'ascii "--courier-medium-r-normal-*-18-*-*-*-*-*-iso8859-1")

  (set-fontset-font fs 'vietnamese-viscii-upper "-*-fixed-medium-r-*--18-*-viscii1.1-1")
  (set-fontset-font fs 'vietnamese-viscii-lower "-*-fixed-medium-r-*--18-*-viscii1.1-1")
  ;;(set-fontset-font fs 'chinese-gb2312 "-*-*shanheisun*-medium-r-*--18-*-gb2312*-*")
  ;;(set-fontset-font fs 'chinese-big5-1 "-*-*shanheisun*-medium-r-*--18-*-big5*-*")
  ;;(set-fontset-font fs 'chinese-big5-2 "-*-*shanheisun*-medium-r-*--18-*-big5*-*")
  ;;(set-fontset-font fs 'chinese-gb2312 "-*-*-medium-r-*--18-*-gb2312*-*")
  ;;(set-fontset-font fs 'chinese-big5-1 "-*-*-medium-r-*--18-*-big5*-*")
  ;;(set-fontset-font fs 'chinese-big5-2 "-*-*-medium-r-*--*-*-big5*-*")
  ;;(set-fontset-font fs 'chinese-sisheng "-etl-fixed-medium-r-normal-*-18-*-*-*-*-*-sisheng_cwnn-0")
  ;;(set-fontset-font fs 'chinese-cns11643-1 "-hku-fixed-medium-r-normal-*-18-*-*-*-*-*-cns11643.1992-1")
  ;;(set-fontset-font fs 'chinese-cns11643-2 "-hku-fixed-medium-r-normal-*-18-*-*-*-*-*-cns11643.1992-2")
  (set-fontset-font fs 'latin-iso8859-1 "-*-fixed-medium-r-*--18-*-iso8859-1")
  (set-fontset-font fs 'latin-iso8859-2 "-*-fixed-medium-r-*--18-*-iso8859-2")
  (set-fontset-font fs 'latin-iso8859-3 "-*-fixed-medium-r-*--18-*-iso8859-3")
  (set-fontset-font fs 'latin-iso8859-4 "-*-fixed-medium-r-*--18-*-iso8859-4")
  (set-fontset-font fs 'cyrillic-iso8859-5 "-*-fixed-medium-r-*--18-*-iso8859-5")
  (set-fontset-font fs 'arabic-iso8859-6 "-*-fixed-medium-r-*--18-*-iso8859-6")
  (set-fontset-font fs 'greek-iso8859-7 "-*-fixed-medium-r-*--18-*-iso8859-7")
  (set-fontset-font fs 'hebrew-iso8859-8 "-*-fixed-medium-r-*--18-*-iso8859-8")
  (set-fontset-font fs 'latin-iso8859-9 "-*-fixed-medium-r-*--18-*-iso8859-9")
  (set-fontset-font fs 'latin-iso8859-14 "-*-fixed-medium-r-*--18-*-iso8859-14")
  (set-fontset-font fs 'latin-iso8859-15 "-*-fixed-medium-r-*--18-*-iso8859-15")
  (set-fontset-font fs 'arabic-digit ":-*-fixed-medium-r-*-*-*-*-*-*-*-*-mulearabic-0")
  (set-fontset-font fs 'arabic-digit "-*-fixed-medium-r-*-*-*-*-*-*-*-*-mulearabic-0")
  (set-fontset-font fs 'arabic-1-column "-*-fixed-medium-r-*-*-*-*-*-*-*-*-mulearabic-1")
  (set-fontset-font fs 'arabic-2-column "-*-fixed-medium-r-*-*-*-*-*-*-*-*-mulearabic-2")
  (set-fontset-font fs 'ipa "-*-fixed-medium-r-*--18-*-muleipa-1")
  (set-fontset-font fs 'ethiopic "-*-ethio*-medium-r-normal--18-*-*-*-*-*-admas-fontspecific")
  (set-fontset-font fs 'ethiopic "-*-ethio*-medium-r-normal--18-*-*-*-*-*-ethiopic-unicode")
  (set-fontset-font fs 'katakana-jisx0201 "-*-fixed-medium-r-*--18-*-jisx0201.1976-*")
  (set-fontset-font fs 'latin-jisx0201 "-*-fixed-medium-r-*--18-*-jisx0201.1976-*")
  (set-fontset-font fs 'japanese-jisx0208-1978 "-*-fixed-medium-r-*--18-*-jisx0208.1978-*")
  ;;(set-fontset-font fs 'japanese-jisx0208 "-*-fixed-medium-r-*--18-*-jisx0208.1990-*")
  (set-fontset-font fs 'japanese-jisx0208 "-Misc-Fixed-Medium-R-Normal--16-*-jisx0208.1990-0")
  ;;(set-fontset-font fs 'japanese-jisx0212 "-*-fixed-medium-r-*--18-*-jisx0212.1990-*")
  (set-fontset-font fs 'japanese-jisx0212 "-*-fixed-medium-r-*--16-*-jisx0212.1990-*")
  (set-fontset-font fs 'japanese-jisx0213-2 "-*-fixed-medium-r-*--18-*-jisx0213.2000-*")
  ;;(set-fontset-font fs 'korean-ksc5601 "-*-mincho-medium-r-*--18-*-ksc5601.1987-*")
  ;;(set-fontset-font fs 'korean-ksc5601 "-*-medium-r-*--18-*-ksc5601.1987-*")
  (set-fontset-font fs 'lao "-*-fixed-medium-r-normal-*-*-*-*-*-*-*-mulelao-1")
  ;;(set-fontset-font fs 'thai-tis620 "-etl-fixed-medium-r-normal--18-*-tis620.2529-1")
  (set-fontset-font fs 'thai-tis620 "-misc-fixed-medium-r-normal--16-*-tis620.2529-1")
  (set-fontset-font fs 'tibetan "-tibmdxa-fixed-medium-r-normal-*-*-*-*-*-*-*-muletibetan-0")
  (set-fontset-font fs 'tibetan-1-column "-tibmdxa-fixed-medium-r-normal-*-*-*-*-*-*-*-muletibetan-1")
  (set-fontset-font fs 'indian-is13194 "-*-fixed-medium-r-normal-*-18-*-*-*-*-*-is13194-devanagari")
  (set-fontset-font fs 'indian-2-column "-*-fixed-medium-r-normal-*-18-*-*-*-*-*-muleindian-2")
  (set-fontset-font fs 'mule-unicode-0100-24ff "-misc-fixed-medium-r-normal-*-18-*-*-*-*-*-iso10646-1")
  (set-fontset-font fs 'mule-unicode-2500-33ff "-misc-fixed-medium-r-normal-*-18-*-*-*-*-*-iso10646-1")
  (set-fontset-font fs 'mule-unicode-e000-ffff "-misc-fixed-medium-r-normal-*-18-*-*-*-*-*-iso10646-1")

  )

;;-monotype-courier new-medium-r-monospaced--0-0-0-0-m-0-mulearabic-0
;;-monotype-courier new-medium-r-monospaced--0-0-0-0-m-0-mulearabic-1
;;-monotype-courier new-medium-r-monospaced--0-0-0-0-m-0-mulearabic-2

;;        chinese-gb2312:-*-fangsong ti-medium-r-normal--*-*-*-*-*-*-gb2312.1980-*,
;;        unicode-mono:-gnu-unifont-medium-r-normal--16-160-75-75-c-80-iso10646-1,
;;        unicode-wide:-gnu-unifont-medium-r-normal--16-160-75-75-c-80-iso10646-1,
;;        unicode-mono:-redhat-liberation mono-medium-r-normal-*-*-*-*-*-*-*-iso10646-1,
;;        unicode-wide:-redhat-liberation mono-medium-r-normal-*-*-*-*-*-*-*-iso10646-1"
;;        mule-unicode-0100-24ff:-redhat-*-medium-r-normal-*-*-*-*-*-*-*-iso10646-1,
;;        mule-unicode-2500-33ff:-redhat-*-medium-r-normal-*-*-*-*-*-*-*-iso10646-1,
;;        mule-unicode-e000-ffff:-redhat-*-medium-r-normal-*-*-*-*-*-*-*-iso10646-1,
;;        mule-unicode-0100-24ff:-redhat-liberation mono-medium-r-normal-*-*-*-*-*-*-*-iso10646-1,
;;        mule-unicode-2500-33ff:-redhat-liberation mono-medium-r-normal-*-*-*-*-*-*-*-iso10646-1,
;;        mule-unicode-e000-ffff:-redhat-liberation mono-medium-r-normal-*-*-*-*-*-*-*-iso10646-1,


;; emacs customisation
;; -------------------

(message "init.el: Customisation")

;; '(face-font-family-alternatives (quote (("liberation mono" "courier" "fixed") ("helv" "helvetica" "arial" "fixed") ("yhunifont" "ming for iso10646 " "ar pl mingti2l big5" "ar pl shanheisun uni" "ar pl new sung"))))
;; '(face-font-registry-alternatives (quote (("iso10646-1") ("gb2312.1980" "gb2312.80&gb8565.88" "gbk*") ("jisx0208.1990" "jisx0208.1983" "jisx0208.1978") ("ksc5601.1989" "ksx1001.1992" "ksc5601.1987") ("big5-0" "big5.eten-0" "big5*") ("muletibetan-2" "muletibetan-0") ("iso10646-1"))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-buffer+size-width 100)
 '(Buffer-menu-mode-width 120)
 '(buffers-menu-max-size 30)
 '(c-file-offsets (quote ((brace-list-intro . +) (statement-block-intro . +) (knr-argdecl-intro . +) (substatement-open . 0) (label . 0) (statement-cont . +))) t)
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(display-time-mode t)
 '(flyspell-default-dictionary "british")
 '(font-lock-use-colors t)
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(initial-frame-alist (quote ((top . 1) (left . 1) (width . 120) (height . 42))))
 '(ispell-local-dictionary "british")
 '(lpr-page-header-switches (quote ("-F" "-t")))
 '(perl-indent-level 2)
 '(quack-pretty-lambda-p t)
 '(quack-smart-open-paren-p t)
 '(scroll-bar-mode (quote right))
 '(sh-basic-offset 2)
 '(sh-indent-after-do (quote +))
 '(sh-indent-for-do 0)
 '(sh-indent-for-then 0)
 '(sh-indentation 2)
 '(show-paren-mode t)
 '(speedbar-show-unknown-files t)
 '(tcl-indent-level 2)
 '(time-stamp-format "%:y-%02m-%02dT%02H:%02M:%02S %:z")
 '(tooltip-mode nil nil (tooltip))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))


(if (not (eq system-type 'darwin))
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     ;; '(default ((t (:stipple nil :background "lavenderblush" :foreground "blue4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :height 120))))
     ;; '(buffers-tab ((t (:foreground "black" :background "Gray80" :size "10" :slant normal)))))
     ;;
     '(default ((t (:stipple nil :background "lavenderblush" :foreground "blue4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :family "fixed")))))

  ;; from old mac
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:foreground "blue4" :background "lavenderblush" :size "12pt" :family "apple-monaco"))))))


;; adjust window size
;; ------------------

(add-hook 'after-init-hook '(lambda ()
                              (progn
                                (add-to-list 'default-frame-alist (cons 'height my-lines))
                                (add-to-list 'default-frame-alist (cons 'width my-columns)))))


;; function keys
;; -------------

(message "init.el: Function keys")

;;(global-set-key [f1] 'save-buffers-kill-emacs)
(global-set-key [f2] 'save-buffer)
(global-set-key [f3] 'match-paren)
;;(global-set-key [f4] 'switch-to-buffer)
(global-set-key [f4] (lambda () "toggle between buffers" (interactive) (switch-to-buffer nil)))

(global-set-key [f5] 'toggle-case-char-at-point)

(global-set-key [f6] (lambda (arg)
                       "set up flyspell"
                       (interactive "p")
                       (flyspell-mode 1)
                       (flyspell-buffer)))


(global-set-key [f7] 'set-mark-command)
(global-set-key [f8] 'call-last-kbd-macro)

;;(global-set-key [f9] nil)
(global-set-key [f10] 'bury-buffer)
(global-set-key [f11] 'my-server-edit)
(global-set-key [f12] 'delete-other-windows)

;;(global-set-key [C-S-f12] 'wl)


;; miscellaneous keys
;; ------------------

(message "init.el: Global keys")


(global-set-key "\M-z" 'save-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key [kp_5] 'goto-line)   ; 5
(global-set-key [begin] 'goto-line)   ; 5
(global-set-key "\C-\\" 'undo)
(global-set-key "\M-ESC" 'keyboard-quit) ; Esc-Esc
(global-set-key [pause] 'eval-region) ; Pause

(global-set-key [C-prior] 'beginning-of-buffer) ; CTRL-Page Up
(global-set-key [C-next] 'end-of-buffer) ; CTRL-Page Down

(when window-system
  (global-unset-key "\C-z") ; iconify-or-deiconify-frame (C-x C-z)
  (global-unset-key [insert])
  )


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

(global-set-key [mouse-4] 'my-mouse-wheel-down)
(global-set-key [mouse-5] 'my-mouse-wheel-up)


;; w3m
;; ---

;;*(message "init.el: w3m")
;; load the interface
;;* (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
;;*(setq w3m-home-page "http://127.0.0.1/~hsw/")

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

(message "init.el: ocaml")

(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
;;(require 'caml-font)


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


;; Haskell mode
;; ------------

(message "init.el: Haskell")


(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hi$"     . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
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


;; Ruby mode
;; ---------

(message "init.el: ruby")

(setq ruby-program-name "/usr/local/bin/ruby")
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))


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
  (when (or (string= (substring mode-name 0 2) "C/")
            (string= (substring mode-name 0 4) "C++/"))
    (message "tabifying buffer before save")
    (save-excursion
      (tabify (point-min) (point-max)))
    )
)

(setq tabify-regexp "^\t* [ \t]+")

(add-hook 'before-save-hook 'my-tabify)


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


;; Modes
;; -----

(message "init.el: mode alist changes")

(setq auto-mode-alist
      (append
       '(("\\.a77\\'" . asm-mode)
         ("\\.inc\\'" . asm-mode)
         ("\\.js\\'" . java-mode)
         )
       auto-mode-alist
       )
      )

(add-hook 'asm-mode-hook
          '(lambda ()
             (setq comment-column '48)
	     )
	  )

;;(add-hook 'asm-mode-set-comment-hook
;;          '(lambda ()
;;             (setq asm-comment-char ?@)
;;	     )
;;	  )


;; removing spaces at end-of-line
;; ------------------------------

(message "init.el: Remove spaces")

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; matching parentheses
;; --------------------

(message "init.el: Match parenteses")

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise do nothing"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;; spreadsheet calculation
;; -----------------------

(message "init.el: Spreadsheet")

(global-set-key [C-kp-multiply] 'sc-calc)
(global-set-key [C-kp-add] 'sc-total)

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
;;* ; task name<digits> "desc"
;;* ; replace the digits with tj-number and increment tj-number
;;*
;;* (global-set-key [C-kp-multiply] 'tj-increment)
;;* (global-set-key [C-kp-subtract] 'tj-reset)
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

(global-set-key [C-kp-divide] 'binary-increment)


(defun binary-increment-string (bin-str)
  "increment a string looking like '  1111 0101 1110 '"
  (let ((carry 1))
    (concat
     (reverse
      (mapcar '(lambda (x)
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
  (let ((re "[[:space:]]*[01]+[01[:space:]]*[01][[:space:]]"))
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

;; can this be replaces by a variation of the above
(defun my-current-time-zone ()
  "convert time zone to [+-]hh:mm"
  (let ((tz (car (current-time-zone)))
	(sec-per-hr (* 60 60))
        )
    (cond
     ((> tz 0) (format "+%02d:%02d"
                       (/ tz sec-per-hr)
                       (% tz sec-per-hr)))
     ((format "+%02d:%02d"
              (/ (- tz) sec-per-hr)
              (% (- tz) sec-per-hr)))
     )
    )
  )

(defun my-html-insert-timestamp ()
  "Customised timestamp insertion function."
  (insert "Last modified: "
	  (time-stamp-yyyy-mm-dd)
	  "T"
	  (time-stamp-hh:mm:ss)
	  (my-current-time-zone)
	  "\n"
	  )
  )

(defun my-html-replace-timestamp ()
  "Customised timestamp insertion function."
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

(message "init.el: Starting server")

(if (eq system-type 'darwin)
    (message "OS: darwin")
  (progn
    (message "OS: other")
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
            (kill-buffer work-buffer))))))

(when window-system
  (message (format "init.el: server-socket-dir = %s" server-socket-dir))
  ;;(if (and (boundp 'gnuserv-process) (not gnuserv-process)) (gnuserv-start))
  (server-start))


;; British dictionary
;; ------------------

(message "init.el: British dictionary")

;; not sure why the custom-set-variables does not work for this
;; perhaps the initial load of ispell forces "american"
(ispell-change-dictionary "british")


;; special options for non-X11
;; ---------------------------

(unless window-system
  (setq inhibit-splash-screen t)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  )


;; finished
;; --------

(if window-system
    (message "init.el: Initialisation complete (X11)")
  (message "init.el: Initialisation complete (command)")
  )
