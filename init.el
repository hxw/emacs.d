; .emacs
; ======

; function keys
; -------------

;(global-set-key [f1] 'save-buffers-kill-emacs)
(global-set-key [f2] 'save-buffer)
;(global-set-key [f3] 'kill-buffer)
(global-set-key [f4] 'switch-to-buffer)

;(global-set-key [f5] 'vc-toggle-read-only)
;(global-set-key [f6] 'vc-finish-logentry)
;(global-set-key [f7] 'vc-register)
(global-set-key [f8] 'call-last-kbd-macro)

;(global-set-key [f9] 'keyboard-quit) ; special?
(global-set-key [f10] 'bury-buffer)
(global-set-key [f11] 'my-server-edit)
(global-set-key [f12] 'delete-other-windows)

(global-set-key [C-S-f12] 'wl)


; miscellaneous keys
; ------------------

(global-set-key "\M-z" 'save-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key [kp_5] 'goto-line)   ; 5
(global-set-key [begin] 'goto-line)   ; 5
(global-set-key "\C-\\" 'undo)
(global-set-key "\M-ESC" 'keyboard-quit) ; Esc-Esc
(global-set-key [pause] 'eval-region) ; Pause

(global-set-key [C-prior] 'beginning-of-buffer) ; CTRL-Page Up
(global-set-key [C-next] 'end-of-buffer) ; CTRL-Page Down


; mouse wheel
; -----------

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


; Ocaml mode
; ----------

(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(require 'caml-font)
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
;;(autoload 'caml-hilit "caml-hilight" "Hilit19 patterns used for Caml mode" t)


; Ruby mode
; ---------

(setq ruby-program-name "/usr/local/bin/ruby")
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))


; emacs customisation menu
; ------------------------
;;; '(c-offsets-alist (quote ((brace-list-intro . 0) (substatement-open . 0))))
;;; '(c-file-offsets (quote ((statement-block-intro . +) (knr-argdecl-intro . +) (substatement-open . 0) (label . 0) (statement-cont . +))) t)
;;latest '(c-offsets-alist (quote ((brace-list-intro . +) (substatement-open . 0))))


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
 '(display-time-mode t nil (time))
 '(face-font-family-alternatives (quote (("courier" "fixed") ("helv" "helvetica" "arial" "fixed") ("yhunifont" "ming for iso10646 " "ar pl mingti2l big5" "ar pl shanheisun uni" "ar pl new sung"))))
 '(face-font-registry-alternatives (quote (("gb2312.1980" "gb2312.80&gb8565.88" "gbk*") ("jisx0208.1990" "jisx0208.1983" "jisx0208.1978") ("ksc5601.1989" "ksx1001.1992" "ksc5601.1987") ("big5-0" "big5.eten-0" "big5*") ("muletibetan-2" "muletibetan-0") ("iso10646-1"))))
 '(flyspell-default-dictionary "british")
 '(font-lock-use-colors t)
 '(global-font-lock-mode t nil (font-lock))
 '(ispell-local-dictionary "british")
 '(perl-indent-level 2)
 '(scroll-bar-mode (quote right))
 '(sh-basic-offset 2)
 '(sh-indent-after-do (quote +))
 '(sh-indent-for-do 0)
 '(sh-indent-for-then 0)
 '(sh-indentation 2)
 '(show-paren-mode t nil (paren))
 '(speedbar-show-unknown-files t)
 '(tcl-indent-level 2)
 '(time-stamp-format "%:y-%02m-%02dT%02H:%02M:%02S %:z")
 '(tooltip-mode nil nil (tooltip))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "lavenderblush" :foreground "blue4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 136 :width normal :family "wqy"))))
 '(buffers-tab ((t (:foreground "black" :background "Gray80" :size "10" :slant normal)))))


; for the F11 key: if server edit dispatch the buffer, else just kill it
; ----------------------------------------------------------------------

(defun my-server-edit (arg)
"Exit server buffer and kill buffer"
(interactive "p")
  (if server-buffer-clients
    (server-edit)
    (kill-buffer (current-buffer))
  )
)

; miscellaneous items
; -------------------

(line-number-mode 1)

(setq display-time-24hr-format 'T)
(display-time)

(setq vc-initial-comment 'T)

(mouse-avoidance-mode 'animate)

; Modes
; -----

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

;(add-hook 'asm-mode-set-comment-hook
;          '(lambda ()
;             (setq asm-comment-char ?@)
;	     )
;	  )



; matching parentheses or insert a '%'
; ------------------------------------

(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


; spreadsheet calculation
; -----------------------

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


; Toggle case of letter at the cursor
; -----------------------------------

(global-set-key "`" 'toggle-case-char-at-point)

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
;; ===============

(require 'time-stamp)

(add-hook 'write-file-hooks 'time-stamp)

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

(add-hook 'write-file-hooks 'my-html-replace-timestamp)

; for XEmacs html mode
;(setq html-helper-timestamp-hook 'my-html-insert-timestamp)

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)

(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)


;(if (featurep 'mule) (set-language-environment '"Chinese-BIG5"))
;(if (featurep 'mule) (set-language-environment "UTF-8"))
;(utf-translate-cjk-load-tables)

(if (not (member '("-*-*-medium-normal-r-*-13-*-*-*-*-*-fontset-chinese"
		   . "fontset-chinese") fontset-alias-alist))
    (progn
      (create-fontset-from-fontset-spec
       "-*-*-medium-r-normal-*-13-*-*-*-*-*-fontset-chinese,
        ascii:-*-courier-medium-r-*--18-*-iso8859-1,
        vietnamese-viscii-upper:-*-fixed-medium-r-*--16-*-viscii1.1-1,
        vietnamese-viscii-lower:-*-fixed-medium-r-*--16-*-viscii1.1-1,
        chinese-gb2312:-*-*shanheisun*-medium-r-*--18-*-gb2312*-*,
        chinese-big5-1:-*-*shanheisun*-medium-r-*--18-*-big5*-*,
        chinese-big5-2:-*-*shanheisun*-medium-r-*--18-*-big5*-*,
        latin-iso8859-1:-*-fixed-medium-r-*--16-*-iso8859-1,
        latin-iso8859-2:-*-fixed-medium-r-*--16-*-iso8859-2,
        latin-iso8859-3:-*-fixed-medium-r-*--16-*-iso8859-3,
        latin-iso8859-4:-*-fixed-medium-r-*--16-*-iso8859-4,
        cyrillic-iso8859-5:-*-fixed-medium-r-*--16-*-iso8859-5,
        arabic-iso8859-6:-*-fixed-medium-r-*--16-*-iso8859-6,
        greek-iso8859-7:-*-fixed-medium-r-*--16-*-iso8859-7,
        hebrew-iso8859-8:-*-fixed-medium-r-*--16-*-iso8859-8,
        latin-iso8859-9:-*-fixed-medium-r-*--16-*-iso8859-9,
        latin-iso8859-14:-*-fixed-medium-r-*--16-*-iso8859-14,
        latin-iso8859-15:-*-fixed-medium-r-*--16-*-iso8859-15,
        mulearabic-0:-*-fixed-medium-r-*--16-*-mulearabic-0,
        mulearabic-1:-*-fixed-medium-r-*--16-*-mulearabic-1,
        mulearabic-2:-*-fixed-medium-r-*--16-*-mulearabic-2,
        muleaipa-1:-*-fixed-medium-r-*--16-*-muleipa-1,
        ethiopic:-*-ethio*-medium-r-normal--16-*-*-*-*-*-admas-fontspecific
        katakana-jisx0201:-*-fixed-medium-r-*--16-*-jisx0201.1976-*,
        latin-jisx0201:-*-fixed-medium-r-*--16-*-jisx0201.1976-*,
        japanese-jisx0208-1978:-*-fixed-medium-r-*--16-*-jisx0208.1978-*,
        japanese-jisx0208:-*-fixed-medium-r-*--16-*-jisx0208.1983-*,
        katakana-jisx0208:-*-fixed-medium-r-*--16-*-jisx0208.1983-*,
        katakana-jisx0212:-*-fixed-medium-r-*--16-*-jisx0212.1990-*,
        korean-ksc5601:-*-mincho-medium-r-*--16-*-ksc5601.1987-*,
        thai-tis620:-etl-fixed-medium-r-normal--16-*-tis620.2529-1"
	t)

      (setq default-frame-alist
            (append
             '((font . "fontset-chinese"))
             default-frame-alist))
      )
  )


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Xrefactory configuration part ;;
(defvar xref-current-project nil) ;; can be also "my_project_name"
;(defvar xref-key-binding 'global) ;; can be also 'local or 'none
(defvar xref-key-binding 'none) ;; can be also 'local or 'none
(setq load-path (cons "/data/home/hsw/Xrefactory/xref/emacs" load-path))
(setq exec-path (cons "/data/home/hsw/Xrefactory/xref" exec-path))
(load "xrefactory")
;; end of Xrefactory configuration part ;;

;; Xrefactory key definitions

(defun my-xref-add-bindings-to-keymap (keymap)
  "Set up shortcut keys fior Xrefactory"
  (define-key keymap [(f9)] 'xref-refactor)
  (define-key keymap [(shift f9)] 'xref-completion)
;  (define-key keymap [(shift f8)] 'xref-ide-compile-run)
  (define-key keymap [(f7)] 'xref-delete-window)
  (define-key keymap [(f6)] 'xref-push-and-goto-definition)
  (define-key keymap [(shift f6)] 'xref-browse-symbol)
  (define-key keymap [(f5)] 'xref-pop-and-return)
  (define-key keymap [(shift f5)] 'xref-re-push)
  (define-key keymap [(f4)] 'xref-next-reference)
  (define-key keymap [(shift f4)] 'xref-alternative-next-reference)
  (define-key keymap [(f3)] 'xref-previous-reference)
  (define-key keymap [(shift f3)] 'xref-alternative-previous-reference)
)

(my-xref-add-bindings-to-keymap global-map)

(message "xrefactory loaded")


;; email system

(message "loading Wanderlust Email Reader")

(require 'wanderlust-startup) 

; setup server
; ------------

(message "starting server")
(let ((dcop (executable-find "dcop"))
      (dcop-buffer "*dcop*")
      )
  (if dcop
      (save-excursion
	(call-process "/usr/local/bin/dcop" nil dcop-buffer nil "kwin" "KWinInterface" "currentDesktop")
	(set-buffer dcop-buffer)
	(let ((screen-number (string-to-number (buffer-substring-no-properties 1 3)))
	      )
	  (message (format "/tmp/emacs%d-%d" (user-uid) screen-number))
	  (setq server-socket-dir (format "/tmp/emacs%d-%d" (user-uid) screen-number))
	  )
	(kill-buffer dcop-buffer)
	)
    )
  )

(message server-socket-dir)
(server-start)


(message ".emacs complete")
