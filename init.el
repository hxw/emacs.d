; .emacs


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

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(buffers-menu-max-size 30)
 '(c-file-offsets (quote ((statement-block-intro . +) (knr-argdecl-intro . +) (substatement-open . 0) (label . 0) (statement-cont . +))) t)
 '(c-offsets-alist (quote ((brace-list-intro . 0) (substatement-open . 0))))
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(face-font-family-alternatives (quote (("courier" "fixed") ("helv" "helvetica" "arial" "fixed") ("yhunifont" "ming for iso10646 "))))
 '(face-font-registry-alternatives (quote (("gb2312.1980" "gb2312.80&gb8565.88" "gbk*") ("jisx0208.1990" "jisx0208.1983" "jisx0208.1978") ("ksc5601.1989" "ksx1001.1992" "ksc5601.1987") ("big5-0" "big5*") ("muletibetan-2" "muletibetan-0") ("iso10646-1"))))
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
 '(tcl-indent-level 2)
 '(time-stamp-format "%:y-%02m-%02dT%02H:%02M:%02S %:z")
 '(tooltip-mode nil nil (tooltip))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style nil nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
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


; matching parentheses or insert a '%'
; ------------------------------------

(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


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
(if (featurep 'mule) (set-language-environment "UTF-8"))

; setup server
; ------------

(server-start)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; Xrefactory configuration part ;;
;; some Xrefactory defaults can be set here
(defvar xref-current-project nil) ;; can be also "my_project_name"
(defvar xref-key-binding 'global) ;; can be also 'local or 'none
(setq load-path (cons "/data/home/hsw/Xrefactory/xref/emacs" load-path))
(setq exec-path (cons "/data/home/hsw/Xrefactory/xref" exec-path))
(load "xrefactory")
;; end of Xrefactory configuration part ;;
(message "xrefactory loaded")
