(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-buffer+size-width 100)
 '(Buffer-menu-mode-width 120)
 '(ac-exuberant-ctags-tag-file-name "TAGS")
 '(buffers-menu-max-size 30)
 '(c-electric-pound-behavior '(alignleft))
 '(c-file-offsets
   '((brace-list-intro . +)
     (statement-block-intro . +)
     (knr-argdecl-intro . +)
     (substatement-open . 0)
     (label . 0)
     (statement-case-open . +)
     (statement-case-intro . +)
     (statement-cont . +)) t)
 '(c-offsets-alist '((statement-case-open . +)))
 '(case-fold-search t)
 '(clang-format-executable "clang-format90")
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(ctags-update-command "/usr/local/bin/uctags")
 '(ctags-update-other-options
   '("--fields=+iaSt" "--extras=+q" "--exclude='.#*'" "--exclude='*.elc'" "--exclude='*.class'" "--exclude='.git'" "--exclude='.svn'" "--exclude='SCCS'" "--exclude='RCS'" "--exclude='CVS'" "--exclude='EIFGEN'"))
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(hsw-deep-blue))
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes
   '("4d6a19e2c845e50d00810e289f47b733517fb9bb94551035f8333e58beea5712" "caabc4d29d2e06132efa1a8fb12cf0bb074bfe7b720a444af57be6f493c922e6" "1a69a17b68c98407d6156d1e826a19d8d0b56f375ee50b5ee824fa754ced293c" "6a7ee7206d7f0a032de4e090823e4c4edb801d5b3b713149b468bfab92d85760" "e99ccf81a53297c4a245b6a2ea61b7254a059e88caad739bd91dad60112c9b4c" "b5dd2f75cdea917708e27bb659d023bd3d1cfcb0aa2bf39b73ceef705d581580" "9f0a7fc1d2ff2a460d21ad1bf1063ec336ba3b2db2cfbb7c153e45c22b3db8c5" "edd3a623f8258ef411a7989af97ba8b7a48c5100ff9b8585bcbced23a0c32510" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" "1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(default-input-method "rfc1345")
 '(display-line-numbers t)
 '(display-time-mode t nil (time))
 '(fancy-splash-image "~/Backgrounds/aphrodite-rhodes.jpg")
 '(flyspell-default-dictionary "british")
 '(font-lock-use-colors t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(ispell-local-dictionary "british")
 '(js-indent-level 2)
 '(js2-allow-keywords-as-property-names nil)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p nil)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(js2-indent-on-enter-key t)
 '(lua-indent-level 4)
 '(markdown-command
   "pandoc --from=markdown --to=html5 --standalone --self-contained --css=/home/hsw/.emacs.d/pandoc.css")
 '(menu-bar-mode nil)
 '(mouse-buffer-menu-maxlen 30)
 '(mouse-buffer-menu-mode-mult 30)
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-modern rainbow-delimiters rainbow-identifiers rainbow-mode nim-mode go-autocomplete ctags-update tuareg qt-pro-mode cargo go-eldoc go-mode yaml-mode sqlup-mode markdown-mode json-mode js2-mode htmlize haskell-mode flylisp aggressive-indent))
 '(perl-indent-level 2)
 '(ps-font-size '(12 . 12))
 '(ps-landscape-mode t)
 '(ps-lpr-command "/usr/local/bin/lpr-cups")
 '(ps-multibyte-buffer 'bdf-font-except-latin)
 '(ps-printer-name-option "-P")
 '(quack-pretty-lambda-p t)
 '(quack-smart-open-paren-p t)
 '(rust-format-on-save t)
 '(rust-indent-offset 4)
 '(safe-local-variable-values
   '((buffer-file-coding-system . utf-8-unix)
     (org-html-head-include-scripts)))
 '(scroll-bar-mode 'right)
 '(sgml-basic-offset 4)
 '(sh-basic-offset 2)
 '(sh-indent-after-do '+)
 '(sh-indent-for-do 0)
 '(sh-indent-for-then 0)
 '(sh-indentation 2)
 '(show-paren-mode t nil (paren))
 '(speedbar-show-unknown-files t)
 '(tcl-indent-level 2)
 '(time-stamp-format "%:y-%02m-%02dT%02H:%02M:%02S %:z")
 '(tool-bar-mode nil)
 '(tooltip-mode nil nil (tooltip))
 '(uniquify-buffer-name-style 'forward nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
