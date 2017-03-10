;; personal info
(setq user-full-name "Li Shuai")
(setq user-mail-address "lishuaihenu@gmail.com")

(setq inhibit-startup-message t)
;; for gui
(customize-set-variable 'scroll-bar-mode 'right)
(setq x-select-enable-clipboard t)
(tool-bar-mode -1)

;;----------------------------------------------------------------------------
;; key binding
;;----------------------------------------------------------------------------
;; y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; C-k
(setq-default kill-whole-line t)

;; windmove
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
;; for putty
(global-set-key (kbd "ESC <left>") 'windmove-left)
(global-set-key (kbd "ESC <right>") 'windmove-right)
(global-set-key (kbd "ESC <up>") 'windmove-up)
(global-set-key (kbd "ESC <down>") 'windmove-down)

;; move
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; paren mode
(show-paren-mode t)
(setq show-paren-style 'parentheses)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert '."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	((looking-at "\\s\[") (forward-list 1) (backward-char 1))
	((looking-at "\\s\]") (forward-char 1) (backward-list 1))
	((looking-at "\\s\{") (forward-list 1) (backward-char 1))
	((looking-at "\\s\}") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(global-set-key (kbd "M-'") 'match-paren)
(global-set-key (kbd "C-<return>") 'match-paren)

;; M-w
(defadvice kill-line (before check-position activate)
  (if (member major-mode
	      '(emacs-lisp-mode
		c-mode
		c++-mode
		erlang-mode
		go-mode
		plain-tex-mode))
      (if (and (eolp) (not (bolp)))
	  (progn (forward-char 1)
		 (just-one-space 0)
		 (backward-char 1)))))
(defadvice kill-ring-save (before slick-copy activate compile)
  "Copy a single line"
  (interactive (if mark-active (list (region-beginning) (region-end))
		 (message "Copied line")
		 (list (line-beginning-position)
		       (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "kill a single line"
  (interactive (if mark-active (list (region-beginning) (region-end))
		 (list (line-beginning-position)
		       (line-beginning-position 2)))))
(defun qiang-copy-line (arg)
  "Copy line"
  (interactive "p")
  (kill-ring-save (point)
		  (line-end-position))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(global-set-key (kbd "M-k") 'qiang-copy-line)

;; comment
(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'qiang-comment-dwim-line)

;; init file
(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key "\C-xi" 'open-init-file)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;----------------------------------------------------------------------------
;; mouse
;;----------------------------------------------------------------------------
;; copy
(setq mouse-drag-copy-region nil)

;; cursor type
(setq-default cursor-type 'bar)
;; end of line
(setq line-move-visual nil)
(setq track-eol t)

(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;;----------------------------------------------------------------------------
;; tabbar
;;----------------------------------------------------------------------------
(load-file "~/.emacs.d/tabbar.el")
(require 'tabbar)
(tabbar-mode)
(global-set-key (kbd "M-j") 'tabbar-backward)
(global-set-key (kbd "M-k") 'tabbar-forward)

;; theme
(set-face-attribute 'tabbar-default nil
		    :family "Bitstream Vera Sans Mono"
		    :background "gray80"
		    :foreground "gray30"
		    :height 1.0
		    )
(set-face-attribute 'tabbar-button nil
		    :inherit 'tabbar-default
		    :box '(:line-width 1 :color "black")
		    )
;; corrent tab
(set-face-attribute 'tabbar-selected nil
		    :inherit 'tabbar-default
		    :foreground "DarkGreen"
		    :background "LightGoldenrod"
		    :box '(:line-width 2 :color "DarkGoldenrod")
		    :overline "black"
		    :underline "black"
		    :weight 'bold
		    )
(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    )

;;----------------------------------------------------------------------------
;; theme
;;----------------------------------------------------------------------------
(require 'linum)
(global-linum-mode t)
(column-number-mode t)

(when (not window-system)
  (xterm-mouse-mode t)
  (defadvice linum-update-window (around linum-format-dynamic activate)
    (let* ((w (length (number-to-string
		       (count-lines (point-min) (point-max)))))
	   (linum-format (concat "%" (number-to-string w) "d ")))
      ad-do-it)))

(require 'whitespace)
(global-set-key [f6] 'whitespace-mode)
(setq whitespace-line-column 250)

;;(setq whitespace-hspace
;;      '(face tab lines-tail space-after-tab))
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; calm-forest
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'calm-forest t)

(global-hl-line-mode t)
(global-font-lock-mode t)

(setq default-frame-alist
      '((height . 26)
	(width . 80)
	(cursor-color . "orange")))

(load-file "~/.emacs.d/fill-column-indicator.el")
(require 'fill-column-indicator)

(define-globalized-minor-mode global-fci-mode
  fci-mode
  (lambda ()
    (fci-mode 1)))

(global-fci-mode 1)
(setq-default fill-column 80)

;; file path
(setq frame-title-format '(
			   (:eval
			    (if (buffer-file-name)
				(concat
				 (directory-file-name
				  (file-name-directory
				   (abbreviate-file-name
				    (buffer-file-name))))
				 "/%b" )))))

;; display time
(set-time-zone-rule "GMT-8")
(setq display-time-format "%m.%d %I:%M%p")
(display-time)

;;----------------------------------------------------------------------------
;; rfc
;;----------------------------------------------------------------------------
(setq load-path (cons "~/.emacs.d/rfc" load-path))
(setq auto-mode-alist
      (cons '("/rfc[0-9]+\\.txt\\(\\.gz\\)?\\'" . rfcview-mode)
	    auto-mode-alist))
(autoload 'rfcview-mode "rfcview" nil t)

(require 'sb-texinfo)
(eval-after-load "speedbar" '(load-library "sb-rfcview"))

(custom-set-variables
 '(speedbar-supported-extension-expressions
   (append
    speedbar-supported-extension-expressions
    '("rfc[0-9]+\\.txt"))))

;; sr-speedbar in main frame
(load-file "~/.emacs.d/rfc/sr-speedbar.el")
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)

;;----------------------------------------------------------------------------
;; C
;;----------------------------------------------------------------------------
(c-set-offset 'substatement-open 0)

;; kernel style
(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-style "linux")   ;; or k&r
	     ;;(c-set-offset 'case-label '+)
	     ))

(setq-default tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil)
;; opencl
(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))

;;----------------------------------------------------------------------------
;; format
;;----------------------------------------------------------------------------
;; one key format
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successful"))
;; bound to F7
(global-set-key [f7] 'indent-whole)

;; format when copy
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
	   (member major-mode
		   '(emacs-lisp-mode
		     c-mode
		     c++-mode
		     erlang-mode
		     go-mode
		     plain-tex-mode))
	   (let ((mark-even-if-inactive transient-mark-mode))
	     (indent-region (region-beginning) (region-end) nil))))))

;; auto pair
(load-file "~/.emacs.d/autopair.el")
(require 'autopair)
(autopair-global-mode)
(add-hook 'c-mode-common-hook '(lambda () (autopair-mode)))

;;----------------------------------------------------------------------------
;; cperl
;;----------------------------------------------------------------------------
(defalias 'perl-mode 'cperl-mode)

(setq cperl-indent-level 4
      cperl-brace-offset -2)

;;----------------------------------------------------------------------------
;; erlang
;;----------------------------------------------------------------------------
(add-to-list 'load-path
	     (car (file-expand-wildcards "/usr/lib/erlang/lib/tools-*/emacs")))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

(add-hook 'erlang-mode-hook 'erlang-font-lock-level-3)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist '("\\.\\(erl\\|hrl\\|app\\|app.src\\)\\'" . erlang-mode))

;;(defun my-erlang-mode-hook ()
;;  (setq tab-width 4)
;;  (setq indent-tabs-mode nil)
;;  (setq c-basic-offset 4))

;; (setq erlang-indent-level 4)
;; (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

(add-to-list 'load-path "~/.emacs.d/distel/elisp")
(require 'distel)
(distel-setup)

(add-hook 'erlang-mode-hook
	  (lambda ()
	    ;; when starting an Erlang shell in Emacs, default in the node name
	    (setq inferior-erlang-machine-options '("-sname" "emacs" "-setcookie" "cookie"))
	    (modify-syntax-entry ?_ "w")
	    ;; add Erlang functions to an imenu menu
	    (imenu-add-to-menubar "imenu")))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

(setq derl-cookie "cookie")

;;----------------------------------------------------------------------------
;; other language
;;----------------------------------------------------------------------------
;; clisp
;;(add-to-list 'load-path "~/.emacs.d/slime/")
;;(setq inferior-lisp-program "/usr/bin/sbcl")
;;(require 'slime)
;;(slime-setup '(slime-fancy))

;; SML
;;(add-to-list 'load-path "~/.emacs.d/sml-mode/")
;;(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
;;(autoload 'run-sml "sml-proc" "Run an inferiro SML process." t)
;;(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\|fun\\)\\'" . sml-mode))

;; Java
(add-to-list 'load-path "~/.emacs.d/eclim/")
(require 'eclim)
(require 'eclimd)

;; Golang
(add-to-list 'load-path "~/.emacs.d/golang/")
(require 'go-mode-load)

;;----------------------------------------------------------------------------
;; CEDET
;;----------------------------------------------------------------------------
(require 'cedet)
(require 'semantic)
;;(global-ede-mode 1)

(setq semantic-default-submodes
      (append semantic-default-submodes
	      '(global-semantic-idle-local-symbol-highlight-mode
		global-semantic-decoration-mode
		global-semantic-highlight-func-mode
		global-semantic-stickyfunc-mode
		global-semantic-show-unmatched-syntax-mode
		global-semantic-mru-bookmark-mode
		)))

(setq semantic-decoration-styles '(("semantic-decoration-on-includes" . t)
				   ("semantic-decoration-on-protected-members")
				   ("semantic-decoration-on-private-members")))

(semantic-mode 1)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(define-key semantic-mode-map (kbd "C-c , .") 'semantic-ia-fast-jump)
(define-key semantic-mode-map (kbd "C-c , P") 'semantic-analyze-proto-impl-toggle)
(define-key semantic-mode-map (kbd "C-c , h") 'semantic-decoration-include-visit)

;;(require 'semantic-tag-folding nil 'noerror)
;;(global-semantic-tag-folding-mode 1)
;;(global-srecode-minor-mode 1)

;;----------------------------------------------------------------------------
;; auto-complete
;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs "~/.emacs.d/yasnippet/snippets")
(yas/global-mode 1)

(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
;;(define-key ac-completing-map "\t" 'ac-complete)
;;(define-key ac-completing-map "\r" nil)
(global-auto-complete-mode t)

;; clang-complete-async
(add-to-list 'load-path "~/.emacs.d/clang-async")
(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-async/clang-complete")
  (setq ac-sources '(ac-source-clang-async
		     ac-source-yasnippet))
  (ac-clang-launch-completion-process))

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup))

(my-ac-config)

;; golang-autocomplete
(require 'go-autocomplete)

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq ac-sources '(ac-source-go
			       ac-source-yasnippet))
	    (local-set-key (kbd "M-.") 'godef-jump)
	    (local-set-key (kbd "M-,") 'godef-jump-back)))

;; java
(setq eclim-auto-save t)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; eclim autocomplete
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
(add-hook 'java-mode-hook
	  (lambda ()
	    (remove 'ac-source-clang-async 'ac-sources)
	    (local-set-key (kbd "M-.") 'eclim-java-find-declaration)
	    (local-set-key (kbd "M-,") 'pop-tag-mark)
	    (eclim-mode t)))

(setq ac-auto-start 2)
(setq ac-dwim t)

;;----------------------------------------------------------------------------
;; cscope
;;----------------------------------------------------------------------------
(load-file "~/.emacs.d/xcscope.el")
(require 'xcscope)
;;(load-file "~/.emacs.d/ascope.el")
;;(require 'ascope)

;;----------------------------------------------------------------------------
;; ECB
;;----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/ecb-new-cedet")
(require 'ecb)

(setq ecb-tip-of-the-day nil)
(global-set-key [f12] 'ecb-activate)
(global-set-key [C-f12] 'ecb-deactivate)

;;(ecb-layout-define "my-cscope-layout" left nil
;;   (ecb-set-sources-buffer)
;;   (ecb-split-ver 0.5 t)
;;   (other-window 1)
;;   (ecb-set-methods-buffer)
;;   (ecb-split-ver 0.5 t)
;;   (other-window 1)
;;   (ecb-set-cscope-buffer))

(ecb-layout-define "my-cscope-layout" left nil
		   (ecb-set-sources-buffer)
		   (ecb-split-ver 0.4 t)
		   (other-window 1)
		   (ecb-set-methods-buffer))

(defecb-window-dedicator-to-ecb-buffer ecb-set-cscope-buffer t
				       " *ECB cscope-buf*"
  (switch-to-buffer "*cscope*"))

(setq ecb-layout-name "my-cscope-layout")
(setq ecb-history-make-buckets 'never)

(custom-set-variables
 '(ecb-options-version "2.40"))

;;----------------------------------------------------------------------------
;; GDB
;;----------------------------------------------------------------------------
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)

(defun kill-buffer-when-exit ()
  "Close assotiated buffer when a process exited"
  (let ((current-process (ignore-errors (get-buffer-process (current-buffer)))))
    (when current-process
      (set-process-sentinel current-process
			    (lambda (watch-process change-state)
			      (when (string-match "\\(finished\\|exited\\)" change-state)
				(kill-buffer (process-buffer watch-process))))))))
(add-hook 'gdb-mode-hook 'kill-buffer-when-exit)
(add-hook 'shell-mode-hook 'kill-buffer-when-exit)

;; when gdb start, close ecb
(defadvice gdb (before ecb-deactivate activate)
  (when (and (boundp 'ecb-minor-mode) ecb-minor-mode)
    (ecb-deactivate)))
