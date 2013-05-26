
;; personal info
(setq user-full-name "Li Shuai")
(setq user-mail-address "lishuaihenu@gmail.com")

;; for gui
(customize-set-variable 'scroll-bar-mode 'right)
(setq x-select-enable-clipboard t)

(setq default-directory "~/workspace")

;; no welcome mesg
(setq inhibit-startup-message t)
(tool-bar-mode -1)

;;============================= key bond  =================================
;; y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; C-k 
(setq-default kill-whole-line t)
;; shift + space
;;(global-set-key [?\S- ] 'set-mark-command)

;; switch window
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;; move
(global-set-key (kbd "C-;") 'backward-paragraph)
(global-set-key (kbd "C-'") 'forward-paragraph)
(global-set-key (kbd "M-;") 'backward-page)
(global-set-key (kbd "M-'") 'forward-page) 

;; paren mode
(show-paren-mode t)
(setq show-paren-style 'parentheses)
(set-face-foreground 'show-paren-match "#004242")
(set-face-background 'show-paren-match "#B0B7B0")

(global-set-key (kbd "C-<return>") 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert '."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; M-w
(defadvice kill-line (before check-position activate)
  (if (member major-mode
	      '(emacs-lisp-mode
		c-mode
		c++-mode
		erlang-mode
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

;; C-x i open init.el
(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key "\C-xi" 'open-init-file) 

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;============================  mouse  =============================
;; copy
(setq mouse-drag-copy-region nil)

;; cursor type
(setq-default cursor-type 'bar)
;; end of line
(setq line-move-visual nil)
(setq track-eol t)

;; 3
(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

;; move away
;;(mouse-avoidance-mode 'animate)

;;================================  tabbar  ============================
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

;;=================================  theme   =============================
(require 'linum)
(global-linum-mode t)

;; for nw mode
(when (not window-system)
  (setq linum-format "%2d "))

(require 'whitespace)
(global-set-key [f6] 'whitespace-mode)
(setq whitespace-line-column 250)

;;(setq whitespace-hspace
;;      '(face tab lines-tail space-after-tab))
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)

;; current line
(global-hl-line-mode t)
(set-face-background hl-line-face "#2E2E2E")

(load-file "~/.emacs.d/fill-column-indicator.el")
(require 'fill-column-indicator)

(define-globalized-minor-mode global-fci-mode 
  fci-mode 
  (lambda () 
    (fci-mode 1)))

;;(global-fci-mode 1) 
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

;;==============================  rfc  ====================================
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

;;=============================  C style  =================================
(c-set-offset 'substatement-open 0)

;; kernel style
(add-hook 'c-mode-hook
	  '(lambda ()
	     (c-set-style "linux")   ;; or k&r
	     ;;(c-set-offset 'case-label '+)
	     (setq tab-width 8)
	     (setq indent-tabs-mode t)
	     (setq c-basic-offset 8)))

;; google-c-style
;;(load-file "~/.emacs.d/google-c-style.el")
;;(add-hook 'c-mode-common-hook 'google-set-c-style)
;;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; font
(global-font-lock-mode t)

;;============================  format  ==============================
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
		     plain-tex-mode))
	   (let ((mark-even-if-inactive transient-mark-mode))
	     (indent-region (region-beginning) (region-end) nil))))))
;; comment
(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'qiang-comment-dwim-line)

;; auto pair
(load-file "~/.emacs.d/autopair.el")
(require 'autopair)
;;(autopair-global-mode)
(add-hook 'c-mode-common-hook '(lambda () (autopair-mode)))

;;(load-file "~/.emacs.d/bhj-fonts.el")
;;(global-set-key [(control x) (meta -)] (lambda () (interactive) (bhj-step-frame-font-size -1)))
;;(global-set-key [(control x) (meta +)] (lambda () (interactive) (bhj-step-frame-font-size 1)))


;;================================  erlang  ============================
;;(setq tags-file-name "c:/workspace/erlang/TAGS") 

(setq load-path (cons "/usr/lib/erlang/lib/tools-2.6.10/emacs"
		      load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))

(require 'erlang-start)

(add-hook 'erlang-mode-hook 'erlang-font-lock-level-3)

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
	    ;; (setq inferior-erlang-machine-options '("-sname" "emacs"))
	    (setq inferior-erlang-machine-options '("-sname" "emacs@localhost" "-setcookie" "cookie"))
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

;;================================  opencl  ============================
(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))

;;================================   clisp  ==============================
;;(add-to-list 'load-path "~/.emacs.d/slime/")
;;(setq inferior-lisp-program "/usr/bin/sbcl")
;;(require 'slime)
;;(slime-setup '(slime-fancy))

;;=================================   SML   ==============================
;;(add-to-list 'load-path "~/.emacs.d/sml-mode/")
;;(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
;;(autoload 'run-sml "sml-proc" "Run an inferiro SML process." t)
;;(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\|fun\\)\\'" . sml-mode))

;;=================================   Golang   ==============================
;;(setq load-path (cons (expand-file-name "~/.emacs.d/go/") load-path))
;;(require 'go-mode-load)


;;================================  CEDET  ============================
;;(require 'cedet)
;;(global-ede-mode t)

;;
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
				  global-semantic-minor-mode
				  global-idle-summary-mode
				  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

;;(load-file "~/.emacs.d/semantic-tag-folding.el")
;;(global-semantic-tag-folding-mode 1)

;;(global-semantic-decoration-mode 1)
;;(require 'semantic/decorate/include nil 'noerror)

;;================================  ac  ============================

;;================================  cscope  ============================
(load-file "~/.emacs.d/xcscope.el")
(require 'xcscope)
;;(load-file "~/.emacs.d/ascope.el")
;;(require 'ascope)

;;================================  ECB  ============================
(add-to-list 'load-path "~/.emacs.d/ecb-new-cedet")
(require 'ecb)

(setq ecb-tip-of-the-day nil)
;; F12, C-F12
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

(defecb-window-dedicator-to-ecb-buffer ecb-set-cscope-buffer t " *ECB cscope-buf*"
  (switch-to-buffer "*cscope*"))

(setq ecb-layout-name "my-cscope-layout")

(setq ecb-history-make-buckets 'never)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 113 :width normal))))
 ;; '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))
)

;;==============================  GDB  =================================
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

;;when start gdb，close ecb
(defadvice gdb (before ecb-deactivate activate)
  (when (and (boundp 'ecb-minor-mode) ecb-minor-mode)
    (ecb-deactivate)))
