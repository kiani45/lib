
;; #########################
;; ##    FOR DEBUGGING    ##
;; #########################

;;;; with "--debug-init" ?

;; (setq debug-on-error t)
;; (setq stack-trace-on-error t)


;; #####################
;; ##    MY .EMACS    ##
;; #####################

(add-to-list 'load-path "~/lib/emacs")
(add-to-list 'load-path "~/lib/emacs/common")

;; import my modules
(add-to-list 'load-path "~/lib/emacs/kai")
(require 'functions)
(require 'coding-functions)

;; load common settings
(load-file "~/lib/emacs/kai/general.el")


;; ########################
;; ##    KEY BINDINGS    ##
;; ########################

(defadvice terminal-init-xterm (after select-shift-up activate) 
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2R" [S-f3])
  (define-key input-decode-map "\e[1;6R" [C-S-f3])
  (define-key input-decode-map "\e[F"    [end])
  ;; (define-key input-decode-map "\M-[1;5P" [C-f1])
  ;; (define-key input-decode-map "\M-[1;5Q" [C-f2])
  ;; (define-key input-decode-map "\M-[1;5R" [C-f3])
  ;; (define-key input-decode-map "\M-[1;5S" [C-f4])
  (global-set-key [select] 'move-end-of-line))

(global-set-key (kbd "C-M-z") 'previous-buffer)
(global-set-key (kbd "C-M-_") 'next-buffer)

(global-set-key (kbd "C-x 5") 'toggle-window-split) ;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead

;; C-M-h : c-mark-function

;; (global-set-key "\C-d" 'save-buffers-kill-terminal)

(global-set-key "\C-x\ \C-e" 'eval-current-buffer)

;; C-M-h ;; mark-defun

(global-set-key "\C-c\ l" 'copy-whole-line) 
(global-set-key "\C-x\ h" 'insert-header) 
(global-set-key "\C-c\ w" 'copy-string) 

(global-set-key "\C-c\ c" 'toggle-comment-on-line) 

(global-set-key "\C-c\ b" '(lambda() (interactive) (kill-new (buffer-name)))) 

(global-set-key "\C-x\ \C-g" 'my-gdb-wrapper)

(global-set-key [C-delete] 'kill-this-buffer)

(global-set-key "\C-x\ \C-l" 'ielm)  ;; elisp mode

(global-set-key "\C-x\ \C-d" 'dired-jump) ;; dired pwd

;; (global-set-key "\C-o" 'ido-find-file)
;; (global-set-key "\C-f" 'isearch-forward)

(global-set-key "\C-p" 'goto-percent)
(global-set-key "\C-l" 'goto-line)

(global-set-key "\C-b" 'revert-buffer)

;; grep
(setq grep-command "grep -nHR -e ")
(global-set-key (kbd "C-x g") 'grep)

(global-set-key [delete] 'delete-char)

(global-set-key [f1]     'delete-other-windows)
                          
(global-set-key [f2]     'select-next-window)
(global-set-key [C-f2]   'select-previous-window)

;; [f3] =>  'cscope-display-buffer (C/C++ mode)
;; (global-set-key [C-f3]   'cua-set-rectangle-mark)
(global-set-key (kbd "C-x r")   'cua-set-rectangle-mark)

;; window control
(global-set-key [f4]     'enlarge-window)
(global-set-key [C-f4]   'shrink-window)
(global-set-key [S-f4]   'enlarge-window-horizontally)
(global-set-key [C-S-f4] 'shrink-window-horizontally)
       
(global-set-key [C-f5]   'beginning-of-defun-raw)
(global-set-key [C-f6]   'end-of-defun)
                          
(global-set-key [f5]     'rename-file-and-buffer) 
(global-set-key [f6]     'move-file-and-buffer)

(global-set-key [f7]     'comment-or-uncomment-region)
;; (global-set-key [f7]     'comment-region)
;; (global-set-key [C-f7]   'uncomment-region)

(global-set-key [f8]     'magit-status)
                          
(global-set-key [f9]     'my-gdb-many-windows-general)
(global-set-key [C-f9]   'my-gdb-many-windows-assembly)
                          
(global-set-key [f10]    'my-compile)
(global-set-key [C-f10]  'my-kill-compilation)
                          
(global-set-key [f11]    'bookmark-set)
(global-set-key [C-f11]  'bookmark-jump)
                          
(global-set-key [f12]    'bookmark-bmenu-list)
(global-set-key [C-f12]  'my-bmkp-set-desktop-bookmark)

;; delete emacs server
;; (global-set-key [C-f12] '(lambda () (interactive)
;;                            (server-force-delete)
;;                            (save-buffers-kill-terminal)))


;; ################################
;; ##    MAJOR MODES SETTINGS    ##
;; ################################

(add-to-multiple-hooks '(emacs-lisp-mode-hook ielm-mode-hook help-mode-hook completion-list-mode-hook)
                       '(lambda ()
                          (local-set-key [(meta d)] 'describe-function)   ;; C-h f
                          (local-set-key [(meta s)] 'describe-variable))) ;; C-h v

;;;;;;;;;;;;;;;;    SHELL MODE    ;;;;;;;;;;;;;;;;

;; Mode specific options
(setq shell-file-name "/bin/bash") 

(add-hook 'shell-mode-hook
	  '(lambda ()
             (require 'shell-command)
             (shell-command-completion-mode) ;; TAB completion
             (ansi-color-for-comint-mode-on)
             (local-set-key [home] 'comint-bol) ; move to beginning of line, after prompt  
	     (local-set-key [up]  ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
	     (local-set-key [down] ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))))

(global-set-key (kbd "C-x s") 'shell) ; enter shell mode


;;;;;;;;;;;;;;;;    NXHTML MODE    ;;;;;;;;;;;;;;;;

;; Load nxml-mode for files ending in .xml, .xsl, .rng, .xhtml
;; (setq auto-mode-alist
;;       (cons '("\\.\\(xml\\|xsl\\|xsd\\|rng\\|xhtml\\)\\'" . nxml-mode) auto-mode-alist))

(add-hook 'nxml-mode-hook
	  '(lambda ()
             (define-key nxml-mode-map (kbd "C-c /") 'nxml-finish-element)
             (local-set-key [C-down] 'nxml-forward-element)    ; move to beginning of line, after prompt  
             (local-set-key [C-up]   'nxml-backward-element))) ; cycle backward through command history

;; we don't auto load this for .html file (default to nxml mode)
;; run the following function to switch the mode
;; you've better run it under XEMACS

(defun my-nxhtml-mode ()
  "customized nxhtml mode"
  (interactive)
  (load "~/lib/emacs/nxhtml/autostart.el")
  (nxhtml-mode)
  (revert-buffer nil t)
  (setq popcmp-completion-style 'emacs-default)
  ;; (define-key nxhtml-mode-map [C-f12] 'nxhtml-browse-file)
  (define-key nxhtml-mode-map "\M-q"  'nxml-complete))


;;;;;;;;;;;;;;;;    HTML MODE    ;;;;;;;;;;;;;;;;

;(add-hook 'html-mode-hook 'my-html-mode-hook)

;;;;;;;;;;;;;;;;    C/C++ MODE    ;;;;;;;;;;;;;;;;

(add-hook 'c-mode-common-hook '(lambda ()
                                 (check-ede-project-root)
                                 (enable-cscope)
                                 (enable-ebrowse)
                                 (whitespace-mode)
                                 (c-set-style "k&r")
                                 (setq c-basic-offset tab-width)
                                 (setq indent-tabs-mode nil) ;; force only spaces for indentation
                                 (setq c++-tab-always-indent t)
                                 (setq c++-indent-level 4)
                                 (setq c++-basic-offset 4)
                                 (setq c-basic-offset 4) 
                                 (setq c++-auto-hungry-initial-state 'none)
                                 (setq c++-delete-function 'backward-delete-char)
                                 ;; if/switch indention
                                 (setq c++-brace-offset -4)
                                 (c-set-offset 'substatement-open 0)
                                 (c-set-offset 'defun-block-intro 4)
                                 (c-set-offset 'statement-block-intro 4)
                                 ;(c-set-offset 'topmost-intro 2)
                                 ;(c-toggle-auto-newline 1)
                                 (setq c-continued-statement-offset 4)
                                 (setq c++-empty-arglist-indent 4)
                                 (c-set-offset 'substatement-open 0)))

;; #################
;; ##    CEDET    ##
;; #################

(add-to-multiple-hooks '(c-mode-common-hook java-mode-hook python-mode-hook makefile-mode-hook) 'enable-cedet)
;(enable-cedet)


;; ##################
;; ##    OTHERS    ##
;; ##################

;; (setq default-major-mode 'text-mode) ;; will make text-mode default.

;; (show-paren-mode t) ;; highlight matching parentheses next to cursor.


;; #################
;; ##    DEBUG    ##
;; #################

;; (setq debug-on-error t)
;; (setq stack-trace-on-error t)

;; ################
;; ##    TEST    ##
;; ################

(defun query-user (x y)
  "…"
  (interactive "sEnter friend's name: \nsEnter friend's age: ")
  (message "Name is: %s, Age is: %s" x y)
)

(defun ff (arg)
  "Prompt user to enter a file path, with file name completion and input history support."
  (interactive (list (read-file-name "Open directory:")) )
  (message "Path is 「%s」." arg) )

;; make automatically splitted window use 'split-window-vertically'
;; (setq split-height-threshold 80) ;; default to 80
(setq split-width-threshold  nil)   ;; default to 160


;;;;;;;;;;;;;;;;;;;;;

