
;; ;; ############################
;; ;; ##    GENERAL SETTINGS    ##
;; ;; ############################

(setq vc-follow-symlinks nil)

(setq comint-buffer-maximum-size 5120)                             ;; set maximum-buffer size for shell-mode, gdb-mode... 
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer) ;; truncate buffer to comint-buffer-maximum-size.

(setq inhibit-startup-message t)                     ;; close start-up message
(setq make-backup-files         nil)                 ;; Don't want any backup files 
(setq auto-save-list-file-name  nil)                 ;; Don't want any .saves files 
(setq auto-save-default         nil)                 ;; Don't want any auto saving 
                                                  
(setq-default indent-tabs-mode nil)                  ;; use space to replace TAB
(setq-default tab-width 4)                           ;; set TAB width
(setq tab-width 4)                                   
(setq indent-line-function 'insert-tab)
                                                  
(setq line-number-mode t)                            ;; show line number
(setq column-number-mode t)                          ;; show column number

(global-font-lock-mode t)                            ;; font lock mode
(setq font-lock-maximum-size 256000)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; if such mode is on, turn it off 
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))   ;; if such mode is on, turn it off 

(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; get rid of annoying prompt

(fset 'yes-or-no-p 'y-or-n-p)

;; "Buffer has a running process; kill it?"
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; "Active processes exist; kill them and exit anyway? "
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(setq query-on-exit nil)

;;;;;;

(ansi-color-for-comint-mode-on)

(setq dired-recursive-deletes 'top)

(setq scroll-step 1 ;; default: half page
      scroll-margin 1
      scroll-conservatively 5)

;; parenthesis
; (show-paren-mode t) ; show corresponding parenthesis
(global-set-key "%" 'match-paren) ; jump to corresponding parenthesis
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Enable duplex printing
(setq-default ps-spool-duplex t)

;; compilation
(setq compilation-scroll-output t)
;(setq compilation-context-lines 40)


;; ###############
;; ##    GIT    ##
;; ###############

;(require 'magit)

;; ####################
;; ##    CUA MODE    ##
;; ####################

(cua-mode t)
(setq cua-auto-tabify-rectangles nil)   ;; don't tabify after rectangle commands
(transient-mark-mode t)                 ;; no region when it is not highlighted
(setq cua-keep-region-after-copy t)     ;; standard Windows behaviour

;; ################
;; ##    FACE    ##
;; ################

;; color tuning
;; => http://jasonm23.github.com/emacs-theme-editor/
(defun my-color-theme ()
  (interactive)
  (color-theme-install
   '(my-color-theme
      ((background-color . "#000000")
      (background-mode . dark)
      (border-color . "#1a1a1a")
      (cursor-color . "#fce94f")
      (foreground-color . "#e5eae5")
      (mouse-color . "black"))
     (fringe ((t (:background "#1a1a1a"))))
     (mode-line ((t (:foreground "#e0e0e0" :background "#4a4a4a"))))
     (region ((t (:background "#3e5456"))))
     (font-lock-builtin-face ((t (:foreground "#81c3f3"))))
     (font-lock-comment-face ((t (:foreground "#ff6672"))))
     (font-lock-function-name-face ((t (:foreground "#b7ee7c"))))
     (font-lock-keyword-face ((t (:foreground "#79caf6"))))
     (font-lock-string-face ((t (:foreground "#ef9948"))))
     (font-lock-type-face ((t (:foreground"#19eb41"))))
     (font-lock-variable-name-face ((t (:foreground "#f0f075"))))
     (minibuffer-prompt ((t (:foreground "#7abcff" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))

(add-to-list 'load-path "~/lib/emacs/color-theme/")
(require 'color-theme)
(color-theme-initialize)
(my-color-theme)
;(color-theme-arjen)

;(load-theme 'zenburn)


;; ########################
;; ##    GRAPHIC FACE    ##
;; ########################

(if (display-graphic-p)
    (progn (menu-bar-mode t)
           (set-face-attribute 'default nil :height 135)
           (custom-set-faces
            '(tabbar-default ((((class color grayscale) (background dark)) (:inherit variable-pitch :background "color-102" :foreground "color-151" :box (:line-width 2 :color "grey75" :style released-button) :slant italic :weight ultra-light :height 1.0 :width normal))))))
  (menu-bar-mode nil))

;; turn off menu bar if in terminal mode
(if (display-graphic-p) (menu-bar-mode t) (menu-bar-mode nil))


;; #####################
;; ##    FOR XTERM    ##
;; #####################

(require 'xclip)

;; check 'cat -v'
(if window-system
    (xclip-copy-paste-disable)
  (xclip-copy-paste-enable))

;; #################################
;; ##    IDO SEARCH COMPLETION    ##
;; #################################

(require 'ido)
(ido-mode t)                         ; for buffers and files
(setq
 ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
 ; ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
 ido-work-directory-list '("~/")
 ido-everywhere t                    ; use for many file dialogs
 ido-case-fold  t                    ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30      ; should be enough
 ido-max-work-file-list      50      ; remember many
 ido-use-filename-at-point nil       ; don't use filename at point (annoying)
 ido-use-url-at-point nil            ; don't use url at point (annoying)
 ido-enable-flex-matching t          ; be flexible
 ido-max-prospects 4                 ; don't spam my minibuffer
 ido-confirm-unique-completion t)    ; for RET, even with unique completion

;; #######################
;; ##    TABBAR MODE    ##
;; #######################

;(require 'tabbar)
(require 'tabbar-extension)
(tabbar-mode)

;; customize color

(set-face-attribute 'tabbar-default nil
 :background "gray20" :foreground "gray20" :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute 'tabbar-unselected nil
 :background "gray30" :foreground "white" :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute 'tabbar-selected nil
 :background "gray75" :foreground "black" :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute 'tabbar-highlight nil
 :background "white" :foreground "black" :underline nil :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute 'tabbar-separator nil
 :background "gray20" :height 0.6)

;; abc.txt<2>  => abc.txt
(defun remove-angle-bracket (string)
  (let ((ind (string-match "<" string)))
    (cond
     (ind (substring string 0 ind))
     (t string))))

;; abc.txt.old => abc.txt
(defun remove-old-extension (string)
  (if (member (file-name-extension string) `("old", "bak"))
      (substring string 0 -4)
    string))

;; use above two functions to get correct file extension
(defun my-file-name-extension (string)
  (file-name-extension (remove-old-extension (remove-angle-bracket string))))

(defun my-skip-group ()
"whether this buffer is in the group we would like to skip during switching group"
(if (and (or
          (string= (substring (buffer-name) 0 1) "*")
          ;; (eq major-mode 'dired-mode)
          (string= (buffer-name) "TAGS"))
         (not (or
               (member (buffer-name) '("*ielm*" "*Tree*" "*Members*"))
               (string-match "^\*gud" (buffer-name))
               (string-match "^\*compilation\*" (buffer-name))
               ))) t nil))

(defun tabbar-buffer-groups ()
   (list
    (cond
     ((eq major-mode 'dired-mode) '"dired")
     ((eq major-mode 'ibuffer-mode) '"ibuffer")
     ((eq major-mode 'compilation-mode) '"*compilation*")
     ((or (eq major-mode 'c-mode)
          (eq major-mode 'c++-mode)) '"c/c++")
     ((or (eq major-mode 'ebrowse-tree-mode)
          (eq major-mode 'ebrowse-member-mode)) '"ebrowse")
     ((eq major-mode 'eassist-mode) '"eassist")
     ((or (eq major-mode 'sgml-mode)
          (eq major-mode 'nxml-mode)
          (eq major-mode 'html-mode)
          (eq major-mode 'nxml-mode)
          (eq major-mode 'css-mode)) '"*ml" )
     ((my-skip-group) '"hidden")
     (t '"others") ;; all others

     ;; ((member (my-file-name-extension (buffer-name))
     ;;          `("sh", "py", "pl", "exp")) "script")
     ;; (t
     ;;  ;; Return `mode-name' if not blank, `major-mode' otherwise.
     ;;  (if (and (stringp mode-name)
     ;;           ;; Take care of preserving the match-data because this function is called when updating the header line.
     ;;           (save-match-data (string-match "[^ ]" mode-name)))
     ;;      mode-name
     ;;    (symbol-name major-mode)))

     ))) 

;; skip "hidden" group
(defun my-tabbar-backward-group ()
  (interactive)
  (tabbar-backward-group)
  (while (my-skip-group) (tabbar-backward-group)))

(defun my-tabbar-forward-group ()
  (interactive)
  (tabbar-forward-group)
  (while (my-skip-group) (tabbar-forward-group)))

;; tabbar movement
(global-set-key [M-left]       'tabbar-backward-tab)
(global-set-key [M-right]      'tabbar-forward-tab)
(global-set-key [M-up]         'my-tabbar-backward-group)
(global-set-key [M-down]       'my-tabbar-forward-group)



;; ########################
;; ##    IBUFFER MODE    ##
;; ########################

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)
(define-key ibuffer-mode-map (kbd "RET") 'ibuffer-do-view-other-frame)

;; display format
(setq ibuffer-formats
      '( (mark modified read-only                        
               (name 55 55 :left :elide)
               ;(size 10 10 :left)
               (mode 20 20 :left :elide)
               (filename-and-process)
               ;(name 16 16 :left) (size 6 -1 :right)
               )))

;; grouping
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("c/c++"  (or
                          (mode . c-mode)
                          (mode . c++-mode)
                          (mode . c++-c-mode)))
               ("ebrowse" (or
                           (mode . ebrowse-tree-mode)
                           (mode . ebrowse-member-mode)))
               ("ab" (or
                         (name . ".*\.msg$")
                         (name . ".*\.out$")
                         (name . ".*\.ref$")
                         (name . ".*\.conf$")))
               ("xml"    (or
                          (mode . xml-mode)
                          (mode . nxml-mode)
                          (mode . sgml-mode)))
               ("java"   (mode . java-mode))
               ("script" (or
                          (mode . sh-mode)
                          (mode . python-mode)
                          (mode . perl-mode)
                          (mode . tcl-mode)
                          (mode . cperl-mode)))
               ("dired" (mode . dired-mode))
               ("private" (name . "^\\..*"))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (mode . muse-mode)))
               ("secret"  (name . "^\\*.*\\*$"))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))))))

(add-hook 'ibuffer-mode-hook '(lambda () (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)


;; ###################
;; ##    RECENTF    ##
;; ###################

(require 'recentf)
(setq recentf-auto-cleanup 'never) 
(recentf-mode 1)
(setq recentf-max-saved-items 3000)
(setq recentf-max-menu-items 3000)
(global-set-key "\C-x\ \C-o" 'recentf-open-files)

(global-set-key "\C-x\ \C-r" 'recentf-ido-find-file)
;(global-set-key "\C-x\ \C-r" 'recentf-ido-find-file-no-dir-name)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun recentf-ido-find-file-no-dir-name ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
		    (cons (file-name-nondirectory x)
			  x))
		  recentf-list))
	 (filename-list
	  (remove-duplicates (mapcar #'car file-assoc-list)
			     :test #'string=))
	 (filename (ido-completing-read "Choose recent file: "
					filename-list
					nil
					t)))
    (when filename
      (find-file (cdr (assoc filename
			     file-assoc-list))))))

;; ######################
;; ##    WHITESPACE    ##
;; ######################

;; (whitespace-mode) ;; enable globally

(setq whitespace-display-mappings
 '((space-mark 32 [183] [46])    ; normal space
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10])    ; newlne
   (tab-mark 9 [9655 9] [92 9])  ; tab
))
(setq whitespace-style '(trailing lines space-before-tab indentation space-after-tab) whitespace-line-column 125)


;; ########################
;; ##    AUTOCOMPLETE    ##
;; ########################

(add-to-list 'load-path "~/lib/emacs/auto-complete-1.3.1/")
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/lib/emacs/auto-complete-1.3.1/dict")
(setq ac-auto-start nil)

(global-set-key "\M-q" 'auto-complete)
(global-set-key "\M-a" 'ac-complete-semantic-all)

(defun ac-complete-semantic-all()
  (interactive)
  (auto-complete '(ac-source-semantic        ;; complete symbol under current namespace (. ->)
                   ac-source-semantic-raw))) ;; complete symbol in raw namespace

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;(add-to-list 'ac-modes '(xml-mode nxml-mode css-mode js-mode ))
(add-to-list 'ac-modes 'nxml-mode)
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'shell-script-mode)

;(add-hook 'nxml-mode (lambda () (add-to-list 'ac-sources '(ac-source-words-in-all-buffer ac-source-css-property))))


;; ###############
;; ##    Man    ##
;; ###############

(global-set-key "\C-x\ m" 'man)
(setq Man-switches "-a") ;; M-n / M-p to go to next/previous man page
;(setq Man-notify-method 'bully)
(setq Man-notify-method 'friendly)


;; ##########################
;; ##    dired & dired+    ##
;; ##########################

;;;; skip hidden files (press 'o' to toggle)
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; allow dired to be able to delete or copy a whole dir.
;; “always” means no asking. “top” means ask once. Any other symbol means ask each and every time for a dir and subdir.
(setq dired-recursive-copies  (quote always))
(setq dired-recursive-deletes (quote top))
(setq dired-listing-switches "-alh")

(add-hook 'dired-mode-hook
          (lambda ()
            ;; (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-find-file
            (define-key dired-mode-map "\d" 'dired-up-directory) ; backspace => dired-up-directory
            (define-key dired-mode-map "o"  'dired-omit-mode)    
            ))

(require 'dired+)

;; ####################
;; ##    openwith    ##
;; ####################

(require 'openwith)
(openwith-mode t)

;;  M-x customize-group RET openwith RET
(custom-set-variables
 '(openwith-associations (quote (("\\.pdf\\'" "evince" (file)) 
                                 ("\\.mp3\\'" "rhythmbox" (file)) 
                                 ("\\.\\(?:mpe?g\\|avi\\|mp4\\|webm\\|mkv\\|wmv\\|rmvb\\)\\'" "smplayer" (file)) 
                                 ("\\.\\(?:odp\\|ppt\\)\\'" "/usr/lib/libreoffice/program/oosplash" ("--impress" file)) 
                                 ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file)) 
                                 ("\\.dia\\'" "dia-normal" ("--integrated" file))))))
(custom-set-faces)


;; #####################
;; ##    CSS-color    ##
;; #####################

(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background 
                     (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  "enable rainbow-mode like effect"
  (font-lock-add-keywords nil hexcolour-keywords))

(defun insert-random-color-hsl ()
  "Insert a random color string of CSS HSL format.
   Example output: hsl(100,24%,82%)"
  (interactive)
  (insert (format "hsl(%d,%d%%,%d%%)" (random 360) (random 100) (random 100))) )

;(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'emacs-startup-hook 'hexcolour-add-to-font-lock)


;; ################################
;; ##    bookmark & bookmark+    ##
;; ################################

(make-directory "~/.emacs.d/desktop" t) ;; create desktop-dir if does not exist
(make-directory "~/.emacs.d/bmk" t) ;; create bmk-dir if does not exist

(setq bookmark-default-file  "~/.emacs.d/bmk/emacs.bmk")
(setq bookmark-bmenu-file-column 80)
(setq bookmark-save-flag nil) ;; no auto save

(add-to-list 'load-path "~/lib/emacs/bookmark-plus/")
(require 'bookmark+)

(setq bmkp-last-as-first-bookmark-file nil) ;; always starts with default-file

(defun my-bookmark-save-as ()
  "A wrapper around 'bookmark-save' to fulfill 'save as then switch'"
  (interactive)
  (let ((bmk-file (read-file-name "Save bookmark as: " "~/.emacs.d/bmk/")))
    (bookmark-save nil bmk-file)
    (bmkp-switch-bookmark-file-create bmk-file)))

(defun my-bmkp-set-desktop-bookmark () ;; bound to 'C-F12'
  "A wrapper around 'bmkp-set-desktop-bookmark'"
  (interactive)
  (let ((desktop-bmk (read-string "Set desktop bookmark: ")))
    (setq desktop-bmk (concat "~/.emacs.d/desktop/" desktop-bmk))
    (when (file-exists-p desktop-bmk) ;; rm old desktop-bmk file
      (delete-file desktop-bmk))
    (bmkp-set-desktop-bookmark desktop-bmk)))


(defadvice bmkp-jump-bookmark-file (around switchp-is-true (bookmark &optional switchp batchp))
  "Set arg 'switchp' always to true"
  (let ((switchp t)) ad-do-it))
(ad-activate 'bmkp-jump-bookmark-file)

;; customize sorting order for sort-cycling-alist
(setq bmkp-sort-orders-for-cycling-alist
      (quote (("by bookmark name" . bmkp-alpha-p)
              ("by last bookmark access" (bmkp-bookmark-last-access-cp) bmkp-alpha-p)
              ("by file name" (bmkp-file-alpha-cp) bmkp-alpha-p)
              ("by bookmark type" (bmkp-info-cp bmkp-url-cp bmkp-gnus-cp bmkp-local-file-type-cp bmkp-handler-cp) bmkp-alpha-p))))

(define-key bookmark-bmenu-mode-map "t" 'bookmark-bmenu-toggle-filenames)
(define-key bookmark-bmenu-mode-map "s" 'bookmark-bmenu-save)
(define-key bookmark-bmenu-mode-map "w" 'my-bookmark-save-as)                 ;; save as
(define-key bookmark-bmenu-mode-map "S" 'bmkp-bmenu-change-sort-order-repeat) ;; cycling sort
(define-key bookmark-bmenu-mode-map "B" 'bmkp-set-bookmark-file-bookmark)
(define-key bookmark-bmenu-mode-map "b" 'bmkp-revert-bookmark-file)           ;; reload 'bmkp-current-bookmark-file'

(define-key bookmark-bmenu-mode-map (kbd "RET") (lambda () (interactive)
                                                  (bookmark-bmenu-this-window)
                                                  (delete-other-windows)))

;; g        - refresh (different from reload 'bmkp-current-bookmark-file')
;; l        - load (append to current bookmark)
;; L        - load
;; .        - Show all bookmarks
;; <C-down> - Show the info, then move to next bookmark
;; <C-up>	- Show the info, then move to previous bookmark
;; w        - Show location of bookmark (in minibuffer)
;; r        - Rename or relocate bookmark
;; ?        - Show this help
;; q        - Quit (`*Bookmark List*')


;; ################
;; ##    TEST    ##
;; ################
