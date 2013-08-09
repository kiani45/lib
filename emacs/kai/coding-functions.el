
;; ################
;; ##    Make    ##
;; ################

(if (string-match "datapower" (pwd)) ;; if working on DP project, set cscope init dir
    (setq compile-command "jmake -k ")
  (setq compile-command "make -k "))


;; ###############
;; ##    GDB    ##
;; ###############

;(setq gdb-many-windows t)
;(setq gdb-use-separate-io-buffer t)

;; M-x gud-gdb ;; faster start-up
;; M-x gdb     ;; including gdb-many-window
;;             ;; emacs 24 default to gdb/MI (finer thread control & non-stop debugging)

; cycle backward/forward through command history
;; (add-hook 'gud-mode-hook '(lambda ()
;;                             (local-set-key [up] '(lambda () (interactive) (if (comint-after-pmark-p) (comint-previous-input 1) (previous-line 1))))
;;                             (local-set-key [down] '(lambda () (interactive) (if (comint-after-pmark-p) (comint-next-input 1) (forward-line 1))))))

(defun my-gdb-wrapper (&optional choice)
  "interactively run gud-gdb or gdb"
  ;;(interactive  "nRun GDB: [1] gdb [2] gud-gdb ") ;; must enter a number
  (interactive (list (read-number "Run GDB: [1] gud-gdb [2] gdb " 1))) ;; set default to 1
  (if (= choice 1)
      (call-interactively 'gud-gdb)
    (call-interactively 'gdb)))

  ;; (if (file-exists-p ".gdbinit")
  ;;     (find-file ".gdbinit"))

(defun my-gdb-many-windows-general ()
  (interactive)
  (defadvice gdb-setup-windows (around setup-more-gdb-windows activate)
    "customized gdb layout" ;; for gdb-many-windows
    (split-window-horizontally)
    (other-window 1)
    ;;(shrink-window-horizontally (/ (window-width) 3))
    ;;(enlarge-window-horizontally (/ (window-width) 3))
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-breakpoints-buffer))

    (split-window-vertically)
    (shrink-window ( / (window-height) 2))
    (other-window 1)
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-stack-buffer))

    (split-window-vertically)
    (other-window 1)
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-threads-buffer))

    (split-window-vertically)
    (other-window 1)
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-locals-buffer))

    ;;(other-window 1)  ;; move to gud window
    (other-window -3)   ;; move to stack window
    (gdb-frames-select) ;; display source file
    )
  (call-interactively 'gdb-many-windows)
)

(defun my-gdb-many-windows-assembly ()
  (interactive)
  (defadvice gdb-setup-windows (around setup-more-gdb-windows activate)
    "customized gdb layout" ;; for gdb-many-windows
    (split-window-horizontally)
    (other-window 1)
  
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-assembler-buffer))
    
    (other-window 1)    ;; move to gud window
    (gdb-frames-select) ;; display source file
    )
  (call-interactively 'gdb-many-windows)
)


;; ##################
;; ##    CSCOPE    ##
;; ##################

(defun enable-cscope ()
  (interactive)
  (load-file "~/lib/emacs/cscope/contrib/xcscope/xcscope.el")
  (require 'xcscope)
  (setq cscope-index-recursively t)
  
  ;; (when (string-match "datapower" (pwd)) ;; if working on DP project, set cscope init dir
  ;;   (setq cscope-initial-directory (concat (getenv "WORKDIR") "/datapower/")))

  ;; (when (string-match "linux" (pwd)) ;; if working on DP project, set cscope init dir
  ;;   (setq cscope-initial-directory (concat (getenv "HOME") "/linux/")))

  (if (string-match "datapower" (pwd)) ;; if working on DP project, set cscope init dir
      (setq cscope-initial-directory (concat (getenv "WORKDIR") "/datapower/"))
    (if (string-match "program" (pwd)) 
        (setq cscope-initial-directory (concat (getenv "HOME") "/program/"))
      (if (string-match "linux" (pwd)) 
          (setq cscope-initial-directory (concat (getenv "HOME") "/linux/")))
      )
    nil)

  ;; global keymap
  (local-set-key [f3]        'cscope-display-buffer)
  (local-set-key [(meta s)]  'cscope-find-this-symbol)                      ;; s: symbol
  (local-set-key [(meta d)]  'cscope-find-global-definition)                ;; d: definition
  (local-set-key [(meta c)]  'cscope-find-functions-calling-this-function)  ;; c: call
  (local-set-key [(meta p)]  'cscope-pop-mark)                              ;; u: up-stack

  ;; cscope keymap
  (define-key cscope-list-entry-keymap "q"        'quit-current-window)
  (define-key cscope-list-entry-keymap [f3]       'quit-current-window)
  (define-key cscope-list-entry-keymap [M-up]     'cscope-history-backward)
  (define-key cscope-list-entry-keymap [M-down]   'cscope-history-forward)
  (define-key cscope-list-entry-keymap [C-up]     'cscope-prev-symbol)
  (define-key cscope-list-entry-keymap [C-down]   'cscope-next-symbol)
  (define-key cscope-list-entry-keymap [(meta d)] 'cscope-find-global-definition)
  (define-key cscope-list-entry-keymap [(meta s)] 'cscope-find-this-symbol)
  (define-key cscope-list-entry-keymap [(meta p)] 'cscope-pop-mark)
)


;; ###################
;; ##    EBROWSE    ##
;; ###################

(defun enable-ebrowse ()
  (interactive)
  (require 'kai-ebrowse) ;; use my own customized ebrowse.el

  (global-set-key "\M-,"  'ebrowse-tags-display-member-buffer)

  ;; member-mode map
  (define-key ebrowse-member-mode-map "l"   'ebrowse-toggle-long-short-display)
  (define-key ebrowse-member-mode-map "m"    'ebrowse-display-variables-member-list)
  (define-key ebrowse-member-mode-map "\S-m" 'ebrowse-display-static-variables-member-list)
  (define-key ebrowse-member-mode-map "f"    'ebrowse-display-function-member-list)
  (define-key ebrowse-member-mode-map "\S-f" 'ebrowse-display-static-functions-member-list)
  (define-key ebrowse-member-mode-map "t"    'ebrowse-display-types-member-list)
  (define-key ebrowse-member-mode-map "u"    'ebrowse-toggle-public-member-filter)
  (define-key ebrowse-member-mode-map "o"    'ebrowse-toggle-protected-member-filter)
  (define-key ebrowse-member-mode-map "i"    'ebrowse-toggle-private-member-filter)
  (define-key ebrowse-member-mode-map "r"    'ebrowse-remove-all-member-filters)

  ;; tree-mode map
  (define-key ebrowse-tree-mode-map   "l"    'ebrowse-toggle-file-name-display)
  (define-key ebrowse-tree-mode-map   "m"    'ebrowse-tree-command:show-member-variables)                  
  (define-key ebrowse-tree-mode-map   "\S-m" 'ebrowse-tree-command:show-static-member-variables)         
  (define-key ebrowse-tree-mode-map   "f"    'ebrowse-tree-command:show-member-functions)
  (define-key ebrowse-tree-mode-map   "\S-f" 'ebrowse-tree-command:show-static-member-functions)                    
  (define-key ebrowse-tree-mode-map   "t"    'ebrowse-tree-command:show-types)         
)

;; ebrowse-tags-find-definition
;; ebrowse-tags-find-declaration
;; ebrowse-back-in-position-stack
;; ebrowse-forward-in-position-stack


;; #################
;; ##    CEDET    ##
;; #################

;; See cedet/common/cedet.info for configuration details.

(defun enable-cedet ()
  (interactive "P")
  (when (and (not (featurep 'cedet))
             (file-exists-p (buffer-name)))
    (load-file "~/lib/emacs/cedet-1.1/common/cedet.el") ;; IMPORTANT: must place this *before* any CEDET component
    (global-ede-mode t)
    (enable-semantic)
    ))

;; IMPORTANT: this is used to tell semantic which paths to use for db file searching
;; (defun check-ede-project-root ()
;;   ;; if working on DP project, set this (used by semanticdb)
;;   (when (string-match "\\/datapower\\/" (pwd))
;;     (message "Invoking project-DataPower-hook...")
;;     (ede-cpp-root-project "DataPower"
;;                           ;:file (concat (getenv "WORKDIR") "/datapower/Makefile.common")
;;                           :file (concat (substring (pwd) 10 (match-end 0)) "Makefile.common")
;;                           ;:system-include-path '( "/usr/include/" )
;;                           :include-path '( "/router" "/util" "/db-process" "/mgmtstore" "/sidecar" "/soliddb-process" "/eventlog")
;;                           ; :spp-table '( ("__USE_POSIX" . "1") ) ;; not work, why?
;;                           )))
;; check http://www.gnu.org/software/emacs/manual/html_node/ede/ede_002dcpp_002droot.html
(defun check-ede-project-root ()
  ;; if working on DP project, set this (used by semanticdb)
  (if (string-match "\\/datapower\\/" (pwd))
      (progn
        (message "Invoking project-DataPower-hook...")
        (ede-cpp-root-project "DataPower"
                              ;:file (concat (getenv "WORKDIR") "/datapower/Makefile.common")
                              :file (concat (substring (pwd) 10 (match-end 0)) "Makefile.common") ;; just to specify root dir
                              ;:system-include-path '( "/usr/include/" )
                              :include-path '( "/router" "/util" "/db-process" "/mgmtstore" "/sidecar" "/soliddb-process" "/eventlog")
                              ; :spp-table '( ("__USE_POSIX" . "1") ) ;; not work, why?
                              ))
    (if (string-match "\\/program\\/" (pwd))
        (progn
          (message "Invoking project-DataPower-hook...")
          (ede-cpp-root-project "program"
                              :file (concat (getenv "HOME") "/program/Makefile") ;; just to specify root dir
                              ;:system-include-path '( "/usr/include/" )
                              :include-path '( "/interview" "/lib" "/network" "/LPI")
                              ; :spp-table '( ("__USE_POSIX" . "1") ) ;; not work, why?
                              )))
    ))


(defun enable-semantic ()

  ;; [semanticdb]
  ;;
  ;; http://www.gnu.org/software/emacs/manual/html_mono/semantic.html
  ;;
  ;; db files are stored under ~/.semanticdb/ (default)
  ;;
  ;; the parsed result is cached in memory and will be flushed to disk under the following conditions:
  ;;     1. when exit emacs
  ;;     2. after modifying source code
  ;;     3. idle for a long time (default: 60 sec)
  ;; => case 2 and 3 will cause emacs hang for a while if cached data is large

  ;; case 2 can be avoided by:
  ;;     predefine "semanticdb-hooks" without including (semanticdb-save-all-db-idle auto-save-hook)
  ;;     this is orignally defined in "semanticdb-mode.el"
  (defvar semanticdb-hooks
    '((semanticdb-semantic-init-hook-fcn semantic-init-db-hook)
      (semanticdb-synchronize-table semantic-after-toplevel-cache-change-hook)
      (semanticdb-partial-synchronize-table semantic-after-partial-cache-change-hook)
      (semanticdb-revert-hook before-revert-hook)
      (semanticdb-kill-hook kill-buffer-hook)
      (semanticdb-kill-hook change-major-mode-hook)
      (semanticdb-kill-emacs-hook kill-emacs-hook)
      ;(semanticdb-save-all-db-idle auto-save-hook)
      )
    "List of hooks and values to add/remove when configuring semanticdb.")

  ;; MISSING TAG/SYMBOL?
  ;; => check "2.4.4 Debugging the Semantic Analyzer"
  ;; semantic-spp-lex-describe           ;; check defined macro
  ;; semanticdb-find-test-translate-path ;; list xxx.h
  ;; semantic-lex-c-preprocessor-symbol-map or semantic-lex-c-preprocessor-symbol-file

  ;; for missing symbol like sigxxx defined in signal.h
  ;; this should be set before semantic is loaded
  (semantic-c-add-preprocessor-symbol "__USE_POSIX" "1")

  ;; Select one of the following canned features:
  (semantic-load-enable-minimum-features)   ;; * enables database and idle reparse engines
 ;(semantic-load-enable-code-helpers)       ;; * enables summary mode, imenu support, and the semantic navigator
 ;(semantic-load-enable-gaudy-code-helpers) ;; enables intellisense mode, decoration mode, stickyfunc mode and regular code helpers
 ;(semantic-load-enable-all-exuberent-ctags-support) ;; enables Exuberant ctags (BAD for C++ templates or boost)

  ;; case 3 can be avoided by:
  ;;     turn off "global-semantic-idle-scheduler-mode" or
  ;;     just extend "semantic-idle-scheduler-work-idle-time" if you do not want to disable auto-idle-parse

  ;; (global-semantic-idle-scheduler-mode nil)
  ;; (setq semantic-idle-scheduler-verbose-flag t)
  (setq semantic-idle-scheduler-idle-time 20)       ;; default: 2 sec (invoke reparse)
  (setq semantic-idle-scheduler-work-idle-time 300) ;; default: 60 sec (invoke auto save)
  (setq semantic-idle-work-parse-neighboring-files-flag nil) ;; don't parse all files under PWD when idle

  ;; (global-set-key "\C-e" 'semanticdb-find-adebug-lost-includes) ;; for debug purpose: check unfound .h files
  ;; (setq-mode-local c++-mode semanticdb-find-default-throttle '(project unloaded recursive)) ;; Remove 'system from the throttle
  ;; (setq-mode-local c-mode semanticdb-find-default-throttle   '(project unloaded recursive))   ;; Remove 'system from the throttle
  ;; (setq-mode-local c++-mode semanticdb-find-default-throttle '(project unloaded recursive system)) ;; Remove 'system from the throttle
  ;; (setq-mode-local c-mode semanticdb-find-default-throttle   '(project unloaded recursive system))   ;; Remove 'system from the throttle

  ;; show symbol/tag summary
  (defun my-semantic-ia-show-summary (point) ;; redefined
    "Display a summary for the symbol under POINT; if failed, call 'semantic-analyze-current-tag'"
    (interactive "P")
    (let* ((ctxt (semantic-analyze-current-context point))
           (pf (when ctxt (semantic-analyze-interesting-tag ctxt))))
      (if pf
          (message "%s" (semantic-format-tag-summarize pf nil t))
        (semantic-analyze-current-tag))))

  (global-set-key "\C-\\" 'my-semantic-ia-show-summary)
  (add-hook 'data-debug-hook' (lambda()
                                (global-set-key (kbd "q") 'quit-current-window)))

  ;; mru-bookmark
  ;; (global-semantic-mru-bookmark-mode 1)
  ;; (global-set-key [C-f12] 'semantic-mrub-switch-tags)

  (global-set-key (kbd "C-j") 'semantic-ia-fast-jump)

  (require 'eieio-opt)
  (global-set-key [(meta .)]  'semantic-complete-analyze-inline) ;; require eieio-opt

  ;; electrical assist
  (require 'eassist)
  (global-set-key [(control meta f)] 'eassist-list-methods)
  (global-set-key [(control meta p)] 'eassist-switch-h-cpp)
  ; (define-key eassist-mode-map  "q"  'kill-this-buffer)
)

(provide 'coding-functions)
