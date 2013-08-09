
;; ########################
;; ##    KEY BINDINGS    ##
;; ########################

;(global-set-key [(control r)]  'query-replace)  ;; C-M-%

;; ;; search forward with Ctrl-f
;; (global-set-key [(control f)]  'isearch-forward)
;; (define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-s"))
;; (define-key minibuffer-local-isearch-map [(control f)]
;;   (lookup-key minibuffer-local-isearch-map "\C-s"))

;; ;; search backward with Alt-f
;; (global-set-key [(meta f)] 'isearch-backward)
;; (define-key isearch-mode-map [(meta f)] (lookup-key isearch-mode-map "\C-r"))
;; (define-key minibuffer-local-isearch-map [(meta f)]
;;   (lookup-key minibuffer-local-isearch-map "\C-r"))

; (global-set-key "\C-x\ \C-s" 'shell) ;; enter shell mode

;; (global-set-key "\C-o"  'find-file)    ;; open file            ; C-x C-f
;; (global-set-key "\C-s"  'save-buffer)  ;; save with Ctrl-s     ; C-x C-s 
;;                                        ;; save as (write-file) ; C-x C-w

;(global-set-key "\C-w"  'kill-this-buffer)


;; ################
;; ##    GTAG    ##
;; ################

;; (add-to-list 'load-path "/home/kai/lib/emacs/global-5.9.2")
;; (require 'gtags)

;; (setq load-path (cons "/home/kai/p4/b2b-mqfte/datapower" load-path))
;; (autoload 'gtags-mode "gtags" "" t)

;; (setq c++-mode-hook
;;       '(lambda ()
;;          (gtags-mode 1)))

;; (setq gtags-default-mode 'c++-mode)

;; (defun ww-next-gtag ()
;;   "Find next matching tag, for GTAGS."
;;   (interactive)
;;   (let ((latest-gtags-buffer
;;          (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
;;                                  (buffer-list)) ))))
;;     (cond (latest-gtags-buffer
;;            (switch-to-buffer latest-gtags-buffer)
;;            (next-line)
;;            (gtags-select-it nil))
;;           ) ))

;; (global-set-key "\M-;" 'ww-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
;; (global-set-key "\M-." 'gtags-find-tag) ;; M-. finds tag
;; (global-set-key [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
;; (global-set-key [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.



;; #########################
;; ##    AUTO COMPLETE    ##
;; #########################

;(setq tags-directory "$WORKDIR/datapower/")

;; (add-to-list 'load-path "~/lib/emacs/auto-complete-1.3/")
;; (require 'auto-complete)
;; (require 'auto-complete-config)

;; (add-to-list 'ac-dictionary-directories "~/lib/emacs/auto-complete-1.3/ac-dict")

;; (require 'auto-complete-etags)
;; (require 'auto-complete-extension)


;; (global-auto-complete-mode t)
;; (setq ac-auto-start nil)
;; (setq ac-candidate-max 20)

;; (defun auto-complete-wrapper ()
;; "before invoking auto-complte, enable auto-complete-mode if disabled"
;; (interactive)
;; (if auto-complete-mode (auto-complete)
;;   (auto-complete-mode)
;;   (auto-complete)))

;; (global-set-key "\M-q" 'auto-complete-wrapper)
;; (define-key ac-complete-mode-map "\C-n" 'ac-next)
;; (define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (make-local-variable 'ac-sources)
;;             (setq ac-sources '(ac-source-words-in-buffer ac-source-symbols))))

;; ;; using etags for auto complete
;; (defvar ac-source-etags
;;   '((candidates . (lambda () (all-completions ac-target (tags-completion-table))))))

;; (defun add-ac-source-etags ()
;;   (make-local-variable 'ac-sources)
;;   (if (string-match "\\/datapower\\/" (pwd))
;;       (progn 
;;         (unless (file-exists-p (concat (getenv "WORKDIR") "/datapower/TAGS")) (error "No TAGS file found"))
;;         (visit-tags-table (concat (getenv "WORKDIR") "/datapower/TAGS")))
;;     (unless (file-exists-p "./TAGS") (error "No TAGS file found"))
;;     (visit-tags-table "./TAGS"))
;;   (add-to-list 'ac-sources 'ac-source-etags))

;; (add-hook 'c++-mode-hook 'add-ac-source-etags)


;; #################
;; ##    ETAGS    ##
;; #################

(defun build-index-etags ()
  "run my customized script to generate TAGS under either current dir or $WROKDIR/datapower"
  (interactive)
  (shell-command "build-index-etags"))


;; ###############
;; ##    ECB    ##
;; ###############

;; ecb
(global-set-key [C-f1] 'ecb-goto-window-methods)
(global-set-key [C-f2] 'ecb-goto-window-edit1)

(global-set-key [C-f3] '(lambda ()
                          (interactive)
                          (unless (featurep 'ecb) (my-ecb-activate) (ecb-toggle-ecb-windows))
                          (ecb-toggle-ecb-windows)))

;;(define-key ecb-create-layout-mode-map "q" 'ecb-toggle-ecb-windows)

;; to deprecate this
(defun my-ecb-activate ()
"if not running ecb, load it!!"
(interactive)
(unless (featurep 'cedet) (error "CEDET not loaded !!"))
(if (fboundp 'ecb-activate) ;; function defined
    (ecb-activate)
  (add-to-list 'load-path "~/lib/emacs/ecb-2.40")
  (require 'ecb) 
  (custom-set-variables '(ecb-options-version "2.40"))
  (setq ecb-layout-name "left9")
  (ecb-activate)))



;; #################
;; ##    CEDET    ##
;; #################

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
(load-file "~/lib/emacs/cedet-1.1/common/cedet.el")

;; (defun load-cedet ()
;;   "load cedet on necessary"
;;   (interactive)
;;   (when (and (file-exists-p (buffer-name))
;;              (not (featurep 'cedet)))
;;     (load-file "~/lib/emacs/cedet-1.1/common/cedet.el")))


;; #####################
;; ##    CEDET-EDE    ##
;; #####################

;; Enable EDE (Project Management) features
(global-ede-mode t)

(defun find-project-hook ()
  (if (string-match "\\/datapower\\/" (pwd))
      (progn
        (ede-cpp-root-project "DataPower"
          :file (concat (getenv "WORKDIR") "/datapower/Makefile.common")
          :include-path '( "/router" "/util" "/db-process" "/mgmtstore" "/sidecar" "/soliddb-process" "/eventlog")
          ;:system-include-path '( "/usr/include/" )
          )
        (message "Invoking project-DataPower-hook...")))
  ;; (progn
  ;;   (ede-cpp-root-project "zzz"
  ;;     :file "~/test/Makefile")
  ;;     :include-path '( "/src")
  ;;     :system-include-path '( "/usr/include" "/usr/include/sys")
  ;;     :spp-table '( ("MOOSE" . "") ("__USE_POSIX" . "")) 
  ;;     (message "Invoking project-test-hook..."))
  ) ; no ELSE statement

(add-hook 'c++-mode-hook 'find-project-hook)


;; ##########################
;; ##    CEDET-SEMANTIC    ##
;; ##########################

;; http://www.gnu.org/software/emacs/manual/html_mono/semantic.html

;; [semanticdb]
;;
;; db files are stored under ~/.semanticdb/
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


;; Select one of the following canned features:
(semantic-load-enable-minimum-features) ;; * enables database and idle reparse engines
;(semantic-load-enable-code-helpers) ;; * enables summary mode, imenu support, and the semantic navigator
;(semantic-load-enable-gaudy-code-helpers) ;; enables intellisense mode, decoration mode, stickyfunc mode and regular code helpers
;(semantic-load-enable-all-exuberent-ctags-support) ;; enables Exuberant ctags (BAD for C++ templates or boost)


;; 3 can be avoided by:
;;     turn off "global-semantic-idle-scheduler-mode" or
;;     just extend "semantic-idle-scheduler-work-idle-time" if you do not want to disable auto-idle-parse

;(global-semantic-idle-scheduler-mode nil)
;(setq semantic-idle-scheduler-verbose-flag t)
(setq semantic-idle-scheduler-idle-time 3)        ;; default: 2 se c (invoke reparse)
(setq semantic-idle-scheduler-work-idle-time 300) ;; default: 60 sec (invoke auto save)
(setq semantic-idle-work-parse-neighboring-files-flag nil) ;; don't parse all files under PWD when idle


;; (global-set-key "\C-e" 'semanticdb-find-adebug-lost-includes) ;; for debug purpose: check unfound .h files
;; (setq-mode-local c++-mode semanticdb-find-default-throttle '(project unloaded recursive)) ;; Remove 'system from the throttle
;; (setq-mode-local c-mode semanticdb-find-default-throttle   '(project unloaded recursive))   ;; Remove 'system from the throttle
;; (setq-mode-local c++-mode semanticdb-find-default-throttle '(project unloaded recursive system)) ;; Remove 'system from the throttle
;; (setq-mode-local c-mode semanticdb-find-default-throttle   '(project unloaded recursive system))   ;; Remove 'system from the throttle

;; MISSING TAG/SYMBOL?
;; => check "2.4.4 Debugging the Semantic Analyzer"
;; semantic-spp-lex-describe           ;; check defined macro
;; semanticdb-find-test-translate-path ;; list xxx.h
;; semantic-lex-c-preprocessor-symbol-map or semantic-lex-c-preprocessor-symbol-file
(semantic-c-add-preprocessor-symbol "__USE_POSIX" "1") ;; for those missing symbol (e.g. sigxxx)

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
            (local-set-key (kbd "q") 'quit-current-window)))

;; mru-bookmark
(global-semantic-mru-bookmark-mode 1)
(global-set-key [C-f12] 'semantic-mrub-switch-tags)

;; electrical assist
(global-set-key [(control meta f)] 'eassist-list-methods)
(global-set-key [(control meta s)] 'eassist-switch-h-cpp)
(add-hook 'eassist-mode-hook '(lambda()
            (local-set-key (kbd "q") 'kill-this-buffer)))

(global-set-key (kbd "C-j")   'semantic-ia-fast-jump)

(require 'eieio-opt)
(global-set-key [(meta .)]  'semantic-complete-analyze-inline) ;; require eieio-opt

