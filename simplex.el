;; simplex mode
(require 'compile)

(defvar simplex-mode-hook nil
  "initial hook for simplex mode"
  )

(defun simplex-compilation-mode-hook ()
  "Hook function to set local value for `compilation-error-screen-columns'."
  ;; In Emacs > 20.7 compilation-error-screen-columns is buffer local.
  (or (assq 'compilation-error-screen-columns (buffer-local-variables))
      (make-local-variable 'compilation-error-screen-columns))
  (setq compilation-error-screen-columns nil))

(defvar simplex-output-format 'pdf
  "The format of generated file")

(defvar simplex-verbose nil
  "Show verbose information when run compiler")

(defvar simplex-compiler nil
  "The compiler used to generate output")

(defvar simplex-extra-options ""
  "Extra compiler options for simplex")

(defvar simplex-mode-map
  (let ((map (make-sparse-keymap)))
    ;; add key bindings for simplex mode here
    ;;
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\C-c\C-c" 'simplex-compile)
    (define-key map "\C-c\C-v" 'simplex-view)
    map)
  "Keymap for simplex mode")

(defconst simplex-font-lock-keywords-1
  '(("^\\(@title|@preamble|@abstract|@address|@authors|@book\\)" . font-lock-keyword-face)
    ("^[ \t]*graph[ \t]+\\(TD|\\TB\\|BT\\RL\\|LR\\)" . font-lock-keyword-face)
    ("--\\(.*$\\)" . font-lock-comment-face)
    ("{\\(.*\\)}" . font-lock-string-face)
    (":\\([^%\012]*\\)[^%\012]*$" . font-lock-warning-face)
    )
  "keyword in simplex mode"
  )

(defconst simplex-new-scope-regexp
  "^[ \t]*\\(loop\\|opt\\|subgraph\\|graph\\|sequenceDiagram\\|gantt\\|gitGraph\\|{\\)\\([ \t]*\\|$\\)"
  "keyword to start a new scope(indent level)")

(defconst simplex-end-scope-regexp
  "^[ \t]*\\(end\\|}\\)\\([ \t]*\\|$\\)"
  "keyword for end a scope(maybe also start a new scope)")

(defconst simplex-section-regexp
  "^[ \t]*\\(section\\)[ \t]+"
  "section keyword")

(defconst simplex-else-regexp
  "^[ \t]*\\(else\\)"
  "else keyword")

(defconst simplex-alt-regexp
  "^[ \t]*\\(alt\\)"
  "alt keyword")

(defun simplex-output-ext ()
  "get the extendsion of generated file"
  (if (eq simplex-output-format 'pdf)
      ".pdf"
    ".html"))

(defun mk-simplex-options ()
  (format "%s" simplex-extra-options))

(defun mk-simplex-command (file-name)
  (let ((cmd (format "simplex %s %s "  (mk-simplex-options) file-name)))
    cmd))


(defun get-simplex-compiler ()
  (unless simplex-compiler
    (setq simplex-compiler
	  (let ((simplex (executable-find "simplex")))
	    (if simplex
		simplex
	      "simplex"))))
  simplex-compiler)

(defun run-compiler (compiler)
  (let ((cmd (mk-simplex-command (buffer-file-name))))
    (let ((buffer-name "*simplex compilation")
	  (compilation-mode-hook (cons 'simplex-compilation-mode-hook
				       compilation-mode-hook)))
      (compilation-start cmd nil
			 #'(lambda (mode-name)
			     buffer-name)))))

;;;###autoload
(defun simplex-compile ()
  (interactive)
  (run-compiler 'simplex))

;;;###autoload
(defun simplex-view ()
  (interactive)
  (let ((dst-file-name (concat (file-name-sans-extension (buffer-file-name))
                               (simplex-output-ext))))
    (if (file-exists-p dst-file-name)
        (find-file-other-window dst-file-name)
      (error "Please compile the it first!\n"))))

;; disable debug in default
;;
(defvar simplex-debug-enabled nil
  "enable/disable debug")

(defmacro simplex-debug (fmt &rest args)
  `(when simplex-debug-enabled
     (message ,fmt ,@args)))

(defun simplex-indent-line ()
  "indent current line in simplex mode"
  (interactive)
  (simplex-debug "line no @ %d\n" (line-number-at-pos))
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let (cur-indent)
      (cond
       ((looking-at simplex-end-scope-regexp)
        (progn
          (simplex-debug "found end scope\n")
          (save-excursion
            (forward-line -1)
            (if (or (looking-at simplex-new-scope-regexp)
                    (looking-at simplex-alt-regexp))
                (setq cur-indent (current-indentation))
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))))
       ((looking-at simplex-section-regexp)
        (let ((found-section nil)
              (need-search t))
          (simplex-debug "found section\n")
          (save-excursion
            (while need-search
              (forward-line -1)
              (cond
               ((looking-at simplex-section-regexp)
                (progn
                  (simplex-debug "found section\n")
                  (setq found-section t)
                  (setq cur-indent (current-indentation))
                  (simplex-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
               ((or (looking-at simplex-new-scope-regexp)
                    (looking-at simplex-alt-regexp))
                (progn
                  (simplex-debug "found new scope\n")
                  (setq cur-indent (+ (current-indentation) tab-width))
                  (simplex-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
               ((looking-at simplex-end-scope-regexp)
                (progn
                  (simplex-debug "found end scope\n")
                  (setq cur-indent (current-indentation))
                  (simplex-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
               ((bobp)
                (progn
                  (setq cur-indent 0)
                  (setq need-search nil)))
               (t t))))
          (if (< cur-indent 0)
              (setq cur-indent 0))))
       ((looking-at simplex-else-regexp)
        (let ((need-search t))
          (simplex-debug "else\n")
          (save-excursion
            (while need-search
              (forward-line -1)
              (cond
               ((or (looking-at simplex-else-regexp)
                    (looking-at simplex-alt-regexp))
                (progn
                  (simplex-debug "found matched alt/else\n")
                  (setq cur-indent (current-indentation))
                  (simplex-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
               ((looking-at simplex-end-scope-regexp)
                (progn
                  (simplex-debug "found end\n")
                  (setq cur-indent (- (current-indentation) tab-width))
                  (setq need-search nil)))
               ((bobp)
                (progn
                  (setq cur-indent 0)))
               (t t))))))
       (t
        (let ((need-search t)
              (start-scope (looking-at simplex-new-scope-regexp)))
          (simplex-debug "normal indent\n")
          (save-excursion
            (while need-search
              (forward-line -1)
              (cond
               ((looking-at simplex-end-scope-regexp)
                (progn
                  (simplex-debug "found end scope\n")
                  (setq cur-indent (current-indentation))
                  (simplex-debug "cur-indent %d\n" cur-indent)
                  (setq need-search nil)))
                ((or (looking-at simplex-new-scope-regexp)
                     (looking-at simplex-alt-regexp))
                 (progn
                   (simplex-debug "found begin scope\n")
                   (setq cur-indent (+ (current-indentation) tab-width))
                   (simplex-debug "cur-indent %d\n" cur-indent)
                   (setq need-search nil)))
                ((looking-at simplex-section-regexp)
                 (progn
                   (simplex-debug "found section \n")
                   (if start-scope
                       (setq cur-indent (current-indentation))
                     (setq cur-indent (+ (current-indentation) tab-width)))
                   (simplex-debug "cur-indent %d\n" cur-indent)
                   (setq need-search nil)))
                ((bobp)
                 (progn
                   (setq cur-indent 0)
                   (setq need-search nil)))))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;;;###autoload
(defun simplex-mode ()
  "Major mode for editing simplex scripts"
  (interactive)
  (kill-all-local-variables)
  (use-local-map simplex-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(simplex-font-lock-keywords-1))
  (set (make-local-variable 'indent-line-function)
       'simplex-indent-line)
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (setq major-mode 'simplex-mode)
  (setq mode-name "simplex")
  (run-hooks 'simplex-mode-hook))

(provide 'simplex-mode)
