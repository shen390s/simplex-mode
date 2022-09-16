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

(defvar simplex-extra-options " --cjk "
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

(defvar simplex-tab-width 4
  "tab width in simplex mode")

(defvar simplex-decls
  '("@abstract" "@address" "@article" "@authors"
    "@book" "@cfoot" "@chead" "@closing" "@date"
    "@doublespacing" "@draft" "@endnotes" "@fontsize"
    "@landscape" "@language" "@lfoot" "@lhead" "@margin-bottom"
    "@margin-left" "@margin-right" "@margin-top" "@margins"
    "@newpagesections" "@opening" "@pagestyle" "@preamble"
    "@recipient" "@report" "@rfoot" "@rhead" "@scrartcl" "@slides"
    "@signature" "@title" "@tocdepth" "@xeCJK")
  "simplex mode declaration keywords")

(defvar simplex-length
  '("baselineskip" "baselinestretch" "columnsep" "columnseprule" "columnwidth"
    "evensidemargin" "headheight" "oddsidemargin" "paperheight" "paperwidth"
    "parindent" "parskip" "tabcolsep" "textfloatsep" "textheight" "textwidth"
    "topmargin")
  "simplex page control length variable")

(defvar simplex-commands
  '("appendix" "bfseries" "bold" "cation" "center" "colbreak" "columns"
    "em" "endcolumns" "endfigure" "endignore" "endnoinclude" "endnotes"
    "figure" "figures" "float-barrier" "footnotesize" "hfill" "huge"
    "Huge" "ignore" "image-angle" "image-defaults" "image-height" "image-page"
    "image-scale" "image-size" "image-trim" "image-width" "image" "italic"
    "itshape" "large" "LARGE" "Large" "left" "lipsum" "mdseries" "newpage"
    "noinclude" "noindent" "normalfont" "normalsize" "pagebreak" "pagenumbering"
    "pagestyle" "reset" "right" "rmfamily" "scriptsize" "scschape" "sffamily"
    "slshape" "small" "tableofcontents" "thispagestyle" "tiny" "ttfamily"
    "upshape" "vfill")
  "simplex commands")

(defvar simplex-other-keywords
  '("label" ".digraph" ".graph" ".table" "#include")
  "Other simplex keywords")

(defvar simplex-other-special
  '(":" "=" "."  "-"   "*" "+"  "!" ">" "%" "#")
  "Other special chars for simplex")

(defun mk-simplex-match-regexp (keys)
  (concat "^" (regexp-opt keys t)))

(defconst simplex-font-lock-keywords-1
  `((,(mk-simplex-match-regexp simplex-decls) . font-lock-keyword-face)
    (,(mk-simplex-match-regexp simplex-length) . font-lock-variable-name-face)
    (,(mk-simplex-match-regexp simplex-commands) . font-lock-function-name-face)
    (,(mk-simplex-match-regexp simplex-other-keywords) . font-lock-type-face)
    ("%\\(.*$\\)" . font-lock-comment-face)
    ("{\\(.*\\)}" . font-lock-string-face)
;;    (":\\([^%\012]*\\)[^%\012]*$" . font-lock-warning-face)
    )
  "keyword in simplex mode"
  )

(defun is-simplex-special ()
  (let ((simplex-special (append simplex-decls simplex-length
				 simplex-commands simplex-other-keywords
				 simplex-other-special)))
    (let ((m-rexp (concat "^" (regexp-opt simplex-special t))))
      (looking-at m-rexp))))

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

(defun simplex-determine-ctx ()
  (save-excursion
    (setf ctx nil)
    (while (and (not ctx)
		(not (bobp)))
      (beginning-of-line)
      (let ((ctx-tag (cond
		      ((looking-at (mk-simplex-match-regexp simplex-decls))
		       (if (looking-at "@title")
			   'title
			 'other-decl))
		      ((looking-at (mk-simplex-match-regexp simplex-other-special))
		       'special-block)
		      ((looking-at "^[\t\b ]*$")
		       'paragraph)
		      ((looking-at (mk-simplex-match-regexp (append simplex-length
								    simplex-commands)))
		       'command-or-length)
		      (t 'unknown))))
	(if (not (eq ctx-tag 'unknown))
	    (setf ctx (list ctx-tag (line-number-at-pos)))
	  (forward-line -1))))
    (when (not ctx)
      (setf ctx (list 'doc-start 1)))
    ctx))
      
(defun simplex-indent-line ()
  "Indent current line in simplex mode"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (progn
      (let ((ctx (simplex-determine-ctx))
	    (cur-line (line-number-at-pos)))
	(let ((cur-indent 
	       (pcase (car ctx)
		 ('doc-start 0)
		 ('title
		  (if (= (second ctx) cur-line)
		      0
		    (* 2 simplex-tab-width)))
		 ('other-decl
		  (if (= (second ctx) cur-line)
		      0
		    simplex-tab-width))
		 ('paragraph simplex-tab-width)
		 ('command-or-length
		  (if (= (second ctx) cur-line)
		      0
		    simplex-tab-width))
		 ('special-block -1)
		 ('comment -1)
		 (t -1))))
	  (simplex-debug "line %d ctx %s indent %d"
	    		 cur-line ctx cur-indent)
	  (when (>= cur-indent 0)
	    (indent-line-to cur-indent)))))))

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
  (set (make-local-variable 'comment-start) "^%")
  (set (make-local-variable 'comment-end) "")
  (setq major-mode 'simplex-mode)
  (setq mode-name "simplex")
  (run-hooks 'simplex-mode-hook))

(provide 'simplex-mode)
