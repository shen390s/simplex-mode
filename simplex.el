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
  '("appendix" "bfseries" "bold" "caption" "center" "colbreak" "columns"
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
  '("label" "#include" "#image")
  "Other simplex keywords")

(defvar simplex-extra-blocks
  '(".digraph" ".graph" ".table" ".ascii"
    ".code" ".code$" ".comment" ".equation"
    ".haskell" ".java" ".latex" ".math"
    ".php" ".python" ".verbatim" ".#" ".@"
    ".%" ".$" ".!")
  "Other blocks")

(defvar simplex-other-special
  '(":" "="  "-"   "*" "+"  "!" ">" "%" "#")
  "Other special chars for simplex")

(defun mk-simplex-match-regexp (keys)
  (concat "^" (regexp-opt keys t)))

(defconst simplex-font-lock-keywords-1
  `(("^\\(@[a-zA-Z][a-zA-Z]*\\)" . font-lock-keyword-face)
     ;;(,(mk-simplex-match-regexp simplex-decls) . font-lock-keyword-face)
    (,(mk-simplex-match-regexp simplex-length) . font-lock-variable-name-face)
    (,(mk-simplex-match-regexp simplex-commands) . font-lock-function-name-face)
    (,(mk-simplex-match-regexp simplex-other-keywords) . font-lock-type-face)
    (,(mk-simplex-match-regexp simplex-extra-blocks) . font-lock-preprocessor-face)
    
    ("%\\(.*$\\)" . font-lock-comment-face)
;;    ("{\\(.*\\)}" . font-lock-string-face)
;;    (":\\([^%\012]*\\)[^%\012]*$" . font-lock-warning-face)
    )
  "keyword in simplex mode")

(defun is-simplex-special ()
  (let ((simplex-special (append simplex-decls simplex-length
				 simplex-commands simplex-other-keywords
				 simplex-extra-blocks
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
(defvar simplex-debug-enabled t
  "enable/disable debug")

(defmacro simplex-debug (fmt &rest args)
  `(when simplex-debug-enabled
     (message ,fmt ,@args)))

(defvar last-indent-line nil
  "The last line to be indented")
(defvar last-line-indent nil
  "The offset of indent of last line")

(defun simplex-last-indent-valid (cur-line)
  (let ((indent-valid 
	 (cond
	  ((or (not last-indent-line)
	       (not last-line-indent)) nil)
	  ((< cur-line last-indent-line) nil)
	  (t (let ((valid 'unknown))
	       (save-excursion
		 (goto-line cur-line)
		 (while (and (eq valid 'unknown)
			     (> (line-number-at-pos) (+ last-indent-line 1)))
		   (forward-line -1)
		   (beginning-of-line)
		   (cond
		    ((looking-at "^[\b\t ]*$")
		     (simplex-debug "line %d is empty"
				    (line-number-at-pos))
		     t)
		    (t (setf valid nil))))
		 (when (eq valid 'unknown)
		   (setf valid t))
		 valid))))))
    (simplex-debug "cur-line %d last-indent-line %s last-line-indent %s valid %s"
		   cur-line last-indent-line last-line-indent indent-valid)
    indent-valid))

(defun simplex-calc-indent ()
  (let ((cur-indent nil)
	(cur-line (line-number-at-pos)))
    (if (not (simplex-last-indent-valid cur-line))
      (save-excursion
	(while (and (not cur-indent)
		    (not (bobp)))
	  (forward-line -1)
	  (beginning-of-line)
	  (cond
	   ((looking-at "^\\.[\t\b ]+")
	    (setf cur-indent simplex-tab-width))
	   ((looking-at "^@title")
	    (setf cur-indent (* 2 simplex-tab-width)))
	   ((looking-at (mk-simplex-match-regexp simplex-decls))
	    (setf cur-indent simplex-tab-width))
	   ((looking-at "^center[\t\b ]*")
	    (setf cur-indent (* 3 simplex-tab-width)))
	   ((looking-at "^\\.#")
	    (setf cur-indent -1))
	   ((looking-at "^\\.!")
	    (setf cur-indent -1))
	   ((looking-at (mk-simplex-match-regexp simplex-commands))
	    (setf cur-indent simplex-tab-width))
	   ((looking-at (mk-simplex-match-regexp simplex-extra-blocks))
	    (setf cur-indent -1))
	   ((looking-at (mk-simplex-match-regexp '("=" ".")))
	    (setf cur-indent simplex-tab-width))
	   ((looking-at (mk-simplex-match-regexp simplex-other-special))
	    (setf cur-indent -1))
	   (t t))
	  (when cur-indent
	    (simplex-debug "calc indent according line %d indent %d"
			   (line-number-at-pos) cur-indent)))
	(unless cur-indent
	  (setf cur-indent 0)))
      (setf cur-indent last-line-indent))
    (setf last-indent-line cur-line)
    (setf last-line-indent cur-indent)
    (simplex-debug "last-indent-line %d last-line-indent %d cur-line %d"
		   last-indent-line last-line-indent cur-line)
    last-line-indent))

(defun simplex-indent-line ()
  "Indent current line in simplex mode"
  (if (bobp)
      (indent-line-to 0)
    (let ((cur-indent -1))
      (cond
       ((looking-at "^[\b\t ]*$")
	(simplex-debug "line %d is empty"
		       (line-number-at-pos))
	(setf cur-indent -1))
       ;;((is-simplex-special) (setf cur-indent 0))
       ((looking-at (concat "^" (regexp-opt (append simplex-decls simplex-length
						    simplex-commands simplex-other-keywords
						    simplex-extra-blocks))))
	(simplex-debug "line %d is declare or keywords"
		       (line-number-at-pos))
	(setf cur-indent 0))
       ((looking-at (concat "^" (regexp-opt simplex-other-special)))
	(simplex-debug "line %d is special"
		       (line-number-at-pos))
	(setf cur-indent 0))
       (t
	(simplex-debug "calculating line indent for line %d"
		       (line-number-at-pos))
	(setf cur-indent (simplex-calc-indent))))
      (simplex-debug "line %s indent %d"
		     (line-number-at-pos) cur-indent)
      (when (>= cur-indent 0)
	(indent-line-to cur-indent)))))

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
