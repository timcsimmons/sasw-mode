(require 'comint)

(defcustom inferior-SASw-program "sascli"
  "Program name for invoking an inferior SAS session"
  :type 'string
  :group 'inferior-SASw)

(defcustom inferior-SASw-program-args '()
  "Arguments given when invoking an inferior SAS session"
  :type 'string
  :group 'inferior-SASw)

(defcustom inferior-SASw-prompt-regexp "SAS> "
  "Regexp to recognize prompts in the inferior SAS session"
  :type 'string
  :group 'inferior-SASw)

(defcustom inferior-SASw-prompt-output-regexp ""
  "Regexp to recognize prompts in the inferior SAS session"
  :type 'string
  :group 'inferior-SASw)

(defcustom inferior-SASw-prompt-cont-regexp "[.][.][.] "
  "Regexp to recognize continuation prompts in the inferior SAS session"
  :type 'string
  :group 'inferior-SASw)

(defvar inferior-SASw-mode-hook '()
  "Hook for customizing inferior SASw mode")

(defun inferior-SASw--initialize ()
  "Helper function to initialize SAS"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(put 'inferior-SASw-mode 'mode-class 'special)

(define-derived-mode inferior-SASw-mode comint-mode "Inferior SAS"
  "Major mode for interacting with an active SAS session. Runs
SAS as a subprocess of Emacs, with I/O through an Emacs
buffer. Variable `inferior-SASw-program' controls which program
is used to run SAS. Variable `inferior-SASw-prompt' can customize
this mode for different SAS interpreters.

TODO: How to run multiple sessions?

\\{inferior-SASw-mode-map}"
  (setq comint-prompt-regexp (format "^\\(?:%s\\|%s\\|%s\\)"
				     inferior-SASw-prompt-regexp
				     inferior-SASw-prompt-output-regexp
				     inferior-SASw-prompt-cont-regexp))
  (setq comint-buffer-maximum-size 32767))

(add-hook 'inferior-SASw-mode-hook 'inferior-SASw--initialize)

;;;###autoload
(defun inferior-SASw (cmd)
  "Run an inferior SAS process, input and output via buffer
`*SAS*'. If there is a process already running in `*SAS*', just
switch to that buffer.  With argument, allow you to edit the
command line (default is value of `inferior-SASw-program')."
  (interactive (list (if current-prefix-arg
			 (read-string "Run SAS: " inferior-SASw-program)
		       inferior-SASw-program)))
  (let* ((former-buffer-file-name (buffer-file-name))
	 (SASw-cwd (split-string (replace-regexp-in-string "/" "\\" (car (split-string (buffer-file-name) "/[^/]+[.]sas")) nil t) ":")))
    (if (not (comint-check-proc "*SAS*"))
	(let ((cmdlist (split-string cmd)))
	  (set-buffer (apply (function make-comint)
			     "SAS" (car cmdlist) nil (cdr cmdlist)))
	  (inferior-SASw-mode)))
    (setq inferior-SASw-buffer "*SAS*")
    (comint-send-string (inferior-SASw-proc) ";;;;;;\n")
    (comint-send-string (inferior-SASw-proc) "options linesize=72;\n")
    (comint-send-string (inferior-SASw-proc) "x 'chdir \"")
    (comint-send-string (inferior-SASw-proc) (concat "" (upcase (car SASw-cwd))))
    (comint-send-string (inferior-SASw-proc) ":")
    (comint-send-string (inferior-SASw-proc) (car (cdr SASw-cwd)))
    (comint-send-string (inferior-SASw-proc) "\"';\n")
    (if (file-exists-p (concat (car (split-string former-buffer-file-name "/[a-z_ 0-9]+[.]sas")) "/autoexec.sas"))
	(comint-send-string (inferior-SASw-proc) "%include 'autoexec.sas';\n"))
    )
  (pop-to-buffer "*SAS*")
)


(defalias 'run-SAS 'inferior-SASw)

(defun SAS-send-region (start end &optional and-go)
  "Send the current region to the inferior SAS process. Prefix
argument means to switch to the SAS buffer afterwards."
  (interactive "r\nP")
  (let ((program))
    (setq program (replace-regexp-in-string "\\([()]\\)" "\\1" (buffer-substring start end)))
    (comint-send-string (inferior-SASw-proc) program))
  (comint-send-string (inferior-SASw-proc) "\n")
  (comint-send-string (inferior-SASw-proc) ";;;;;;\n")
  (if and-go (switch-to-SAS t)))


(defun SAS-send-keyword-step (kw)
  "Send the current keyword step surrounding point"
  (interactive)
  (widen)
  (beginning-of-line)
  (let ((start) (end))
    (if (looking-at kw)
	(progn
	  (beginning-of-line)
	  (setq start (point)))
      (re-search-backward kw (point-min) t)
      (beginning-of-line)
      (setq start (point)))
    (forward-char)
    (re-search-forward "^[^[:space:]]+" (point-max) t)
    (next-line)
    (setq end (point))
    (SAS-send-region start end))
  (re-search-forward "^[^[:space:]]+" (point-max) t)
  (beginning-of-line))

(defun SAS-send-step ()
  "Send the current top level paragraph to the inferior SAS process"
  (interactive)
  (widen)
  (beginning-of-line)
  (if (looking-at "^data ")
      (SAS-send-keyword-step "^data ")
    (beginning-of-line)
    (if (looking-at "^proc ")
	(SAS-send-keyword-step "^proc ")
      (beginning-of-line)
      (if (looking-at "^[%]macro ")
	  (SAS-send-keyword-step "^[%]macro ")
	(beginning-of-line)
	(if (looking-at "^[^[:space:]]+")
	    (progn
	      (let ((start) (end))
		(setq start (point))
		(end-of-line)
		(setq end (point))
		(SAS-send-region start end)
		(re-search-forward "^[^[:space:]]+" (point-max) t)
		(beginning-of-line)))
	  (re-search-forward "^[^[:space:]]+" (point-max) t)
	  (beginning-of-line))))))




(defun SAS-switch-to-proc-buffer (eob-p)
  "Switch to the inferior SAS process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inferior-SASw-buffer)
      (let ((pop-up-frames
	     ;; Be willing to use another frame
	     ;; that already has the window in it.
	     (or pop-up-frames
		 (get-buffer-window inferior-SASw-buffer t))))
	(pop-to-buffer inferior-SASw-buffer))
      (run-SAS inferior-lisp-program))
  (when eob-p
	 (push-mark)
    (goto-char (point-max))))



(defun inferior-SASw-proc ()
  (let ((proc (get-buffer-process (if (derived-mode-p 'inferior-lisp-mode)
				      (current-buffer)
				    inferior-SASw-buffer))))
    (or proc
	(error "No SAS subprocess; see variable `inferior-SASw-buffer'"))))

(defcustom SASw-indent-offset 4
  "Default indentation offset for SAS."
  :group 'sasw
  :type 'integer
  :safe 'integerp)


;; Next two functions lifted straight out of ESS
;; (just till we get something better)
(defun ess-sas-tab-to-tab-stop ()
  "Tab to next tab-stop and set left margin."
  (interactive)
  (tab-to-tab-stop)
  (setq left-margin (current-column))
  )

(defun ess-sas-backward-delete-tab ()
  "Moves the cursor to the previous tab-stop, deleting any characters
on the way."
  (interactive)

  (let* ((ess-sas-column (current-column))
         ;; remainder of current-column and sas-indent-width
         (ess-sas-remainder (% ess-sas-column SASw-indent-offset)))

    (if (not (= ess-sas-column 0))
        (progn
          (if (= ess-sas-remainder 0)
              (setq ess-sas-remainder SASw-indent-offset))

          (let ((backward-delete-char-untabify-method 'nil))
            (backward-delete-char-untabify ess-sas-remainder t)
            (setq ess-sas-column (- ess-sas-column ess-sas-remainder))
            (move-to-column ess-sas-column)
            (setq left-margin ess-sas-column))
          ))
    ))

(defvar sasw-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movement
;;    (define-key map [remap backward-sentence] 'python-nav-backward-block)
;;    (define-key map [remap forward-sentence] 'python-nav-forward-block)
;;    (define-key map [remap backward-up-list] 'python-nav-backward-up-list)
;;    (define-key map "\C-c\C-j" 'imenu)
    ;; Indent specific
;;    (define-key map "\177" 'python-indent-dedent-line-backspace)
;;    (define-key map (kbd "<backtab>") 'python-indent-dedent-line)
;;    (define-key map "\C-c<" 'python-indent-shift-left)
;;    (define-key map "\C-c>" 'python-indent-shift-right)
;;    (define-key map ":" 'python-indent-electric-colon)
    ;; Skeletons
;;    (define-key map "\C-c\C-tc" 'python-skeleton-class)
;;    (define-key map "\C-c\C-td" 'python-skeleton-def)
;;    (define-key map "\C-c\C-tf" 'python-skeleton-for)
;;    (define-key map "\C-c\C-ti" 'python-skeleton-if)
;;    (define-key map "\C-c\C-tt" 'python-skeleton-try)
;;    (define-key map "\C-c\C-tw" 'python-skeleton-while)
    ;; Shell interaction
    (define-key map "\C-c\C-p" 'run-SAS)
;;    (define-key map "\C-c\C-s" 'python-shell-send-string)
    (define-key map "\C-c\C-r" 'SAS-send-region)
    (define-key map (kbd "<C-return>") 'SAS-send-step)
;;    (define-key map (kbd "<C-return>") 'SAS-eval-line-and-step)
;;    (define-key map "\C-\M-x" 'python-shell-send-defun)
;;    (define-key map "\C-c\C-c" 'compile)
;;    (define-key map "\C-c\C-l" 'python-shell-send-file)
    (define-key map "\C-c\C-z" 'SAS-switch-to-proc-buffer)
    ;; Some util commands
;;    (define-key map "\C-c\C-v" 'python-check)
;;    (define-key map "\C-c\C-f" 'python-eldoc-at-point)
    ;; TODO Do tabbing properly
    (define-key map [tab] 'ess-sas-tab-to-tab-stop)
    (define-key map [C-tab] 'ess-sas-backward-delete-tab)
    ;; Utilities
    (substitute-key-definition 'complete-symbol 'completion-at-point
                               map global-map)
    map)
  "Keymap for `sasw-mode'.")



(defun SAS-in-c-comment-p ()
  "Test if point preceeds C-style comment opener but not closer"
  (interactive)
  (widen)
  (let* ((limit (point))
	 (char-pt (char-after (point)))
	 (char-pt-1 (char-after (1+ (point))))
	 (char-pt-0 (char-after (1- (point))))
	 (follows-c-start (re-search-backward (rx ?/ ?*) nil t))
	 (follows-c-end (re-search-forward (rx ?* ?/) limit t)))
    (goto-char limit)
    (or
     (and (eq char-pt ?/) (eq char-pt-1 ?*))
     (and (eq char-pt-0 ?/) (eq char-pt ?*))
     (and (eq char-pt ?*) (eq char-pt-1 ?/))
     (and (eq char-pt-0 ?*) (eq char-pt ?/))
    (and follows-c-start (not follows-c-end)))))


(defun SAS-move-to-this-statement-end ()
  "Move point just past the next semicolon outside a C-style comment"
  (interactive)
  (widen)
  (re-search-forward (rx (or ";" buffer-end)) nil t)
  (while (SAS-in-c-comment-p)
    (re-search-forward (rx (or ";" buffer-end)) nil t))
  (point))


(defun SAS-at-statement-start-p ()
  "Test if point is at the start of a statement, the first non-space, non-C-comment character after a semicolon or buffer start"
  (interactive)
  (widen)
  (let* ((limit (point))
	 (result
	  (progn
	    (if (> (point) (point-min))
		(backward-char))
	    (while
		(and (or (SAS-in-c-comment-p) (looking-at-p (rx space)))
		     (> (point) (point-min)))
	      (backward-char))
	    (if (or (eq (point) (point-min)) (eq (char-after) ?\;))
		t
	      nil))))
    (goto-char limit)
    (if (or (SAS-in-c-comment-p) (looking-at-p (rx space)))
	nil
      result)))

  
(defun SAS-move-to-this-statement-start ()
  "Move to first non-space character follow"
  (interactive)

  (while (and (> (point) (point-min)) (not (SAS-at-statement-start-p)))
    (backward-char))

  (if (and (eq (point) (point-min)) (looking-at-p (rx ?/ ?*)))
      (forward-char)
      (while (not (SAS-at-statement-start-p))
	(forward-char)))
    
  (point))


(defun SAS-syntax-propertize-function (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ((rx ?*) (0 (ignore (SAS-syntax-propertize-statement)))))
   start end))

  
(defun SAS-syntax-propertize-statement ()
  (unless (SAS-in-c-comment-p)
    (let* ((start (SAS-move-to-this-statement-start))
	   (comment (or (eq (char-after) ?*)
			(and (eq (char-after) ?%)
			     (eq (char-after (1+ (point))) ?*))))
	   (syntax-style (string-to-syntax "!"))
	   (end (SAS-move-to-this-statement-end)))
      (unless (not comment)
	(put-text-property start   (1+ start) 'syntax-table syntax-style)
	(put-text-property (1- end) end       'syntax-table syntax-style)))))


(defconst SAS-font-lock-keywords
  ;; Keywords
  `(
    (,(rx (group line-start (0+ space)) (group (or "data" "ods" "libname" "filename" "options" "run" "quit") symbol-end)) 2 font-lock-keyword-face)
    
    (,(rx (group line-start (0+ space)) (group (or "title" "footnote") (optional digit) symbol-end)) 2 font-lock-keyword-face)

    ;; Macros
    (,(rx (or ?% ?&) (1+ word) symbol-end) 0 font-lock-preprocessor-face)
        
    ;; SQL queries
    (,(rx line-start (0+ space) (or "select" "from" "where" "group by" "having" "order by" "on") symbol-end) 0 font-lock-keyword-face)
    (,(rx line-start (0+ space) (optional "natural" (1+ space)) (or "left" "right" "inner") (optional (1+ space) "outer") (1+ space) "join" symbol-end) 0 font-lock-keyword-face)
    (,(rx symbol-start (group (or "case" "when" "as")) symbol-end) 1 font-lock-keyword-face)
    (,(rx line-start (0+ space) (or "create" "drop") (1+ space) (or "table" "view") symbol-end) 0 font-lock-keyword-face)
    
    (,(rx line-start (0+ space) "insert" (1+ space) "into" symbol-end) 0 font-lock-keyword-face)

    ;; SAS data step statement keywords
    (,(rx line-start (0+ space) (group (or "set" "merge" "by" "if" "value" "length" "format" "informat" "label" "attrib" "keep" "drop" "rename" "input" "cards" "datalines")) symbol-end) 1 font-lock-keyword-face)
    (,(rx symbol-start (group (or "if" "then" "else" "and" "or" "not" "in" "function" "subroutine" "endsub")) (1+ space)) 1 font-lock-keyword-face)
    (,(rx symbol-start (group (or "do" "end")) symbol-end) 1 font-lock-keyword-face)
    
    ;; Base SAS procedures
    (,(rx line-start (0+ space) "proc" (1+ space) (or "append"
"authlib" "calendar" "catalog" "cdisc" "chart" "cimport" "compare" "contents" "convert" "copy" "corr" "cport" "datekeys" "datasets" "dbcstab" "delete" "display" "document" "ds2" "explode" "export" "fcmp" "fedsql" "fontreg" "format" "forms" "freq" "fslist" "groovy" "hadoop" "hdmd" "http" "import" "infomaps" "items" "javainfo" "json" "localedata" "lua" "means" "metadata" "metalib" "metaoperate" "migrate" "mschart" "odslist" "odstable" "odstext" "options" "optload" "optsave" "pds" "pdscopy" "plot" "pmenu" "presenv" "print" "printto" "proto" "prtdef" "prtexp" "pwencode" "qdevice" "rank" "registry" "release" "report" "scaproc" "sgdesign" "sgpanel" "sgplot" "sgrender" "sgscatter" "soap" "sort" "source" "sql" "sqoop" "standard" "stream" "summary" "tabulate" "tapecopy" "tapelabel" "template" "templates" "timeplot" "transpose" "trantab" "univariate" "xsl")
	  symbol-end) 0 font-lock-keyword-face)

    ;; SAS/Stat procedures
    (,(rx line-start (0+ space) "proc" (1+ space) (or "aceclus" "adaptivereg" "anova" "bchoice" "boxplot" "calis" "cancorr" "candisc" "catmod" "cluster" "corresp" "discrim" "distance" "factor" "fastclus" "fmm" "freq" "gam" "gampl" "gee" "genmod" "glimmix" "glm" "glmmod" "glmpower" "glmselect" "hpcandisc" "hpfmm" "hpgenselect" "hplmixed" "hplogistic" "hpmixed" "hpnlmod" "hppls" "hpprincomp" "hpquantselect" "hpreg" "hpsplit" "iclifetest" "icphreg" "inbreed" "irt" "kde" "krige2d" "lattice" "lifereg" "lifetest" "loess" "logistic" "mcmc" "mds" "mi" "mianalyze" "mixed" "modeclus" "multtest" "nested" "nlin" "nlmixed" "npar1way" "orthoreg" "phreg" "plan" "plm" "pls" "power" "princomp" "prinqual" "probit" "quantlife" "quantreg" "quantselect" "reg" "robustreg" "rsreg" "score" "seqdesign" "seqtest" "sim2d" "simnormal" "spp" "stdize" "stdrate" "stepdisc" "surveyfreq" "surveyimpute" "surveylogistic" "surveymeans" "surveyphreg" "surveyreg" "surveyselect" "tpspline" "transreg" "tree" "ttest" "varclus" "varcomp" "variogram")
symbol-end) 0 font-lock-keyword-face)
    
    ;; Functions
    (,(rx "%macro" (1+ space) (group (1+ word))) 1 font-lock-function-name-face)
    (,(rx line-start (0+ space) "function" (1+ space) (group (1+ word)) symbol-end) 1 font-lock-function-name-face)
    

    ;; Builtins
    (,(rx symbol-start (or "work" "sasuser" "sashelp") symbol-end) 0 font-lock-builtin-face)

    ;; Constants
    (,(rx symbol-start ?_ (1+ word) ?_ symbol-end) 0 font-lock-constant-face)

    ;; assignments
    (,(rx line-start (0+ space) (or "libname" "filename") (1+ space) (group (1+ word))) 1 font-lock-variable-name-face)
    
    (,(rx line-start (0+ space) (or "data") (1+ space) (group (optional (1+ word) ".")) (group (1+ word))) 2 font-lock-variable-name-face)
    
    (,(rx line-start (0+ space) "create" (1+ space) (or "table" "view") (1+ space) (group (optional (1+ word) ".")) (group (1+ word)) symbol-end) 2 font-lock-variable-name-face)
    (,(rx symbol-start "out" (0+ space) "=" (0+ space) (optional (1+ word) ?. ) (group (1+ word) symbol-end)) 1 font-lock-variable-name-face)
))

(defvar SASw-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?%  "."  table)
    (modify-syntax-entry ?/  ". 14"  table) ; C-style comments
    (modify-syntax-entry ?*  ". 23"  table) ; C-style comments
    (modify-syntax-entry ?_  "w"  table)
    table)
  "Syntax table for SAS code.")

(define-derived-mode sasw-mode prog-mode "SAS"
  "Major mode for editing SAS files

\\{sasw-mode-map}"
  :syntax-table SASw-syntax-table
  (setq-local tab-width SASw-indent-offset)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "*/")

  (setq-local font-lock-defaults '(SAS-font-lock-keywords nil t nil))
  (setq-local syntax-propertize-function #'SAS-syntax-propertize-function)
  )

(provide 'sasw)


