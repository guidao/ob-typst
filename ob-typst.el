(require 'org-macs)
(org-assert-version)

(require 'ob)

(defvar org-babel-default-header-args:typst
  '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a typst source block.")

(defun org-babel-expand-body:typst (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body
		t
		t))))
     vars)
    body))

(defun org-babel-execute:typst (body params)
  "Execute a block of Dot code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (cdr (or (assq :file params)
			    (error "You need to specify a :file parameter"))))
	 (cmdline (or (cdr (assq :cmdline params))
		      (format "-f%s" (file-name-extension out-file))))
	 (cmd (or (cdr (assq :cmd params)) "typst"))
	 (coding-system-for-read 'utf-8) ;use utf-8 with sub-processes
	 (coding-system-for-write 'utf-8)
	 (in-file (org-babel-temp-file "typst-")))
    (with-temp-file in-file
      (insert (org-babel-expand-body:typst body params)))
    (org-babel-eval
     (concat cmd
	     " " "compile"
    	     " " cmdline
	     " " (org-babel-process-file-name in-file)
	     " " (org-babel-process-file-name out-file)) "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:typst (_session _params)
  "Return an error because Dot does not support sessions."
  (error "Dot does not support sessions"))

(provide 'ob-typst)

;;; ob-typst.el ends here
