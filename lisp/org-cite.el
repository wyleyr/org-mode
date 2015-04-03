;;; -*- lexical-binding: t -*-

;;; TODO: license etc.

(require 'org-bibtex)
(require 'cl-lib)
(require 'subr-x)
;(require 'let-alist)
;;; TODO: would also be nice to have a let-plist, and let-org-element
(require 'regexp-opt)
(require 'format-spec)
(require 'json)

;;; TODO: calling with args backend and info is redundant, since
;;; backend can be gotten from info


;;; Utilities

(defmacro org-cite--plist-put-multiple (plist &rest rest)
  ;; TODO:
  ;; - tests etc.
  ;; - maybe make more general and move to org-macs
  (declare (indent 1))
  (if rest
      (progn
	(when (= (length rest) 1)
	  (error "Odd number of values"))
	(cl-destructuring-bind (key val . more) rest
	  `(org-cite--plist-put-multiple
	    (plist-put ,plist ,key ,val) ,@more)))
    plist))

;; Helper; shouldn't Elisp have this already??
(defun org-cite--map-plist (f plist)
  "Map F over the key-value pairs in PLIST.  F is called with
each key and its associated value as its first and second
arguments, respectively."
  (if (or (null plist) (null (cdr plist))) '() ; values must be paired
    (cons (funcall f (car plist) (cadr plist))
	  (org-cite--map-plist f (cddr plist)))))

;;; TODO: defcustom
;; (defconst org-cite--citeproc-java-lib-path
;;   (expand-file-name "../lib/"
;; 		    (file-name-directory
;; 		     (file-truename (executable-find "citeproc-java"))))
;;   "The path to the citeproc-java installation.")

(defconst org-cite--org-citeproc-binary
  (expand-file-name "../bin/org-citeproc"))

(defconst org-cite--org-csl-dir
  (expand-file-name "../etc/csl/"
		    (file-name-directory (or load-file-name (buffer-file-name)))))

;; (defconst org-cite--citeproc-classpath
;;   (concat (mapconcat (lambda (x) (concat org-cite--citeproc-java-lib-path x))
;; 		     '("citeproc-java-0.6.jar"
;; 		       "commons-lang-2.6.jar"
;; 		       "jbibtex-1.0.8.jar"
;; 		       "antlr4-runtime-4.1.jar"
;; 		       "rhino-1.7R4.jar"
;; 		       "mapdb-0.9.6.jar"
;; 		       "styles-1.0.1-SNAPSHOT.jar"
;; 		       "locales-1.0.1-SNAPSHOT.jar"
;; 		       "org.abego.treelayout.core-1.0.1.jar")
;; 		     ":")
;; 	  ":" org-cite--org-csl-dir))

;; (defun org-cite--run-citeproc-java (&rest args)
;;   "Run the javadoc program, passing it ARGS.

;; Raises an error if the program exits with a non-zero status,
;; otherwise returns stdout as a string.

;; We need to hack our own CSL file into the classpath, which is why
;; we can't just use the wrapper script distributed by citeproc-java."
;;   ;; TODO: does this work with windows paths?
;;   (with-temp-buffer
;;     (let ((result-code
;; 	   (apply #'call-process "java" nil '(t nil) nil
;; 		  "-classpath"
;; 		  org-cite--citeproc-classpath
;; 		  "de.undercouch.citeproc.CSLTool"
;; 		  args)))
;;       (unless (zerop result-code)
;; 	(error "Non-zero exit from citeproc-java; args = %S"
;; 	       args))
;;       (buffer-string))))

;; JSON generating utilities

(defun org-cite--get-references (citation)
  "Get the list of citation-references within a citation, parsing
them from the buffer if necessary."
  (let* ((contents (org-element-contents citation))
	 (refs (remove-if-not
		(lambda (o) (and (listp o) (eq (car o) 'citation-reference)))
		contents)))
    (unless refs 
      ; citation-references within citation were not previously
      ; parsed, so we need to parse them:
      (save-excursion
	(goto-char (org-element-property :contents-begin citation))
	(let ((end (org-element-property :contents-end citation)))
	  (while (< (point) end)
	    (let ((reference (org-element-lineage
			      (org-element-context) '(citation-reference) t)))
	      (push reference refs)
	      (goto-char (org-element-property :end reference)))))
	(setq refs (nreverse refs))))
    refs))

(defun org-cite-citations-to-json (citations)
  "Translate a list of Org citation objects to a JSON array that can
be read by citeproc-js"
  (if (null citations) ""
    (json-encode-array (mapcar 'org-cite--citation-to-plist citations))))

(defun org-cite-citation-to-json (citation)
    "Translate an Org citation object to a JSON citation data
object that can be read by citeproc-js"
  (let ((plist (org-cite--citation-to-plist citation)))
    (if plist (json-encode-plist plist)
      "")))

(defun org-cite--citation-to-plist (citation)
    "Translate an Org citation object to a plist, in preparation
for JSON encoding"
  (let* ((refs (org-cite--get-references citation))
         (refs-plists (mapcar 'org-cite--citation-reference-to-plist refs))
	 ; json.el guesses wrong about how to encode a list of plists
	 ; like refs-plists, so we transform it to a vector to ensure it
	 ; is eventually encoded as an array of objects, rather than an object:
	 (refs-vec (apply 'vector refs-plists))
	 (prefix (org-element-property :prefix citation))
	 (suffix (org-element-property :suffix citation))
	 (props-plist nil))
    ; TODO: option for "strict" citeproc-js JSON, to exclude these properties?
    (when prefix
      (setq props-plist
	    (plist-put props-plist :common-prefix (org-element-interpret-data prefix))))
    (when suffix
      (setq props-plist
	    (plist-put props-plist :common-suffix (org-element-interpret-data suffix))))
    ; TODO: noteIndex property?
    (cond
     ((and refs-plists props-plist)
      (list :citationItems refs-vec :properties props-plist))
     (refs-plists
      (list :citationItems refs-vec)))))

(defun org-cite--citation-properties-to-json (citation)
  "Translate the common prefix and suffix of a citation to JSON
data"
  ; TODO: noteIndex
  ; TODO: common prefix and suffix are non-standard extensions;
  ; option for "strict" citeproc-js JSON?
  ; TODO: add whitespace after prefix and before suffix?
  (let* ((cprefix (org-element-property :prefix citation))
	 (json-prefix (and cprefix
			   (org-cite--property-to-json :common-prefix cprefix)))
	 (csuffix (org-element-property :suffix citation))
	 (json-suffix (and csuffix
			   (org-cite--property-to-json :common-suffix csuffix)))
	 (json-props (remove-if-not 'identity (list json-prefix json-suffix))))
    (when json-props
	(format "\"properties\": { %s }" (mapconcat 'identity json-props ", ")))))
	
(defun org-cite-citation-reference-to-json (reference)
  "Translate a citation-reference within an Org citation object
to JSON data that can be read by citeproc-js"
  (let ((plist (org-cite--citation-reference-to-plist reference)))
    (if plist (json-encode-plist plist)
      "")))

(defun org-cite--citation-reference-to-plist (reference)
  "Translate a citation-reference within an Org citation object
to a plist, in preparation for JSON encoding"
  (let* ((parenp (org-element-property :parenthetical
				       (org-element-property :parent reference)))
	 (fields (list :prefix :suffix
		       ; not :key, because it must be renamed :id.
		       ; TODO: org-citeproc will interpret these if
		       ; they are provided, but they are not currently
		       ; parsed by Org:
		       :suppress-author :author-only :label :locator))
	 (plist
	  (append (list :id (org-element-property :key reference))
		  (if (not parenp) (list :author-in-text t))
		  (org-cite--object-extract-plist reference fields)))
	 ; these fields need to be further interpreted before encoding:
	 (prefix (plist-get plist :prefix))
	 (suffix (plist-get plist :suffix)))
    (when prefix
      (setq plist
	    (plist-put plist :prefix (org-element-interpret-data prefix))))
    (when suffix
      (setq plist
	    (plist-put plist :suffix (org-element-interpret-data suffix))))
    plist))
	 

(defun org-cite--object-extract-plist (object keywords)
  "Extract a plist from OBJECT whose keys are each of the
KEYWORDS where OBJECT has a non-nil property value.  OBJECT
should be an Org object, and KEYWORDS a list of keywords."
  (flet ((extract (props accumulated)
	    (if (null props)
		accumulated
	      (let* ((prop (car props))
		     (val (org-element-property prop object))
		     (new-acc (if val
				  (cons prop (cons val accumulated))
				accumulated)))
		(extract (cdr props) new-acc)))))
   (extract keywords '())))

(defun org-cite--property-to-json (prop val)
  "Translate a property of a citation or citation-reference to JSON data"
  ; TODO: prefix and suffix vals should be transcoded by the current
  ; export backend
  (case prop
    (:key (format "\"id\": \"%s\"" val))
    (:prefix (format "\"prefix\": \"%s\"" (org-element-interpret-data val)))
    (:suffix (format "\"suffix\": \"%s\"" (org-element-interpret-data val)))
    (:common-prefix
     (format "\"common-prefix\": \"%s\"" (org-element-interpret-data val)))
    (:common-suffix
     (format "\"common-suffix\": \"%s\"" (org-element-interpret-data val)))
    (:parenthetical (if (not val) (format "\"author-in-text\": true") nil))
    ;; TODO: citeproc implementations use these fields, so we should extract them:
    (:suppress-author (if val (format "\"suppress-author\": true") nil))
    (:author-only (if val (format "\"author-only\": true") nil))
    (:label (format "\"label\": \"%s\"" val))
    (:locator (format "\"locator\": \"%s\"" val))
    ; otherwise, case returns nil
    ))

(defun org-cite--run-org-citeproc (json-buffer backend cslfile bibfiles)
  "Run the org-citeproc program, passing it the following arguments:
The contents of JSON-BUFFER will be passed as the program's stdin.
BACKEND should be a symbol naming an export backend.
CSLFILE should be a path to a CSL file.
BIBFILES should be a list of paths to bibliography database files in a format
that pandoc-citeproc can read.

Returns a buffer containing the results of org-citeproc.
"
  (let* ((output-buffer (get-buffer-create "*Org-citeproc results*"))
	 (output-format
	  (cond
	   ((org-export-derived-backend-p backend 'ascii) "ascii")
	   ((org-export-derived-backend-p backend 'html) "html")
	   ((org-export-derived-backend-p backend 'odt) "odt")
	   (t (error "Backend not supported by org-citeproc: %s" backend))))
	 (args (append (list output-format (expand-file-name cslfile))
		       (mapcar 'expand-file-name bibfiles))))
    (save-excursion
      (set-buffer output-buffer)
      (erase-buffer)
      (set-buffer json-buffer)
      (unless (zerop (apply #'call-process-region
			(point-min)
			(point-max)
			org-cite--org-citeproc-binary
			nil
			output-buffer
			nil
			args))
	(error "Non-zero exit from org-citeproc; args = %S"
	       args)))
    output-buffer))


;;; Citation modes

(defvar org-cite--citation-modes nil
  "An alist of mappings of citation modes to functions to format
a citation in that mode.  A citation mode is a manner of
formatting an in-text citation.  See:
<http://mid.gmane.org/m2k2z0mekp.fsf@tsdye.com>.

The formatting function is called with 4 arguments:
- the backend
- the info plist
- a plist of options for this citation containing:
  - :capitalized (TODO: unimplemented in the parser)
  - :parenthesized
  - :prefix
  - :suffix
- the citation database entry for this citation (cons key alist-of-vals)
- a function which can be called to retrieve the exported full
  citation of this key, using the current document's settings.
  This is useful for e.g. footnote styles.  It is passed as a
  function, rather than as a value, to avoid computing it when it
  is not needed.  (TODO: memoize the citation generation function
  to avoid needing this).")

(defun org-cite-add-citation-mode (mode fn)
  "Add a citation mode.

See `org-cite--citation-modes'."
  (setq org-cite--citation-modes
	(cons (cons mode fn)
	      org-cite--citation-modes)))

(defun org-cite-add-citation-mode-latex (mode nonparen paren)
  "A convenience function to add a citation mode for latex export only.

Will generate an error when used in other document types."
  ;; TODO: capitalized/non
  (org-cite-add-citation-mode mode
    (cl-function
     (lambda (backend _ (&key parenthesized prefix suffix) (cite-key . _) _)
       (if (org-export-derived-backend-p backend 'latex)
	   (format (if parenthesized paren nonparen)
		   prefix suffix cite-key)
	 (error "Citation mode `%s' is only defined for latex backends" mode))))))

(defconst org-cite--author-year-format
  "%p%a (%y%s)")

(defconst org-cite--author-year-parens-format
  "(%p%a %y%s)")

;;; TODO: add a convenience function to add a mode which is defined by
;;; latex commands and a set of template

(defun org-cite--mode-author-year (backend info cite-opts db-entry _full-cite-fn)
  (let ((parenthesized (plist-get cite-opts :parenthesized))
	(prefix (plist-get cite-opts :prefix))
	(suffix (plist-get cite-opts :suffix))
	(cite-key (car db-entry))
	(_cite-info (cdr db-entry)))
    (if (org-export-derived-backend-p backend 'latex)
	(format
	 (if parenthesized "\\parencite[%s][%s]{%s}" "\\textcite[%s][%s]{%s}")
	 ;; TODO: check on whether biblatex is going to automatically
	 ;; insert a semicolon after the year; I fear it is:
	 ;; \textcite[][foo]{bar} -> Bar (2015; foo)
	 prefix suffix cite-key)
      (format-spec
       (if parenthesized
	   org-cite--author-year-format
	 org-cite--author-year-parens-format)
       `((?a . ,(nth 1 (assoc cite-key (plist-get info :cite-author-years))))
	 (?y . ,(nth 2 (assoc cite-key (plist-get info :cite-author-years))))
	 (?p . ,prefix)
	 (?s . ,suffix)
	 ;; TODO: what else?
	 )))))

(org-cite-add-citation-mode "author-year" #'org-cite--mode-author-year)

;;; TODO: footnote -- will require fiddling with the info plist to get
;;; the proper definition/reference pair

;;; TODO: numbered citations (referring to numbers in bibliography,
;;; not footnotes) -- will require a counter mechanism in info
;;; additional to the footnote one.  A general counter might be
;;; desirable elsewhere (linguistic examples), so maybe it can be made
;;; generic.


;;; Citation styles

(defun org-cite--replace-in-string (orig rep str)
  (replace-regexp-in-string (regexp-quote orig) rep str
			    nil t))

(defmacro org-cite--replacements-in-string (str &rest reps)
  ;; TODO: indent declaration
  `(thread-last ,str
     ,@(mapcar (lambda (x)
		 `(org-cite--replace-in-string ,(nth 0 x) ,(nth 1 x)))
	       reps)))

(defun org-cite--html-to-org-inner (node)
  (cond
   ((null node) "")
   ((stringp node)
    ;; TODO: this should be a general fn like org-export-quote or
    ;; something.
    (org-trim (org-cite--replacements-in-string node
						;; Undo html quoting
						("&#38;" "&")
						("&#60;" "<")
						("&#62;" ">")
						;; TODO: superscripts

						;; Quote meaningful characters
						;; for org.
						("*" "\\ast{}")
						("_" "\\under{}")
						("/" "\\slash{}")
						("^" "\\asciicirc{}")
						("+" "\\plus{}")
						("~" "\\tilde{}")
						;; TODO: any more?
						)))
   ((listp node)
    (cl-destructuring-bind (tag attrs &rest children) node
      (let ((kids (mapconcat #'org-cite--html-to-org-inner children
			     ;; TODO: verify that this is the correct separator
			     "")))
	;; See file src/formatters.js in citeproc-js:
	;; https://bitbucket.org/fbennett/citeproc-js/src/default/src/formats.js
	(cl-case tag
	  ((i em)
	   ;; TODO: is there a difference between i and em?
	   (format "/%s/" kids))
	  (b
	   (format "*%s*" kids))
	  ;; TODO: what happens to this further along, if the parent
	  ;; document has turned off sub/superscripts?
	  (sub
	   (format "_{%s}" kids))
	  (sup
	   (format "^{%s}" kids))
	  (span
	   (let ((style (alist-get 'style attrs)))
	     (cond
	      ((null style)
	       (org-trim kids))
	      ((string= style "text-decoration:underline;")
	       (format "_%s_" kids))
	      ((member style '(
			       ;; TODO: these probably happen in the
			       ;; middle of e.g. underlining, to
			       ;; switch it off.  How can we implement
			       ;; this in org?  Maybe it's better to
			       ;; use asciidoc as the input format???
			       "font-style:normal;"
			       "font-variant:normal;"
			       "font-weight:normal;"
			       "text-decoration:none;"
			       ;; TODO: figure out what to do for
			       ;; these cases.  Small-caps can be
			       ;; hacked with upcase.
			       "font-variant:small-caps;"
			       "baseline"))
	       (org-trim kids)))))
	  (div
	   (if (equal "csl-entry" (alist-get 'class attrs))
	       (concat "- " kids "\n")
	     (org-trim kids)))
	  (otherwise
	   (org-trim kids))))))
   (t (error "wtf")
      ;; TODO: better error handling
      )))

(defun org-cite--html-to-org (html)
  (let ((parse (with-temp-buffer
		 (insert html)
		 (libxml-parse-html-region (point-min) (point-max)))))
    (org-cite--html-to-org-inner parse)))

(defun org-cite-format-bibliography (info)
  "Return the formatted bibliography for a document.

Export backends should call this function to get the result of
generating a bibliography for citations in the document, and wrap
its return value in any backend-specific markup they wish.

In the special case of a latex-derived backend, this function
just returns a string containing the \\printbibliography
command."
  (let ((backend (plist-get info :back-end))
	(processed-bib (plist-get info :processed-bibliography)))
    (if (org-export-derived-backend-p backend 'latex)
	;; TODO:
	;; - options for the bibliography command
	;; - support plain bibtex and/or natbib as well
	;; - maybe allow inserting processed bib anyway (since pandoc-citeproc
	;;   is capable of generating LaTeX)
	"\\printbibliography{}"
      ;; TODO:
      ;; - add section header to output
      ;; - figure out the in-buffer format for this (keyword vs. section
      ;;   with special property vs ...)
      ;; - less hacky way of generating the org syntax than inserting
      ;;   textually (ideally, generate the org syntax tree directly)
      ;; - formatting options for the bibliography (numbered
      ;;   vs. unnumbered list, ...)
      (if processed-bib processed-bib
        ; TODO: is error the right thing to do here?  
	(error "No processed bibliography found")))))


;;; Lookup types

(defvar org-cite--lookup-types nil
  "Types of citation lookup backends.

Alist from type to list of:

- Function called at the beginning of export, with the rest of
  the keyword line after #+BIBDB: type, and the info plist.
  Should cache whatever it needs in the info plist.

- Function to lookup a citation.  Called with the key and the
  info plist.  Will be memoized by
  `org-cite--lookup' (TODO).  Should return an alist of
  keys and values about the citation (author, year, title, etc.)

- A boolean; non-nil = this lookup type is remote.  All local
  lookups will be tried before any remote one is.
  TODO: not yet implemented")

(defun org-cite-add-lookup-type (type prep-fn lookup-fn remotep)
  (declare (indent 1))
  (setq org-cite--lookup-types
	(cons (list type prep-fn lookup-fn remotep)
	      org-cite--lookup-types)))

(defun org-cite--org-bibtex-prep (path info)
  (plist-put info :cite-org-bibtex-files
	     (cons path (plist-get info :cite-org-bibtex-files))))

(defun org-cite--org-bibtex-lookup (key info)
  (let ((files (plist-get info :cite-org-bibtex-files)))
    (or (cl-dolist (file files)
	  (with-current-buffer (find-file-noselect file)
	    ;; TODO: close the buffer if it was newly opened by us
	    ;; TODO: more efficient way to do this?
	    (org-map-entries
	     (lambda ()
	       (cl-return (mapcar (lambda (x) (cons (intern (car x)) (cdr x)))
				  ;; TODO: push this into org-bibtex;
				  ;; diallow customizing the bibtex
				  ;; type property.
				  (cons (cons org-bibtex-type-property-name
					      (org-bibtex-get org-bibtex-type-property-name))
					(org-bibtex--all-properties)))))
	     (format "+%s=\"%s\"" org-bibtex-key-property key)
	     'file)))
	(error "Could not find key %s" key))))

(org-cite-add-lookup-type "org-bibtex"
  #'org-cite--org-bibtex-prep
  #'org-cite--org-bibtex-lookup
  nil)

(defun org-cite--bibtex-prep (path info)
  ; store path separately in cite-bibtex-files for the sake of
  ; lookups, but also in cite-all-bib-files, which will be passed to
  ; org-citeproc, since org-citeproc can read bibtex format among
  ; others
  (plist-put info :cite-bibtex-files
	     (cons path (plist-get info :cite-bibtex-files)))
  (plist-put info :cite-all-bib-files
	     (cons path (plist-get info :cite-all-bib-files))))

(defun org-cite--bibtex-lookup (key info)
  (let* ((bibtex-files (plist-get info :cite-bibtex-files))
	 (pos (bibtex-find-entry key t nil t))) ; bibtex-find-entry implicitly reads bibtex-files
	(if pos (goto-char pos)
	  (error "Could not find key %s" key))))

(org-cite-add-lookup-type
 "bibtex"
 #'org-cite--bibtex-prep
 #'org-cite--bibtex-lookup
 nil)

;;; TODO: DOI resolver via internet

(defun org-cite-lookup (key info)
  ;; TODO: memoize, document
  (cl-dolist (lookup-type (plist-get info :cite-lookup-types))
    (when-let ((result (funcall (nth 2 (assoc lookup-type org-cite--lookup-types))
				key info)))
      (cl-return result))))

;;; TODO: to support latex, we need to insert the results of the
;;; lookup (from remote sources in particular) into a temporary bib
;;; file that can be used during compilation.  Should we use
;;; \begin{filecontents} to insert into the latex document itself?

(defun org-cite--to-bibtex (cite-info)
  (let* ((key (car cite-info))
	 (info (cdr cite-info))
	 (type (cdr (assoc 'btype info))) ;; alist-get??
	 (info (remove (assq 'btype info) info)))
    (concat (format
	     ;; TODO: use the proper entry type
	     "@%s{%s,\n" type key)
	    (mapconcat (lambda (pair)
			 (format "%s = {%s}"
				 (symbol-name (car pair))
				 ;; TODO: Quote the value for bibtex?
				 ;; Since this is currently used to
				 ;; communicate with citeproc-java and
				 ;; (ideally) biber, maybe it should
				 ;; be pseudo-bibtex allowing UTF8
				 ;; etc.
				 (cdr pair)))
		       info
		       ",\n")
	    "}")))


;;; Integration with export functions

(defun org-cite--collect-citation-key (info citation)
  (let ((used-keys (plist-get info :cite-used-keys))
	(key (org-element-property :key citation)))
    (unless (member key used-keys)
      (plist-put info :cite-used-keys
		 (cons key used-keys)))))

(defun org-cite--collect-citation (info citation)
  (let ((all-cites (plist-get info :all-cites))
	(unique-id (gensym "cite")))
    ; modify citation by assigning it a unique id as we collect
    ; it into all-cites, so we can later unambiguously look up the
    ; processed version when exporting it
    (org-element-put-property citation :internal-id unique-id)
    (plist-put info :all-cites (cons citation all-cites))))

(defun org-cite--keys-in-citation (citation)
  "Return a list of the keys used in a citation object"
  ;; TODO: need to make this work with multi-cites
  (list (org-element-property :key citation)))

(defun org-cite--make-bibtex (info)
  (let* ((all-cites (plist-get info :all-cites))
	 (used-keys (plist-get info :cite-used-keys))
	 (file (make-temp-file "org-cite" nil ".bib"))
	 (btex-files (plist-get info :cite-bibtex-files))
	 (org-btex-files (plist-get info :cite-org-bibtex-files))
	 (all-bib-files (plist-get info :cite-all-bib-files)))
    (when org-btex-files
      (plist-put info :cite-bibtex-files (cons file btex-files))
      (plist-put info :cite-all-bib-files (cons file all-bib-files))
      (with-temp-file file
	(insert
	 (mapconcat
	  (lambda (key)
	    ;; TODO: what if key is not in an org-bibtex file...?
	    (org-cite--to-bibtex (cons key (org-cite-lookup key info))))
	  used-keys
	  "\n\n"))))
    nil))

(defun org-cite--get-author-year (info)
  (if-let ((keys (plist-get info :cite-used-keys)))
      (let* ((author-years
	      ;; TODO: does this reorder citations within the cite?  If
	      ;; so it will be necessary to call this one-by-one... :-/
	      (org-trim (apply #'org-cite--run-citeproc-java
			       "-b" (plist-get info :cite-bibtex-file)
			       "-s" "org-csl"
			       "-c"
			       keys))))
	(plist-put info :cite-author-years
		   (cl-mapcar (lambda (key ay)
				(let* ((s (split-string ay "////"))
				       (a (nth 0 s))
				       (y (nth 1 s)))
				  (list key a y)))
			      keys
			      (split-string author-years "||||"))))
    (message "No citations")
    nil))

(defvar org-cite--cites-separator-re "////$"
  "Regular expression for the separator between citations in
org-citeproc results")

(defvar org-cite--cites-bib-separator-re "^====$"
  "Regular expression for the separator between citations and the
bibliography in org-citeproc results")

(defun org-cite--process-citations (backend info)
  "Use org-citeproc to process the citations in the current document and produce
a bibliography.

Stores processed citations on INFO under :processed-citations, which is an alist
mapping citations' :internal-id fields to strings.

Stores the processed bibliography on INFO under :processed-bibliography as a
string.

Assumes INFO has been prepared to have :all-cites, :cite-csl-file, and
:cite-bibtex-files properties, as `org-cite-export-prepare' does,
and that citations in :all-cites have an :internal-id property."
  (let* ((all-cites (plist-get info :all-cites))
	 ;; TODO: props for filenames?
	 (cslfile (plist-get info :cite-csl-file))
	 (bibfiles (plist-get info :cite-bibtex-files))
	 (results-buffer nil)
	 (end-of-cites nil))
    (unless all-cites (message "No citations"))
    (when (and all-cites (not (org-export-derived-backend-p backend 'latex)))
      (with-temp-buffer
	(insert (org-cite-citations-to-json all-cites))
	(setq results-buffer
	      (org-cite--run-org-citeproc (current-buffer) backend cslfile bibfiles))))
    (when results-buffer
      (save-excursion
	(set-buffer results-buffer)
	(goto-char (point-min))
	(unless (re-search-forward org-cite--cites-bib-separator-re nil t)
	  (error "Could not find separator between citations and bibliography"))
	(setq end-of-cites (1- (match-beginning 0)))
	; after search, point is after separator, so stash the
	; bibliography first
	(plist-put info :processed-bibliography (buffer-substring (1+ (point)) (point-max)))
	; next, stash the processed citations
	; TODO: should we just store this info in the citation objects themselves?
	; That modifies the document tree, but would prevent the tree and info
	; from getting out of sync.
	(goto-char (point-min))
	(save-restriction
	  (narrow-to-region (point-min) end-of-cites)
	  (let ((processed-cites nil)
		(last-pos (point)))
	    (while (re-search-forward org-cite--cites-separator-re nil t)
	      (setq processed-cites
		    (cons (buffer-substring last-pos (match-beginning 0))
			  processed-cites))
	      (setq last-pos (goto-char (1+ (match-end 0)))))
	    (setq processed-cites (nreverse processed-cites))
	    (unless (eq (length all-cites) (length processed-cites))
	       (error "org-citeproc did not return correct number of citations"))
	    (plist-put info :processed-citations
                	; TODO: we need to address the issue of
			; incompatibility between pandoc-style nested
			; multi-citations and LaTeX-style
			; multi-citations in the in-text case.
			; Depending on how that's resolved, the
			; mapping between all-cites and
			; processed-cites may not be 1-1...
		       (mapcar*
			(lambda (ct res) (list (org-element-property :internal-id ct) res))
			all-cites
			processed-cites)))))))
  nil)

(defun org-cite-export-prepare (tree info)
  "Build a citation database from the #+BIBDB keywords in TREE.

Store the information in INFO.  Returns a modified copy of INFO;
does not modify TREE."
  (let* ((backend (plist-get info :back-end))
	 (lookup-types (mapcar #'car org-cite--lookup-types))
	 (lookup-types-re (regexp-opt lookup-types))
	 cite-mode cite-style
	 used-lookup-types)
    (org-element-map tree 'keyword
      (lambda (kw)
	(let ((kw-key (org-element-property :key kw))
	      (kw-val (org-element-property :value kw)))
	  (when (and (string= kw-key "BIBDB")
		     (string-match (rx-to-string `(and
						   string-start
						   (group (regexp ,lookup-types-re))
						   (? " " (group (* not-newline)))))
				   kw-val))
	    ;; TODO: Here we need \addbibresource support for Latex
	    (let ((lookup-type (match-string 1 kw-val)))
	      (setq info
		    (funcall (nth 1 (assoc lookup-type org-cite--lookup-types))
			     (match-string 2 kw-val)
			     info))
	      (add-to-list 'used-lookup-types lookup-type)))
	  (when (string= kw-key "CITATION_MODE")
	    ;; TODO: handle multiple specifications of CITATION_MODE
	    (setq cite-mode kw-val))
	  (when (string= kw-key "CITATION_STYLE")
	    ;; TODO: handle multiple specifications of CITATION_STYLE
	    (setq cite-style kw-val))
	  (when (string= kw-key "CSL_FILE")
	    ;; TODO: this should probably not be a separate keyword, but rather
	    ;; inferred from CITATION_STYLE in an appropriate way.  Need a 
	    ;; defined semantics for CITATION_STYLE across LaTeX/non-LaTeX targets.
	    ;; TOOD: this doesn't get picked up when exporting subtrees
	    (plist-put info :cite-csl-file kw-val)))))
    ;; TODO: is it possible for this procedure to overgenerate?
    ;; Better might be to add the keys one by one in
    ;; `org-cite--do-export', but IDK how to ensure this is all done
    ;; before the bibliography is exported.
    ;; TODO: :cite-used-keys is required by org-cite--make-bibtex.  This information
    ;; should instead be recovered from :all-cites.
    (org-element-map tree 'citation
      (apply-partially #'org-cite--collect-citation-key info))

    (org-element-map tree 'citation
      (apply-partially #'org-cite--collect-citation info))

    ;; TODO: populate a temp file with all entries in bibtex format,
    ;; needed for the citation processor.  Provide an option to stuff
    ;; this into filecontents in latex.  Requires greedy lookup of
    ;; citations from remote resources.
    ;; (org-cite--plist-put-multiple info
    ;;   :cite-function (cdr (assoc cite-mode org-cite--citation-modes))
    ;;   :cite-bibentry-function (cdr (assoc cite-style org-cite--citation-styles))
    ;;   :cite-lookup-types used-lookup-types)

    ;; Now that the lookup info is in the plist, we'll generate a temp
    ;; file with all the bib entries in it and stash the path to it in
    ;; the plist.  TODO: arrange to remove this file later.
    (org-cite--make-bibtex info)
    ;; Finally, process the citations and store the results on info
    (org-cite--process-citations backend info)
    info))

(defun org-cite-format-citation (citation _contents info)
  "Export a citation object.

Export backends should call this function to get the result of
processing a citation, and wrap its return value in any
backend-specific markup they wish."
  (let* ((int-id (org-element-property :internal-id citation))
	 (processed-cites (plist-get info :processed-citations))
	 (processed (cadr (assoc int-id processed-cites))))
    (unless processed
      ; TODO: is error the right thing to do here?  
      (error "No processed citation found for citation %s" int-id))
    processed))

(provide 'org-cite)
