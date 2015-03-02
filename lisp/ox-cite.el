;;; -*- lexical-binding: t -*-

;;; TODO: license etc.

(require 'org-bibtex)
(require 'cl-lib)
(require 'subr-x)
(require 'let-alist)
;;; TODO: would also be nice to have a let-plist, and let-org-element
(require 'regexp-opt)

;;; TODO: calling with args backend and info is redundant, since
;;; backend can be gotten from info

;;; Ideas about name formatting:
;;; - define a function to parse from bibtex format into a list of
;;;   (first last von), or whatever it is that bibtex actually gives
;;;   us
;;; - define some functions that work with that format to give First
;;;   von Last, F. von Last, von Last, Von Last, etc.
;;; - provide combinators:
;;;   - Apply format X to the first name in the list and Y to the rest
;;;   - Shorten the list to X et al if it's longer than N names
;;;   - Join the list with commas
;;;   - ...


;;; Utilities

(defmacro org-export-cite--plist-put-multiple (plist &rest rest)
  ;; TODO:
  ;; - tests etc.
  ;; - maybe make more general and move to org-macs
  (declare (indent 1))
  (if rest
      (progn
	(when (= (length rest) 1)
	  (error "Odd number of values"))
	(cl-destructuring-bind (key val . more) rest
	  `(org-export-cite--plist-put-multiple
	    (plist-put ,plist ,key ,val) ,@more)))
    plist))

;;; For testing
;; (org-export-cite--plist-put-multiple nil
;;   :foo 'bar
;;   :baz 'quux)


;;; Citation modes

(defvar org-export-cite--citation-modes nil
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

(defun org-export-cite-add-citation-mode (mode fn)
  (declare (indent 1))
  "Add a citation mode.

See `org-export-cite--citation-modes'."
  (setq org-export-cite--citation-modes
	(cons (cons mode fn)
	      org-export-cite--citation-modes)))

(defun org-export-cite-add-citation-mode-latex (mode nonparen paren)
  "A convenience function to add a citation mode for latex export only.

Will generate an error when used in other document types."
  ;; TODO: capitalized/non
  (org-export-cite-add-citation-mode mode
    (cl-function
     (lambda (backend _ (&key parenthesized prefix suffix) (cite-key . _) _)
       (if (org-export-derived-backend-p backend 'latex)
	   (format (if parenthesized paren nonparen)
		   prefix suffix cite-key)
	 (error "Citation mode `%s' is only defined for latex backends" mode))))))

(defun org-export-cite--mode-author-year (backend _info cite-opts db-entry _full-cite-fn)
  (let ((parenthesized (plist-get cite-opts :parenthesized))
	(prefix (plist-get cite-opts :prefix))
	(suffix (plist-get cite-opts :suffix))
	(cite-key (car db-entry))
	(cite-info (cdr db-entry)))
    (if (org-export-derived-backend-p backend 'latex)
	(format
	 (if parenthesized "\\parencite[%s][%s]{%s}" "\\textcite[%s][%s]{%s}")
	 ;; TODO: check on whether biblatex is going to automatically
	 ;; insert a semicolon after the year; I fear it is:
	 ;; \textcite[][foo]{bar} -> Bar (2015; foo)
	 prefix suffix cite-key)
      (let-alist cite-info
	;; TODO: proper author formatting, fall back to editor if
	;; no author is given

	;; TODO: build a proper template system, similar to
	;; `reftex-cite-format'.  It's annoying that this sort of
	;; thing is reimplemented in all sorts of annoying ways all
	;; over org's code base.  See `org-compile-prefix-format' for
	;; a clever approach to efficiency.  See also
	;; `org-capture-fill-template' for a full example of how
	;; powerful the functionality needs to be.
	(format
	 (if parenthesized "(%s%s %s%s)" "%s%s (%s)%s")
	 prefix  .author .year suffix )))))

(org-export-cite-add-citation-mode "author-year"
  #'org-export-cite--mode-author-year)

;;; TODO: footnote -- will require fiddling with the info plist to get
;;; the proper definition/reference pair

;;; TODO: numbered citations (referring to numbers in bibliography,
;;; not footnotes) -- will require a counter mechanism in info
;;; additional to the footnote one.  A general counter might be
;;; desirable elsewhere (linguistic examples), so maybe it can be made
;;; generic.


;;; Citation styles

(defvar org-export-cite--citation-styles nil
  "An alist of mappings of citation styles to functions to format
a citation in that style.  A citation style is a manner of
formatting a reference in the bibliography.

The formatting function is called with 3 arguments:
- the backend
- the info plist
- the citation database entry for this citation (cons key
  alist-of-vals) TODO: Is it better only to pass the key, since
  lookups will be cached and thus cheap?")

(defun org-export-cite-add-citation-style (style fn)
  (declare (indent 1))
  "Add a citation style.

See `org-export-cite--citation-styles'."
  (setq org-export-cite--citation-styles
	(cons (cons style fn)
	      org-export-cite--citation-styles)))

(defun org-export-cite--author-year-style (backend _info db-entry)
  ;; This function generates org text, which is then exported by
  ;; `org-export-cite-format-bibliography'.  This frees us from having
  ;; to worry about backend-specific formatting here.
  (let ((_cite-key (car db-entry))
	(cite-info (cdr db-entry)))
    (if (org-export-derived-backend-p backend 'latex)
	;; Latex formats its bibliography entries itself, and doesn't
	;; need to call this function.  We might want to allow latex
	;; to call here, though, in order to get an absolutely uniform
	;; bibliography in latex and other backend types (but we could
	;; never fake being as good as latex is, so maybe it's not
	;; worht bothering)
	(error "Should not be called.")
      (let-alist cite-info
	;; TODO: needs to be customized for different citation types:
	;; book, journal article, etc.  Also need to allow hooking in
	;; external citation formatters, e.g. citeproc-java.

	;; TODO: need to escape potential org syntax in the title etc.

	(format "*%s* (%s).  /%s/."
		.author .year .title)))))

(org-export-cite-add-citation-style "author-year-title"
  #'org-export-cite--author-year-style)


;;; Lookup types

(defvar org-export-cite--lookup-types nil
  "Types of citation lookup backends.

Alist from type to list of:

- Function called at the beginning of export, with the rest of
  the keyword line after #+BIBDB: type, and the info plist.
  Should cache whatever it needs in the info plist.

- Function to lookup a citation.  Called with the key and the
  info plist.  Will be memoized by
  `org-export-cite--lookup' (TODO).  Should return an alist of
  keys and values about the citation (author, year, title, etc.)

- A boolean; non-nil = this lookup type is remote.  All local
  lookups will be tried before any remote one is.
  TODO: not yet implemented")

(defun org-export-cite-add-lookup-type (type prep-fn lookup-fn remotep)
  (declare (indent 1))
  (setq org-export-cite--lookup-types
	(cons (list type prep-fn lookup-fn remotep)
	      org-export-cite--lookup-types)))

(defun org-export-cite--org-bibtex-prep (path info)
  (plist-put info :cite-org-bibtex-files
	     (cons path (plist-get info :cite-org-bibtex-files))))

(defun org-export-cite--org-bibtex-lookup (key info)
  (let ((files (plist-get info :cite-org-bibtex-files)))
    (or (cl-dolist (file files)
	  (with-current-buffer (find-file-noselect file)
	    ;; TODO: close the buffer if it was newly opened by us
	    ;; TODO: more efficient way to do this?
	    (org-map-entries
	     (lambda ()
	       (cl-return (mapcar (lambda (x) (cons (intern (car x)) (cdr x)))
				  (org-bibtex--all-properties))))
	     (format "+%s=\"%s\"" org-bibtex-key-property key)
	     'file)))
	(error "Could not find key %s" key))))

(org-export-cite-add-lookup-type "org-bibtex"
  #'org-export-cite--org-bibtex-prep
  #'org-export-cite--org-bibtex-lookup
  nil)

;;; TODO: bibtex, DOI resolver via internet

(defun org-export-cite-lookup (key info)
  ;; TODO: memoize, document
  (cl-dolist (lookup-type (plist-get info :cite-lookup-types))
    (when-let ((result (funcall (nth 2 (assoc lookup-type org-export-cite--lookup-types))
				key info)))
      (cl-return result))))

;;; TODO: to support latex, we need to insert the results of the
;;; lookup (from remote sources in particular) into a temporary bib
;;; file that can be used during compilation.  Should we use
;;; \begin{filecontents} to insert into the latex document itself?


;;; Integration with export functions

(defun org-export-cite-prepare (tree info)
  "Build a citation database from the #+BIBDB keywords in TREE.

Store the information in INFO.  Returns a modified copy of INFO;
does not modify TREE."
  (let* ((lookup-types (mapcar #'car org-export-cite--lookup-types))
	 (lookup-types-re (regexp-opt lookup-types))
	 cite-mode cite-style
	 used-lookup-types
	 used-citations)
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
		    (funcall (nth 1 (assoc lookup-type org-export-cite--lookup-types))
			     (match-string 2 kw-val)
			     info))
	      (add-to-list 'used-lookup-types lookup-type)))
	  (when (string= kw-key "CITATION_MODE")
	    ;; TODO: handle multiple specifications of CITATION_MODE
	    (setq cite-mode kw-val))
	  (when (string= kw-key "CITATION_STYLE")
	    ;; TODO: handle multiple specifications of CITATION_MODE
	    (setq cite-style kw-val)))))
    ;; TODO: is it possible for this test to overgenerate?  Better
    ;; might be to add the keys one by one in
    ;; `org-export-cite--do-export', but IDK how to ensure this is all
    ;; done before the bibliography is exported.
    (org-element-map tree 'citation
      (lambda (cite)
	(add-to-list 'used-citations (org-element-property :key cite))))
    (org-export-cite--plist-put-multiple info
      :cite-function (cdr (assoc cite-mode org-export-cite--citation-modes))
      :cite-bibentry-function (cdr (assoc cite-style org-export-cite--citation-styles))
      :cite-lookup-types used-lookup-types
      :cite-citations used-citations)))

(defun org-export-cite-format-bibentry (key info)
  (funcall (plist-get info :cite-bibentry-function)
	   (plist-get info :back-end)
	   info
	   (cons key (org-export-cite-lookup key info))))

(defun org-export-cite-format-citation (citation _contents info)
  "Export a citation object.

Export backends should call this function to get a general
citation text, and wrap its return value in any backend-specific
markup they wish."
  (let ((key (org-element-property :key citation)))
    (funcall (plist-get info :cite-function)
	     (plist-get info :back-end)
	     info
	     `(:capitalized
	       nil  ; TODO: pending parser support
	       :parenthesized ,(org-element-property :parentheticalp citation)
	       :prefix ,(org-export-data (org-element-property :prefix citation) info)
	       :suffix ,(org-export-data (org-element-property :suffix citation) info))
	     (cons key (org-export-cite-lookup key info))
	     (lambda () (org-export-cite-format-bibentry key info)))))

(defun org-export-cite-format-bibliography (info)
  ;; TODO: sort the bibliography
  (let ((backend (plist-get info :back-end)))
    (if (org-export-derived-backend-p backend 'latex)
	;; TODO:
	;; - options for the bibliography command
	;; - support plain bibtex and/or natbib as well
	"\\printbibliography{}"
      ;; TODO:
      ;; - add section header to output
      ;; - figure out the in-buffer format for this (keyword vs. section
      ;;   with special property vs ...)
      ;; - less hacky way of generating the org syntax than inserting
      ;;   textually (ideally, generate the org syntax tree directly)
      ;; - formatting options for the bibliography (numbered
      ;;   vs. unnumbered list, ...)
      (with-temp-buffer
	(insert (mapconcat (lambda (key)
			     (concat "- " (org-export-cite-format-bibentry key info)))
			   (plist-get info :cite-citations)
			   "\n"))
	(org-export-as backend nil nil t)))))
