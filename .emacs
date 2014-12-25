;;; Matching parentheses
;;;
(defun match-parenthesis (arg)
  "Match the current character according to the syntax table.

Based on the freely available match-paren.el by Kayvan Sylvan.
I merged code from goto-matching-paren-or-insert and match-it.

You can define new \"parentheses\" (matching pairs).
Example: angle brackets. Add the following to your .emacs file:

(modify-syntax-entry ?< \"(>\" )
(modify-syntax-entry ?> \")<\" )

You can set hot keys to perform matching with one keystroke.
Example: f6 and Control-C 6.

(global-set-key \"\\C-c6\" 'match-parenthesis)
(global-set-key [f6] 'match-parenthesis)

Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
  (interactive "p")
  (let
      ((syntax (char-syntax (following-char))))
  (cond
   ((= syntax ?\()
    (forward-sexp 1) (backward-char))
   ((= syntax ?\))
    (forward-char) (backward-sexp 1))
   (t (message "No match"))
   )
  ))
(global-set-key "\C-c6" 'match-parenthesis)
(global-set-key [f6] 'match-parenthesis)

;; I rather like my way of inserting html/xml tags
(defun cema-html-insert-tag (tag)
  "Insert an html tag, with possible parameters.

At the prompt, type in the tag with parameters (if any) the way you
would want them to appear in the html text.

If the region is set, the tag heading (with possible parameters) is
inserted before the region, and the tag ending goes after the region.
Thus, the region is assumed to provide the hypertext. With no region
defined, the tag heading and ending are inserted side by side, and
the point is left between them. The hypertext may therefore be inserted
from here.

In some cases, a tag is considered singular, so there is no tag ending.
This happens with <!tag ...>, <?tag ...>, and <tag .../>.

Special case: some browsers do not recognize <tag/> properly, so
instead we insert whitespace like this: <tag />.

Possible improvement: like the standard html package, present a choice
of attributes. (Would this be improvement or not?)

Simon Hawkin <cema@cs.umd.edu> 1998/12/10
Simon Hawkin <SimonHawkin@yahoo.com> 2002/10/17
Simon Hawkin <SimonHawkin@yandex.ru> 2003/04/26
Simon Hawkin <Simon_Hawkin@yahoo.com> 2004/09/29
"
  (interactive "*sTag ")
  ;;
  (modify-syntax-entry ?\_ "w") ; allow tag abc_xyz
  (modify-syntax-entry ?\- "w") ; allow tag abd-xyz
  ;; Some tags do not have an entry (and therefore the ending)
  (setq tag-singular (or
     (posix-string-match "\/$" tag)
     (posix-string-match "^\\(\!\\|\?\\)" tag)
     ))
  ;; Special case: <tag .../>
  (setq tag-slash-adjacent
(and (posix-string-match "\/$" tag)
    (not (posix-string-match "\\( \\|
\\|	\\)\/$" tag)) ; is there a better way to represent eol?
    ))
  ;; More vars
  (setq bracket-open-left "<")
  (if tag-slash-adjacent
      (setq bracket-open-right " />")
    (setq bracket-open-right ">")
    )
  (setq bracket-close-left "</")
  (setq bracket-close-right ">")
  (setq tag-name-start (posix-string-match "[^
]" tag)) 
  (setq tag-whole-end (length tag))
  (if tag-slash-adjacent (setq tag-whole-end (- tag-whole-end 1)) )
  (setq tag-name-end (posix-string-match "$\\|[
]" tag tag-name-start))
  (setq tag-length (+ 2 (- (length tag) tag-name-start))) ; allow for <>
  ;; THE WORK
  ;; Region may or may not be defined.
  (if mark-active
      ;; The region is defined. Wrap the tag around it.
      (progn
(setq start-at (region-beginning))
(setq end-at (+ (region-end) tag-length))
;; Tag, with possible parameters, before the region.
(goto-char start-at)
(insert bracket-open-left
(substring tag tag-name-start tag-whole-end)
bracket-open-right
)
;; Tag only, after the region.
(goto-char end-at)
(if (not tag-singular)
 (insert bracket-close-left
 (substring tag tag-name-start tag-name-end)
 bracket-close-right
 )
 ;; Leave the point in the beginning of the region.
 (goto-char (+ start-at tag-length))
 )
)
    ;; No region. Insert in place.
    (insert bracket-open-left
   (substring tag tag-name-start tag-whole-end)
   bracket-open-right)
    (if (not tag-singular)
(save-excursion
 (insert bracket-close-left
 (substring tag tag-name-start tag-name-end)
 bracket-close-right
 )
 )
      )
    ))
(global-set-key "\C-ch" 'cema-html-insert-tag)

;; Perhaps it makes sense to split code into more manageable pieces.
(defun load-file-if-readable (file-name)
  "If the argument is an existing and readable file, load it as elisp code.

Simon Hawkin 2003/08/15"
  (if (file-readable-p file-name)
      (load-file file-name)
    (let (msg msg0)
      (setq msg (format "WARNING:: file %s could not be loaded ::WARNING"
file-name))
      (message msg)
      (sleep-for 0 500)
      ))
  (file-readable-p file-name) ; return
  )

;; Comments are pretty common
(defun cema-set-comments (begin end)
  "Set how commented lines will be marked (pre- and postfixed).

Convenient to use with comment-region."
  (interactive "sCommented lines begin with \nsCommented lines end with ")
  (setq comment-start begin)
  (setq comment-end end)
  )
(global-set-key "\C-x4#" 'cema-set-comments)
(global-set-key "\C-x#" 'comment-region)

;; Full path name versus just file name
(defun toggle-buffer-identification ()
  "Toggle buffer identification between default and buffer-file-truename."
  (interactive)
  (make-variable-buffer-local 'old-buffer-identification)
  (make-variable-buffer-local 'mode-line-buffer-identification)
  (if
      (equal mode-line-buffer-identification buffer-file-truename)
      (progn 
(setq mode-line-buffer-identification
     old-buffer-identification)
(setq old-buffer-identification buffer-file-truename)
)
    (setq old-buffer-identification mode-line-buffer-identification)
    (setq mode-line-buffer-identification buffer-file-truename)
    )
  (redraw-display)
  )
(global-set-key "\C-x8" 'toggle-buffer-identification)
(global-set-key [f8] 'toggle-buffer-identification)

;; Case sensitive or not?
(defun toggle-case-fold-search ()
  "Toggles variable case-fold-search between nil and non-nil."
  (interactive)
  (if case-fold-search
      (setq case-fold-search nil)
    (setq case-fold-search t)
    )
  (message "Variable case-fold-search is now %s" case-fold-search)
  )
(global-set-key [67108906] 'toggle-case-fold-search) ; Control-*

;; Self-descriptive
(defun append-current-buffer-to-file (&optional filename)
  "Append the contents of the current buffer to a named file.

Simon Hawkin <cema@cs.umd.edu> ca.1995"
  (interactive "@FAppend to file ")
  (save-excursion
    (if filename
(append-to-file (point-min) (point-max) filename)
      )
    ))
(global-set-key "\C-c\C-a" 'append-current-buffer-to-file)

;;
(global-set-key [f11] 'font-lock-mode)
(global-set-key "\C-cf" 'font-lock-mode)
