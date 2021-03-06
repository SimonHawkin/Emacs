;;; 2008/03/31 Simon "Cema" Hawkin <Simon_Hawkin@yahoo.com>
;;; 2004/09/29 Simon "Cema" Hawkin <Simon_Hawkin@yahoo.com>
;;; 2004/07/28 Simon "Cema" Hawkin <simon.hawkin@gmail.com>
;;; 2004/06/17 Simon "Cema" Hawkin <shawkin@locateplus.com>
;;; 2004/02/03 Simon"Cema" Hawkin <SimonHawkin@users.sf.net>
;;; 2003/08/25 Simon "Cema" Hawkin <SimonHawkin@yandex.ru>
;;; Borrowing from the version accumulated in years
;;; what I feel like I really need.

;; Less verbiage at startup
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-message t)
(setq inhibit-startup-echo-area-message t)

;; Favorite colors, less strain on my eyes
(set-background-color "black")
(set-foreground-color "gainsboro") ; antiqueWhite
(set-cursor-color "turquoise")
(set-mouse-color "goldenRod")

;; Semi-obsolete i18n
;(standard-display-european 1)

;; A more natural place to paste
(setq mouse-yank-at-point t)

;; Minimal auto formatting, at least
(setq default-major-mode 'text-mode)

;; Do not backup
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-inhibited t)

;; Syntax
(modify-syntax-entry ?\t " "  ) ; TAB is whitespace
(modify-syntax-entry ?{ "(}" ) ; {...} paired
(modify-syntax-entry ?} "){" )
(modify-syntax-entry ?< "(>" ) ; <...> paired
(modify-syntax-entry ?> ")<" )

;; Favorite short keys
;; Commands that do not have short keys by default (but should, perhaps)
(global-set-key [f12] 'repeat-complex-command)
;
(global-set-key [f5] 'query-replace-regexp)
(global-set-key [S-f5] 'replace-regexp)
;
(global-set-key "\r" 'newline-and-indent) ; nice behavior
(global-set-key "\e\r" 'newline) ; just so I still have it
;
(global-set-key "\C-x " 'fixup-whitespace)
;
(global-set-key "\C-c," 'tags-search)
;
(global-set-key [f3] 'goto-line)
;
(global-set-key [f9] 'save-buffer)

;;; MY LITTLE PIECES OF CODE

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

;; LiveJournal
(load-file-if-readable "c:/Emacs/.emacs.lj.el")

;; C, C++ customization
(load-file-if-readable "c:/Emacs/.emacs.c,c++.el")

(setq auto-mode-alist
      (append '(("\\.java$" . c-mode)
		("\\.cs$" . c-mode)
		) auto-mode-alist)
      )

;; Perl customization
(load-file-if-readable "c:/Emacs/.emacs.perl.el")

;; Visual Basic
(load-file-if-readable "c:/Emacs/visual-basic-mode.el")

(setq auto-mode-alist
      (append '(("\\.asp$" . visual-basic-mode)
		) auto-mode-alist)
      )

;; WEB LOG files
(load-file-if-readable "c:/Emacs/web-log.el")

;; Visual Basic mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" . 
				 visual-basic-mode)) auto-mode-alist))

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
(global-set-key "\C-x\C-a" 'append-current-buffer-to-file)

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

;; Highlight mode, common for X and MS Windows
;; A bit outdated now
(cond
 (window-system ; ... ... ... ... ... ...
  
  (require 'hilit19)

  (defun toggle-highlight-visible ()
    "Toggle highlighting of the visible portion of the current buffer.

Requires the hilit19 package installed. (Standard in Emacs 19+.)

Simon Hawkin <cema@cs.umd.edu> ca.1994"
    (interactive)
    (save-excursion
      (if (equal hilit-auto-rehighlight nil)
	  (progn
					;(message "Highlighting...")
	    (setq hilit-auto-rehighlight 'visible)
	    (run-hook-with-args 'cema-toggle-highlight-hook)
	    (call-interactively 'hilit-repaint-command)
	    )
					;(message "Un-highlighting...")
	(setq hilit-auto-rehighlight nil)
	(hilit-unhighlight-region (point-min) (point-max))
	(message nil)
	)
      )
    )
  (global-set-key [f11] 'toggle-highlight-visible)
  (global-set-key [C-f11] 'hilit-rehighlight-buffer)
))

;;; MODES
;;; AND MODE IMITATIONS

;;; WIN BATCH FILE
(defun batch-win-mode ()
  "Imitation mode. Used only for coloring now."
  (interactive)
  (text-mode)
  (setq mode-name "Batch")
  (setq major-mode 'batch-win-mode)
  )

(setq auto-mode-alist
      (append '(("\\.bat$" . batch-win-mode)
		) auto-mode-alist)
      )

;;; CSS
(defun css-mode ()
  "Imitation mode. Used only for coloring now."
  (interactive)
  (text-mode)
  (setq mode-name "CSS")
  (setq major-mode 'css-mode)
  )

(setq auto-mode-alist
      (append '(("\\.xul$" . html-mode)
		("\\.rdf$" . html-mode)
		("\\.css" . css-mode)
		("\\.wml" . css-mode)
		) auto-mode-alist)
      )

;;; FAVORITE COLORS
;;; AND SYNTAX

;; Colors
(cond
 (window-system

  (require 'hilit19)
  
  (setq hilit-background-mode 'dark
	hilit-auto-highlight nil
	hilit-auto-rehighlight 'nil ; or 'visible
	hilit-inhibit-rebinding t)

  ;; Some better colors
  (hilit-translate
   comment    'dimGray		; 
   comment-javadoc    'blueViolet	; /** Java */
   string     'peru		; goldenrod
   keyword    'yellowGreen	; lightSeaGreen tan aquamarine
   command-silent 'blueViolet	; batch file @...
   command-echo 'peru		; echo ...
   formula    'Gold		; Dark goldenrod is too dark
   label      'orangeRed	; Underlined is messy, bold
				; helps to find them
   include    'turquoise       	; C: #include, #if etc.
   defun      'maroon		; C:?
   define     'blueViolet	; C: #define.
   decl       'maroon		; C: typedef=grey. Elisp: defvar=maroon
   crossref   'Goldenrod	; DarkGoldenrod is too dark
   msg-quote  'gray		; almost like comment
   msg-header 'orange           ; better for eyes
   msg-subject 'purple          ; better for eyes
   msg-from   'purple           ; make it similar to subject
   rmail-header	'magenta	; Want it visible on light green
				; background
   integer	'coral		; can be negative. Choose modes.
   integer16	'coral		; 0x10aF
   variable	'green
   html-special	'magenta	; &nbsp; et cetera
   css-tag	'yellowGreen	;
   css-attribute	'purple	;
   pod-command	'blueViolet	; Perl POD document
   text-para-head	'orange	; head: the rest... in a paragraph
   diff-delim	'golfenrod	; 
   )

  ))

;; Some useful "faces"
(cond
 (window-system
  
  (require 'hilit19)
  
  ;; text mode
  (hilit-add-pattern "^*> .*$" "" 'msg-quote 'text-mode t) ; in the
					; beginning
  (hilit-add-pattern
   "^\\<\\(\\w\\|-\\|_\\)+\\>:"
   "" 'text-para-head 'text-mode)
  ;;
  ;; rmail mode
  (hilit-add-pattern
   "." "" 'rmail-header 'rmail-summary-mode t)
  ;;
  ;; LaTeX mode
  (hilit-add-pattern "\\(\\\\\\w+\\)" "" 'keyword 'latex-mode t)
  (hilit-add-pattern "\\\\item" "" 'define 'latex-mode t)
  (hilit-add-pattern
   "\\\\closing\\|\\\\opening\\|\\\\signature\\|\\\\address"
   "" 'define 'latex-mode t) ; letter style
  (hilit-add-pattern
   "\\\\\\(begin\\|end\\)[	 \n]*{"
   "}" 'define 'latex-mode)
  ;(hilit-add-pattern
  ; "\\\\\\(bigskip\\|centerline\\)"
  ; "" 'keyword 'latex-mode t)
  ;(hilit-add-pattern "\\\\\\(begin\\|end\\)" "" 'keyword 'latex-mode)
  ;;
  ;; C mode
  (hilit-add-pattern
   "\\<\\(-\\|\\)[0-9]+" "" 'integer 'c-mode
    t)					; prepend
  (hilit-add-pattern
   "\\<0\\(x\\|X\\)[0-9a-fA-F]+" "" 'integer16 'c-mode
   t)					; prepend
  (hilit-add-pattern
   "'\\(\\\\.\\|.\\|\\\\[0-9]*\\|\\\\[Xx][0-9]*\\)'"
   "" 'string 'c-mode t)
  ;;
  ;; Perl mode
;;   (hilit-add-pattern
;;    "\\<foreach\\>\\|\\<for\\>\\|\\<next\\>\\|\\<last\\>\\|\\<if\\>\\|\\<while\\>\\|\\<unless\\>\\|\\<for\\>\\|\\<my\\>"
;;    "" 'keyword 'cperl-mode t)
;;   (hilit-add-pattern "\\<sub\\> *\\<.*\\>" "" 'defun 'cperl-mode )
;;   (hilit-add-pattern "\\<use\\>.*$" "" 'include 'cperl-mode t)
;;   (hilit-add-pattern "\"[^\"]*\\\"" "" 'string 'cperl-mode t)
;;   (hilit-add-pattern "\\\"[^\"]*\"" "" 'string 'cperl-mode t)
;;   (hilit-add-pattern "#.*$" "" 'comment 'cperl-mode t)
;;   (hilit-add-pattern "^=" "$" 'pod-command 'cperl-mode t)
  ;;
  ;; Shell-script-mode
  (hilit-add-pattern
   "$#" "" 'variable 'sh-mode t) ; 
  (hilit-add-pattern
   "#" "$" 'comment 'sh-mode) ; in the end
  ;;
  ;; Emacs-Lisp mode
  (hilit-add-pattern
   "global-set-key\\|add-hook" "" 'define 'emacs-lisp-mode) ; in the end
  ;;
  ;; HTML mode. This is new.
  (hilit-add-pattern "<" ">" 'keyword 'html-mode t)
;  (hilit-add-pattern "<[^>]*>" ">" 'keyword 'html-mode t)
  (hilit-add-pattern "\"[^\"]*\"" "" 'string 'html-mode t)
  (hilit-add-pattern "&[-_a-zA-Z0-9]+;" "" 'html-special 'html-mode t)
  (hilit-add-pattern "&#[0-9]+;" "" 'html-special 'html-mode t)
  (hilit-add-pattern "&#x[0-9a-zA-Z]+;" "" 'html-special 'html-mode t)
  (hilit-add-pattern "<!--" "-->" 'comment 'html-mode t)
  ;;
  ;; CSS mode. This is new.
  (hilit-add-pattern "/\\*" "\\*/" 'comment 'css-mode)
  (hilit-add-pattern "[-a-z]+:" "" 'css-attribute 'css-mode) ; this line
					; before... 
  (hilit-add-pattern "^[][_a-zA-Z0-9 ,:\\.-]+\{" "" 'css-tag 'css-mode t) ;
					; ...this one 
  (hilit-add-pattern "}" "" 'css-tag 'css-mode)
  ;;
  ;; Batch win mode. This is new.
  (hilit-add-pattern "^[Rr][Ee][Mm]" "$" 'comment 'batch-win-mode)
  (hilit-add-pattern "^'" "$" 'comment 'batch-win-mode)
  (hilit-add-pattern "^:" "$" 'label 'batch-win-mode)
  (hilit-add-pattern "^@" "$" 'command-silent 'batch-win-mode)
  (hilit-add-pattern
   "\\<\\(if\\|goto\\|set\\|do\\|shift\\|pause\\|exit\\|start\\|for\\|echo\\|copy\\|title\\|mkdir\\|rmdir\\|call\\|date\\|time\\|cd\\)\\>"
   "" 'keyword 'batch-win-mode)
  (hilit-add-pattern "echo" "$" 'command-echo 'batch-win-mode)
  ;;
  ;; Integers.
  (hilit-add-pattern
   "\\<\\(-\\|\\)[0-9]+" "" 'integer)	; append
  ;;
  ;; Diff mode
  (hilit-add-pattern
   "^---" "$" 'diff-delim) ; append
  ;;
))

;;; THE REST IS MODIFIED AUTOMAGICALLY

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(case-fold-search t)
; '(current-language-environment "Cyrillic-ALT")
 '(default-input-method "cyrillic-yawerty")
 '(global-font-lock-mode nil nil (font-lock))
 '(save-place t nil (saveplace))
 '(show-paren-mode nil t)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

;;; Version 22
(tool-bar-mode 0)

;;; FINAL MESSAGE
(message "Emacs initialized for %s%s%s%s."
	 (cond (user-full-name) "")
	 " <"
	 (cond (user-mail-address) (user-login-name)
	       (user-real-login-name) "unknown user")
	 ">")

;;; eof ;;;