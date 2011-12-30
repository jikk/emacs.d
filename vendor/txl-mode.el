;;; txl-mode.el -- major mode for editing TXL programs and grammars.
;; Markus Stoy (mstoy@gmx.de), Rostock (Germany), November/December 2003.

;; Installation (only tested under XEmacs-21.4 for Linux and WindowsXP):
;  - put this file into directory where Emacs can find it (within load-path)
;  - add following lines to Emacs init file (.emacs or init.el or maybe something else)
;  (require 'txl-mode)
;  (setq auto-mode-alist (cons (quote ("\\.\\([tT]xl\\|[gG]rm\\|[gG]rammar\\|[rR]ul\\(es\\)?\\|[mM]od\\(ule\\)?\\)$" . txl-mode)) auto-mode-alist))

;; Features:
;  - syntax highlighting (with font-lock-mode)
;  - automatic indentation according to TXL style guide (perhaps stil buggy...)
;  - compile/debug/run TXL program from within Emacs
;  - comment/uncomment regions (useful since TXL doesn't have block comments)
;  - insert skeletion rules/functions/defines, find and insert matching end's
;  - abbreviations for keywords (with abbrev-mode; scroll down to see a list)
;  - TXL submenu which contains all new functions and their keyboard shortcuts

;; Wish list:
;  - navigation (jump to nonterminal/function/rule under cursor, next/previous nonterminal/function/rule, ...)
;  - remember last entered input file and use this as default value for next run/debug
;  - use comint for run/debug/compile instead of simple shell-command? (which looks ugly under Windows)

;; Known bugs:
;  - '% is highlighted as comment
;  - compile and debug don't work under Windows

;; Oct 16 2008, Ivan N. Veselov <veselov@gmail.com>
;; - added compatibility with Emacs (fixed GNU Emacs/XEmacs compatibility issues
;;   with font-lock-defaults and set-keymap-name).
;;   Tested with GNU Emacs 22.3.1.

;;; Code: ----------------------------------------------------------------------

(defvar txl-mode-hook nil "Normal hook run when entering TXL mode.")

; syntax table -----------------------------------------------------------------
(defvar txl-mode-syntax-table nil "Syntax table used while in TXL mode.")
(if txl-mode-syntax-table ()
  (setq txl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?' "'" txl-mode-syntax-table)   ; apostrophe quotes
  (modify-syntax-entry ?_ "w" txl-mode-syntax-table)   ; underscore is part of words
  (modify-syntax-entry ?% "<" txl-mode-syntax-table)   ; percent starts comments
  (modify-syntax-entry ?\n ">" txl-mode-syntax-table)) ; newline ends comments

; syntax highlighting ----------------------------------------------------------
(defvar txl-mode-keywords nil "Keywords for font-lock-mode used while in TXL mode.")
(if txl-mode-keywords ()
  (setq txl-mode-keywords
	(list
	 ;; preprocessor directives
	 (list "^[ 	]*#[a-z][a-z]*" 0 'font-lock-preprocessor-face)
	 ;; quoted literal symbol
	 (list "'[^]	 ]+" 0 'font-lock-reference-face)
	 ;; builtin rules (with parameters)
	 (list "\\[\\([\\+-\\*/:#_\\.^,=><\\$]\\|div\\|rem\\|index\\|length\\|select\\|head\\|tail\\|~=\\|>=\\|<=\\|grep\\|quote\\|unquote\\|parse\\|unparse\\|reparse\\|read\\|write\\|fget\\|getp\\|fput\\|putp\\|fputp\\|fclose\\|message\\|pragma\\|quit\\|system\\|pipe\\)[ 	]+" 1 'font-lock-builtin-face)
	 ;; builtin rules (without parameters) and predefined nonterminal types
	 (list "\\[\\(!\\|get\\|put\\|print\\|printattr\\|debug\\|breakpoint\\|id\\|number\\|stringlit\\|charlit\\|comment\\|space\\|newline\\|upperlowerid\\|upperid\\|lowerupperid\\|lowerid\\|floatnumber\\|decimalnumber\\|integernumber\\|empty\\|key\\|token\\|any\\)\\]" 1 'font-lock-builtin-face)
	 ;; formatting tokens (without number)
	 (list "\\[\\(NL\\|IN\\|EX\\|TAB\\|SP\\|SPOFF\\|SPON\\|KEEP\\)\\]" 0 'font-lock-comment-face)
	 ;; formatting tokens (with number)
	 (list "\\[\\(IN\\|EX\\|TAB\\|SP\\)_[1-9][0-9]*\\]" 0 'font-lock-comment-face)
	 ;; type keywords
	 (list "\\<\\(attr\\|list\\|opt\\|repeat\\|see\\)\\>" 1 'font-lock-type-face)
	 ;; other keywords
	 (list "\\<\\(all\\|assert\\|by\\|comments\\|compounds\\|construct\\|deconstruct\\|define\\|each\\|end\\|export\\|external\\|function\\|import\\|include\\|keys\\|match\\|not\\|redefine\\|replace\\|rule\\|skipping\\|tokens\\|where\\)\\>" 1 'font-lock-keyword-face)
	 ;; number
	 (list "\\<[0-9]+\\([.][0-9]+\\)?\\([eE][-+]?[0-9]+\\)?\\>" 0 'font-lock-reference-face)
	 )))

; abbreviations ----------------------------------------------------------------
(defvar txl-mode-abbrev-table nil "Abbrev table used while in TXL mode.")
(if txl-mode-abbrev-table ()
  (setq txl-mode-abbrev-table (make-abbrev-table))
  (define-abbrev txl-mode-abbrev-table "ass" "assert" nil)
  (define-abbrev txl-mode-abbrev-table "com" "comments" nil)
  (define-abbrev txl-mode-abbrev-table "cmp" "compounds" nil)
  (define-abbrev txl-mode-abbrev-table "con" "construct" nil)
  (define-abbrev txl-mode-abbrev-table "dec" "deconstruct" nil)
  (define-abbrev txl-mode-abbrev-table "def" "define" nil)
  (define-abbrev txl-mode-abbrev-table "exp" "export" nil)
  (define-abbrev txl-mode-abbrev-table "ext" "external" nil)
  (define-abbrev txl-mode-abbrev-table "fun" "function" nil)
  (define-abbrev txl-mode-abbrev-table "imp" "import" nil)
  (define-abbrev txl-mode-abbrev-table "inc" "include" nil)
  (define-abbrev txl-mode-abbrev-table "red" "redefine" nil)
  (define-abbrev txl-mode-abbrev-table "rpt" "repeat" nil)
  (define-abbrev txl-mode-abbrev-table "rep" "replace" nil)
  (define-abbrev txl-mode-abbrev-table "ski" "skipping" nil)
  (define-abbrev txl-mode-abbrev-table "tok" "tokens" nil))

; keyboard shortcuts -----------------------------------------------------------
(defvar txl-mode-map nil "Keymap for TXL mode.")
(if txl-mode-map ()
  (setq txl-mode-map (make-sparse-keymap))
  (if (functionp 'set-keymap-name)
      (set-keymap-name txl-mode-map 'txl-mode-map))
  (define-key txl-mode-map "\C-cc" 'comment-region)
  (define-key txl-mode-map "\C-cu" 'txl-mode-uncomment-region)
  (define-key txl-mode-map "\C-cd" 'txl-mode-insert-define)
  (define-key txl-mode-map "\C-cf" 'txl-mode-insert-function)
  (define-key txl-mode-map "\C-cr" 'txl-mode-insert-rule)
  (define-key txl-mode-map "\C-c\C-e" 'txl-mode-insert-end)
  (define-key txl-mode-map "\C-c\C-c" 'txl-mode-compile)
  (define-key txl-mode-map "\C-c\C-d" 'txl-mode-debug)
  (define-key txl-mode-map "\C-c\C-r" 'txl-mode-run)
  (define-key txl-mode-map "\C-i" 'txl-mode-indent-line)
  (define-key txl-mode-map "\C-C\C-i" 'indent-region))

; menubar ----------------------------------------------------------------------
(defvar txl-mode-menubar-menu nil "TXL menu.")
(if txl-mode-menubar-menu ()
  (setq txl-mode-menubar-menu
	'("T%_XL"
	  ["Ru%_n " txl-mode-run :suffix (concat (txl-mode-get-name nil) "...")]
	  ["De%_bug " txl-mode-debug :suffix (concat (txl-mode-get-name nil) "...")]
	  ["Com%_pile " txl-mode-compile :suffix (txl-mode-get-name nil)]
	  "--"
	  ["%_Indent Region" indent-region :active (region-exists-p)]
	  ["%_Comment Region"    comment-region :active (region-exists-p)]
	  ["%_Uncomment Region"  txl-mode-uncomment-region (region-exists-p)]
	  "--"
	  ["Insert %_Define" txl-mode-insert-define]
	  ["Insert %_Function" txl-mode-insert-function]
	  ["Insert %_Rule" txl-mode-insert-rule]
	  ["%_End Block" txl-mode-insert-end :active (txl-mode-block)]
	  "--"
	  ["Use %_Abbreviations" (setq abbrev-mode (not abbrev-mode))
	   :style toggle :selected abbrev-mode]
)))

;(defvar txl-mode-input-file "nil" "Name of the last given input file for TXL programs.")


(defun txl-mode () ; -----------------------------------------------------------
  "Major mode for editing TXL programs and grammars.
\\{txl-mode-map}
Turning on TXL mode runs the normal hook `txl-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'txl-mode-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (set-syntax-table txl-mode-syntax-table)
  (if (featurep 'xemacs)
      (setq font-lock-keywords txl-mode-keywords) ;; XEmacs
    (setq font-lock-defaults '(txl-mode-keywords nil nil nil nil))) ;; Emacs
  (setq local-abbrev-table txl-mode-abbrev-table)
  (setq abbrev-mode t)
  (use-local-map txl-mode-map)
  (setq major-mode 'txl-mode
	mode-name "TXL")
  (if (and (featurep 'menubar)
           current-menubar)
      (progn
	(set-buffer-menubar current-menubar)
	(add-submenu nil txl-mode-menubar-menu)))
  (run-hooks 'txl-mode-hook))

; code templates ---------------------------------------------------------------

(defun txl-mode-insert-define ()
  "Insert an empty nonterminal definition."
  (interactive)
  (insert "\ndefine \nend define")
  (end-of-line 0))

(defun txl-mode-insert-function ()
  "Insert an empty function."
  (interactive)
  (insert "\nfunction \n    replace\n    by\nend function")
  (end-of-line -2))

(defun txl-mode-insert-rule ()
  "Insert an empty rule."
  (interactive)
  (insert "\nrule \n    replace\n    by\nend rule")
  (end-of-line -2))

(defun txl-mode-insert-end ()
  "Insert matching end for define, rule, function etc."
  (interactive)
  (let ((current-block (txl-mode-block)))
    (if current-block
	(insert (concat "end " current-block "\n\n"))
      (message "Not inside TXL block."))))

(defun txl-mode-uncomment-region ()
  "Uncomment region."
  (interactive)
  (comment-region (region-beginning) (region-end) -1))

; compile, debug and run TXL programs ------------------------------------------

(defun txl-mode-compile ()
  "Compile TXL program."
  (interactive)
  (shell-command (concat "txlc " (txl-mode-get-name t)) "*TXL Compilation*"))

(defun txl-mode-debug (input-file)
  "Ask input file from user and debug TXL program."
  (interactive "fInput file: ")
  (shell-command (concat "txldb " input-file " " (txl-mode-get-name t) " &") "*TXL Debug*")
  (other-window 1)
  (end-of-buffer))

(defun txl-mode-run (input-file)
  "Ask input file from user and run TXL program."
  (interactive "fInput file: ")
;  (setq txl-mode-input-file input-file)
    (shell-command (concat "txl " input-file " " (txl-mode-get-name t)) "*TXL Output*"))

(defun txl-mode-get-name (full)
  "If buffer file name has ending used by TXL, return base name
and ending `.Txl', otherwise return unchanged. The argument controls whether full path
is included or not."
  (let* ((txl-file-split) (txl-file-end))
    (if full
	(setq txl-file-split (split-string-by-char buffer-file-name ?.))
      (setq txl-file-split (split-string-by-char (buffer-name) ?.)))
    (setq txl-file-end (car (last txl-file-split)))
    (if (string-match "[tT]xl\\|[gG]rm\\|[gG]rammar\\|[rR]ul\\(es\\)?\\|[mM]od\\(ule\\)?" txl-file-end)
	(setq txl-file-end "Txl"))
    (concat (mapconcat 'identity (butlast txl-file-split) ".") "." txl-file-end)))

; automatic indentation --------------------------------------------------------

(defun txl-mode-indent-line (&optional whole-exp)
  "Indent current line as TXL code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (let ((indent (txl-mode-indent-level))
	(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (zerop (- indent (current-column)))
	nil
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

(defun txl-mode-indent-level ()
  "Compute TXL indentation level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ;; beginning of buffer: do not indent
     ((bobp) 0)
     ;; block start/end delimiters: do not indent
     ((looking-at "\\(define\\|redefine\\|keys\\|compounds\\|comments\\|tokens\\|function\\|rule\\|end\\)\\>") 0)
     ;; block-keywords: indent 4
     ((looking-at "\\(import\\|export\\|replace\\|by\\|match\\|deconstruct\\|construct\\|where\\|assert\\|skipping\\)\\>") 4)
     ;; alternative-sperator: indent 4
     ((looking-at "|") 4)
     ;; comments: within blocks indent 4, outside do not indent
     ((looking-at "%") (if (txl-mode-block) 4 0))
     ;; function calls: line up with previous
     ((looking-at "\\[") (txl-mode-indent-fun-level))
     ;; all other stuff: within blocks indent 8, outside do not indent
     (t (if (txl-mode-block) 8 0)))))

(defun txl-mode-block ()
  "If point is currently inside TXL block, return its name (define, rule, function, etc),
otherwise return nil."
  (interactive)
  (save-excursion
    (let ((searching t) (within-block nil))
      (while searching
	(beginning-of-line 0)
	(if (bobp)
	    (setq searching nil))
	(skip-chars-forward " \t")
	(if (looking-at "\\(define\\|redefine\\|keys\\|compounds\\|comments\\|tokens\\|function\\|rule\\)\\>")
	    (setq searching nil within-block (match-string 1)))
	(if (looking-at "end\\>")
	    (setq searching nil within-block nil)))
      within-block)))

(defun txl-mode-indent-fun-level ()
  "Compute indentation level of first function call on previous line.
If there is none, return 8 or 0, depending whether currently inside block."
  (save-excursion
    (end-of-line 0)
    (let ((eol (point)))
      (beginning-of-line)
      (skip-chars-forward "^\[" eol)
      (if (eolp)
	  (progn
	    (end-of-line 2)
	    (if (txl-mode-block) 8 0))
	(current-column)))))

(provide 'txl-mode)
;;; txl-mode.el ends here
