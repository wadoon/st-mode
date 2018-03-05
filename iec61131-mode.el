;;; iec61131-mode.el --- A mode for Programing Lanuguages of IEC61131-3
;;;
;;;
;;;
;;;
;;; Commentary:
;;;

;;; Code:

(require 'rx)

(defvar iec61131-mode-map nil
  "Key map for 'iec61131-mode`")

(setq iec61131-mode-map
      (let ((map (make-keymap)))
	(define-key map "\C-j" 'newline-and-indent)
	map))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.st\\'" . iec61131-mode))


(defun iec61131-regex-endswith (seq)
  ""
  (let ((words (regexp-opt seq 'words)))
    (concat ".*" words "$")))

(defun iec61131-regex-startswith (seq)
  ""
  (let ((words (regexp-opt seq 'words)))
    (concat "[ \t]*" words ".*")))


(defvar iec61131-indent-regex+1 nil "Regex for indentation increment.")
(setq iec61131-indent-regex+1
      (concat
       (iec61131-regex-endswith  '("DO" "THEN" "ELSE"))
       "\\|"
       (iec61131-regex-startswith
	'("FUNCTION_BLOCK" "PROGRAM" "CONFIGURATION" "VAR"
	  "VAR_INPUT" "VAR_OUTPUT"))))

(defvar iec61131-indent-regex-1 nil "Regex for indentation decrement.")
(setq iec61131-indent-regex-1
      "[ \t]*\\<\\(END_.*\\|ELSE\\)\\>[ \t]*")


(defvar iec61131-keywords nil "Keywords to be highlighted.")
(setq iec61131-keywords
      (list "ANY" "ANY_BIT" "ANY_DATE" "ANY_DERIVED" "VAR_OUTPUT"
	    "ANY_ELEMENTARY" "ANY_INT" "ANY_NUM" "ANY_REAL"
	    "ANY_STRING" "ARRAY" "BOOL"
	    "BYTE" "DATE_AND_TIME" "DINT" "DWORD"
	    "INT" "LINT"
	    "LREAL" "LWORD" "REAL" "SINT" "STRING"
	    "TIME" "TIME_OF_DAY" "TOD" "UDINT" "UINT"
	    "ULINT" "USINT" "WORD" "WSTRING" "AT"
	    "BY" "CASE" "COLON" "CONFIGURATION"
	    "CONSTANT" "DATE" "DO" "DT"
	    "ELSE" "ELSEIF" "END_CASE" "END_CONFIGURATION"
	    "END_FOR" "END_FUNCTION" "END_FUNCTION_BLOCK"
	    "END_IF" "END_PROGRAM" "END_REPEAT" "END_RESOURCE"
	    "END_STRUCT" "END_TYPE" "END_VAR" "END_WHILE"
	    "EXIT" "FOR" "FUNCTION" "FUNCTION_BLOCK"
	    "F_EDGE" "IF" "INTERVAL" "NIL"
	    "NON_RETAIN" "OF" "ON" "PRIORITY" "PROGRAM"
	    "READ_ONLY" "READ_WRITE" "REPEAT" "RESOURCE"
	    "RETAIN" "RETURN" "RIGHT_ARROW" "R_EDGE"
	    "SINGLE" "STRUCT" "TASK" "THEN" "TO"
	    "TYPE" "UNTIL" "VAR" "VAR_ACCESS" "VAR_CONFIG"
	    "VAR_EXTERNAL" "VAR_GLOBAL" "VAR_INPUT" "VAR_IN_OUT"
	    "VAR_TEMP" "WHILE" "WITH"))


(defvar iec61131-multi-line-comment-regex nil
  "Regex for multi-line comments.")
(defvar iec61131-single-line-comment-regex nil
  "Regex for single-line comments //.")

(setq iec61131-single-line-comment-regex
      (rx (group "//" (?? not-newline) "\n")))

(setq iec61131-multi-line-comment-regex
      (rx (or (group "/*" (?? anything) "*/")
	      (group "(*" (?? anything) "*)"))))

(defvar iec61131-string-regex nil
  "Regex for string literals.")

(setq iec61131-string-regex
      (rx (or (group "\"" (?? anything) "\"")
	      (group ?' (?? anything) ?'))))

(defvar iec61131-time-regex
  nil "\\(TIME\\|T\\)[#]\\([0-9_]+\\(\\.[0-9]+\\)?\\(ms\\|s\\|m\\|h\\|d\\)\\)+.")

(setq iec61131-time-regex
      (rx word-start (or "TIME" "T") "#" (group (one-or-more (one-or-more digit)
					   (opt ?. (one-or-more digit))
					   (or "ms" ?s ?h ?m ?d))) word-end))

(defvar iec61131-date-regex nil "")
(setq iec61131-date-regex
      (rx word-start (or "DATE" "D") "#"
	  (group (repeat 4 digit) ?- (repeat 2 digit) ?- (repeat 2 digit)) word-end))

(defvar iec61131-datetime-regex nil)
(setq iec61131-datetime-regex
      (rx word-start (or "DATE_AND_TIME" "DT") ?#
	  (group (repeat 4 digit) ?- (repeat 2 digit) ?- (repeat 2 digit))
	  ?: (repeat 2 digit) ?: (repeat 2 digit) ?: (repeat 2 digit) word-end))

(defvar iec61131-bool-regex nil)
(setq iec61131-bool-regex (rx word-start (or "TRUE" "FALSE" "BIT#1" "BIT#0") word-end))

(defvar iec61131-font-lock-keywords "" nil)
(setq iec61131-font-lock-keywords
  `(
    (,iec61131-multi-line-comment-regex . font-lock-comment-face)
    (,iec61131-string-regex . font-lock-constant-face)
    (,iec61131-time-regex . font-lock-constant-face)
    (,iec61131-datetime-regex . font-lock-constant-face)
    (,iec61131-date-regex . font-lock-constant-face)
    ("\\(TIME_OF_DAY\\|TOD\\)#[012][0-9]:[0-5][0-9]:[0-5][0-9]\\(\\.[0-9]\\{,3\\}\\)"
     . font-lock-constant-face)
    ("\\<.*#.*\\>" . font-lock-constant-face)
    ("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
    (,iec61131-bool-regex  . font-lock-constant-face)
    (,(concat "\\<" (regexp-opt iec61131-keywords) "\\>") . font-lock-builtin-face)))


(defun iec61131-indent-line ()
  "Identation function for iec61131-mode."
  (interactive)
  (beginning-of-line)
  (let ((not-indented t)
	cur-indent)
    (setq cur-indent
	  (save-excursion
	    (cond
	     ;; Beginning of the buffer => no indentation
	     ((bobp) 0)
	     ;; current line is deindentation
	     ((looking-at-p iec61131-indent-regex-1)
	      (forward-line -1) ;; do not understand this
	      (- (current-indentation) tab-width))
	     (t
	      (forward-line -1)
	      (if (looking-at-p iec61131-indent-regex+1)
		  (+ (current-indentation) tab-width)
		(current-indentation))))))
    (indent-line-to cur-indent)))

(defvar iec61131-mode-syntax-table nil
  "IEC 61131 Syntax Table.")

(setq iec61131-mode-syntax-table
      (let ((table (make-syntax-table)))
	(modify-syntax-entry ?_ "w" table)
	(modify-syntax-entry ?/ ". 12b" table)
	(modify-syntax-entry 40 ". 1n" table) ;; (
	(modify-syntax-entry 41 ". 4n" table) ;; )
	(modify-syntax-entry ?* ". 23n" table)
	(modify-syntax-entry ?\n "> b" table)
	table))

(defun iec61131-insert-if (&optional condition then else)
  (interactive)
  (let ((condition (read-string "Condition:" condition))
	(then (read-string "Then:" then))
	(else (read-string "Else:" else)))
    (insert "IF ")
    (insert condition)
    (insert "THEN\n")
    (insert then)
    (insert "\nELSE\n")
    (insert else)
    (insert "END_IF")))


(define-derived-mode
  iec61131-mode fundamental-mode
  "IEC 61131"
  "A major mode for editing Structured Text files after IEC 61131-3"
  :syntax-table iec61131-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local font-lock-defaults
              '(iec61131-font-lock-keywords nil t)) ;set CASE-FOLD t
  (setq-local indent-line-function 'iec61131-indent-line))

(provide 'iec61131-mode)

;;; iec61131-mode.el ends here
