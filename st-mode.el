;;; st-mode.el --- A mode for StructuredText after IEC61131-3
;;;

;;;


;;; Commentary:
;;

;;; Code:

(defvar st-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for st-mode.")

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.st\\'" . st-mode))


(defun st-regex-endswith (seq)
  (let ((words (regexp-opt seq 'words)))
    (concat ".*" words "$")))

(defun st-regex-startswith (seq)
  (let ((words (regexp-opt seq 'words)))
    (concat "[ \t]*" words ".*")))


(defvar st-indent-regex+1
  (concat
   (st-regex-endswith  '("DO" "THEN" "ELSE"))
   "\\|"
   (st-regex-startswith
    '("FUNCTION_BLOCK" "PROGRAM" "CONFIGURATION" "VAR"
      "VAR_INPUT" "VAR_OUTPUT"))))


(defvar st-indent-regex-1
  "[ \t]*\\<\\(END_.*\\|ELSE\\)\\>[ \t]*")


(defvar  st-keywords
  (list "ANY" "ANY_BIT" "ANY_DATE" "ANY_DERIVED" "VAR_OUTPUT"
        "ANY_ELEMENTARY" "ANY_INT"     "ANY_NUM"     "ANY_REAL"
        "ANY_STRING"     "ARRAY"     "BOOL"
        "BYTE"    "DATE_AND_TIME"    "DINT"    "DWORD"
        "INT"    "LINT"
        "LREAL"    "LWORD"    "REAL"    "SINT"    "STRING"
        "TIME"    "TIME_OF_DAY"    "TOD"    "UDINT"    "UINT"
        "ULINT"    "USINT"    "WORD"    "WSTRING"    "AT"
        "BY"    "CASE"    "COLON"    "CONFIGURATION"
        "CONSTANT" "DATE" "DO" "DT"
        "ELSE" "ELSEIF"     "END_CASE"    "END_CONFIGURATION"
        "END_FOR"    "END_FUNCTION"    "END_FUNCTION_BLOCK"
        "END_IF"    "END_PROGRAM"    "END_REPEAT"    "END_RESOURCE"
        "END_STRUCT"    "END_TYPE"    "END_VAR"    "END_WHILE"
        "EXIT"    "FOR"    "FUNCTION"    "FUNCTION_BLOCK"
        "F_EDGE"    "IF"    "INTERVAL"    "NIL"
        "NON_RETAIN"    "OF"     "ON"     "PRIORITY"     "PROGRAM"
        "READ_ONLY"     "READ_WRITE"     "REPEAT"     "RESOURCE"
        "RETAIN"     "RETURN"     "RIGHT_ARROW"     "R_EDGE"
        "SINGLE" "STRUCT"      "TASK"     "THEN"     "TO"
        "TYPE"     "UNTIL"     "VAR" "VAR_ACCESS"     "VAR_CONFIG"
        "VAR_EXTERNAL"     "VAR_GLOBAL"  "VAR_INPUT"     "VAR_IN_OUT"
        "VAR_TEMP"     "WHILE"     "WITH"))



(defvar st-comment-regex
  "\\((\\*.*\\*)\\)"
  "regex for marking StructuredText comments")

(defvar st-font-lock-keywords
  (list
   `(,st-comment-regex . font-lock-comment-face)
   '("\".*?\"" . font-lock-constant-face)
   '("'.*?'" . font-lock-constant-face)
   '("\\(TIME\\|T\\)[#]\\([0-9_]+\\(\\.[0-9]+\\)?\\(ms\\|s\\|m\\|h\\|d\\)\\)+"
     . font-lock-constant-face)
   '("\\(DT\\|DATE_AND_TIME\\)#[0-9]\\{4\\}-[01]?[0-9]-[0123][0-9]-[012][0-9]:[0-5][0-9]:[0-5][0-9]"
     . font-lock-constant-face)
   '("\\(D\\|DATE\\)#[0-9]\\{4\\}-[01]?[0-9]-[0123]?[0-9]"
     . font-lock-constant-face)
   '("\\(TIME_OF_DAY\\|TOD\\)#[012][0-9]:[0-5][0-9]:[0-5][0-9]\\(\\.[0-9]\\{,3\\}\\)"
     . font-lock-constant-face)
   '(\\<".*#.*\\>" . font-lock-constant-face)
   '("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
   '("\\(TRUE\\|FALSE\\)" . font-lock-constant-face)
   `(,(concat "\\<" (regexp-opt st-keywords) "\\>")
     . font-lock-builtin-face)))


(defun st-indent-line ()
  "."
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
	     ((looking-at-p st-indent-regex-1)
	      (forward-line -1) ;; do not understand this
	      (- (current-indentation) tab-width))
	     (t
	      (forward-line -1)
	      (if (looking-at-p st-indent-regex+1)
		  (+ (current-indentation) tab-width)
		(current-indentation))))))
    (indent-line-to cur-indent)))

(defvar st-mode-syntax-table nil
  "")

(setq st-mode-syntax-table
      (let ((table (make-syntax-table)))
	(modify-syntax-entry ?_ "w" table)
	(modify-syntax-entry ?/ ". 12b" table)
	(modify-syntax-entry 40 ". 1n" table) ;; (
	(modify-syntax-entry 41 ". 4n" table)
	(modify-syntax-entry ?* ". 23n" table)
	(modify-syntax-entry ?\n "> b" table)
	table))

(defun st-insert-if (&optional condition then else)
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
  st-mode
  fundamental-mode
  "Structured Text"
  "A major mode for editing Structured Text files after IEC 61131-3"
  :syntax-table st-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local font-lock-defaults
              '(st-font-lock-keywords))
  (setq-local indent-line-function 'st-indent-line))

(provide 'st-mode)

;;; st-mode.el ends here
