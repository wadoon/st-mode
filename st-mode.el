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

'("{*" "*}")

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
   '(".*#.*" . font-lock-constant-face)
   '("[0-9]+\\(\\.[0-9]+\\)?" . font-lock-constant-face)
   '("\\(TRUE\\|FALSE\\)" . font-lock-constant-face)
   `(,(concat "\\<" (regexp-opt st-keywords) "\\>")
     . font-lock-builtin-face)))

(defvar st-indent-words
  '())

(defun st-indent-line ()
  "."
  (interactive)
  (beginning-of-line)

  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          cur-indent)
      (if (looking-at "^[ \t]*END_")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent
                    (max 0
                         (- (current-indentation)
                            tab-width)))))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*END_")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at
                   (concat "[ \t]*"
                           (regexp-opt st-indent-words)))
                  (progn
                    (setq cur-indent
                          (+ (current-indentation)
                             tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))


(defconst st-indent-words
  (list "IF" "ELSE" "ELSEIF" "FUNCTION_BLOCK"
        "PROGRAM" "CONFIGURATION" "VAR" "VAR_INPUT" "VAR_OUTPUT"))

(defvar st-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

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
