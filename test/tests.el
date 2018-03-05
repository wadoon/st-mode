;;;; iec61131-test.el --- Tests for markdown-mode

;; Test for iec61131-mode.

;; Definitons copied from https://github.com/jrblevin/markdown-mode/blob/master/tests/markdown-test.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:
(require 'ert)
(require 'cl-lib)

(defconst iec61131-test-dir
  (expand-file-name (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst iec61131-test-font-lock-function
  (if (and noninteractive (fboundp 'font-lock-ensure))
      #'font-lock-ensure #'font-lock-fontify-buffer))

(defmacro iec61131-test-string-mode (mode string &rest body)
  "Run BODY in a temporary buffer containing STRING in MODE."
  (declare (indent 2))
  `(let ((win (selected-window)))
     (unwind-protect
         (with-temp-buffer
           (set-window-buffer win (current-buffer) t)
           (erase-buffer)
           (funcall ,mode)
           (setq-default indent-tabs-mode nil)
           (insert ,string)
           (goto-char (point-min))
           (funcall iec61131-test-font-lock-function)
           (prog1 ,@body (kill-buffer))))))

(defmacro iec61131-test-file-mode (mode file &rest body)
  "Open FILE from `iec61131-test-dir' in MODE and execute BODY."
  (declare (indent 2))
  `(let ((fn (concat iec61131-test-dir ,file)))
     (save-window-excursion
       (with-temp-buffer
         (insert-file-contents fn)
         (funcall ,mode)
         (goto-char (point-min))
         (funcall iec61131-test-font-lock-function)
         ,@body))))

(defmacro iec61131-test-string (string &rest body)
  "Run BODY in a temporary buffer containing STRING in `iec61131-mode'."
  (declare (indent 1))
  `(iec61131-test-string-mode 'iec61131-mode ,string ,@body))
(def-edebug-spec iec61131-test-string (form body))

(defmacro iec61131-test-file (file &rest body)
  "Open FILE from `iec61131-test-dir' in `iec61131-mode' and execute BODY."
  (declare (indent 1))
  `(iec61131-test-file-mode 'iec61131-mode ,file ,@body))
(def-edebug-spec iec61131-test-file (form body))

(defmacro iec61131-test-temp-file (file &rest body)
  "Open FILE from `iec61131-test-dir' visiting temp file and execute BODY.
This file is not saved."
  (declare (indent 1))
  `(let ((fn (concat iec61131-test-dir ,file))
         (tmp (make-temp-file "iec61131-test" nil ".text"))
         buf)
     (save-window-excursion
       (unwind-protect
           (progn
             (setq buf (find-file tmp))
             (insert-file-contents fn)
             (iec61131-mode)
             (goto-char (point-min))
             (funcall iec61131-test-font-lock-function)
             ,@body
             (set-buffer-modified-p nil))
         (when (buffer-live-p buf) (kill-buffer buf))
         (delete-file tmp)))))
(def-edebug-spec iec61131-test-temp-file (form body))

(defun iec61131-test-report-property-range (begin end prop)
  "Report buffer substring and property PROP from BEGIN to END."
  (message "Buffer substring: %s" (buffer-substring begin (1+ end)))
  (message "Properties in range are as follows:")
  (dolist (loc (number-sequence begin end))
    (message "%d: %s" loc (get-char-property loc prop))))

(defun iec61131-test-range-has-property (begin end prop value)
  "Verify that range BEGIN to END has PROP equal to or containing VALUE."
  (let (vals fail-loc)
    (setq fail-loc
          (catch 'fail
            (dolist (loc (number-sequence begin end))
              (setq vals (get-char-property loc prop))
              (if (and vals (listp vals))
                  (unless (memq value vals)
                    (throw 'fail loc))
                (unless (eq vals value)
                  (throw 'fail loc))))))
    (when fail-loc
      (message "Testing range (%d,%d) for property %s equal to %s."
               begin end prop value)
      (message "Expected value (%s) not found in property (%s) at location %d" value prop fail-loc)
      (iec61131-test-report-property-range begin end prop))
    (should-not fail-loc)))

(defun iec61131-test-range-property-equals (begin end prop value)
  "Verify that range BEGIN to END has property PROP equal to VALUE."
  (let ((fail-loc
         (catch 'fail
           (dolist (loc (number-sequence begin end))
             (unless (eq (get-char-property loc prop) value)
               (throw 'fail loc))))))
    (when fail-loc
      (message "Testing range (%d,%d) for property %s equal to %s."
               begin end prop value)
      (message "Expected value (%s) not found in property (%s) at location %d" value prop fail-loc)
      (iec61131-test-report-property-range begin end prop))
    (should-not fail-loc)))

(defun iec61131-test-range-has-face (begin end face)
  "Verify that the range from BEGIN to END has face FACE."
  (iec61131-test-range-has-property begin end 'face face))

(defun iec61131-test-range-face-equals (begin end face)
  "Verify that the range from BEGIN to END has face equal to FACE."
  (iec61131-test-range-property-equals begin end 'face face))

(defun iec61131-test-goto-heading (title)
  "Move the point to section with TITLE."
  (let ((regexp (format "\\(^#+ %s\\( #+\\)?\\|^%s\n[=-]+\n\\)" title title)))
    (if (re-search-forward regexp nil t)
        (goto-char (match-end 0)))))

(defun iec61131-command-identity (begin end output-buffer)
  "A placeholder `iec61131-command' function for testing.
Extracts region from BEGIN to END and inserts in OUTPUT-BUFFER."
  (let ((text (buffer-substring-no-properties begin end)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert text))))

(defun iec61131-tests ()
  "Run all defined test cases for `iec61131-mode'."
  (interactive)
  (ert "iec61131"))


(require 'iec61131-mode)
(ert-deftest test-iec61131-first ()
  "My first test for `first.st'"
  (iec61131-test-file "first.st"
    (iec61131-test-range-has-face 0 3 'comment-face)))


;;; Example tests:

;; (ert-deftest test-iec61131-example/string ()
;;   "An example string test using the `ert' framework."
;;   (iec61131-test-string "foo *bar* baz"
;;                         (goto-char 5)
;;                         (delete-char 1)
;;                         (should (looking-at "bar"))))

;; (ert-deftest test-iec61131-example/file ()
;;   "An example file test using the `ert' framework."
;;   (iec61131-test-file "inline.text"
;;                       (goto-char 9)
;;                       (should (looking-at "\*"))))


(provide iec61131-tests)
;;; iec61131-tests.el ends here
