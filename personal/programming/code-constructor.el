;;; Code Constructor module

(provide 'code-constructor)

(defcustom cc-header-file-exts '("h" "hpp")
  "*List of file extensions that defines something as a header
  file. Do not include a leading dot for each extension.")

(defcustom cc-impl-file-exts '("c" "cpp")
  "*List of file extensions that defines something as an
  implementation file.  Do not include a leading dot for each
  extention.")



;; --------------- Private helpers ---------------

(defun cc-trim-hex-prefix(hex-string)
  "If the hex string starts with a 0x prefix, trim it off.
Return the result, trimmed, or untouched depending on what was
passed in."
  (if (string-match-p "^0x" hex-string)
      (substring hex-string 2)
    hex-string))

(defun cc-get-anchored-hex-token()
  "Get the hex num starting at current point.  If nothing is
found, return nil.  Optionally match and return prefix of 0x."
  (if (looking-at "\\(0x\\)?[0-9A-Fa-f]+")
      (match-string 0)
    'nil))



;; --------------- API ---------------

(defun cc-numbers(beg end)
  "Delete a rectangle and replace with a sequence of number
starting with the number on the top line."
  (interactive "r")
  (save-excursion
    (let ((counter 0) (line-num 0) left-column right-column num-lines)
      ;; Figure out where the left and right side of the rectangle is
      (goto-char end)
      (setq right-column (current-column))
      (goto-char beg)
      (setq left-column (current-column))
      (setq num-lines (count-lines beg end))

      ;; Get the starting count from the first line
      (let (saved-point starting-number)
	(setq saved-point (point))
	(move-to-column right-column)
	(setq starting-number (buffer-substring beg (point)))
	(setq counter (string-to-number starting-number))
	(goto-char saved-point))

      ;; Clear the rectangle
      (delete-rectangle beg end)

      ;; Insert the new numbers for every line in the rectangle
      (while (< line-num num-lines)
	(insert (number-to-string counter))
	(vertical-motion 1)

	;; Move to the correct column or end of the line
	(move-to-column left-column)

	(setq line-num (+ line-num 1))
	(setq counter (+ counter 1))))))

(defun cc-delete-hex-column(beg end)
  "Delete all hex numbers starting at point down to the bottom
line of input rectangle.  Don't modify anything left of point in
the buffer."
  (interactive "r")
  (save-excursion
    (let ((line-num 0) left-column current-num-string num-lines)
      ;; Figure out where the left and right side of the rectangle is
      (goto-char beg)
      (setq left-column (current-column))
      (setq num-lines (count-lines beg end))

      ;; Get the count start
      (setq current-num-string (cc-get-anchored-hex-token))

      (while (< line-num num-lines)
	;; Remove old stuff
	(delete-char (string-width current-num-string))
	(vertical-motion 1)

	;; Move to the correct column or end of the line
	(move-to-column left-column)

	(setq line-num (+ line-num 1))
	(setq current-num-string (cc-get-anchored-hex-token))))))

(defun cc-numbers-hex(beg end)
  "Delete all hex digits starting at point in a column down to
the bottom of a rectangle, then replace with a sequence of hex
numbers starting with the number on the top line."
  (interactive "r")
  (save-excursion
    (let ((counter 0) (line-num 0) left-column current-num-string num-lines)
      ;; Figure out where the left and right side of the rectangle is
      (goto-char beg)
      (setq left-column (current-column))
      (setq num-lines (count-lines beg end))

      ;; Get the count start
      (setq current-num-string (cc-get-anchored-hex-token))
      (setq counter (string-to-number (cc-trim-hex-prefix (cc-get-anchored-hex-token)) 16))

      (cc-delete-hex-column beg end)

      ;; Insert the new numbers for every line in the rectangle
      (while (< line-num num-lines)
	(insert (format "0x%X" counter))
	(vertical-motion 1)

	;; Move to the correct column or end of the line
	(move-to-column left-column)

	(setq line-num (+ line-num 1))
	(setq counter (+ counter 1))))))

(defun cc-find-other-file ()
  "If in the implementation file, find the header file.  If in
the header file, find the implementation file.  Look in the
buffer list or current directory for the other file."
  (interactive)
  (let (current-filename exts found-file)
    (setq current-filename (buffer-file-name (current-buffer)))
    (and (cc-priv-is-header-file current-filename)
	 (setq exts cc-impl-file-exts))
    (and (cc-priv-is-impl-file current-filename)
	 (setq exts cc-header-file-exts))
    (setq found-file (cc-priv-find-module-file current-filename exts))
    (unless found-file
      (message "Unable to find other module file"))
    found-file))

(defun cc-priv-find-module-file (file extension-list)
  "See if the combination of the module name and an extension
from the extension-list var result in an open buffer name or file
within the current working directory.  The module is essentially
a filename without extension.  If a match is found, switch to
it."
  (let (module-name target-file-name file-found)
    (setq module-name (cc-priv-get-module-name file))
    (while (and extension-list (not file-found))
      (setq target-file-name (concat module-name "." (car extension-list)))
      (if (or (cc-priv-find-buffer target-file-name) (cc-priv-find-file target-file-name))
	    (setq file-found t))
      (setq extension-list (cdr extension-list)))
    file-found))

(defun cc-priv-find-buffer (file)
  "Find file within open buffers"
  (if (get-buffer file)
      (switch-to-buffer file)
    nil))

(defun cc-priv-find-file (file)
  "Find file in current directory"
  (if (file-exists-p file)
      (find-file file)
    nil))

(defun cc-priv-get-module-name (file)
  (file-name-nondirectory (file-name-sans-extension file)))

(defun cc-priv-is-header-file (file)
  (member (file-name-extension file) cc-header-file-exts))

(defun cc-priv-is-impl-file (file)
  (member (file-name-extension file) cc-impl-file-exts))


;; --------------- Line Formatting Functions ---------------

(defun cc-chomp-line (beg end string)
  "Remove a string from the end of each line in a region"
  (interactive "r\nsString to remove: ")
  (cc-chomp-line-engine beg end string 'search-forward))

(defun cc-chomp-line-regexp (beg end string)
  "Remove a regexp from the end of each line in a region"
  (interactive "r\nsRegexp to remove: ")
  (cc-chomp-line-engine beg end string 're-search-forward))

(defun cc-chomp-line-engine (beg end string search-func)
  "Helper function for line chomping.  Typically use
search-forward and re-search-forward for the search-func"
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))

      ;; loop through the region line by line
      (while (< (point) (point-max))
	(let (line-end-pos)
	  (end-of-line)
	  (setq line-end-pos (point))
	  (beginning-of-line)

	  ;; constrain the search to line end and replace with empty string
	  (if (eq (funcall search-func string line-end-pos t) line-end-pos)
	      (replace-match ""))
	  (next-line))))))

(defun cc-append-to-line (beg end string)
  "Add a string to the end of each line in a region"
  (interactive "r\nsString to append: ")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))

      ;; loop through the region line by line
      (while (< (point) (point-max))
	(end-of-line)
	(insert string)
	(next-line)))))

(defun cc-align-table (beg end)
  "Align columns of a 'table'"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (align (point-min) (point-max))
      (align-regexp (point-min) (point-max) ",\\(\\s-*\\)" 1 1 t)
      (delete-trailing-whitespace))))

(defun cc-join-lines (beg end string)
  "Join every line in a region into a single line.  Connect each
line with 'string"
  (interactive "r\nsConnect lines with: ")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-max))
      (while (> (point) (point-min))
	(join-line)
	(delete-char 1)
	(insert string)
	(beginning-of-line)))))

(defun cc-collapse-space-region (beg end)
  "Collapse whitespace down to single instance within region.
Much like just-one-space, except this works on a region and not
just the area around point."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

(defun cc-xargs (beg end)
  "Join every line or token into a space separated list"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-max))
      ;; Join every line from end to beginning
      (while (> (point) (point-min))
        (join-line)
        (end-of-line)
        (beginning-of-line))
      ;; Every token should have exactly one space between it and the next
      (cc-collapse-space-region (point-min) (point-max)))))

(defun cc-bprintf (beg end)
  "Take a space separated list and turn it into a line separated
list using bash-printf-like functionality."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (delete-horizontal-space)
      (while (re-search-forward "\\s-+" nil t)
        (replace-match "\n")))))

(defun cc-macro-rjustify ()
  "Right justify macro values to fill-column for line"
  (interactive)
  (save-excursion
    (let (tail padding bounds)
      (end-of-line)
      (setq tail (- (current-column) 1))
      (setq padding (- fill-column tail))
      (setq bounds (point))
      (message "Padding is %d" padding)
      (beginning-of-line)
      (if (and (> padding 0) (re-search-forward "^#define [-_a-zA-Z0-9().,]+" bounds t))
          (progn (insert-char ?  padding) padding)))))

(defun cc-macro-rjustify-block ()
  "Right justify macro values to fill-column for a block"
  (interactive)
    (while (cc-macro-rjustify)
      (forward-line)))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))
