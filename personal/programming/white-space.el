(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")


(defun whitespace-set-indention-to-spaces()
  (interactive)
  (message "indent with spaces")
  (set-variable 'indent-tabs-mode nil)
  (set-variable 'c-basic-offset 4)
  (setq tab-width 4))
  
(defun whitespace-set-indention-to-tabs()
  (interactive)
  (message "indent with tabs")
  (smart-tabs-mode-enable)
  (set-variable 'c-basic-offset 8)
  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset))

(defun whitespace-indent-mode-hook()
  "Set indent mode and other things based on source formatting in file"
  (interactive)
  (if (string= (whitespace-detect-tabs-or-spaces) "spaces")
      (whitespace-set-indention-to-spaces)
    (whitespace-set-indention-to-tabs)))

(defun whitespace-detect-tabs-or-spaces()
  "Run through the first char of each line and detect tab or
space chars to determine what style of indention the buffer is
using.  Defaults to space mode if there's no indention."
  (interactive)
  (let ((tabs 0) (spaces 0) (space-char 32) (tab-char 9) num-buffer-lines)
    (save-excursion
      (setq num-buffer-lines (count-lines (point-min) (point-max)))
      (beginning-of-buffer)
      (while (< (line-number-at-pos) num-buffer-lines)
        (and (eq (char-after) space-char) (setq spaces (1+ spaces)))
        (and (eq (char-after) tab-char) (setq tabs (1+ tabs)))
        (forward-line)))
    (if (> tabs spaces)
        "tabs"
      "spaces")))

(provide 'white-space)
