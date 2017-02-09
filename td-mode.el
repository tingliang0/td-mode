;;; td-mode.el --- Major mode for td scripts.
;;

;;; Commentary:
;; A very basic version of major mode for td scripts.
;; Current features:
;;
;; - syntax highlight
;; - basic indent
;;

;;; Code:

;; hook
(defvar td-mode-hook nil)

;; keymap
(defvar td-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for Td `'major-mode.")

;; autoload
(add-to-list 'auto-mode-alist '("\\.td$" . td-mode))

;; syntax highlight
(defvar td-font-lock-defaults
  `((
     ("-*\\(number\\|boolean\\|string\\|include\\|typedef\\|struct\\)-*" . font-lock-keyword-face)
     )))

;; syntax table
(defvar td-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Td mode syntax table.")

;; indent
(defun td-indent-line ()
  "Indent current line as sproto code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at ".*}$")        ; curline is end of block
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at ".*}$") ; preline is start block
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at ".*{$") ; preline is start of block
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (progn
            (indent-line-to cur-indent)
            (if (looking-at ".*{$")
                (save-excursion
                  (forward-line 2)
                  (if (looking-at ".*}$")
                      (indent-line-to cur-indent)))))
        (indent-line-to 0)))))


;; entry
(defun td-mode ()
  "Major mode editing td files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map td-mode-map)
  (set (make-local-variable 'font-lock-defaults) td-font-lock-defaults)
  (set (make-local-variable 'indent-line-function) 'td-indent-line)
  (set-syntax-table td-mode-syntax-table)
  (setq major-mode 'td-mode)
  (setq mode-name "td")
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'tab-width) 4)
  (run-hooks 'sproto-mode-hook))

(provide 'td-mode)
;;; td-mode.el ends here
