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

;; entry
(defun td-mode ()
  "Major mode editing sproto files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map td-mode-map)
  (set (make-local-variable 'font-lock-defaults) td-font-lock-defaults)
  ;; (set (make-local-variable 'indent-line-function) 'sproto-indent-line)
  (set-syntax-table td-mode-syntax-table)
  (setq major-mode 'td-mode)
  (setq mode-name "td")
  (set (make-local-variable 'comment-start) "#")
  (run-hooks 'sproto-mode-hook))

(provide 'td-mode)
;;; td-mode.el ends here
