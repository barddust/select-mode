(defun select-mode-mark-line (&optional N)
  (interactive)
  (select-mode-mark-to-beginning-of-line)
  (select-mode-mark-to-end-of-line))

(defun select-mode--point-after-mark-p ()
  (>= (point) (mark t)))

(defun select-mode-mark-to-beginning-of-line ()
  (interactive)
  (when (select-mode--point-after-mark-p)
    (exchange-point-and-mark))
  (dust/beginning-of-line))

(defun select-mode-mark-to-end-of-line ()
  (interactive)
  (unless (select-mode--point-after-mark-p)
    (exchange-point-and-mark))
  (dust/end-of-line))

(defun select-mode-mark-thing-at-point ()
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (forward-word)
  (backward-word)
  (set-mark-command nil)
  (forward-word))

(defconst select-mode--map-alist
  '((";" . exchange-point-and-mark)
    ("w" . kill-ring-save)
    ("W" . kill-region)
    ("d" . delete-region)
    ("h" . backward-char)
    ("l" . forward-char)
    ("j" . next-line)
    ("k" . previous-line)
    ("e" . backward-word)
    ("r" . forward-word)
    ("a" . select-mode-mark-to-beginning-of-line)
    ("b" . select-mode-mark-to-end-of-line)
    ("C-l" . select-mode-mark-line)
    ("g" . keyboard-quit)
    ("v" . select-mode-mark-thing-at-point)
    )
  )

(defvar select-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (it select-mode--map-alist)
      (define-key map (kbd (car it)) (cdr it)))
    map))

;;;###autoload
(define-minor-mode select-mode
  "Keybindings when region is activated.")

;;;###autoload
(define-global-minor-mode global-select-mode
  select-mode (lambda ())
  (if global-select-mode
      (progn
        (add-hook 'activate-mark-hook #'select-mode--on)
        (add-hook 'deactivate-mark-hook #'select-mode--off))
    (remove-hook 'activate-mark-hook #'select-mode-on)
    (remove-hook 'deactivate-mark-hook #'select-mode-off)))

(defun select-mode--on ()
  (interactive)
  (select-mode 1))

(defun select-mode--off ()
  (interactive)
  (select-mode -1))

(provide 'select-mode)
