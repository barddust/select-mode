
(defcustom select-mode-disable-modes nil
  "A list of MODEs, where select-mode won't be activated.")

(defvar select-mode--last-input-method nil)

(defun select-mode-forward-char (N)
  (interactive "p")
  (forward-char N))

(defun select-mode-backward-char (N)
  (interactive "p")
  (backward-char N))

(defun select-mode-next-line (N)
  (interactive "p")
  (next-line N))

(defun select-mode-previous-line (N)
  (interactive "p")
  (previous-line N))

(defun select-mode-forward-word (N)
  (interactive "p")
  (forward-word N))

(defun select-mode-backward-word (N)
  (interactive "p")
  (backward-word N))

(defun select-mode-beginning-of-line ()
  (move-beginning-of-line 1))

(defun select-mode-end-of-line ()
  (move-end-of-line 1))

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
  (select-mode-beginning-of-line))

(defun select-mode-mark-to-end-of-line ()
  (interactive)
  (unless (select-mode--point-after-mark-p)
    (exchange-point-and-mark))
  (select-mode-end-of-line))

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
    ("h" . select-mode-backward-char)
    ("H" . mark-whole-buffer)
    ("l" . select-mode-forward-char)
    ("j" . select-mode-next-line)
    ("k" . select-mode-previous-line)
    ("e" . select-mode-backward-word)
    ("r" . select-mode-forward-word)
    ("a" . select-mode-mark-to-beginning-of-line)
    ("b" . select-mode-mark-to-end-of-line)
    ("C-l" . select-mode-mark-line)
    ("g" . keyboard-quit)
    ("v" . select-mode-mark-thing-at-point)
    ("0" . digit-argument)
    ("1" . digit-argument)
    ("2" . digit-argument)
    ("3" . digit-argument)
    ("4" . digit-argument)
    ("5" . digit-argument)
    ("6" . digit-argument)
    ("7" . digit-argument)
    ("8" . digit-argument)
    ("9" . digit-argument)
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
  (when current-input-method
    (setq select-mode--last-input-method current-input-method)
    (set-input-method nil))
  (unless (member major-mode select-mode-disable-modes)
    (select-mode 1)))

(defun select-mode--off ()
  (interactive)
  (when select-mode--last-input-method
    (set-input-method select-mode--last-input-method)
    (setq select-mode--last-input-method nil))
  (select-mode -1))

(provide 'select-mode)
