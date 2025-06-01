(require 'org)

(defface gptel-org-response-highlight-face
  '((((class color) (min-colors 88) (background light)) :background "#e0f8e0") ; Light green
    (((class color) (min-colors 88) (background dark)) :background "#1a301a") ; Dark green
    (t :inherit secondary-selection))
  "Face for highlighting gptel response regions in Org mode."
  :group 'gptel)

;; Buffer-local variable to store the diagnostic overlays
(defvar-local gptel-org--response-highlight-overlays nil
  "List of overlays used for highlighting gptel responses.")

(defun gptel-org--delete-highlight-overlays ()
  "Delete all gptel diagnostic highlight overlays in the buffer."
  (mapc #'delete-overlay gptel-org--response-highlight-overlays)
  (setq-local gptel-org--response-highlight-overlays nil))

;;;###autoload
(defun alg/gptel-toggle-highlight ()
  "Toggle diagnostic highlighting of gptel response regions in an Org buffer using overlays.

Reads response extents from the buffer's :GPTEL_BOUNDS: property."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org mode buffers"))

  (if gptel-org--response-highlight-overlays
      ;; --- Turn Highlighting OFF ---
      (progn
        (gptel-org--delete-highlight-overlays)
        (message "GPTEL response highlighting OFF."))

    ;; --- Turn Highlighting ON ---
    (let* ((bounds-prop (org-entry-get (point-min) "GPTEL_BOUNDS"))
           (response-bounds (when bounds-prop
                              (cdr (assoc 'response (read bounds-prop))))))
      (unless response-bounds
        (user-error "Could not find :GPTEL_BOUNDS: response data at buffer start"))

      ;; Make sure any remnants are gone before creating new ones
      (gptel-org--delete-highlight-overlays)

      (dolist (bound response-bounds)
        (let* ((start (car bound))
               (end (cadr bound))
               (ov (make-overlay start end nil nil t))) ; t = move-inclusive
          (overlay-put ov 'face 'gptel-org-response-highlight-face)
          (overlay-put ov 'priority 100) ; High priority to override others
          (overlay-put ov 'evaporate t) ; Delete if text underneath is deleted
          (push ov gptel-org--response-highlight-overlays)))

      (if gptel-org--response-highlight-overlays
          (message "GPTEL response highlighting ON.")
        (message "GPTEL response highlighting: No regions found to highlight.")))))

;;;###autoload
(defun alg/gptel-toggle-verbosity ()
    "Toggle Gptel's logging verbosity."
    (interactive)
    (if (eq gptel-log-level 'info)
        (progn
          (customize-set-variable 'gptel-log-level nil)
          (customize-set-variable 'gptel-stream t)
          (message "Disabled gptel logging"))
      (progn
        (customize-set-variable 'gptel-log-level 'info)
        (customize-set-variable 'gptel-stream nil)
        (message "Enabled gptel logging"))))

(provide 'alg-gptel-highlighter)
