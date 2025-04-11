 (require 'gptel)
 (require 'gptel-context)
;;(eval-when-compile (require 'gptel))
(message "initializing alg-gptel-saverestore.el")

;;;###autoload
(defun alg/gptel-save-context ()
  "Save current gptel context as elisp code in the current buffer.
Inserts code that when evaluated will restore the current context,
including files (with absolute paths), buffer names, and selected tools."
  (interactive)
  (let* ((context-items gptel-context--alist)
         (files '()) 
         (buffers '())
         (tools (mapcar #'gptel-tool-name gptel-tools)))
    ;; Process context items
    (dolist (item context-items)
      (cond ((stringp (car item))
             (push (car item) files))
            ((buffer-live-p (car item))
             (push (buffer-name (car item)) buffers))))
    ;; Insert the code directly
    (insert "(alg/gptel-restore-context\n")
    (insert (format " '%S  ; Files\n" (reverse files)))
    (insert (format " '%S  ; Buffers\n" (reverse buffers)))
    (insert (format " '%S) ; Tools\n" tools))))

;;;###autoload
(defun alg/gptel-restore-context (files buffers tools)
  "Initialize gptel context with specified FILES, BUFFERS and TOOLS.

FILES is a list of file paths (tildes will be expanded).
BUFFERS is a list of buffer names as strings.
TOOLS is a list of tool names as strings.

Clears existing context and tools before initializing new ones."
  ;; Check if gptel is active anywhere
  (unless (cl-some (lambda (buf) 
                     (buffer-local-value 'gptel-mode (get-buffer buf)))
                   (buffer-list))
    (user-error "No active gptel buffers found. Enable gptel-mode first"))
  ;; Clear existing context and tools
  (gptel-context-remove-all)
  (setq gptel-tools nil)
  
  ;; Add files to context
  (dolist (file files)
    (let ((expanded-path (expand-file-name file)))
      (condition-case err
          (gptel-add-file expanded-path)
        (error (message "Warning: Could not add file %s: %s" 
                       expanded-path (error-message-string err))))))
  
  ;; Add buffers to context
  (dolist (buf-name buffers)
    (if-let ((buf (get-buffer buf-name)))
        (with-current-buffer buf
          ;; Use gptel-context--add-region directly to avoid interactive prompting
          (gptel-context--add-region buf (point-min) (point-max) t))
      (message "Warning: Buffer %s not found" buf-name)))
  
  ;; Set active tools
  (setq gptel-tools
        (condition-case err
            (mapcar #'gptel-get-tool tools)
          (error (message "Warning: Error setting tools: %s"
                         (error-message-string err))
                 nil))))

(provide 'alg-gptel-saverestore)
