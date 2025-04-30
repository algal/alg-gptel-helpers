;;; alg-gptel-export.el --- Export gptel conversations to various formats -*- lexical-binding: t; -*-

;; Author: Alexis Gallagher
;; URL: https://github.com/algal/alg-gptel-export
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (gptel "20250424.42") (markdown-mode "2.7"))
;; Keywords: convenience, ai
;;
;;; Commentary:
;; This package provides functionality to export gptel conversations to markdown.
;;
;; External dependencies:
;; - pandoc (https://pandoc.org/) for high-quality org->markdown conversion
(require 'gptel)
(require 'markdown-mode)
(require 'org)
(require 'ox-md)

(defun alg-gptel-export--extract-message-boundaries ()
  "Extract alternating message boundaries from a gptel buffer.
Return a list of pairs (TYPE BOUNDS), where TYPE is either 'ai-message
or 'non-ai-message, and BOUNDS is a cons cell of the form (START . END)."
  (unless (and (derived-mode-p 'org-mode) gptel-mode)
    (user-error "This function must be run in an org-mode gptel buffer"))
  
  (let ((result '())
        (bounds (gptel--get-buffer-bounds))
        (buffer-end (point-max))
        (last-end (point-min))
        (ai-regions '()))
    
    ;; First, extract all AI message regions from the gptel bounds
    (when-let ((response-bounds (alist-get 'response bounds)))
      (dolist (region response-bounds)
        (let ((start (car region))
              (end (cadr region)))
          (push (cons start end) ai-regions))))
    
    ;; Sort AI regions by start position
    (setq ai-regions (sort ai-regions (lambda (a b) (< (car a) (car b)))))
    
    ;; Build the alternating list of message boundaries
    (dolist (ai-region ai-regions)
      (let ((ai-start (car ai-region))
            (ai-end (cdr ai-region)))
        
        ;; Add non-AI region if there's content before this AI message
        (when (< last-end ai-start)
          (push (list 'non-ai-message (cons last-end ai-start)) result))
        
        ;; Add AI region
        (push (list 'ai-message (cons ai-start ai-end)) result)
        
        ;; Update last-end for next iteration
        (setq last-end ai-end)))
    
    ;; Add final non-AI region if needed
    (when (< last-end buffer-end)
      (push (list 'non-ai-message (cons last-end buffer-end)) result))
    
    ;; Return the result in chronological order
    (nreverse result)))

(defun alg-gptel-export--extract-user-content (tagged-region)
  "Process a non-ai-message region into user-annotation and user-message parts.
TAGGED-REGION should be a pair (TYPE BOUNDS) where TYPE is 'non-ai-message
and BOUNDS is (START . END).
Returns a list of (TYPE BOUNDS) pairs, where TYPE is either 'user-annotation
or 'user-message."
  (unless (eq (car tagged-region) 'non-ai-message)
    (user-error "Expected a non-ai-message region"))
  
  (let* ((bounds (cadr tagged-region))
         (start (car bounds))
         (end (cdr bounds))
         (result '())
         (user-prefix (or (alist-get major-mode gptel-prompt-prefix-alist) ""))
         (response-prefix (or (alist-get major-mode gptel-response-prefix-alist) "")))
    
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        
        ;; Look for user-prefix
        (if (search-forward user-prefix nil t)
            (let* ((prefix-end (point))
                  (message-start prefix-end)
                  (message-end nil))
              
              ;; If there's content before the user-prefix, it's an annotation
              (when (> (- prefix-end (length user-prefix)) (point-min))
                (let ((annotation-start (point-min))
                      (annotation-end (- prefix-end (length user-prefix))))
                  
                  ;; Trim whitespace from annotation
                  (save-restriction
                    (narrow-to-region annotation-start annotation-end)
                    (goto-char (point-min))
                    (skip-chars-forward " \t\n\r")
                    (setq annotation-start (point))
                    (goto-char (point-max))
                    (skip-chars-backward " \t\n\r")
                    (setq annotation-end (point)))
                  
                  ;; Only add if non-empty after trimming
                  (unless (>= annotation-start annotation-end)
                    (push (list 'user-annotation 
                                (cons annotation-start annotation-end))
                          result))))
              
              ;; Find end of user message (next response prefix or end of region)
              (if (search-forward response-prefix nil t)
                  (setq message-end (- (point) (length response-prefix)))
                (setq message-end (point-max)))
              
              ;; Add the user message
              (push (list 'user-message (cons message-start message-end)) result))
          
          ;; No user-prefix found - treat entire region as annotation
          (let ((annotation-start (point-min))
                (annotation-end (point-max)))
            
            ;; Trim whitespace
            (save-restriction
              (narrow-to-region annotation-start annotation-end)
              (goto-char (point-min))
              (skip-chars-forward " \t\n\r")
              (setq annotation-start (point))
              (goto-char (point-max))
              (skip-chars-backward " \t\n\r")
              (setq annotation-end (point)))
            
            ;; Only add if non-empty after trimming
            (unless (>= annotation-start annotation-end)
              (push (list 'user-annotation (cons annotation-start annotation-end)) result))))))
    
    ;; Return results in chronological order
    (nreverse result)))


(defun alg-gptel-export--extract-regions ()
  "Extract all content regions from a gptel buffer.
Returns a list of tagged extents, where each extent is a pair
(TYPE . BOUNDS) and TYPE is one of: 'user-annotation, 'user-message,
or 'ai-message."
  (unless (and (derived-mode-p 'org-mode) gptel-mode)
    (user-error "This function must be run in an org-mode gptel buffer"))
  
  (let ((message-boundaries (alg-gptel-export--extract-message-boundaries))
        (result '()))
    
    ;; Process each message boundary
    (dolist (boundary message-boundaries)
      (if (eq (car boundary) 'ai-message)
          ;; AI messages go into result unchanged
          (push boundary result)
        ;; Non-AI messages need to be further processed
        (let ((user-content (alg-gptel-export--extract-user-content boundary)))
          ;; Add each user content part to the result
          (dolist (content user-content)
            (push content result)))))
    
    ;; Return results in chronological order
    (nreverse result)))


;; export

(defun alg-gptel-export--to-markdown ()
  "Export the current gptel buffer to a markdown string.
The export format uses H2 headers with human/robot emojis to represent the conversation.
Requires the ox-md package from Org mode."
  (unless (and (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
               gptel-mode)
    (user-error "This function must be run in an org-mode or markdown-mode gptel buffer"))
  
  (require 'ox-md)
  
  (let* ((regions (alg-gptel-export--extract-regions))
         (regions-to-process (cdr regions)) ; Skip the first region
         (export-text ""))
    
    (dolist (region regions-to-process)
      (let* ((type (car region))
             (bounds (cadr region))
             (start (car bounds))
             (end (cdr bounds))
             (content (buffer-substring-no-properties start end)))
        
        ;; Process the region based on its type
        (cond
         ((eq type 'user-message)
          ;; Add human header and convert content to markdown
          (setq export-text 
                (concat export-text 
                        "## 👨 human\n\n" 
                        (alg-gptel-export--org-to-markdown content)
                        "\n\n")))
         
         ((eq type 'ai-message)
          ;; Add robot header and convert content to markdown
          (setq export-text 
                (concat export-text 
                        "## 🤖 assistant\n\n" 
                        (alg-gptel-export--org-to-markdown content)
                        "\n\n")))
         
         ((eq type 'user-annotation)
          ;; Just convert to markdown and include
          (setq export-text 
                (concat export-text 
                        (alg-gptel-export--org-to-markdown content)
                        "\n\n"))))))
    
    export-text))


(defun alg-gptel-export--org-to-markdown (org-content)
  "Convert org-mode content to markdown using pandoc.
Optimized for clean GitHub Flavored Markdown output."
  (if (derived-mode-p 'markdown-mode)
      org-content  ; Already markdown, pass through
    (unless (executable-find "pandoc")
      (user-error "Please install pandoc (https://pandoc.org/) for converting org to markdown"))
    (when (executable-find "pandoc")
      (with-temp-buffer
        (insert org-content)
        ;; Direct conversion from Org to GitHub Flavored Markdown
        (shell-command-on-region
         (point-min) (point-max)
         "pandoc -f org -t gfm --wrap=none --markdown-headings atx"
         t t)
        (buffer-string)))))


;; (defun alg/gptel-export-to-markdown-file (filename)
;;   "Export the current gptel buffer to a markdown file.
;; FILENAME is the path where the markdown file will be saved."
;;   (interactive "FExport to markdown file: ")
;;   (let ((markdown-content (alg-gptel-export--to-markdown)))
;;     (with-temp-file filename
;;       (insert markdown-content))
;;     (message "Exported to %s" filename)))

;;;###autoload
(defun alg/gptel-export-to-markdown-buffer ()
  "Export the current gptel buffer to a new markdown buffer.
Creates a new buffer with the exported markdown content and displays it."
  (interactive)
  (let* ((source-buffer-name (buffer-name))
         (markdown-content (alg-gptel-export--to-markdown))
         (export-buffer-name 
          (if (buffer-file-name)
              ;; If this is a file buffer, use its basename with .md extension
              (let* ((file-name (buffer-file-name))
                     (base-name (file-name-sans-extension 
                                 (file-name-nondirectory file-name))))
                ;; For markdown files, add "-export" to distinguish
                (if (string-equal (file-name-extension file-name) "md")
                    (format "%s-export.md" base-name)
                  (format "%s.md" base-name)))
            ;; For non-file buffers, use a sensible default name
            (format "*%s-markdown-export*" source-buffer-name)))
         ;; Use generate-new-buffer-name to ensure uniqueness
         (unique-buffer-name (generate-new-buffer-name export-buffer-name))
         (export-buffer (get-buffer-create unique-buffer-name)))
    
    (with-current-buffer export-buffer
      (erase-buffer)
      (insert markdown-content)
      (markdown-mode)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    
    (pop-to-buffer export-buffer)
    (message "Exported to buffer %s" unique-buffer-name)
    export-buffer))

(provide 'alg-gptel-export)

