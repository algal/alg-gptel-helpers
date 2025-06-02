(require 'gptel)

(defun gptel--insert-file-string-around-advice (orig-fun path)
  "Use document format for Anthropic backends, original otherwise."
  (if (cl-typep gptel-backend 'gptel-anthropic)
      (progn
        (insert "<document>\n")
        (insert (format "  <source>%s</source>\n" (file-name-nondirectory path)))
        (insert "<document_content>")
        (let ((pm (point-marker)))
          (set-marker-insertion-type pm t)
          (insert-file-contents path)
          (goto-char pm))
        (insert "</document_content>\n")
        (insert "</document>\n"))
    (funcall orig-fun path)))

(defun anthropic-markup-enabled-p ()
  "Check if Anthropic markup advice is currently active."
  (advice-member-p #'gptel--insert-file-string-around-advice 
                   'gptel--insert-file-string))

(defun alg/toggle-anthropic-markup (&optional enable)
  "Toggle Anthropic document markup for gptel file insertion."
  (interactive "P")
  (let* ((currently-enabled (anthropic-markup-enabled-p))
         (should-enable (if enable enable (not currently-enabled))))
    (if should-enable
        (unless currently-enabled
          (advice-add 'gptel--insert-file-string
                      :around #'gptel--insert-file-string-around-advice)
          (message "Anthropic markup enabled"))
      (when currently-enabled
        (advice-remove 'gptel--insert-file-string 
                       #'gptel--insert-file-string-around-advice)
        (message "Anthropic markup disabled")))))

(provide 'alg-gptel-anthropic-markup)
