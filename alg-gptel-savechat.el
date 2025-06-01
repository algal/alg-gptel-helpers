;;;###autoload
(defun alg/gptel-save-chat (filename)
  "Prompt for a filename and save the current buffer to it.
The prompt is prefilled with '~/Documents/chats/YYYY-MM-DD-chat-'
where YYYY-MM-DD is the current date. The user can then
complete the filename."
  (interactive
   (list
    (let* ((datestamp (format-time-string "%Y-%m-%d"))
           (default-dir (expand-file-name "~/Documents/chats/"))
           (initial-filename-part (concat datestamp "-chat-")))
      (read-file-name "Save chat to: "
                      default-dir         ;; Directory
                      nil                 ;; Default (if user enters empty)
                      nil                 ;; Mustmatch (nil for new file)
                      initial-filename-part)))) ;; Initial input for filename part
  (write-file filename)
  (message "Chat saved to: %s" (abbreviate-file-name filename)))

(provide 'alg-gptel-savechat)
