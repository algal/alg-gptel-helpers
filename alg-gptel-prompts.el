(defun alg--read-prompts (path)
  "Reads *.txt in PATH. Returns ((NAME . TEXT)...)"
  (let ((files (directory-files (expand-file-name path) t "\\.txt$")))
    (cl-loop for file in files
             collect (cons (intern (file-name-base file))
                         (with-temp-buffer
                           (insert-file-contents file)
                           (string-trim (buffer-string)))))))

(defun alg/generate-gptel-directives (path)
  "Returns ((rewrite . gptel--rewrite-directive-default) FILE.txt...)"
  (cons '(rewrite . gptel--rewrite-directive-default)
        (alg--read-prompts path)))

(provide 'alg-gptel-prompts)
