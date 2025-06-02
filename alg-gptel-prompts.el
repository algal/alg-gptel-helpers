(message "initializing alg-gptel-gptel-prompts.el")

;; TO USE: 
;; 
;; add a stanza like the following to your use-package:
;;
;;  :config
;;  (setq gptel-directives
;;        (alg/generate-gptel-directives 
;;         (expand-file-name "prompts" user-emacs-directory)))
;;
;; to search for promp like ~/.emacs.d/prompts/teacher.txt etc.

(defun alg--read-prompts (path)
  "Reads *.txt in PATH. Returns ((NAME . TEXT)...)"
  (let ((files (directory-files (expand-file-name path) t "\\.txt$")))
    (cl-loop for file in files
             collect (cons (intern (file-name-base file))
                         (with-temp-buffer
                           (insert-file-contents file)
                           (string-trim (buffer-string)))))))

;;;###autoload
(defun alg/generate-gptel-directives (path)
  "Returns ((rewrite . gptel--rewrite-directive-default) FILE.txt...)"
  (cons '(rewrite . gptel--rewrite-directive-default)
        (alg--read-prompts path)))

(provide 'alg-gptel-prompts)

