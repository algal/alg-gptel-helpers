(require 'alg-gptel-export)
(require 'alg-gptel-highlighter)
(require 'alg-gptel-saverestore)
(require 'alg-gptel-tools)
(require 'alg-gptel-upgrade)
(require 'alg-gptel-savechat)
(require 'alg-gptel-prompts)


;;;###autoload
(defun alg/org-kill-file-link ()
  "Places a org link to the current file on the yank buffer"
  (interactive)
  (unless  (fboundp #'kill-file-name)
    (user-error "Function kill-file-name not available")
    (let* ((name (kill-file-name))
           (bname (file-name-nondirectory name))
           (s (format "[[file:%s][%s]]" name bname)))
      (kill-new s)
      (message s))))

(provide 'alg-gptel-helpers)


