(require 'package)

;;;###autoload
(defun alg/gptel-upgrade ()
  "Offer to upgrade gptel to latest version and remove older versions."
  (interactive)
  (package-refresh-contents)
  (let* ((installed-versions (alist-get 'gptel package-alist))
         (hosted-versions (alist-get 'gptel package-archive-contents))
         ;; Explicitly sort both lists, newest first
         (sorted-installed 
          (sort installed-versions
                (lambda (a b)
                  (version-list-< (package-desc-version b)
                                (package-desc-version a)))))
         (sorted-hosted
          (sort hosted-versions
                (lambda (a b)
                  (version-list-< (package-desc-version b)
                                (package-desc-version a)))))
         (current (car sorted-installed))
         (latest (car sorted-hosted)))
    
    (if (null current)
        (message "gptel is not installed")
      (if (null latest)
          (message "No gptel version found in package archives")
        ;; Compare versions explicitly
        (if (version-list-< (package-desc-version current)
                           (package-desc-version latest))
            (when (y-or-n-p 
                   (format "Upgrade gptel from %s to %s? "
                          (package-version-join 
                           (package-desc-version current))
                          (package-version-join 
                           (package-desc-version latest))))
              (package-install latest)
              ;; After upgrade, offer to delete old versions
              (dolist (old-version (cdr sorted-installed))
                (when (y-or-n-p 
                       (format "Delete old version %s? "
                              (package-version-join
                               (package-desc-version old-version))))
                  (package-delete old-version))))
          (message "gptel is already at the latest version (%s)"
                  (package-version-join (package-desc-version current))))))))

(provide 'alg-gptel-upgrade)


