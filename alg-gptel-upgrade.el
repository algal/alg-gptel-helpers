(require 'package)
(require 'cl-lib)

;;;###autoload
(defun alg/gptel-upgrade ()
  "Offer to upgrade gptel to latest version and remove older versions."
  (interactive)
  (package-refresh-contents)
  (let* ((installed-versions (alist-get 'gptel package-alist))
         (hosted-versions (alist-get 'gptel package-archive-contents))
         ;; Sort newest first
         (sorted-installed
          (sort installed-versions
                (lambda (a b) (version-list-< (package-desc-version b) (package-desc-version a)))))
         (sorted-hosted
          (sort hosted-versions
                (lambda (a b) (version-list-< (package-desc-version b) (package-desc-version a)))))
         (current (car sorted-installed))
         (latest (car sorted-hosted))
         (upgrade-succeeded nil)
         ;; The version descriptor that should be active after potential upgrade
         (active-desc current))

    ;; --- Initial Checks and Upgrade ---
    (cond
     ((null current)
      (message "gptel is not installed.")
      ;; Ensure deletion phase doesn't run
      (setq active-desc nil))
     ((null latest)
      (message "No gptel version found in package archives. Cannot check for upgrade."))
     ;; --- Check if upgrade is possible ---
     ((version-list-< (package-desc-version current) (package-desc-version latest))
      (when (y-or-n-p
             (format "Upgrade gptel from %s to %s? "
                     (package-version-join (package-desc-version current))
                     (package-version-join (package-desc-version latest))))
        (condition-case err
            (progn
              (package-install latest)
              (message "Successfully upgraded gptel to %s." (package-version-join (package-desc-version latest)))
              (setq upgrade-succeeded t)
              ;; Update the active descriptor to the newly installed one
              (setq active-desc latest))
          (error
           ;; Keep the original 'current' as active-desc if upgrade fails
           (message "Upgrade failed: %s" err)))))
     ;; --- Already Up-to-date ---
     (t
      (message "gptel is already at the latest version (%s)."
               (package-version-join (package-desc-version current)))))

    ;; --- Deletion Phase ---
    ;; Run if gptel was installed initially (active-desc is non-nil)
    (when active-desc
      (let ((active-version-str (package-version-join (package-desc-version active-desc))))
        ;; Iterate through all originally installed versions
        (cl-loop for old-version in sorted-installed
                 for old-version-str = (package-version-join (package-desc-version old-version))
                 when (and (not (string= old-version-str active-version-str))
                           (y-or-n-p (format "Delete obsolete version %s? " old-version-str)))
                 do (condition-case err
                         (progn
                           (package-delete old-version)
                           (message "Deleted %s." old-version-str))
                       (error (message "Failed to delete %s: %s" old-version-str err))))))))


(provide 'alg-gptel-upgrade)


