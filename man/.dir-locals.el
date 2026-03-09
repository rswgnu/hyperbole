((texinfo-mode
  . ((before-save-hook
      . (lambda ()
          (let ((our-set-env
                 (lambda () (let ((day (string-trim (format-time-string "%e" (current-time))))
                                  (month (capitalize (format-time-string "%B" (current-time))))
                                  (year (format-time-string "%Y" (current-time))))
                              (save-excursion
                                (goto-char (point-min))
                                (when (re-search-forward "\\(@set UPDATED [[:word:]]+ [[:digit:]]+, [[:digit:]]+\\)" nil t)
                                  (replace-match (format "@set UPDATED %s %s, %s" month day year) nil t nil nil))
                                (goto-char (point-min))
                                (when (re-search-forward "\\(@set UPDATED-MONTH [[:word:]]+ [[:digit:]]+\\)" nil t)
                                  (replace-match (format "@set UPDATED-MONTH %s %s" month year) nil t nil nil))
                                (goto-char (point-min))
                                (when (re-search-forward "\\(Printed [[:word:]]+ [[:digit:]]+, [[:digit:]]+\.\\)" nil t)
                                  (replace-match (format "Printed %s %s, %s." month day year) nil t nil nil))
                                (goto-char (point-min))
                                (when (re-search-forward "\\([[:word:]]+ [[:digit:]]+, [[:digit:]]+ @c AUTO-REPLACE-ON-SAVE\\)" nil t)
                                  (replace-match (format "%s %s, %s @c AUTO-REPLACE-ON-SAVE" month day year) nil t nil nil)))))))
            (if (fboundp 'with-locale-environment)
                (with-locale-environment "en_US.utf8"
                  (funcall our-set-env))
              (funcall our-set-env))))))))
