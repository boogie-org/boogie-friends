(load (expand-file-name "common.el" (file-name-directory load-file-name)))

(defconst my-package-version (with-temp-buffer
                               (insert-file-contents (expand-file-name "../boogie-friends-pkg.el" base-directory))
                               (concat "boogie-friends-" (nth 2 (read (current-buffer))))))

(defun my-package-version ()
  (princ my-package-version))
