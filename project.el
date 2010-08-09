

(chgbg 'g)
(setq frame-title-format "Promiwag – %b")


;(setq history-format "%cD%n%s%b")
(setq history-format "%s%n%b")
(defun ghistory (number)
  (interactive "MNumber of commits: ")
  ;(shell-command
  (let ((before (point)))
    (progn 
      (shell-command-on-region 
       (point) (point)
       (format "git log --reverse -n %s --pretty=format:\"%s\""
               number history-format)
       t t)
      (kill-region before (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation:

(setq omake-command
      (if (eq system-type 'darwin) 
          "/Users/seb/usr/godi311/bin//omake --project"
        "omake --project"))

(set-mk-command omake-command)
(setq compile-command omake-command)
(setq compilation-read-command nil)

;; Remove usual call to eshell:
(seb-skey "mk" 'compile)


(defun files-in-below-directory (directory)
  "List the .ml files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/21.0.100/lisp/"
  (interactive "DDirectory name: ")
  (let (ml-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.ml'
       ;; and if so, append its name to a list.
       ((equal ".ml" (substring (car (car current-directory-list)) -3))
        (setq ml-files-list
              (cons (car (car current-directory-list)) ml-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; mlse descend into the directory and repeat the process
          (setq ml-files-list
                (append
                 (files-in-bmlow-directory
                  (car (car current-directory-list)))
                 ml-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    ml-files-list))
(setq mlfiles-list (files-in-below-directory "./src/"))
(seb-skey "ffml" '(lambda ()
                   (interactive)
                   (find-file 
                    (ido-completing-read "ML File: " mlfiles-list))))


(progn 
  (compile omake-command)
  (magit-status "./")
  (windmove-right)
  (split-window-vertically 20)
  (windmove-down)
  (split-window-vertically 20)
  (find-file "TODO")
  (windmove-down)
  (eshell))


