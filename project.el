

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
(seb-funkey "mk" save-all-and-mk
            (progn (save-some-buffers) (compile omake-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening files:

;; This keeps the directory of the present file
(setq project-directory default-directory)

(defun list-of-files()
  (interactive)
  (append 
   (list 
    (concat project-directory "/project.el")
    (concat project-directory "/OMakefile")
    )
   (split-string (shell-command-to-string
                  (concat 
                   "find "
                   project-directory "src/ "
                   project-directory "tools/ "
                   " -name '*.ml' -or -name 'OMakefile'")))))

(defun key-of-file(filename)
  ;; from http://xahlee.org/emacs/elisp_idioms.html
  (with-temp-buffer
    (insert filename)
    (goto-char (point-min))
    (while (search-forward ".ml" nil t) (replace-match ""))
    (goto-char (point-min))
    (while (search-forward project-directory nil t) (replace-match ""))
    (buffer-string) ; get result
    ))

(defun make-assoc(filenames)
  (mapcar '(lambda (f) (list (key-of-file f)  f)) filenames))

(defun make-key-list(filenames)
  (mapcar 'key-of-file filenames))

(defun open-project-file()
  (interactive)
  (let ((filelist (make-assoc (list-of-files))))
    (let
        ((key (ido-completing-read "Src: "
                                   (mapcar 'car filelist))))
      (find-file (car (cdr (assoc key filelist)))))))
(seb-skey "po" 'open-project-file)


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


