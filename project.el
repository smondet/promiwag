(set-mk-command 
 (if (eq system-type 'darwin) 
     "/Users/seb/usr/godi311/bin//omake"
   "omake"))

(chgbg 'g)

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



