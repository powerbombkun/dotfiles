;;; anything-switch-project.el --- Switch to other your project with anything

;; Copyright (C) 2010  Hiroshige Umino

;; Author: Hiroshige Umino <yaotti@gmail.com>
;; blog: http://d.hatena.ne.jp/yaotti (japanese)
;; Keywords: anything

;;; Commentary:
;; anything-switch-project.el is now support only git projects


;;; How to use:
;; (require 'anything-switch-project)
;; (global-set-key (kbd "C-c f") 'anything-switch-project)

;;; Configuration:
;; add your projects' paths like this
;; (setq asp:my-projects
;;       '("~/sshfs/external-server/project/"
;;         "~/project/root/controlled/by/svn/"))


(require 'anything)

(defvar asp:locate-command "locate")
(defvar asp:project-root-file '(".git/config")) ; too slow with regexp like "\.git$"

(defvar asp:my-projects nil
  "add your projects which you want to switch")

(defvar asp:candidates
  (let* ((patterns
          (mapconcat 'identity asp:project-root-file " "))
         (command (concat asp:locate-command " " patterns))
         (dirs (split-string (shell-command-to-string command) "\n"))
         (val))
    (dolist (dir dirs val)
      (if (and (not (string= dir ""))
               (not (string-match "modules" dir)) ;exclude submodule dirs
               (string-match (substitute-in-file-name "$HOME") dir))
          (setq val
                (cons
                 (expand-file-name (concat dir "./../../")) ;XXX: dirty :-/
                 val)) ;remove ".git/config" from path name
        ))))

(defvar anything-c-source-switch-project
  '((name . "Switch To Other Project")
    (candidates . (lambda () (append asp:candidates asp:my-projects)))
    (type . file)
    ))


(defun anything-switch-project ()
  "Switch to other project (controlled by git only...)"
  (interactive)
  (anything 'anything-c-source-switch-project
            ""
            "Open project: "
            nil))

(provide 'anything-switch-project)
