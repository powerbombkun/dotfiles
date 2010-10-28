;;設定
;;(require 'hg-status)
;;psvn.el http://www.xsteve.at/prg/emacs/psvn.el からの移植

(eval-when-compile (require 'cl))
(require 'vc-hg)
(add-to-list 'vc-handled-backends 'Hg)

(defvar hg-status-state-mark-modeline t
  "modeline mark display or not") 

(defun hg-status-update-modeline ()
  "Update modeline state dot mark properly"
  (when (and buffer-file-name (hg-status-in-vc-mode?))
    (hg-status-update-state-mark
     (hg-status-interprete-state-mode-color
      (vc-hg-state buffer-file-name)))))

(defun hg-status-update-state-mark (color)
  (hg-status-uninstall-state-mark-modeline)
  (hg-status-install-state-mark-modeline color))

(defun hg-status-uninstall-state-mark-modeline ()
  (setq mode-line-format
        (remove-if #'(lambda (mode) (eq (car-safe mode)
                                        'hg-status-state-mark-modeline))
                   mode-line-format))
  (force-mode-line-update t))

(defun hg-status-install-state-mark-modeline (color)
  (push `(hg-status-state-mark-modeline
          ,(hg-status-state-mark-modeline-dot color))
        mode-line-format)
  (force-mode-line-update t))

(defun hg-substring-no-properties (string &optional from to)
  (if (fboundp 'substring-no-properties)
      (substring-no-properties string from to)
    (substring string (or from 0) to)))

(defun hg-status-in-vc-mode? ()
  "Is vc-hg active?"
  (interactive)
  (and vc-mode (string-match "^ HG" (hg-substring-no-properties vc-mode))))

(defun hg-status-state-mark-modeline-dot (color)
  (propertize "    "
              'display
              `(image :type xpm
                      :data ,(format "/* XPM */
static char * data[] = {
\"18 13 3 1\",
\"  c None\",
\"+ c #000000\",
\". c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"
                                     color)
                      :ascent center)))

(defsubst hg-status-interprete-state-mode-color (stat)
  "Interpret vc-hg-state symbol to mode line color"
  (case stat
    ('edited "tomato")
    ('up-to-date "GreenYellow")
    ('unknown  "gray")
    ('added    "blue")
    ('deleted  "gray30")
    ('missing  "gray30")
    ('removed  "gray30")
    ('ignored  "gray30")
    ('unregistered  "gray30")
    ('unmerged "purple")
    (t "red")))

(defadvice vc-after-save (after hg-status-vc-hg-after-save activate)
    (when (hg-status-in-vc-mode?) (hg-status-update-modeline)))

(defadvice vc-find-file-hook (after hg-status-vc-hg-find-file-hook activate)
    (when (hg-status-in-vc-mode?) (hg-status-update-modeline)))

(provide 'hg-status)
