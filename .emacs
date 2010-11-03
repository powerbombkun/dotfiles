;; -*- mode: emacs-lisp ; Coding: utf-8 -*-
;;
;; author powerbombkun
;; last update 2010-11-03 15:13:05
;;-----------------------------------------------------------------------------
;; OS を判別
;;------------------------------------------------------------------------------
(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (or (equal system-type 'usg-unix-v)
          (or  (equal system-type 'berkeley-unix)
               (equal system-type 'cygwin)))))

(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-system-v
  (equal system-type 'usg-unix-v))
(defvar run-bsd
  (equal system-type 'berkeley-unix))
(defvar run-cygwin ;; cygwin も unix グループにしておく
  (equal system-type 'cygwin))

(defvar run-w32
  (and (null run-unix)
       (or (equal system-type 'windows-nt)
           (equal system-type 'ms-dos))))

(defvar run-darwin (equal system-type 'darwin))
;;------------------------------------------------------------------------------
;; Emacsen の種類とヴァージョンを判別
;;------------------------------------------------------------------------------
(defvar run-emacs20
  (and (equal emacs-major-version 20)
       (null (featurep 'xemacs))))
(defvar run-emacs21
  (and (equal emacs-major-version 21)
       (null (featurep 'xemacs))))
(defvar run-emacs22
  (and (equal emacs-major-version 22)
       (null (featurep 'xemacs))))
(defvar run-emacs23
  (and (equal emacs-major-version 23)
       (null (featurep 'xemacs))))
(defvar run-meadow (featurep 'meadow))
(defvar run-meadow1 (and run-meadow run-emacs20))
(defvar run-meadow2 (and run-meadow run-emacs21))
(defvar run-meadow3 (and run-meadow run-emacs22))
(defvar run-xemacs (featurep 'xemacs))
(defvar run-xemacs-no-mule
  (and run-xemacs (not (featurep 'mule))))
(defvar run-carbon-emacs (and run-darwin window-system))
;;------------------------------------------------------------------------------
;; PATH
;;------------------------------------------------------------------------------
(when run-carbon-emacs
  (setq exec-path (cons "/usr/local/bin" exec-path))
  (setenv "PATH"
          (concat '"/usr/local/bin:" (getenv "PATH"))))
;;------------------------------------------------------------------------------
;; elisp load-path
;;------------------------------------------------------------------------------
(setq load-path (append '("~/.emacs.d/elisp"
                          "~/.emacs.d/elisp/anthy"
                          "~/.emacs.d/elisp/themes"
                          "~/.emacs.d/elisp/config"
                          "~/.emacs.d/elisp/org")
                        load-path))
;;------------------------------------------------------------------------------
;; auto-mode-alist
;;------------------------------------------------------------------------------
(setq auto-mode-alist
      (append '(("\\.org$"  . org-mode)
                ("\\.yml$"  . yaml-mode)
                ("\\.asm$"  . asm-mode)
                ("\\.css$"  . css-mode)
                ("\\.mk$"   . makefile-mode)
                ("Makefile" . makefile-mode)
                ("makefile" . makefile-mode)
                ) auto-mode-alist))
;;------------------------------------------------------------------------------
;; magic-mode-alist
;;------------------------------------------------------------------------------
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
;;------------------------------------------------------------------------------
;; eval-safe
;; 安全な評価。評価に失敗してもそこで止まらない。
;; http://www.sodan.org/~knagano/emacs/dotemacs.html#eval-safe
;;------------------------------------------------------------------------------
(defmacro eval-safe (&rest body)
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

;;------------------------------------------------------------------------------
;; yes or no の簡略化
;;------------------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)
;;------------------------------------------------------------------------------
;; cl はどこで使ってるかわからんので、とりあえず require しとく。
;;------------------------------------------------------------------------------
(eval-when-compile
  (require 'cl))
;;------------------------------------------------------------------------------
;; start up message
;;------------------------------------------------------------------------------
(setq inhibit-startup-message t)
;;------------------------------------------------------------------------------
;;; widen-window
;;------------------------------------------------------------------------------
;; (require 'widen-window)
;; (setq ww-width nil)
;; (global-widen-window-mode t)
;; (defadvice anything (around disable-ww-mode activate)
;;   (ad-deactivate-regexp "widen-window")
;;   (unwind-protect
;;       ad-do-it
;;     (ad-activate-regexp "widen-window")))
;;------------------------------------------------------------------------------
;;; window-move
;;------------------------------------------------------------------------------
(windmove-default-keybindings)
;;------------------------------------------------------------------------------
;;; beep
;;------------------------------------------------------------------------------
(setq ring-bell-function 'ignore)
;;------------------------------------------------------------------------------
;; color-theme
;; refe : http://www.cs.cmu.edu/~maverick/GNUEmacsColorThemeTest/index-el.html
;;------------------------------------------------------------------------------
(when (require 'color-theme nil t)
  (when run-linux
    (color-theme-initialize)
    (color-theme-clarity))
  (when run-w32
    (color-theme-initialize)
    (color-theme-clarity))
  (when run-darwin
    (color-theme-initialize)
    (color-theme-clarity)))
;;------------------------------------------------------------------------------
;; recent files
;;------------------------------------------------------------------------------
(setq recentf-max-saved-items 2000)
(recentf-mode 1)
;;------------------------------------------------------------------------------
;; global-set-key
;;------------------------------------------------------------------------------
;;
;; C-h => backspace
;;
(if (eq window-system 'x)
    (progn

      (define-key function-key-map [backspace] [8])
      (put 'backspace 'ascii-character 8)
      ))
(global-set-key "\C-h" 'backward-delete-char)
;;
;; align -> C-ca
;;
(global-set-key "\C-ca" 'align)
;;
;; align-rule
;;
(require 'align)
;; ruby
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list          ;TODO add to rcodetools.el
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))
;;
;; indent-region -> C-ci
;;
(global-set-key "\C-ci" 'indent-region)
;;
;; 全角文字と半角文字の間に自動でスペースを挿入
;;
;; http://d.hatena.ne.jp/rubikitch/20090220/text_adjust
;; http://web.archive.org/web/20070208231732/http://taiyaki.org/elisp/mell/src/mell.el
;; http://web.archive.org/web/20070220213120/http://www.taiyaki.org/elisp/text-adjust/src/text-adjust.el
(require 'text-adjust)
(global-set-key "\C-cj" 'text-adjust-space-region)
;;
;; browse-kill-ring.el
;;
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)


(global-set-key "\C-q" 'delete-window)
(global-set-key "\C-c\C-f" 'query-replace)
;;------------------------------------------------------------------------------
;; 鬼軍曹 mode (drill-instructor)
;;------------------------------------------------------------------------------
;(require 'drill-instructor)
;(setq drill-instructor-global t)
;;------------------------------------------------------------------------------
;; 保存系
;;------------------------------------------------------------------------------
(require 'auto-save-buffers)
(run-with-idle-timer 30 t 'auto-save-buffers)             ; 30秒以上操作がなければ自動保存
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; 行末スペースの削除
;;------------------------------------------------------------------------------
;; 時刻の表示
;;------------------------------------------------------------------------------
(setq display-time-24hr-format t)
(display-time)
;;------------------------------------------------------------------------------
;; tool-bar-mode        : disable
;; scroll-bar-mode      : disable
;;------------------------------------------------------------------------------
(cond (window-system
       (tool-bar-mode nil)
       (set-scroll-bar-mode nil)
       ))
;;------------------------------------------------------------------------------
;; fullscreen (carbon emacs)
;;------------------------------------------------------------------------------
(when run-carbon-emacs
  (defun my-toggle-frame-size ()
    (interactive)
    (if (frame-parameter nil 'fullscreen)
        (set-frame-parameter nil 'fullscreen nil)
      (set-frame-parameter nil 'fullscreen 'fullboth)
      (message "Full-screen changed")))
  (global-set-key "\C-cm" 'my-toggle-frame-size))
;;------------------------------------------------------------------------------
;; Japanese
;;------------------------------------------------------------------------------
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when run-linux
  (push "/usr/share/emacs/site-lisp/anthy" load-path)
  (load-library "anthy")
  (setq default-input-method 'japanese-egg-anthy)
  )
;;------------------------------------------------------------------------------
;; Command candidate
;;------------------------------------------------------------------------------
(icomplete-mode 1)
;;------------------------------------------------------------------------------
;; 対応する括弧
;;------------------------------------------------------------------------------
(show-paren-mode 1)
;;------------------------------------------------------------------------------
;; find file at point
;;------------------------------------------------------------------------------
(ffap-bindings)
;;------------------------------------------------------------------------------
;; tab
;;------------------------------------------------------------------------------
(setq-default tab-width 4 indent-tabs-mode nil)
;;------------------------------------------------------------------------------
;; Show tab, zenkaku-space
;;------------------------------------------------------------------------------
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-2 '((t (:background "gray18"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defface my-face-u-2 '((t (:underline nil))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defvar my-face-u-2 'my-face-u-2)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("^[ ]+$" 0 my-face-u-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode
                                  nil
                                (font-lock-mode t))) t)
;;------------------------------------------------------------------------------
;;; color
;;------------------------------------------------------------------------------
(global-font-lock-mode t)
;;------------------------------------------------------------------------------
;; region
;;------------------------------------------------------------------------------
(delete-selection-mode 1)
;;------------------------------------------------------------------------------
;: Undo
;;------------------------------------------------------------------------------
(setq undo-limit 100000)
(setq undo-strong-limit 130000)
;;------------------------------------------------------------------------------
;; Redo
;;------------------------------------------------------------------------------
(when (require 'redo nil t)
  (define-key ctl-x-map (if window-system "U" "r") 'redo)
  (define-key global-map [?\C-.] 'redo))
;;------------------------------------------------------------------------------
;; shell
;;------------------------------------------------------------------------------
;;(setq shell-file-name "zsh")
;;(setenv "SHELL" shell-file-name)
(require 'shell-history)
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
   "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(when run-meadow
  (setq explicit-shell-file-name "c:\\cygwin\\bin\\zsh.exe") ;
  (modify-coding-system-alist 'process "shell" '(undecided-dos . sjis-unix)))

(global-set-key "\M-u" 'shell)

;;------------------------------------------------------------------------------
;; iswitch
;;------------------------------------------------------------------------------
(iswitchb-mode 1)

;;------------------------------------------------------------------------------
;; occur to isearch
;;------------------------------------------------------------------------------
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))
;;------------------------------------------------------------------------------
;; howm
;; http://howm.sourceforge.jp/
;;------------------------------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/elisp/howm")
;; (setq howm-menu-lang 'ja)
;; (global-set-key "\C-c,," 'howm-menu)
;; (autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t)

;;------------------------------------------------------------------------------
;; time-stamp
;;------------------------------------------------------------------------------
(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-start "last update ")
(setq time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S")
(setq time-stamp-end "$")
(setq time-stamp-line-limit 20)
;;------------------------------------------------------------------------------
;; grep
;;------------------------------------------------------------------------------
(require 'grep)
(require 'grep-edit)

(defadvice grep-edit-change-file (around inhibit-read-only activate)
  ""
  (let ((inhibit-read-only t))
    ad-do-it))
;; (progn (ad-disable-advice 'grep-edit-change-file 'around 'inhibit-read-only) (ad-update 'grep-edit-change-file))

(defun my-grep-edit-setup ()
  (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  (set (make-local-variable 'inhibit-read-only) t)
  )
(add-hook 'grep-setup-hook 'my-grep-edit-setup t)

;;------------------------------------------------------------------------------
;; compile
;;------------------------------------------------------------------------------
(global-set-key "\C-c\C-c" 'compile)
(add-hook 'c-mode-common-hook
          (lambda ()(local-set-key "\C-c\C-c" 'compile)))
;;------------------------------------------------------------------------------
;; C/C++
;;------------------------------------------------------------------------------
;;(require 'google-c-style)
;;(add-hook 'c-mode-common-hook 'google-set-c-style)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "k&r")
             (setq indent-tabs-mode nil)
             (setq c-basic-offset 4)))
;;------------------------------------------------------------------------------
;; doxygen.el
;;------------------------------------------------------------------------------
(require 'doxygen)

(add-hook 'c-mode-common-hook
 '(lambda ()
    (global-set-key  "\C-c1" 'doxygen-insert-function-comment)
    (global-set-key  "\C-c2" 'doxygen-insert-comment)))

;;------------------------------------------------------------------------------
;; ruby
;;------------------------------------------------------------------------------
;;
;; mode
;;
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))


(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;;
;; irb
;;
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
;;
;; debugger
;;
(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)


;;------------------------------------------------------------------------------
;; ruby-block.el
;; http://d.hatena.ne.jp/khiker/20071130/emacs_ruby_block
;;------------------------------------------------------------------------------
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;;
;; refe
;;
(require 'refe)
;;
;; flymake
;;
(require 'flymake)
(set-face-background 'flymake-errline "red3")
(set-face-background 'flymake-warnline "orange3")
;;
;; ruby
;;
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(add-hook
 'ruby-mode-hook
 '(lambda ()
    ;; Don't want flymake mode for ruby regions in rhtml files
    (if (not (null buffer-file-name)) (flymake-mode))
    ;; エラー行で C-c d するとエラーの内容をミニバッファで表示する
    (define-key ruby-mode-map "\C-cd" 'credmp/flymake-display-err-minibuf)))

(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))
;;
;; http://blogs.dion.ne.jp/moe_moe/archives/7481321.html
;;
;;(setq ri-ruby-script "~/.emacs.d/elisp/ri-emacs.rb")
;;(autoload 'ri "ri-ruby.el" nil t)
;;(add-hook 'ruby-mode-hook (lambda ()
;;                            (local-set-key "\M-r" 'ri)
;;                            (local-set-key "\M-c" 'ri-ruby-complete-symbol)
;;                            (local-set-key "\M-g" 'ri-ruby-show-args)
;;                            ))
;;
;; gem install rcodetools
;;
;;(require 'anything-rcodetools)
;;------------------------------------------------------------------------------
;; YAML
;;------------------------------------------------------------------------------
(require 'yaml-mode)
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;;------------------------------------------------------------------------------
;; session
;;------------------------------------------------------------------------------
;; ミニバッファ履歴リストの最大長： t なら無限
(setq history-length t)
;; session.el
;;   kill-ring やミニバッファで過去に開いたファイルなどの履歴を保存する
(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回閉じたときの位置にカーソルを復帰
  (setq session-undo-check -1))
;;------------------------------------------------------------------------------
;; minibuf-isearch
;;------------------------------------------------------------------------------
;; minibuf-isearch
;;   minibuf で isearch を使えるようにする
(require 'minibuf-isearch nil t)
;;------------------------------------------------------------------------------
;; Dynamic Macro
;;------------------------------------------------------------------------------
;; http://pitecan.com/papers/JSSSTDmacro/dmacro.el
(defconst *dmacro-key* "\C-^" "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

;;------------------------------------------------------------------------------
;; GNU GLOBAL
;;------------------------------------------------------------------------------
(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbo)
         (local-set-key "\M-p" 'gtags-find-pattern)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))

(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))
(add-hook 'gtags-select-mode-hook 'hl-line-mode)
;;------------------------------------------------------------------------------
;; summarye 関数一覧の表示
;;------------------------------------------------------------------------------
(autoload 'se/make-summary-buffer "summarye" nil t)
(global-set-key [f11] 'se/make-summary-buffer)
(global-set-key "\C-x\C-l" 'se/make-summary-buffer)

;;------------------------------------------------------------------------------
;;; dabbrev
;;------------------------------------------------------------------------------
;;
;; dabbrev-japanese
;;
(require 'dabbrev)                    ; for dabbrev-abbrev-char-regexp
(defun dabbrev-expand-by-category (arg)
  (interactive "*P")
  (unless (bobp)
    (let (ch ch-ca-set regexp)
      (setq ch (char-before))
      (setq ch-ca-set (char-category-set ch))
      (setq regexp
            (cond ((aref ch-ca-set ?a)  ;"\\ca") ; ASCII
                   "[-_A-Za-z0-9]") ; 後ろ向き、うーんどうすればよいのやら
                  ((aref ch-ca-set ?j)           ; Japanese
                   (cond ((aref ch-ca-set ?K) "\\cK") ; katakana
                         ((aref ch-ca-set ?A) "\\cA") ; 2byte alphanumeric
                         ((aref ch-ca-set ?H) "\\cH") ; hiragana
                         ((aref ch-ca-set ?C)         ;"\\cC") ; kanji
                         ;"\\cC+\\cH*") ; for okuri-gana ; doesnot work
                          "\\cC")
                         (t "\\cj")))         ; fool-proof
                  ((aref ch-ca-set ?k) "\\ck") ; hankaku-kana
                  ((aref ch-ca-set ?r) "\\cr") ; Japanese roman ?
                  ;; etc.
                  (t dabbrev-abbrev-char-regexp))) ; save all
      (message (category-set-mnemonics ch-ca-set)) (message regexp)
      (let ((dabbrev-abbrev-char-regexp regexp)
            ;;(dabbrev-abbrev-skip-leading-regexp "\\s-")
            )
        (dabbrev-expand arg)))))
(substitute-key-definition 'dabbrev-expand 'dabbrev-expand-by-category esc-map)
;;-------------------------------------------------------------------
;; dabbrev-highlight
;;-------------------------------------------------------------------
(require 'dabbrev-highlight)
;;-------------------------------------------------------------------
;; dabbrev-expand-multiple
;;-------------------------------------------------------------------
(require 'dabbrev-expand-multiple)
(global-set-key "\M-/" 'dabbrev-expand-multiple)
;; 補完候補を一度に 3 つにする.
(setq dabbrev-expand-multiple-select-keys '("a" "s" "d"))


;;------------------------------------------------------------------------------
;; auto-complete
;; http://www.emacswiki.org/emacs/auto-complete.el
;;-----------------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-gtags)
;;(require 'ac-company)

;;(global-auto-complete-mode t)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)

;; ac-company で company-xcode を有効にする
;;(ac-company-define-source ac-source-company-xcode company-xcode)

(ac-config-default)
(add-hook 'auto-complete-mode-hook
          '(lambda ()
             (add-to-list 'ac-modes 'org-mode)
             (add-to-list 'ac-modes 'text-mode)
             (add-to-list 'ac-modes 'yaml-mode)
             (add-to-list 'ac-modes 'html-mode)
             (add-to-list 'ac-modes 'css-mode)
             (add-to-list 'ac-modes 'objc-mode)
             (add-to-list 'ac-modes 'makefile-mode)
             (add-to-list 'ac-modes 'asm-mode)))
;;             (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers))

(setq ac-candidate-menu-width 60)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;------------------------------------------------------------------------------
;; ac-anything
;; http://d.hatena.ne.jp/rubikitch/20090210/auto_complete_anything
;;------------------------------------------------------------------------------
(require 'ac-anything)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-anything)
;;;------------------------------------------------------------------------------
;;; vc-svn.el = subversion
;;------------------------------------------------------------------------------
(add-to-list 'vc-handled-backends 'SVN)
(setq vc-dired-recurse nil)
;;------------------------------------------------------------------------------
;; svn
;;------------------------------------------------------------------------------
(require 'psvn)
;; (require 'dsvn)
;; (autoload 'svn-status "dsvn" "Run `svn status'." t)
;; (autoload 'svn-update "dsvn" "Run `svn update'." t)

(when run-darwin
  (add-hook 'svn-log-edit-mode-hook
            '(lambda ()
               (set-buffer-file-coding-system 'utf-8)))
  (setq svn-status-svn-executable "svn2")
  (setq process-coding-system-alist
        (cons '("svn" . utf-8) process-coding-system-alist))
  (setq svn-status-svn-process-coding-system 'utf-8))

;;(global-set-key (kbd "M-:") 'svn-status)

;;------------------------------------------------------------------------------
;; egg -- Emacs Got Git
;; http://wiki.github.com/bogolisk/egg
;;------------------------------------------------------------------------------
(require 'egg)
;;------------------------------------------------------------------------------
;; imenu
;;------------------------------------------------------------------------------
(require 'imenu)
;;------------------------------------------------------------------------------
;; anything
;;------------------------------------------------------------------------------
(require 'anything-startup)
;;(setq anything-c-filelist-file-name "/tmp/all.filelist")
;;(setq anything-grep-candidates-fast-directory-regexp "^/tmp")

(setq anything-c-adaptive-history-file
      (expand-file-name "~/.emacs.d/anything/anything-history"))
(global-set-key (kbd "C-l") 'anything)
(global-set-key (kbd "C-M-l") 'anything-resume)
(require 'anything-gtags)
;;(anything-lisp-complete-symbol-set-timer 1500)

(setq anything-sources
      '( anything-c-source-buffers+
         anything-c-source-recentf
         anything-c-source-files-in-current-dir+
         anything-c-source-imenu
         anything-c-source-gtags-select
;;         anything-c-source-locate
;;         anything-c-source-bookmarks
         anything-c-source-emacs-commands
;;         anything-c-source-complete-shell-history
;;         anything-c-source-complex-command-history
         ))

;;-------------------------------------------------------------------
;; http://d.hatena.ne.jp/kitokitoki/20100925/p1
;;-------------------------------------------------------------------
(defvar anything-c-source-shell-history
  `((name . ".zsh_history")
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (insert-file-contents "~/.zsh_history"))))
    (candidates-in-buffer)
    (candidate-number-limit . 99999)
    (action ("Insert" . insert))))

(defun anything-shell-history ()
  (interactive)
  (anything 'anything-c-source-shell-history))

(add-hook 'shell-mode-hook
  (lambda()
    (define-key shell-mode-map (kbd "C-o") 'anything-shell-history)))

;;-------------------------------------------------------------------
;;  hatena-bookmark
;;-------------------------------------------------------------------
;; (require 'anything-hatena-bookmark)
;;-------------------------------------------------------------------
;;  anything-switch-project
;;  http://d.hatena.ne.jp/yaotti/20100403/1270293726
;;-------------------------------------------------------------------
;;(require 'anything-switch-project)
;;(global-set-key (kbd "C-c f") 'anything-switch-project)
;;
;; Org-mode
;;

(require 'org)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key global-map "\C-cl" 'org-store-link)
            (define-key global-map "\C-ca" 'org-agenda)
            (setq org-log-done t)))

;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;;
;; freemind
;;
;(require 'freemind)
;;
;; pdf
;;
(require 'doc-view)
;;
;; Twittering mode
;;
(require 'twittering-mode)
(setq twittering-username "powerbombkun")
(add-hook 'twittering-mode-hook
          (lambda ()
            (setq twittering-status-format "%i %s %@:\n %t // from %f%L")
            (twittering-icon-mode t)
            (twittering-scroll-mode t)
            (set-face-bold-p 'twittering-username-face t)
            (set-face-foreground 'twittering-username-face "DeepSkyBlue3")
            (set-face-foreground 'twittering-uri-face "gray35")
            (define-key twittering-mode-map (kbd "<") (lambda () (interactive) (goto-char (point-min))))
            (define-key twittering-mode-map (kbd ">") (lambda () (interactive) (goto-char (point-max))))))

;;(setq twittering-timer-interval 3600) ;; 1 hour
(global-set-key "\C-ct" 'twittering-mode)
;;
;; hatena
;;
;;(require 'simple-hatena-mode)
;;------------------------------------------------------------------------------
;; text-translator
;;------------------------------------------------------------------------------
;;(require 'text-translator)
;;(global-set-key "\C-x\M-t" 'text-translator)
;;(global-set-key "\C-x\M-T" 'text-translator-translate-last-string)
;;(setq text-translator-auto-selection-func
;;      'text-translator-translate-by-auto-selection-enja)
;;(global-set-key "\C-xt" 'text-translator-translate-by-auto-selection)
;;------------------------------------------------------------------------------
;; custom setting
;;------------------------------------------------------------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(fringe-mode 0 nil (fringe))
 '(tool-bar-mode nil)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )



;;------------------------------------------------------------------------------
;; install-elisp command
;;------------------------------------------------------------------------------
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;;------------------------------------------------------------------------------
;;rrse - ruby マニュアル
;;------------------------------------------------------------------------------
;; (load "rrse")
;; (rrse-setup)

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              (define-key ruby-mode-map [f1] 'rrse-help)))

;;------------------------------------------------------------------------------
;; pc-bufsw (C-tab でバッファ切り替え)
;;------------------------------------------------------------------------------
(require 'pc-bufsw)
(pc-bufsw::bind-keys (quote [C-tab]) (quote [C-S-tab]))

;;------------------------------------------------------------------------------
;; パスワード管理 http://d.hatena.ne.jp/pakepion/20081019/1224381473
;;------------------------------------------------------------------------------

(autoload 'alpaca-after-find-file "alpaca" nil t)
(add-hook 'find-file-hooks 'alpaca-after-find-file)

;;------------------------------------------------------------------------------
;; シンボリックリンクファイルを開く際の質問を表示しない
;;------------------------------------------------------------------------------

(setq vc-follow-symlinks t)

;;------------------------------------------------------------------------------
;; gdb
;;------------------------------------------------------------------------------

(setq gdb-many-windows t)
;; "IO buffer" が必要ない場合は  nil で
(setq gdb-use-separate-io-buffer t)
;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))

(add-hook
 'gdb-mode-hook
 '(lambda ()
    ; kye bindings ...
    (global-set-key  [f2] 'gud-break)
    (global-set-key  [f3] 'gud-remove)
    (global-set-key  [f4] 'gud-run)
    (global-set-key  [f5] 'gud-cont)
    (global-set-key  [f6] 'gud-finish)
    (global-set-key  [f7] 'gud-display)
    (global-set-key  [f8] 'gud-print)
    (global-set-key  [f9] 'gud-next)
    (global-set-key  [f10] 'gud-step)

    (setq mode-line-format
          "2:break 3:clear 4:run 5:continue 6:finish 7:display 8:print 9:next 10:step")))

;;------------------------------------------------------------------------------
;; テンプレート自動挿入
;;------------------------------------------------------------------------------

(require 'autoinsert)

;; テンプレートのディレクトリ
(setq auto-insert-directory "~/.emacs.d/insert/")
(setq user-full-name "Junsei Takahashi")

;; 各ファイルによってテンプレートを切り替える
(setq auto-insert-alist
      (nconc '(
               ("\\.cpp$" . ["template.cpp" my-template])
               ("\\.c$"   . ["template.c" my-template])
               ("\\.h$"   . ["template.h" my-template])
               ("\\.sh$"  . ["template.sh" my-template])
               ("\\.rb$"  . ["template.rb" my-template])
               ("\\.mk$"  . ["template.mk" my-template])
               ("makefile". ["template.mk" my-template])
               ("Makefile". ["template.mk" my-template])
               ) auto-insert-alist))

;; ここが腕の見せ所
(defvar template-replacements-alists
  '(("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%name%"             . user-full-name)
    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%"    . (lambda () (format "_%s_H" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))))

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
        (progn
          (goto-char (point-min))
          (replace-string (car c) (funcall (cdr c)) nil)))
    template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;;------------------------------------------------------------------------------
;; 画面分割 + 移動
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
;;------------------------------------------------------------------------------
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-u") 'other-window-or-split)

;;(define-key global-map "\C-t" 'other-window)


;;------------------------------------------------------------------------------
;; プルダウンメニュー補完
;; http://d.hatena.ne.jp/kitokitoki/20091108/p4
;;------------------------------------------------------------------------------
;;(require 'pulldown)


;;------------------------------------------------------------------------------
;; オートインデント
;; http://at-aka.blogspot.com/2006/12/emacs-c.html
;;------------------------------------------------------------------------------
;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              ;; センテンスの終了である ';' を入力したら、自動改行+インデント
;;              (c-toggle-auto-hungry-state 1)
;;              ;; RET キーで自動改行+インデント
;;              (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

;;------------------------------------------------------------------------------
;; c-mode補完
;; http://d.hatena.ne.jp/khiker/20060710/1152550709
;;------------------------------------------------------------------------------
;; (load "brackets")
;; (add-hook 'c-mode-common-hook
;;           '(lambda()
;;              (progn
;;                ;; { で{}を書く
;;                (define-key c-mode-map "{" 'insert-braces)
;;                ;; ( で()を書く
;;                (define-key c-mode-map "(" 'insert-parens)
;;                ;; " で""を書く
;;                (define-key c-mode-map "\"" 'insert-double-quotation)
;;                ;; [ で[]を書く
;;                (define-key c-mode-map "[" 'insert-brackets)
;;                ;; Ctrl+c }でregionを{}で囲む
;;                (define-key c-mode-map "\C-c}" 'insert-braces-region)
;;                ;; Ctrl+c )でregionを()で囲む
;;                (define-key c-mode-map "\C-c)" 'insert-parens-region)
;;                ;; Ctrl+c ]でregionを[]で囲む
;;                (define-key c-mode-map "\C-c]" 'insert-brackets-region)
;;                ;; Ctrl+c "でregionを""で囲む
;;                (define-key c-mode-map "\C-c\"" 'insert-double-quotation-region)
;;                )))

;;------------------------------------------------------------------------------
;; one-key.el
;; http://d.hatena.ne.jp/rubikitch/20090127/onekey
;;------------------------------------------------------------------------------

;; (require 'one-key)

;; (defvar one-key-menu-Register-and-Rectangle-alist nil
;;   "The `one-key' menu list for REGISTER-AND-RECTANGLE.")

;; (setq one-key-menu-Register-and-Rectangle-alist
;;       '(
;;         (("C-@" . "point-to-register") . point-to-register)
;;         (("SPC" . "point-to-register") . point-to-register)
;;         (("+" . "increment-register") . increment-register)
;;         (("b" . "bookmark-jump") . bookmark-jump)
;;         (("c" . "clear-rectangle") . clear-rectangle)
;;         (("d" . "delete-rectangle") . delete-rectangle)
;;         (("f" . "frame-configuration-to-register") . frame-configuration-to-register)
;;         (("g" . "insert-register") . insert-register)
;;         (("i" . "insert-register") . insert-register)
;;         (("j" . "jump-to-register") . jump-to-register)
;;         (("k" . "kill-rectangle") . kill-rectangle)
;;         (("l" . "bookmark-bmenu-list") . bookmark-bmenu-list)
;;         (("m" . "bookmark-set") . bookmark-set)
;;         (("n" . "number-to-register") . number-to-register)
;;         (("o" . "open-rectangle") . open-rectangle)
;;         (("r" . "copy-rectangle-to-register") . copy-rectangle-to-register)
;;         (("s" . "copy-to-register") . copy-to-register)
;;         (("t" . "string-rectangle") . string-rectangle)
;;         (("w" . "window-configuration-to-register") . window-configuration-to-register)
;;         (("x" . "copy-to-register") . copy-to-register)
;;         (("y" . "yank-rectangle") . yank-rectangle)
;;         (("C-SPC" . "point-to-register") . point-to-register)
;;         ))

;; (defun one-key-menu-Register-and-Rectangle ()
;;   "The `one-key' menu for REGISTER-AND-RECTANGLE"
;;   (interactive)
;;   (one-key-menu "REGISTER-AND-RECTANGLE" one-key-menu-Register-and-Rectangle-alist))

;; (define-key global-map "\C-xr" 'one-key-menu-Register-and-Rectangle)

;;-------------------------------------------------------------------
;;  winden-window.el
;;  http://d.hatena.ne.jp/hayamiz/20081113/1226568875
;;-------------------------------------------------------------------
;;(require 'widen-window)
;;(global-widen-window-mode t)

;; \C-aでインデントを飛ばした行頭に移動
;; (defun beginning-of-indented-line (current-point)
;;   "インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデント文字しかない場合は、行頭に戻る。"
;;   (interactive "d")
;;   (if (string-match
;;        "^[ ¥t]+$"
;;        (save-excursion
;;          (buffer-substring-no-properties
;;           (progn (beginning-of-line) (point))
;;           current-point)))
;;       (beginning-of-line)
;;     (back-to-indentation)))

;; (global-set-key "\C-a" 'beginning-of-indented-line)

;;-------------------------------------------------------------------
;;  smart-compile.el
;;-------------------------------------------------------------------
(require 'smart-compile)
(global-set-key "\C-c\C-v" 'smart-compile)

;;-------------------------------------------------------------------
;;  auto-install.el
;;  http://d.hatena.ne.jp/rubikitch/20091221/autoinstall
;;-------------------------------------------------------------------
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
;;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)             ; 互換性確保

;;-------------------------------------------------------------------
;;  text-translator.el
;;  http://d.hatena.ne.jp/rubikitch/20100228/translator
;;-------------------------------------------------------------------
(require 'text-translator)
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)

;; debug
(add-hook 'c-mode-common-hook
        '(lambda ()
            (define-key c-mode-base-map "\C-c\C-g"       'gdb)))

(add-hook 'ruby-mode-hook
        '(lambda ()
            (define-key ruby-mode-map "\C-c\C-g"       'rubydb)))

;;-------------------------------------------------------------------
;;  pos-tip.el
;;  http://d.hatena.ne.jp/m2ym/20100320/1269073216
;;-------------------------------------------------------------------
(require 'pos-tip)

(defun my-describe-function (function)
  "Display the full documentation of FUNCTION (a symbol) in tooltip."
  (interactive (list (function-called-at-point)))
  (if (null function)
      (pos-tip-show
       "** You didn't specify a function! **" '("red"))
    (pos-tip-show
     (with-temp-buffer
       (let ((standard-output (current-buffer)))
         (prin1 function)
         (princ " is ")
         (describe-function-1 function)
         (buffer-string)))
     nil nil nil 0)))

(define-key emacs-lisp-mode-map (kbd "C-;") 'my-describe-function)
;;-------------------------------------------------------------------
;;  前回終了時のサイズを記憶
;;
;;-------------------------------------------------------------------
(defun my-window-size-save ()
  (let* ((rlist (frame-parameters (selected-frame)))
         (ilist initial-frame-alist)
         (nCHeight (frame-height))
         (nCWidth (frame-width))
         (tMargin (if (integerp (cdr (assoc 'top rlist)))
                      (cdr (assoc 'top rlist)) 0))
         (lMargin (if (integerp (cdr (assoc 'left rlist)))
                      (cdr (assoc 'left rlist)) 0))
         buf
         (file "~/.framesize.el"))
    (when (file-writable-p file)
      (with-temp-buffer
        (erase-buffer)
        (insert (concat
                 ;; 初期値をいじるよりも modify-frame-parameters
                 ;; で変えるだけの方がいい?
                 "(delete 'width initial-frame-alist)\n"
                 "(delete 'height initial-frame-alist)\n"
                 "(delete 'top initial-frame-alist)\n"
                 "(delete 'left initial-frame-alist)\n"
                 "(setq initial-frame-alist (append (list\n"
                 "'(width . " (int-to-string nCWidth) ")\n"
                 "'(height . " (int-to-string nCHeight) ")\n"
                 "'(top . " (int-to-string tMargin) ")\n"
                 "'(left . " (int-to-string lMargin) "))\n"
                 "initial-frame-alist))\n"
                 ;;"(setq default-frame-alist initial-frame-alist)"
                 ))
        (write-region (point-min) (point-max) file)
        ))))

(defun my-window-size-load ()
  (let* ((file "~/.framesize.el"))
    (if (file-exists-p file)
        (load-library file))))

(my-window-size-load)

;; Call the function above at C-x C-c.
(defadvice save-buffers-kill-emacs
  (before save-frame-size activate)
  (my-window-size-save))


;バックアップファイルを作らない
(setq make-backup-files nil)

;;-------------------------------------------------------------------
;;  redmine.el
;;  http://e-arrows.sakura.ne.jp/2010/03/released-redmine-el.html
;;-------------------------------------------------------------------
;; (require 'redmine)

;; (require 'js-doc)
;; (add-hook 'c-mode-common-hook
;;           #'(lambda ()
;;               (define-key c-mode-map "\C-ci" 'js-doc-insert-function-doc)
;;               (define-key c-mode-map "@" 'js-doc-insert-tag)))

;;-------------------------------------------------------------------
;;  c-eldoc.el
;;  http://d.hatena.ne.jp/mooz/20100421/p1
;;-------------------------------------------------------------------
(require 'c-eldoc)
(require 'eldoc-extension)
(setq eldoc-idle-delay 0.20)
(setq eldoc-echo-area-use-multiline-p t)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-turn-on-eldoc-mode)
            ))


;;-------------------------------------------------------------------
;;  test-case-mode.el
;;  http://d.hatena.ne.jp/rubikitch/20100501/testcase
;;-------------------------------------------------------------------
(require 'test-case-mode)
(add-hook 'find-file-hook 'enable-test-case-mode-if-test)
;;(add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)

(require 'css-mode)
;;-------------------------------------------------------------------
;;  imenu-tree
;;  http://d.hatena.ne.jp/kitokitoki/20100517/p2
;;-------------------------------------------------------------------
;; (require 'imenu-tree)
;; (global-set-key (kbd "M-h") 'imenu-tree)


;;-------------------------------------------------------------------
;;  japanese-holidays.el
;;  http://d.hatena.ne.jp/rubikitch/20090216/1234746280
;;-------------------------------------------------------------------
(require 'calendar)
(setq  number-of-diary-entries 31)
(define-key calendar-mode-map "f" 'calendar-forward-day)
(define-key calendar-mode-map "n" 'calendar-forward-day)
(define-key calendar-mode-map "b" 'calendar-backward-day)
(setq mark-holidays-in-calendar t)
;; (install-elisp "http://www.meadowy.org/meadow/netinstall/export/799/branches/3.00/pkginfo/japanese-holidays/japanese-holidays.el")
(require 'japanese-holidays)
(setq calendar-holidays
      (append japanese-holidays local-holidays other-holidays))
(setq calendar-weekend-marker 'diary)
(add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
(add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)

;;-------------------------------------------------------------------
;;  ewm.el
;;  http://d.hatena.ne.jp/kiwanami/20100528/1275038929
;;-------------------------------------------------------------------
;;(require 'ewm)
;;(global-set-key (kbd "M-+") 'ewm:start-management)

;;-------------------------------------------------------------------
;;  e-palette.el
;;  http://fujim.tumblr.com/post/361299617/emacs-e-palette-el
;;-------------------------------------------------------------------
;; (autoload 'e-palette "e-palette" nil)
;; (define-key global-map [f3] 'e-palette)

;;-------------------------------------------------------------------
;;  git-status,hg-status
;;  http://d.hatena.ne.jp/kitokitoki/20100824/p1
;;-------------------------------------------------------------------
(require 'git-status)
(require 'hg-status)

;;-------------------------------------------------------------------
;;  test-case-mode.el
;;  http://d.hatena.ne.jp/rubikitch/20100501/testcase
;;-------------------------------------------------------------------
(require 'test-case-mode)
(add-hook 'find-file-hook 'enable-test-case-mode-if-test)
(add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)

;;-------------------------------------------------------------------
;;  sr-speedbar.el
;;  http://d.hatena.ne.jp/wadap/20100828/1282984286
;;-------------------------------------------------------------------
;; (require 'sr-speedbar)
;; (setq sr-speedbar-right-side nil)

;;-------------------------------------------------------------------
;;  file-info
;;  http://ubulog.blogspot.com/2007/12/emacs_30.html
;;-------------------------------------------------------------------
(defun file-info () "
    show current buffer information"
  (interactive)
  (if (buffer-file-name (current-buffer))
      (progn
        (let* ((file-name (buffer-file-name (current-buffer)))
               (f-attr (file-attributes file-name))
               (f-size (nth 7 f-attr))  ; ファイルサイズ
               (f-mode (nth 8 f-attr))  ; ファイル属性
               (mes1 (format "file path: %s\n" file-name))
               (mes2 (format "file size: %s byte\n" f-size))
               (mes3 (format "file type: %s" f-mode))
               (mess (concat mes1 mes2 mes3)))
          (message "%s" mess)))
nil ))

(global-set-key "\C-c\C-i" 'file-info)

;;------------------------------------------------------------------------------
;; 行番号表示
;;------------------------------------------------------------------------------
(global-set-key [f9] 'linum-mode)

;;------------------------------------------------------------------------------
;; template
;; http://www.bookshelf.jp/soft/meadow_37.html#SEC544
;;------------------------------------------------------------------------------
(defvar my-template-text-file "~/.emacs.d/.template")
(defvar my-template-buffer nil)
(defvar my-template-point nil)

(defun my-template-insert ()
  (interactive)
  (let (content)
    (when (setq
           content
           (get-text-property (point) :content))
      (save-excursion
        (set-buffer my-template-buffer)
        (save-excursion
          (goto-char my-template-point)
          (insert content))))))

(defun my-template-select ()
  (interactive)
  (let ((buffer
         (get-buffer-create "*select template*"))
        templates begin template-map text)
    (setq my-template-buffer (current-buffer)
          my-template-point  (point))
    (unless (file-readable-p my-template-text-file)
      (error "Couldn't read template file: %s"))
    (with-temp-buffer
      (insert-file-contents my-template-text-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\(.*\\)$" nil t)
        (when begin
          (setq templates
                (cons
                 (cons
                  (car templates)
                  (buffer-substring
                   begin (1- (match-beginning 0))))
                 (cdr templates))))
        (setq templates (cons (match-string 1) templates))
        (setq begin (1+ (match-end 0))))
      (when begin
        (setq templates
              (cons
               (cons
                (car templates)
                (buffer-substring begin (point-max)))
               (cdr templates)))))
    (pop-to-buffer buffer)
    (setq buffer-read-only nil
          major-mode       'template-select-mode
          mode-name        "Select Template"
          template-map     (make-keymap))
    (suppress-keymap template-map)
    (define-key template-map " "    'my-template-insert)
    (define-key template-map "\C-m" 'my-template-insert)
    (define-key template-map "n"    'next-line)
    (define-key template-map "p"    'previous-line)
    (define-key template-map "q"    'kill-buffer-and-window)
    (use-local-map template-map)
    (buffer-disable-undo)
    (delete-region (point-min) (point-max))
    (dolist (tt templates)
      (setq text (concat (car tt) "\n"))
      (put-text-property
       0 (length text) :content (cdr tt) text)
      (insert text)
      (goto-char (point-min)))
    (delete-region (1- (point-max)) (point-max))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

;;------------------------------------------------------------------------------
;; calculate bootup time
;; http://www.gfd-dennou.org/member/uwabami/cc-env/EmacsBasic.html
;;------------------------------------------------------------------------------
;; (defun message-startup-time ()
;;   (message
;;    "Emacs loaded in %dms"
;;    (/ (- (+ (third after-init-time) (* 1000000 (second after-init-time)))
;;          (+ (third before-init-time) (* 1000000 (second before-init-time))))
;;       1000)))
;; (add-hook 'after-init-hook 'message-startup-time)

;;------------------------------------------------------------------------------
;; リージョンを選択してruby実行
;; http://d.hatena.ne.jp/kitokitoki/20101016/p5
;;------------------------------------------------------------------------------
(defun my-ruby-eval-region ()
  (interactive)
  (when (region-active-p)
    (let ((region-str (buffer-substring-no-properties (region-beginning) (region-end)))
          (result-buf "*ruby*")
          (temp-file (make-temp-file "my-ruby-eval-region-")))
      (with-temp-file temp-file
        (insert region-str))
      (shell-command (concat "ruby " temp-file) result-buf)
      (view-buffer-other-window result-buf t
                                (lambda (buf)
                                  (kill-buffer-and-window)
                                  (delete-file temp-file))))))
(add-hook 'ruby-mode-hook
  (lambda()
    (define-key ruby-mode-map (kbd "C-c C-r") 'my-ruby-eval-region)
    ))

