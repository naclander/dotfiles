;; -*- lexical-binding: t -*-

; Byte compile everything after emacs upgrade
;(byte-recompile-directory package-user-dir nil 'force)

; Function to use to reinstall ( resync/redownload ) all installed packages
;http://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs
;(defun package-reinstall-activated ()
;  "Reinstall all activated packages."
;  (interactive)
;  (dolist (package-name package-activated-list)
;    (when (package-installed-p package-name)
;      (unless (ignore-errors                   ;some packages may fail to install
;                (package-reinstall package-name)
;                (warn "Package %s failed to reinstall" package-name))))))



; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; I find that C-h C-f makes a good keybinding for find-function
; given that c-h f is bound by default to describe-function
(global-set-key (kbd "C-h C-f") 'find-function)

; Scroll only one line at a time like in vim
(setq scroll-step            1
      scroll-conservatively  10000)

; scroll one line at a time (less "jumpy" than defaults) ( using mouse )
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

; Disable menu-bar
(menu-bar-mode -1)

; Highlight current line
(global-hl-line-mode 1)

; Add Syntax highlighting
; -- Systemd
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))

; IX pastebin package
(require 'ix)

; Set redraw disaply key-map
; Also clear highlighting -- needed for persistent highlighting:
; https://github.com/juanjux/evil-search-highlight-persist
(global-set-key (kbd "C-l") (lambda ()
			      (interactive)
			      (evil-search-highlight-persist-remove-all)))

; Pretty lambdas!
(add-hook 'after-change-major-mode-hook 'prettify-symbols-mode)

; Enabel winner-mode, for window manipulation
(add-hook 'after-change-major-mode-hook 'winner-mode)

; Max and min buttons for GUI emacs
; We need the zoom-frm package because standard text increase decrease
; does not work well with https://github.com/alpaker/Fill-Column-Indicator
(if window-system (progn
    (global-set-key (kbd "C--" ) 'zoom-frm-out)
    (global-set-key (kbd "C-=") 'zoom-frm-in)
    (global-set-key (kbd "C-0") 'zoom-frm-unzoom)))

; Disable welcome menu
(setq inhibit-startup-screen t)

; Disable toolbar
(tool-bar-mode -1)

; Set customization file somewhere else
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

; Load the theme
; Originally I just did a load-theme, but that caused wierd white space issues.
(load-theme 'sanityinc-tomorrow-eighties t)
; Load this last so that git-guttter can set the face too.
;(add-hook 'after-init-hook (lambda () (load-theme 'sanityinc-tomorrow-eighties)))

; Highlight parentheses.
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control.
(setq vc-make-backup-files t)

; Enable visual-line-mode ( set wrap on )
(global-visual-line-mode t)

; Set default browser
(setq browse-url-firefox-program "firefox-aurora")
(setq browse-url-browser-function 'browse-url-firefox)

(global-set-key (kbd "C-x b") 'helm-mini)

;----------------------------------Projectile-----Configuration-------------------

(require 'projectile)
(setq projectile-enable-caching t)

;--------------------------------------------------------------------------------

;----------------------------------Undo-Tree-----Configuration-------------------

; Permanent undo
(setq undo-tree-auto-save-history t)

; Set undo directory
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;--------------------------------------------------------------------------------

;----------------------------------iBuffer-----Configuration---------------------

; Change list-buffers to ibuffer
(defalias 'list-buffers 'ibuffer)

;--------------------------------------------------------------------------------

;----------------------------------Tramp ----Configuration-----------------------

(require 'tramp)

; Recommendation from arch-linux wiki re: slow networking in tramp
; https://wiki.archlinux.org/index.php/emacs#When_network_is_limited
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(tramp-set-completion-function "sshx"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))
(setq tramp-default-method "sshx")

; The timeout is number of seconds since last remote command
; for rereading remote directory contents.
; 0 re-reads immediately during file name completion, nil
; uses cached directory contents. 
(setq tramp-completion-reread-directory-timeout nil)

;--------------------------------------------------------------------------------

;-------------------------auto-highlight-symbol----Configuration-----------------

; As described here:
; https://www.hiroom2.com/2016/10/31/emacs-auto-highlight-symbol-package/
(add-hook 'after-init-hook 'global-auto-highlight-symbol-mode)

;--------------------------------------------------------------------------------

;----------------------------------linum-mode----Configuration-------------------

; Add line numbers
(global-nlinum-mode t)
;(global-linum-mode t)
; Set a bar after line numbers
(setq linum-format "%2d\u2502 ")
(setq nlinum-format "%2d\u2502 ")

; Linum-mode fix for zoom in/out in gui mode
(require 'linum)
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
          (ceiling (* (if (boundp 'text-scale-mode-step)
                  (expt text-scale-mode-step
                    text-scale-mode-amount) 1)
              (if (car (window-margins))
                  (car (window-margins)) 1)
              ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

;--------------------------------------------------------------------------------

;----------------------------------winner-mode---Configuration-------------------

; Mappings for undoing and redoing window configurations
(global-set-key (kbd "C-S-U" ) 'winner-undo)
(global-set-key (kbd "C-S-R") 'winner-redo)

;--------------------------------------------------------------------------------

;----------------------------------Org-wikish Configuration----------------------

; org-wikish is not in melpa yet
(add-to-list 'load-path "~/.emacs.d/misc/org-wikish/")
(load "org-wikish")

; Set the org-wikish directory
(setq org-wikish-notes-directory "~/.emacs.d/org-wikish/")

; There is also an evil org-wikish keybinding in the Evil section

;--------------------------------------------------------------------------------

;----------------------------------Org Configuration-----------------------------

(require 'org)

(setq org-return-follows-link t)
(setq org-open-non-existing-files t)

(defun org-insert-heading-with-timestamp ()
  (interactive)
  (org-insert-heading-respect-content)
  (org-time-stamp-inactive))

(define-key org-mode-map (kbd "<C-return>") 'org-insert-heading)
(define-key org-mode-map (kbd "<C-S-return>") 'org-insert-heading-with-timestamp)

; Open org links in the same window 
(setq org-link-frame-setup '((file . find-file)))

; Start all org files unfolded by default
(setq org-startup-folded nil)

;--------------------------------------------------------------------------------

;----------------------------------Python Configuration--------------------------

; Enable elpy
(elpy-enable)

; Set ipython as the shell interpreter ( such as when pressing C-c C-c )
(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt")

;--------------------------------------------------------------------------------

; --------------------------------Rainbow-mode Configuration --------------------

; Enable rainbow-mode
(require 'rainbow-mode)
; Originally I used the 'after-chage-major-mode-hook and it broke colors in
; helm and in magit -- not sure why
(add-hook 'prog-mode-hook 'rainbow-mode)

;--------------------------------------------------------------------------------

; --------------------------------Eyebrowse Configuration ---------------------

; I would have preferred to use perspective-el:
; https://github.com/nex3/perspective-el
; but that project currently does not work with emacs26
; https://github.com/nex3/perspective-el/issues/64
; I should switch to that when they fix the issues
; For now, lets just use eyebrowse ( meh )
; https://github.com/wasamasa/eyebrowse
(eyebrowse-mode)

; I also have a hydra configuration in the hydra section

;--------------------------------------------------------------------------------

; --------------------------------Shackle Configuration -------------------------

; Align helm and help windows at the bottom with a ratio of 40%
; https://github.com/wasamasa/shackle
(shackle-mode t)
(setq helm-display-function 'pop-to-buffer) ; make helm play nice
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.5)
                      ("\\`\\*help.*?\\*\\'" :regexp t :align t :size 0.5)))

;--------------------------------------------------------------------------------

; --------------------------------Rainbow Delimiters Configuration --------------

; Enable rainbow parentheses
( add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

; Custom face for rainbow parentheses taken from
;https://ericscrivner.me/2015/06/better-emacs-rainbow-delimiters-color-scheme/
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))


;--------------------------------------------------------------------------------

; --------------------------------simpleclip Configuration ----------------------

(require 'simpleclip)

(simpleclip-mode 1)

; Map simpleclip to keybindings used in terminals
(global-set-key (kbd "C-S-C" ) 'simpleclip-copy)
(global-set-key (kbd "C-S-X" ) 'simpleclip-cut)
(global-set-key (kbd "C-S-V" ) 'simpleclip-paste)

;--------------------------------------------------------------------------------

; --------------------------------Fill Column Indicator Configuration -----------

; https://www.emacswiki.org/emacs/FillColumnIndicator

; Set the column at 80
(setq fci-rule-column 80)
(setq fci-rule-width 10)
(setq fci-rule-color "red")
(fci-mode t)
(add-hook 'after-change-major-mode-hook 'fci-mode)

; Workaround for fci-mode and company-mode
; https://github.com/alpaker/Fill-Column-Indicator/issues/54
(defun on-off-fci-before-company(command)
   (when (string= "show" command)
     (turn-off-fci-mode))
   (when (string= "hide" command)
     (turn-on-fci-mode)))

(advice-add 'company-call-frontends :before #'on-off-fci-before-company)

;--------------------------------------------------------------------------------

; --------------------------------Git-Gutter Configuration ----------------------

; For now, disable git-gutter until
; https://github.com/syohex/emacs-git-gutter/issues/143
; is resolved

(require 'git-gutter)

; Enable git-gutter
;(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

; Live update
(setq git-gutter:modified-sign "≈")
(setq git-gutter:update-interval 2)


;; ; Customize symbols and colors

;; ; Because we have an after-init-hook for the theme we need to have one for this too
 (add-hook 'after-init-hook (lambda ()
 			     (set-face-foreground 'git-gutter:modified "gold")))

;--------------------------------------------------------------------------------

; --------------------------------Evil Configuration ----------------------------
(require 'highlight)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(require 'evil)
(require 'evil-magit)

; Make C-j and C-k move down and up 10 lines
(evil-global-set-key 'motion (kbd "C-j")
		     (lambda ()
		       (interactive
			(evil-next-line 10))))
(evil-global-set-key 'motion (kbd "C-k")
		     (lambda ()
		       (interactive
			(evil-previous-line 10))))

; Go back to previous buffer
(evil-global-set-key 'motion (kbd "C-b") 'evil-switch-to-windows-last-buffer)

; Map ';' to bring up the evil command buffer
(evil-global-set-key 'motion ";" 'evil-ex)

; Enable evil mode in all buffers
(setq evil-motion-state-modes (append evil-emacs-state-modes
			       evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

; Treat '_' and '-' as a word character
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

(evil-mode 1)

; org-wikish keybindings. This makes it so that you can press enter to create
; and follow links, just like in vimwiki
(defun evil-org-follow-link()
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (org-open-at-point)
     (org-wikish-link-word-at-point)))
(evil-define-key 'normal org-mode-map (kbd "RET") 'evil-org-follow-link)

;--------------------------------------------------------------------------------

;---------------------------------Highlight sexp Configuration -----------------------

;https://www.emacswiki.org/emacs/HighlightSexp
(require 'highlight-sexp)
(add-hook 'lisp-mode-hook 'highlight-sexp-mode)
(add-hook 'clojure-mode-hook 'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)

;--------------------------------------------------------------------------------

;---------------------------------Powerline Configuration -----------------------
(require 'powerline)
(require 'powerline-evil)

; Taken from:
; https://github.com/aaronbieber/dotfiles/blob/master/configs/emacs.d/lisp/init-powerline.el
(defface my-pl-segment1-active
  '((t (:foreground "#000000" :background "#E1B61A")))
  "Powerline first segment active face.")
(defface my-pl-segment1-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline first segment inactive face.")
(defface my-pl-segment2-active
  '((t (:foreground "#F5E39F" :background "#8A7119")))
  "Powerline second segment active face.")
(defface my-pl-segment2-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline second segment inactive face.")
(defface my-pl-segment3-active
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
  "Powerline third segment active face.")
(defface my-pl-segment3-inactive
  '((t (:foreground "#CEBFF3" :background "#3A2E58")))
"Powerline third segment inactive face.")

(defun air--powerline-default-theme ()
  "Set up my custom Powerline with Evil indicators."
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
                          (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
                          (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (powerline-evil-tag) evil-face)
                                         ))
                                     (if evil-mode
                                         (funcall separator-left (powerline-evil-face) seg1))
                                     (powerline-buffer-id seg1 'l)
                                     (powerline-raw "[%*]" seg1 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format seg1 'l))
                                     (powerline-raw " " seg1)
                                     (funcall separator-left seg1 seg2)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object seg2 'l))
                                     (powerline-major-mode seg2 'l)
                                     (powerline-process seg2)
                                     (powerline-minor-modes seg2 'l)
                                     (powerline-narrow seg2 'l)
                                     (powerline-raw " " seg2)
                                     (funcall separator-left seg2 seg3)
                                     (powerline-vc seg3 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) seg3 'l))))
                          (rhs (list (powerline-raw global-mode-string seg3 'r)
                                     (funcall separator-right seg3 seg2)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) seg2 'l))
                                     (powerline-raw "%4l" seg2 'l)
                                     (powerline-raw ":" seg2 'l)
                                     (powerline-raw "%3c" seg2 'r)
                                     (funcall separator-right seg2 seg1)
                                     (powerline-raw " " seg1)
                                     (powerline-raw "%6p" seg1 'r)
                                     (when powerline-display-hud
                                       (powerline-hud seg1 seg3)))))
                     (concat (powerline-render lhs)
                             (powerline-fill seg3 (powerline-width rhs))
(powerline-render rhs)))))))

;(powerline-default-theme)
(air--powerline-default-theme)

;--------------------------------------------------------------------------------

; --------------------------------Company Mode Configuration --------------------

; Enable company-mode
(add-hook 'after-init-hook 'global-company-mode)

; Vim keybinding in company-mode window
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

; Enable company-quickhelp
; EDIT: company-quickhelp does not currently work in terminal mode:
; https://github.com/expez/company-quickhelp/issues/62
; EDIT EDIT: company-quickhelp also overrides company-active-map somehow
; so I can't rebind my keys, like above. I'm giving up on this package for now,
; even though its pretty cool.
(setq company-quickhelp-delay 10)
(company-quickhelp-mode 1)
company--electric-saved-window-configuration

;--------------------------------------------------------------------------------

;--------------------------------Helm Configuration -----------------------------

; Enable helm-mode
(helm-mode 1)

; Map M-x to start helm
(global-set-key (kbd "M-x") 'helm-M-x)

; Define things like helm-map
(require 'helm)
(require 'helm-config)

; Use C-j and C-k to navigate inside helm buffer
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)

; Use tab to give function definition
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)


(define-key helm-map (kbd "C-b") 'helm-find-files-up-one-level)


;--------------------------------------------------------------------------------


;----------------------------------Hydra Configuration---------------------------

(require 'hydra)

(global-set-key
 (kbd "C-M-e")
 (defhydra hydra-eval (:exit t)
   "Evaluate a"
   ("r" eval-region "region" )
   ("b" eval-buffer "buffer" ) ))
   

; Easy window splitting commands using Hydra
; Taken from http://oremacs.com/2015/02/03/one-hydra-two-hydra/
(global-set-key
 (kbd "C-M-w")
 (defhydra hydra-window (:exit t)
   "window"
   ("h" evil-window-left)
   ("j" evil-window-down)
   ("k" evil-window-up)
   ("l" evil-window-right)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
        "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
        "horz")
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
        "swap")
   ("d" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
        "del")
   ("o" delete-other-windows "max")
   ("m" winner-undo "min")
   ("q" nil "cancel")))

; Use hydra to control eyebrowse:
(global-set-key
 (kbd "C-M-SPC")
 (defhydra hydra-perspective (:exit t)
   "perspective"
   ( "s" eyebrowse-switch-to-window-config "switch")
   ( "c" eyebrowse-create-window-config "create")
   ( "k" eyebrowse-close-window-config "kill")
   ( "r" eyebrowse-rename-window-config "rename")
   ( "n" eyebrowse-next-window-config "next")
   ( "p" eyebrowse-prev-window-config "previous")))

; Use hydra for projectile
; We need to "unbind" the dired keymap so that in works in dired mode. Then we
; need to bind the hydra using bind-key*, becayse global-set-key doesn't work, for some reason.
 (add-hook 'dired-mode-hook 'my-dired-mode-hook)
     (defun my-dired-mode-hook ()
       (define-key dired-mode-map (kbd "C-M-p") nil))
(bind-key*
 (kbd "C-M-p")
 (defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find               Search/Tags          Project
------------------------------------------------------------------------------------------
_f_: file            _a_: helm-ag           _i_: Ibuffer
_d_: dir             _A_: ag                _D_: Dired
_b_: buffer          _o_: search-buffers    _s_: shell
_r_: recent file     _g_: grep

  "
  ("a"   helm-projectile-ag)
  ("A"   projectile-ag)
  ("b"   helm-projectile-switch-to-buffer)
  ("d"   helm-projectile-find-dir)
  ("f"   helm-projectile-find-file)
  ("g"   ggtags-update-tags)
  ("i"   helm-projectile-ibuffer)
  ("o"   projectile-multi-occur)
  ("r"   helm-projectile-recentf)
  ("D"   projectile-dired)
  ("s"   projectile-run-shell)
  ("q"   nil "cancel" :color blue)))

;--------------------------------------------------------------------------------
