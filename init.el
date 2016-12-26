;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     ocaml
     html
     python
     rust
     javascript
     typescript
     markdown
     helm
     auto-completion
     better-defaults
     emacs-lisp
     common-lisp
     git
     markdown
     org
     ranger
     games
     (shell :variables
            shell-default-height 20
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)
     syntax-checking
     version-control
     (latex :variables
            latex-build-command "LaTeX")
     )
   dotspacemacs-additional-packages
   '(
     pug-mode
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(evil-search-highlight-persist)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((projects . 3)
                                (recents . 2))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(material
                         material-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Inconsolata"
                               :size 20
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil))

(defun dotspacemacs/user-init ()
  (setq ranger-override-dired t))

(defun dotspacemacs/user-config ()
  ;; Function definitions
  (defun latex-build-for-tex ()
    "Run latex build when .tex file is saved."
    (when (string= (file-name-extension buffer-file-name) "tex")
      (latex/build)))
  (defun silence ()
    (interactive))

  ;; Setup for proof-general
  (load "~/.emacs.d/lisp/PG/generic/proof-site")
  (eval-after-load "proof-script"
    '(progn
       (define-key
         proof-mode-map
         (kbd "<M-down>")
         'proof-assert-next-command-interactive)
       (define-key
         proof-mode-map
         (kbd "<M-up>")
         'proof-undo-last-successful-command)))

  ;; Mode hooks
  (add-to-list 'auto-mode-alist '("\\.lalrpop$" . rust-mode))

  ;; Key bindings
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'before-save-hook 'latex-build-for-tex)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (evil-ex-define-cmd "q[uit]" 'evil-window-delete)
  (define-key evil-motion-state-map [down-mouse-1] 'silence)
  (define-key evil-motion-state-map [mouse-1] 'silence)

  ;; Safe local variables
  (put 'compilation-read-command 'safe-local-variable 'null))
