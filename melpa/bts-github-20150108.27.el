;;; bts-github.el --- A plugin of bts.el for GitHub -*- coding: utf-8; -*-

;; Copyright (C) 2015  Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Keywords: convenience
;; Package-Version: 20150108.27
;; URL: https://github.com/aki2o/emacs-bts-github
;; Version: 0.0.5
;; Package-Requires: ((bts "0.0.1") (gh "0.8.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; Nothing.

;;; Dependencies:
;; 
;; - bts.el ( see <https://github.com/aki2o/emacs-bts> )
;; - gh.el ( see <https://github.com/sigma/gh.el> )

;;; Installation:
;;
;; Put this to your load-path.
;; And put the following lines in your .emacs or site-start.el file.
;; 
;; (require 'bts-github)

;;; Configuration:
;;
;; ;; About config item, see Customization or eval the following sexp.
;; ;; (customize-group "bts-github")

;;; Customization:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'user-variable :prefix "bts-github:[^:]" :docstring t)
;; `bts-github:ignore-labels'
;; List of label name applied `bts:summary-ignored-ticket-face'.
;; `bts-github:max-specpdl-size'
;; Number for `max-specpdl-size' changed for fetching big data temporarily.
;; `bts-github:max-lisp-eval-depth'
;; Number for `max-lisp-eval-depth' changed for fetching big data temporarily.
;; `bts-github:summary-id-width'
;; Width of issue ID column in summary buffer.
;; `bts-github:summary-label-width'
;; Width of issue labels column in summary buffer.
;; `bts-github:summary-label-decorating'
;; Whether to decorate issue labels column.
;; 
;;  *** END auto-documentation

;;; API:
;; 
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "bts-github:[^:]" :docstring t)
;; `bts-github:make-project-view'
;; Function for `bts:system-project-view'.
;; `bts-github:make-query-view'
;; Function for `bts:system-query-view'.
;; `bts-github:make-ticket-single-view'
;; Function for `bts:system-ticket-single-view'.
;; `bts-github:fetch-issue'
;; Function for `bts:system-ticket-fetcher'.
;; `bts-github:fetch-latest-issue'
;; Function for `bts:system-ticket-latest'.
;; `bts-github:regist-issue'
;; Function for `bts:system-ticket-register'.
;; `bts-github:make-summary-format'
;; Function for `bts:system-summary-format'.
;; `bts-github:make-ticket-unique-string'
;; Function for `bts:system-ticket-unique-string'.
;; 
;;  *** END auto-documentation
;; [Note] Functions and variables other than listed above, Those specifications may be changed without notice.

;;; Tested On:
;; 
;; - Emacs ... GNU Emacs 24.3.1 (i686-pc-linux-gnu, GTK+ Version 3.4.2) of 2014-02-22 on chindi10, modified by Debian
;; - bts.el ... Version 0.0.1
;; - gh.el


;; Enjoy!!!


;;; Code:
(require 'cl-lib)
(require 'bts)
(require 'gh-repos)
(require 'gh-issues)
(require 'gh-issue-comments)
(require 'gh-users)

(defgroup bts-github nil
  "A plugin of bts for GitHub."
  :group 'bts
  :prefix "bts-github:")

(defcustom bts-github:ignore-labels '("duplicate" "invalid" "wontfix")
  "List of label name applied `bts:summary-ignored-ticket-face'."
  :type '(repeat string)
  :group 'bts-github)

(defcustom bts-github:max-specpdl-size 13000
  "Number for `max-specpdl-size' changed for fetching big data temporarily."
  :type 'integer
  :group 'bts-github)

(defcustom bts-github:max-lisp-eval-depth 6000
  "Number for `max-lisp-eval-depth' changed for fetching big data temporarily."
  :type 'integer
  :group 'bts-github)

(defcustom bts-github:summary-id-width 4
  "Width of issue ID column in summary buffer."
  :type 'integer
  :group 'bts-github)

(defcustom bts-github:summary-label-width 15
  "Width of issue labels column in summary buffer."
  :type 'integer
  :group 'bts-github)

(defcustom bts-github:summary-label-decorating t
  "Whether to decorate issue labels column."
  :type 'boolean
  :group 'bts-github)

(defface bts-github:issue-comment-header-face
  '((t (:inherit font-lock-function-name-face :underline t)))
  "Face for a header of issue comments in issue widget buffer."
  :group 'bts-github)

(defface bts-github:summary-label-face
  '((t (:box t)))
  "Face for each label of issue labels column in summary buffer."
  :group 'bts-github)


;;;;;;;;;;;;;
;; Utility

(defun bts-github::hash-exists-p (key table)
  (let ((novalue (cl-gensym)))
    (not (eq (gethash key table novalue) novalue))))

(defvar bts-github::timer-hash (make-hash-table :test 'equal))
(defmacro bts-github::with-prefer-cache (cache-key cache-var force cb lexical-vars &rest body)
  (declare (indent 0))
  `(or (when (not ,force)
         (gethash ,cache-key ,cache-var))
       (when (not (bts-github::hash-exists-p ,cache-key ,cache-var))
         (bts:aif (not ,cb)
             ;; use yaxception:$~ to avoid freeze from sexp which uses defclass
             (yaxception:$~
               (yaxception:try
                 (let ((ret (puthash ,cache-key (progn ,@body) ,cache-var)))
                   (bts--debug "github cached data of %s" ,cache-key)
                   (bts--trace* "github full cached data is\n%s" ret)
                   ret))
               (yaxception:catch 'error e
                 (bts--error "github failed delayed work for %s : %s"
                             ,cache-key (yaxception:get-text e))))
           (when (or (not (gethash ,cache-key bts-github::timer-hash))
                     (not (timerp (gethash ,cache-key bts-github::timer-hash))))
             (lexical-let (,@(loop for v in lexical-vars
                                   collect (list v v)))
               (puthash ,cache-key
                        (run-with-idle-timer
                         1 nil (lambda (key var cb)
                                 ;; use yaxception:$~ to avoid freeze from sexp which uses defclass
                                 (yaxception:$~
                                   (yaxception:try
                                     (bts--debug "github start delayed work for %s" key)
                                     (let ((ret (puthash key (progn ,@body) var))
                                           (timer (gethash key bts-github::timer-hash)))
                                       (bts--debug "github cached data of %s" key)
                                       (bts--trace* "github full cached data is\n%s" ret)
                                       (when (timerp timer)
                                         (cancel-timer timer))
                                       (bts--debug "github call delayed work callback for %s" key)
                                       (run-with-timer 0 nil cb ret)))
                                   (yaxception:catch 'error e
                                     (bts--error "github failed delayed work for %s : %s"
                                                 key (yaxception:get-text e))
                                     (bts:show-message "Failed delayed work for %s : %s"
                                                       key (yaxception:get-text e)))
                                   (yaxception:finally
                                     (remhash key bts-github::timer-hash))))
                         ,cache-key ,cache-var ,cb)
                        bts-github::timer-hash)))
           'scheduled))))

(defmacro bts-github::pick-up-data (&rest body)
  (declare (indent 0))
  `(oref (progn ,@body) :data))

(defun* bts-github::get-api-instance (&rest args &key target account token &allow-other-keys)
  (let ((cls (case target
               (repo          'gh-repos-api)
               ((label issue) 'gh-issues-api)
               (comment       'gh-issue-comments-api)
               (user          'gh-users-api))))
    (make-instance cls
                   :auth (make-instance 'gh-oauth-authenticator
                                        :username account
                                        :token token))))

(defun* bts-github::fetch-any-response (&rest args &key target query params &allow-other-keys)
  (bts--debug "github start fetch any response for %s.\nquery... %s\nparams... %s"
              target query params)
  (let* ((max-specpdl-size bts-github:max-specpdl-size)
         (max-lisp-eval-depth bts-github:max-lisp-eval-depth)
         (api (apply 'bts-github::get-api-instance args))
         (cls (case target
                (repo  (oref api repo-cls))
                (label (oref api label-cls))
                (issue (oref api issue-cls))
                (user  (oref api users-cls)))))
    (bts-github::pick-up-data
      (gh-api-authenticated-request
       api (gh-object-list-reader cls) "GET" query nil params))))

(defsubst bts-github::make-up-string-response (str)
  (replace-regexp-in-string "\r" "" str))


(defvar bts-github::cache-repository (make-hash-table :test 'equal))

(defun bts-github::get-repo-cache-key (type owner)
  (format "repo:%s/%s" type owner))

(defun bts-github::clear-repo-cache (type owner)
  (remhash (bts-github::get-repo-cache-key type owner)
           bts-github::cache-repository))

(defsubst* bts-github::fetch-your-repo (&rest args)
  (apply 'bts-github::fetch-any-response :target 'repo :query "/user/repos" args))

(defsubst* bts-github::fetch-user-repo (&rest args &key owner &allow-other-keys)
  (bts-github::pick-up-data
    (bts--debug "github start gh-repos-user-list")
    (gh-repos-user-list (apply 'bts-github::get-api-instance :target 'repo args) owner)))

(defsubst* bts-github::fetch-org-repo (&rest args &key owner &allow-other-keys)
  (bts-github::pick-up-data
    (bts--debug "github start gh-repos-org-list")
    (gh-repos-org-list (apply 'bts-github::get-api-instance :target 'repo args) owner)))

(defun* bts-github::fetch-repo (&rest args &key type owner force cb &allow-other-keys)
  (bts--debug "github start fetch repo of %s as %s" owner type)
  (let ((key (bts-github::get-repo-cache-key type owner)))
    (bts-github::with-prefer-cache
      key bts-github::cache-repository force cb (args type)
      (case type
        (own  (apply 'bts-github::fetch-your-repo args))
        (user (apply 'bts-github::fetch-user-repo args))
        (org  (apply 'bts-github::fetch-org-repo args))
        (t    (error (bts:make-message "Failed github fetch repository. Invalid type : %s" type)))))))

(defun* bts-github::fetch-repo-names (&rest args)
  (let ((repos (apply 'bts-github::fetch-repo args)))
    (if (symbolp repos)
        repos
      (loop for repo in repos
            collect (oref repo :name)))))


(defvar bts-github::cache-label (make-hash-table :test 'equal))

(defun bts-github::get-label-cache-key (owner repo)
  (format "label:%s/%s" owner repo))

(defun bts-github::clear-label-cache (owner repo)
  (remhash (bts-github::get-label-cache-key owner repo)
           bts-github::cache-label))

(defun* bts-github::fetch-label (&rest args &key owner repo force cb &allow-other-keys)
  (bts--debug "github start fetch label of %s in %s" repo owner)
  (let ((key (bts-github::get-label-cache-key owner repo)))
    (bts-github::with-prefer-cache
      key bts-github::cache-label force cb (args repo owner)
      (bts-github::pick-up-data
        (gh-issues-label-list (apply 'bts-github::get-api-instance :target 'label args) owner repo)))))

(defun* bts-github::fetch-label-names (&rest args)
  (let ((lbls (apply 'bts-github::fetch-label args)))
    (if (symbolp lbls)
        lbls
      (loop for lbl in lbls
            collect (oref lbl :name)))))


(defvar bts-github::cache-assignee (make-hash-table :test 'equal))

(defun bts-github::get-assignee-cache-key (owner repo)
  (format "assignee:%s/%s" owner repo))

(defun bts-github::clear-assignee-cache (owner repo)
  (remhash (bts-github::get-assignee-cache-key owner repo)
           bts-github::cache-assignee))

(defun* bts-github::fetch-assignee (&rest args &key owner repo force cb &allow-other-keys)
  (bts--debug "github start fetch assignee of %s in %s" repo owner)
  (let ((key (bts-github::get-assignee-cache-key owner repo)))
    (bts-github::with-prefer-cache
      key bts-github::cache-assignee force cb (args repo owner)
      (let ((query (format "/repos/%s/%s/assignees" owner repo)))
        (apply 'bts-github::fetch-any-response :target 'user :query query args)))))

(defun* bts-github::fetch-assignee-names (&rest args)
  (let ((users (apply 'bts-github::fetch-assignee args)))
    (if (symbolp users)
        users
      (loop for user in users
            collect (oref user :login)))))


(defvar bts-github::cache-comment (make-hash-table :test 'equal))

(defun bts-github::get-comment-cache-key (owner repo issue)
  (format "comment:%s/%s/%s" owner repo issue))

(defun bts-github::clear-comment-cache (owner repo issue)
  (remhash (bts-github::get-comment-cache-key owner repo issue)
           bts-github::cache-comment))

(defun* bts-github::fetch-comment (&rest args &key owner repo issue force cb &allow-other-keys)
  (bts--debug "github start fetch comment of %s/%s in %s" repo issue owner)
  (let ((key (bts-github::get-comment-cache-key owner repo issue))
        (issue (cond ((stringp issue) (string-to-number issue))
                     (t               issue))))
    (bts-github::with-prefer-cache
      key bts-github::cache-comment force cb (args repo owner issue)
      (bts-github::pick-up-data
        (gh-issue-comments-list (apply 'bts-github::get-api-instance :target 'comment args)
                                owner repo issue)))))


(defvar bts-github::regexp-date
  (rx-to-string `(and bos
                      (group (+ (any "0-9"))) "-" (group (+ (any "0-9"))) "-" (group (+ (any "0-9")))
                      "T"
                      (group (+ (any "0-9"))) ":" (group (+ (any "0-9"))) ":" (group (+ (any "0-9")))
                      "Z"
                      eos)))
(defsubst bts-github::conv-tz-to-seconds (tz)
  (if (not (string-match bts-github::regexp-date tz))
      (bts--error "github failed conv tz to seconds : %s" tz)
    (float-time (encode-time (string-to-number (match-string-no-properties 6 tz))
                             (string-to-number (match-string-no-properties 5 tz))
                             (string-to-number (match-string-no-properties 4 tz))
                             (string-to-number (match-string-no-properties 3 tz))
                             (string-to-number (match-string-no-properties 2 tz))
                             (string-to-number (match-string-no-properties 1 tz))))))


(defvar bts-github::msg-now-loading
  (propertize "Now loading..." 'face 'font-lock-keyword-face))


;;;;;;;;;;;;;
;; Project

(defvar bts-github::msg-fill-access-not-yet
  (propertize "Input YourName/AccessToken" 'face 'font-lock-doc-face))

(defvar bts-github::msg-fill-repo-not-yet
  (propertize "Input Type/Owner" 'face 'font-lock-doc-face))

(defun bts-github::gen-project-repos-layout ()
  (when (eq (bts:widget-get-value 'target) 'choice)
    `("[ Select Target Repositories ]" BR
      bts:widget-expand-flex-layout BR BR)))

(defun bts-github::gen-project-flex-option-layout ()
  (let* ((idx (bts:widget-get-flex-current-index))
         (mdl (bts:widget-get-model))
         (repo-type (bts:widget-get-flex-value 'repo-type idx mdl))
         (repo-owner (or (bts:widget-get-flex-value 'repo-owner idx mdl) ""))
         (account (or (bts:widget-get-value 'account mdl) ""))
         (token (or (bts:widget-get-value 'token mdl) ""))
         (repo-lbl "Name"))
    (cond
     ((or (not repo-type) (string= repo-owner ""))
      `((:type label :value ,repo-lbl :unarrayed t) bts-github::msg-fill-repo-not-yet " "))
     ((or (string= account "") (string= token ""))
      `((:type label :value ,repo-lbl :unarrayed t) bts-github::msg-fill-access-not-yet " "))
     (t
      (let ((repos (bts-github::fetch-repo-names
                    :account account
                    :token token
                    :type (if (string= account repo-owner) 'own repo-type)
                    :owner repo-owner
                    :cb `(lambda (repos)
                           (bts:widget-update-buffer :buffer-or-name ,(buffer-name) :showp t)))))
        (if (eq repos 'scheduled)
            `((:type label :value ,repo-lbl :unarrayed t) bts-github::msg-now-loading " ")
          `(:type select :name repo :label ,repo-lbl :unarrayed t :options ,repos :require t)))))))

(defun bts-github::reload-project-master (mdl)
  (setq bts-github::cache-repository (make-hash-table :test 'equal))
  (bts:widget-update-buffer :showp t))

(defun bts-github::submit-project (mdl)
  (bts--debug "github start submit project.")
  (let ((repos (loop for m in (bts:widget-get-flex-models mdl)
                     for type = (assoc-default 'repo-type m)
                     for owner = (assoc-default 'repo-owner m)
                     for name = (assoc-default 'repo m)
                     if (and type
                             (not (string= owner ""))
                             (not (string= name "")))
                     collect `(:type ,type :owner ,owner :name ,name))))
    
    `(,@(bts:make-plist-from-alist mdl :includes '(account target))
      :secret-token ,(assoc-default 'token mdl)
      :repos ,repos)))

(defun bts-github:make-project-view (project)
  "Function for `bts:system-project-view'."
  (let* ((topts '(("All issues in owned/member/organization repositories" . all)
                  ("All issues in member/organization repositories"       . other)
                  ("All issues in owned repositories"                     . own)
                  ("Selected Repositories Issues"                         . choice)))
         (lo `((:type text :name account :label "YourName" :size 15 :require t :leave on-update) BR
               (:type text :name token :label "AccessToken" :size 50 :require t :leave on-update) BR
               (:type select :name target :label "Target" :options ,topts :require t :action on-update
                      :value all)
               BR BR (bts-github::gen-project-repos-layout)
               "  " (:type button :name clear-cache :title " Clear Cache "
                           :action bts-github::reload-project-master)))
         (flo '((:type select :name repo-type :label "Type" :unarrayed t :require t
                       :options (("user" . user) ("organization" . org)) :radio t :horizontal t
                       :value user)
                (:type text :name repo-owner :label "Owner" :unarrayed t :size 15 :require t
                       :leave on-update)
                (bts-github::gen-project-flex-option-layout)))
         (defs (when project
                 (bts:make-alist-from-plist project
                                            :includes '(account token target))))
         (fdefs (when project
                  (loop for repo in (plist-get project :repos)
                        collect `((repo-type  . ,(plist-get repo :type))
                                  (repo-owner . ,(plist-get repo :owner))
                                  (repo       . ,(plist-get repo :name)))))))
    `(:layout ,lo :flex-layout ,flo :defaults ,defs :flex-defaults ,fdefs
              :submit-action bts-github::submit-project)))


;;;;;;;;;;;
;; Query

(defvar bts-github::query-label-help
  (concat
   "Choice of 'all' means any labels are target.
Choice of 'select' means the target is read from right text box.
If the value is empty, it means unlimit.

In the text box,
"
   bts:complex-condition-help))

(defvar bts-github::query-relates
  '(("Issues assigned to you"                   . assigned)
    ("Issues created by you"                    . created)
    ("Issues mentioning you"                    . mentioned)
    ("Issues you are subscribed to updates for" . subscribed)))

(defun bts-github:make-query-view (project query)
  "Function for `bts:system-query-view'."
  (let* ((lo `((:type select :name lbl-include-type :label "IncludedLabel" :options (all select)
                      :require t :radio t :horizontal t :value select :action on-update)
               (when (memq (bts:widget-get-value 'lbl-include-type) '(select nil))
                 `(" " (:type text :name lbl-include :size 50 :validate bts:complex-condition-validation)))
               " " (:type link :tip bts-github::query-label-help)
               BR (:type select :name lbl-exclude-type :label "ExcludedLabel" :options (all select)
                         :require t :radio t :horizontal t :value select :action on-update)
               (when (memq (bts:widget-get-value 'lbl-exclude-type) '(select nil))
                 `(" " (:type text :name lbl-exclude :size 50 :validate bts:complex-condition-validation)))
               " " (:type link :tip bts-github::query-label-help)
               BR (:type select :name state :label "State" :options (open closed all)
                         :require t :radio t :horizontal t :value open)
               BR (:type select :name relate-type :label "Relationship" :options (unlimit limit)
                         :require t :radio t :horizontal t :value unlimit :action on-update)
               (when (memq (bts:widget-get-value 'relate-type) '(limit))
                 `(BR (:type select :name relates :options ,bts-github::query-relates
                             :label "" :label-face default
                             :multiple t :value ,(mapcar 'cdr bts-github::query-relates))))))
         (defs (when query
                 (bts:make-alist-from-plist query))))
    `(:layout ,lo :defaults ,defs)))


;;;;;;;;;;;;;;;;;
;; View Ticket

(defun bts-github::gen-ticket-repo-type-layout ()
  (let* ((proj (bts:widget-get-local-attribute 'project))
         (target (plist-get proj :target))
         (updatep (bts:widget-get-local-attribute 'ticket)))
    (when (and (not updatep)
               (not (eq target 'choice)))
      ;; Need issue repo type if repos of project is not selected in add view
      `(:type select :name type :label "Type"
              :options (("user" . user) ("organization" . org)) :require t
              :radio t :horizontal t :value user))))

(defun bts-github::gen-ticket-repo-owner-layout ()
  (let* ((proj (bts:widget-get-local-attribute 'project))
         (target (plist-get proj :target))
         (updatep (bts:widget-get-local-attribute 'ticket))
         (owners (when (and (not updatep)
                            (eq target 'choice))
                   (-uniq (loop for r in (plist-get proj :repos)
                                collect (plist-get r :owner)))))
         (type-p (and (not updatep)
                      (not (eq target 'choice)))))
    `((when ,type-p
        ;; Insert space because a top of parts is a repo type selection
        "  ")
      (:type ,(cond (updatep 'const)
                    (owners  'select)
                    (t       'text))
             :name owner :label "Owner" :require t :leave on-update
             ,@(cond (owners (list :options owners))
                     (t      (list :size 15 :value (plist-get proj :account))))
             :unarrayed ,type-p))))

(defun bts-github::gen-ticket-repo-name-layout ()
  (let* ((proj (bts:widget-get-local-attribute 'project))
         (target (plist-get proj :target))
         (updatep (bts:widget-get-local-attribute 'ticket))
         (cargs '(:name repo :label "Repo" :unarrayed t :require t :action on-update))
         (mdl (bts:widget-get-model))
         (owner (or (bts:widget-get-value 'owner mdl) ""))
         (type (bts:widget-get-value 'type mdl)))
    `("  "
      ,@(cond (updatep
               ;; Be const in update view because repo of issue is immutable
               `((:type const ,@cargs)))
              ((string= owner "")
               ;; Nothing while owner is not yet inputed
               `((:type const ,@cargs :value "") "Owner is not yet inputed"))
              ((eq target 'choice)
               ;; If project type is choice, make parts using project config
               (let ((opts (-uniq (loop for r in (plist-get proj :repos)
                                        if (string= (plist-get r :owner) owner)
                                        collect (plist-get r :name)))))
                 (if (= (length opts) 1)
                     ;; If candidates is only one, selection is no needed
                     `((:type const ,@cargs :value ,(car opts)))
                   `((:type select ,@cargs :options ,opts)))))
              ((not type)
               ;; Nothing while repo type is not yet selected
               `((:type const ,@cargs :value "") "Type is not yet selected"))
              (t
               ;; Try to fetch repo list of owner
               (let* ((account (plist-get proj :account))
                      (repos (bts-github::fetch-repo-names
                              :acount account
                              :token (plist-get proj :token)
                              :type (if (string= account owner) 'own type)
                              :owner owner
                              :cb `(lambda (repos)
                                     (bts:widget-update-buffer :buffer-or-name ,(buffer-name) :showp t)))))
                 (if (eq repos 'scheduled)
                     ;; If fetching is scheduled, print message about it
                     `((:type const ,@cargs :value "") bts-github::msg-now-loading)
                   ;; If fetching is completed, make select using the result
                   `((:type select ,@cargs :options ,repos)
                     " " (:type link :title "reload" :action bts-github::reload-issue-repo)))))))))

(defun bts-github::reload-issue-repo (mdl)
  (let* ((proj (bts:widget-get-local-attribute 'project))
         (owner (or (bts:widget-get-value 'owner mdl) ""))
         (type (bts:widget-get-value 'type mdl))
         (account (plist-get proj :account)))
    (when (and type
               (not (string= owner "")))
      (bts-github::clear-repo-cache (if (string= account owner) 'own type)
                                    owner)
      (bts:widget-update-buffer :showp t))))

(defun bts-github::gen-ticket-header-layout ()
  (let ((tic (bts:widget-get-local-attribute 'ticket)))
    (when tic ; means update view
      `("  " (:type const :name number :label "Number" :unarrayed t)
        BR (:type link :name url :label "URL" :url ,(plist-get tic :url))
        BR (:type select :name state :label "State" :options (open closed) :require t
                  :radio t :horizontal t)))))

(defun bts-github::gen-ticket-label-layout ()
  (let* ((proj (bts:widget-get-local-attribute 'project))
         (tic (bts:widget-get-local-attribute 'ticket))
         (cargs '(:name labels :label "Labels"))
         (mdl (bts:widget-get-model))
         (owner (or (bts:widget-get-value 'owner mdl) ""))
         (repo (or (bts:widget-get-value 'repo mdl) "")))
    (if (or (string= owner "") (string= repo ""))
        ;; Print message while repo is not yet identified
        `(BR (:type const ,@cargs :value "") "Repository is not yet inputed")
      ;; Try to fetch label list of repo
      (let ((lbls (bts-github::fetch-label-names
                   :account (plist-get proj :account)
                   :token (plist-get proj :token)
                   :owner owner
                   :repo repo
                   :cb `(lambda (lbls)
                          (bts:widget-update-buffer :buffer-or-name ,(buffer-name) :showp t)))))
        (if (eq lbls 'scheduled)
            ;; If fetching is scheduled, print message about it
            `(BR (:type const ,@cargs :value "") bts-github::msg-now-loading)
          ;; If fetching is completed, make select using the result
          `(BR (:type select ,@cargs :options ,lbls :multiple t)
               " " (:type link :title "reload" :action bts-github::reload-issue-label)))))))

(defun bts-github::reload-issue-label (mdl)
  (let* ((owner (or (bts:widget-get-value 'owner mdl) ""))
         (repo (or (bts:widget-get-value 'repo mdl) "")))
    (when (and (not (string= owner ""))
               (not (string= repo "")))
      (bts-github::clear-label-cache owner repo)
      (bts:widget-update-buffer :showp t))))

(defun bts-github::gen-ticket-assignee-layout ()
  (let* ((proj (bts:widget-get-local-attribute 'project))
         (tic (bts:widget-get-local-attribute 'ticket))
         (cargs '(:name assigned-user :label "Assignee"))
         (mdl (bts:widget-get-model))
         (owner (or (bts:widget-get-value 'owner mdl) ""))
         (repo (or (bts:widget-get-value 'repo mdl) "")))
    (if (or (string= owner "") (string= repo ""))
        ;; Print message while repo is not yet identified
        `(BR (:type const ,@cargs :value "") "Repository is not yet inputed")
      ;; Try to fetch label list of repo
      (let ((users (bts-github::fetch-assignee-names
                    :account (plist-get proj :account)
                    :token (plist-get proj :token)
                    :owner owner
                    :repo repo
                    :cb `(lambda (users)
                           (bts:widget-update-buffer :buffer-or-name ,(buffer-name) :showp t)))))
        (if (eq users 'scheduled)
            ;; If fetching is scheduled, print message about it
            `(BR (:type const ,@cargs :value "") bts-github::msg-now-loading)
          ;; If fetching is completed, make select using the result
          `(BR (:type select ,@cargs :options ,users)
               " " (:type link :title "reload" :action bts-github::reload-issue-assignee)))))))

(defun bts-github::reload-issue-assignee (mdl)
  (let* ((owner (or (bts:widget-get-value 'owner mdl) ""))
         (repo (or (bts:widget-get-value 'repo mdl) "")))
    (when (and (not (string= owner ""))
               (not (string= repo "")))
      (bts-github::clear-assignee-cache owner repo)
      (bts:widget-update-buffer :showp t))))

(defsubst bts-github::get-comment-id (comm)
  (let ((url (oref comm :url)))
    (when (string-match "comments/\\([0-9]+\\)\\'" url)
      (string-to-number (match-string-no-properties 1 url)))))

(defun bts-github::gen-ticket-comment-layout ()
  (let ((proj (bts:widget-get-local-attribute 'project))
        (tic (bts:widget-get-local-attribute 'ticket)))
    (when tic
      (let* ((account (plist-get proj :account))
             (owner (plist-get tic :owner))
             (repo (plist-get tic :repo))
             (cuser (plist-get tic :create-user))
             (csec (plist-get tic :created-seconds))
             (body (plist-get tic :body))
             (body-editable-p (bts-github::comment-editable-p account cuser))
             (body-lo (when (and owner repo cuser csec)
                        (bts-github::get-comment-layout cuser csec 'body body body-editable-p)))
             ;; Always fetch because ':comments' is not implemented in gh-issues.el
             ;; (comms-length (or (plist-get tic :comments) 0))
             (comms-length 1)
             (comms (when (> comms-length 0)
                      (bts-github::fetch-comment
                       :account account
                       :token (plist-get proj :token)
                       :owner owner
                       :repo repo
                       :issue (plist-get tic :number)
                       :cb `(lambda (lbls)
                              (bts:widget-update-buffer :buffer-or-name ,(buffer-name) :showp t)))))
             (comms-lo (if (eq comms 'scheduled)
                           '(bts-github::msg-now-loading BR BR)
                         (loop for e in comms
                               for user = (oref (oref e :user) :login)
                               for sec = (bts-github::conv-tz-to-seconds (oref e :updated_at))
                               for editable-p = (bts-github::comment-editable-p account user)
                               for name = (when editable-p
                                            (intern (format "comment-%i" (bts-github::get-comment-id e))))
                               for body = (bts-github::make-up-string-response (oref e :body))
                               append (bts-github::get-comment-layout user sec name body editable-p)))))
        `(BR BR (:type label :value "Comments")
             (:type link :title "reload" :action bts-github::reload-issue-comment)
             BR BR
             ,@body-lo
             ,@comms-lo)))))

(defun bts-github::comment-editable-p (account comm-user)
  (ignore-errors (string= account comm-user)))

(defun bts-github::get-comment-layout (user sec name value editable-p)
  (let* ((dt (format-time-string "%Y/%m/%d %H:%M" (seconds-to-time sec)))
         (hline (format "%s commented on %s" user dt)))
    (list (propertize hline 'face 'bts-github:issue-comment-header-face) 'BR
          (cond (editable-p `(:type text :name ,name :area t :value ,value))
                (name       `(:type const :name ,name :value ,value))
                (t          value))
          'BR
          (when (not editable-p) 'BR))))

(defun bts-github::reload-issue-comment (mdl)
  (let ((tic (bts:widget-get-local-attribute 'ticket)))
    (bts-github::clear-comment-cache (plist-get tic :owner)
                                     (plist-get tic :repo)
                                     (plist-get tic :number))
    (bts:widget-update-buffer :showp t)))

(defun bts-github:make-ticket-single-view (project ticket)
  "Function for `bts:system-ticket-single-view'."
  (let* ((new-comm-lbl (if ticket "New Comment" "Body"))
         (lo `((bts-github::gen-ticket-repo-type-layout)
               (bts-github::gen-ticket-repo-owner-layout)
               (bts-github::gen-ticket-repo-name-layout)
               (bts-github::gen-ticket-header-layout)
               BR (:type text :name title :label "Title" :size 50 :require t)
               (bts-github::gen-ticket-label-layout)
               (bts-github::gen-ticket-assignee-layout)
               (bts-github::gen-ticket-comment-layout)
               BR (:type text :name new-comment :label ,new-comm-lbl :area t)))
         (defs (if ticket
                   (bts:make-alist-from-plist ticket
                                              :excludes '(new-comment)
                                              :exclude-regexp "\\`comment-")
                 `((type  . user)
                   (owner . ,(plist-get project :account)))))
         (attrs `((project . ,project)
                  (ticket  . ,ticket))))
    `(:layout ,lo :defaults ,defs :attributes ,attrs)))


;;;;;;;;;;;;;;;;;;
;; Fetch Ticket

(defsubst bts-github::get-repo-of (issue)
  (let ((url (oref issue :url)))
    (if (string-match "/repos/[^/]+/\\([^/]+\\)/" url)
        (match-string-no-properties 1 url)
      (bts--error "github failed get repo of issue : %s" (oref issue :title)))))

(defsubst bts-github::get-repo-owner-of (issue)
  (let ((url (oref issue :url)))
    (if (string-match "/repos/\\([^/]+\\)/" url)
        (match-string-no-properties 1 url)
      (bts--error "github failed get repo owner of issue : %s" (oref issue :title)))))

(bts:regist-message 'bts-github-ticket-make-failed
  t         "Failed to make ticket from %s/%s's issue[%i]"
  'Japanese "%s/%sのissue[%i]のチケットを作成できませんでした")

(defun bts-github::make-tickets (issues)
  (bts--debug "github start make tickets. issues[%s]" (length issues))
  (loop for i in issues
        for tic = (yaxception:$~
                    (yaxception:try
                      (bts--trace* "github make ticket from %s" i)
                      `(:url ,(oref i :html-url)
                             :repo ,(bts-github::get-repo-of i)
                             :owner ,(bts-github::get-repo-owner-of i)
                             :number ,(oref i :number)
                             :state ,(intern (oref i :state))
                             :title ,(oref i :title)
                             :body ,(bts-github::make-up-string-response (oref i :body))
                             :labels ,(loop for l in (oref i :labels)
                                            collect (assoc-default 'name l))
                             :milestone ,(when (slot-exists-p i :milestone)
                                           (oref (oref i :milestone) :title))
                             :create-user ,(oref (oref i :user) :login)
                             :closed-user ,(when (slot-exists-p i :closed_by)
                                             (oref (oref i :closed_by) :login))
                             :assigned-user ,(when (slot-exists-p i :assignee)
                                               (oref (oref i :assignee) :login))
                             :created-seconds ,(bts-github::conv-tz-to-seconds (oref i :created_at))
                             ;; Not exists slot in gh-issues.el
                             ;; :updated-seconds ,(bts-github::conv-tz-to-seconds (oref i :updated_at))
                             ;; :closed-seconds ,(bts:awhen (oref i :closed_at)
                             ;;                    (bts-github::conv-tz-to-seconds it))
                             ;; :comments ,(oref i :comments)
                             ))
                    (yaxception:catch 'error e
                      (bts--error "github failed make ticket : %s\nissue... %s"
                                  (yaxception:get-text e) i)
                      (ignore-errors
                        (bts:show-message (bts:get-message 'bts-github-ticket-make-failed
                                                           (bts-github::get-repo-owner-of i)
                                                           (bts-github::get-repo-of i)
                                                           (oref i :number))))))
        if tic collect tic))

(defun bts-github::grep-issues-by-regexp (issues propnm regexp match)
  (bts--debug "github start grep issues by regexp. propnm[%s] regexp[%s] match[%s]"
              propnm regexp match)
  (loop for i in issues
        for v = (case propnm
                  (repo  (bts-github::get-repo-of i))
                  (owner (bts-github::get-repo-owner-of i))
                  (title (oref i :title))
                  (body  (oref i :body)))
        for mret = (string-match regexp v)
        if (and match mret)
        collect i
        else if (and (not match) (not mret))
        collect i))

(defun bts-github::grep-issues-by-any-label (issues exclude-p)
  (bts--debug "github start grep issues by any label. issues[%s] exclude-p[%s]"
              (length issues) exclude-p)
  (loop for i in issues
        for has-label = (> (length (oref i :labels)) 0)
        if (or (and exclude-p (not has-label))  ; If exclude, collect issue don't have any label.
               (and (not exclude-p) has-label)) ; If include, collect issue has any label.
        collect i))

(defun bts-github::grep-issues-by-condition (issues condition-value exclude-p)
  (bts--debug "github start grep issues by condition. issues[%s] condition-value[%s] exclude-p[%s]"
              (length issues) condition-value exclude-p)
  (bts:aif (bts:complex-condition-compile condition-value)
      (loop for i in issues
            for labels = (loop for l in (oref i :labels)
                               collect (assoc-default 'name l))
            if (or (and exclude-p
                        (not (bts:complex-condition-match-to-list labels it)))
                   (and (not exclude-p)
                        (bts:complex-condition-match-to-list labels it)))
            collect i)
    (bts--debug "github skipped grep issues by condition : value is not condition")
    issues))

(defun bts-github::make-fetch-issue-label-param (query)
  (bts:awhen (and (eq (plist-get query :lbl-include-type) 'select)
                  (plist-get query :lbl-include))
    (let* ((ccond (bts:complex-condition-compile it))
           (members (when (bts:complex-condition-p ccond)
                      (loop for m in (bts:complex-condition-members ccond)
                            if (not (bts:complex-condition-p m))
                            collect m)))
           (v (cond ((not (bts:complex-condition-p ccond))
                     ccond)
                    ((bts:complex-condition-and-p ccond)
                     (mapconcat 'identity members ","))))
           (ret (when (and v (not (string= v "")))
                  (list (cons 'labels v)))))
      (bts--debug "github made fetch issue label param : %s" ret)
      ret)))

(defun bts-github::make-fetch-repo-issue-param (query &rest params)
  (let ((ret (append (list (cons 'state (plist-get query :state)))
                     (bts-github::make-fetch-issue-label-param query)
                     params)))
    (bts--debug "github made fetch repo issue param : %s" ret)
    ret))

(defun bts-github::make-fetch-any-issue-param (query filter)
  (let ((ret (append (list (cons 'state (plist-get query :state)))
                     (list (cons 'filter filter))
                     (bts-github::make-fetch-issue-label-param query))))
    (bts--debug "github made fetch any issue param : %s" ret)
    ret))

(defun* bts-github::fetch-repo-issue (&key account token query owner repo)
  (bts--debug "github start fetch repo issue. account[%s] owner[%s] repo[%s]"
              account owner repo)
  (loop with resource = (format "/repos/%s/%s/issues" owner repo)
        for relate in (if (eq (plist-get query :relate-type) 'unlimit)
                          '(all)
                        (plist-get query :relates))
        for relate-param = (case relate
                             (assigned  (cons 'assignee account))
                             (created   (cons 'creator account))
                             (mentioned (cons 'mentioned account)))
        if (or (eq relate 'all)
               relate-param)
        append (bts-github::fetch-any-response
                :target 'issue
                :account account
                :token token
                :query resource
                :params (bts-github::make-fetch-repo-issue-param query relate-param))))

(defun* bts-github::fetch-any-issue (&key account token query organization all)
  (bts--debug "github start fetch any issue. account[%s] organization[%s] all[%s]"
              account organization all)
  (loop with resource = (cond (organization (format "/orgs/%s/issues" organization))
                              (all          "/issues")
                              (t            "/user/issues"))
        for filter in (if (eq (plist-get query :relate-type) 'unlimit)
                          '(all)
                        (plist-get query :relates))
        append (bts-github::fetch-any-response
                :target 'issue
                :account account
                :token token
                :query resource
                :params (bts-github::make-fetch-any-issue-param query filter))))

(defun bts-github:fetch-issue (project query)
  "Function for `bts:system-ticket-fetcher'."
  (yaxception:$~
    (yaxception:try
      (bts--debug* "github start fetch issue from %s of %s"
                   (bts:query-get-config-name query) (bts:project-get-config-name project))
      (let* ((account (plist-get project :account))
             (token (plist-get project :token))
             (issues (case (plist-get project :target)
                       (all    (bts-github::fetch-any-issue :account account :token token :query query :all t))
                       (other  (bts-github::grep-issues-by-regexp
                                (bts-github::fetch-any-issue :account account :token token :query query :all t)
                                'owner
                                (format "\\`%s\\'" account)
                                nil))
                       (own    (bts-github::grep-issues-by-regexp
                                (bts-github::fetch-any-issue :account account :token token :query query)
                                'owner
                                (format "\\`%s\\'" account)
                                t))
                       (choice (loop for r in (plist-get project :repos)
                                     for owner = (plist-get r :owner)
                                     for name = (plist-get r :name)
                                     append (case (plist-get r :type)
                                              (user (bts-github::fetch-repo-issue :account account
                                                                                  :token token
                                                                                  :query query
                                                                                  :owner owner
                                                                                  :repo name))
                                              (org  (bts-github::fetch-any-issue :account account
                                                                                 :token token
                                                                                 :query query
                                                                                 :organization owner)))))))
             (inlabel-all (eq (plist-get query :lbl-include-type) 'all))
             (exlabel-all (eq (plist-get query :lbl-exclude-type) 'all))
             (inlabel-val (when (eq (plist-get query :lbl-include-type) 'select)
                            (plist-get query :lbl-include)))
             (exlabel-val (when (eq (plist-get query :lbl-exclude-type) 'select)
                            (plist-get query :lbl-exclude))))
        (bts--info "github fetched issues : %s" (length issues))
        (when exlabel-all
          (setq issues (bts-github::grep-issues-by-any-label issues t))
          (bts--debug "github grepped issues by any label for include : %s" (length issues)))
        (when exlabel-val
          (setq issues (bts-github::grep-issues-by-condition issues exlabel-val t))
          (bts--debug "github grepped issues by condition for exclude : %s" (length issues)))
        (when inlabel-val
          (setq issues (bts-github::grep-issues-by-condition issues inlabel-val nil))
          (bts--debug "github grepped issues by condition for include : %s" (length issues)))
        (when inlabel-all
          (setq issues (bts-github::grep-issues-by-any-label issues nil))
          (bts--debug "github grepped issues by any label for include : %s" (length issues)))
        (bts:ticket-fetch-complete query (bts-github::make-tickets issues))))
    (yaxception:catch 'error e
      (bts--error "github failed fetch issue from %s of %s : %s"
                  (bts:query-get-config-name query)
                  (bts:project-get-config-name project)
                  (yaxception:get-text e))
      (bts:ticket-fetch-failed query))))

(defun bts-github:fetch-latest-issue (project ticket)
  "Function for `bts:system-ticket-latest'."
  (yaxception:$~
    (yaxception:try
      (bts--debug* "github start fetch latest issue of %s in %s"
                   (bts:ticket-get-unique-string ticket)
                   (bts:project-get-unique-string project))
      (let* ((account (plist-get project :account))
             (token (plist-get project :token))
             (owner (plist-get ticket :owner))
             (repo (plist-get ticket :repo))
             (api (bts-github::get-api-instance :target 'issue :account account :token token))
             (issue (bts-github::pick-up-data
                      (gh-issues-issue-get api owner repo (plist-get ticket :number)))))
        (nth 0 (bts-github::make-tickets (list issue)))))
    (yaxception:catch 'error e
      (bts--error "github failed fetch latest issue of %s in %s : %s"
                  (bts:ticket-get-unique-string ticket)
                  (bts:project-get-unique-string project)
                  (yaxception:get-text e)))))


;;;;;;;;;;;;;;;;;;;
;; Regist Ticket

(bts:regist-message 'bts-github-add-label-failed
  t         "Failed to set label for %s/%s's issue[%i]"
  'Japanese "%s/%sのissue[%i]のラベル設定に失敗しました")

(defun* bts-github::add-issue (ticket &key account token)
  (bts--debug "github start add issue")
  (let* ((api (bts-github::get-api-instance :target 'issue :account account :token token))
         (owner (plist-get ticket :owner))
         (repo (plist-get ticket :repo))
         (issue (bts-github::pick-up-data
                  (gh-issues-issue-new api owner repo
                                       (make-instance 'gh-issues-issue
                                                      :title (plist-get ticket :title)
                                                      :state 'open
                                                      :body (plist-get ticket :new-comment))))))
    (yaxception:$~
      (yaxception:try
        (bts:awhen (plist-get ticket :labels)
          (gh-issues-labels-add-to-issue api owner repo (oref issue :number) it)))
      (yaxception:catch 'error e
        (bts--error "github failed add issue label : %s" (yaxception:get-text e))
        (ignore-errors (bts:show-message (bts:get-message 'bts-github-add-label-failed
                                                          (plist-get ticket :owner)
                                                          (plist-get ticket :repo)
                                                          (plist-get ticket :number))))))
    issue))

(defun* bts-github::update-issue (ticket diff &key account token)
  (bts--debug* "github start update issue : %s" (ignore-errors (bts:ticket-get-unique-string ticket)))
  (let* ((api (bts-github::get-api-instance :target 'issue :account account :token token))
         (owner (plist-get ticket :owner))
         (repo (plist-get ticket :repo))
         (number (plist-get ticket :number))
         (issue (bts-github::pick-up-data
                  (gh-issues-issue-update api owner repo number
                                          (make-instance 'gh-issues-issue
                                                         :title (plist-get ticket :title)
                                                         :state (plist-get ticket :state)
                                                         :body (plist-get ticket :body)
                                                         :assignee (plist-get ticket :assigned-user))))))
    (when (assq 'labels diff)
      (gh-issues-labels-remove-all-from-issue api owner repo number)
      (gh-issues-labels-add-to-issue api owner repo number (plist-get ticket :labels)))
    issue))

(defun* bts-github::add-comment (ticket &key account token)
  (bts--debug* "github start add comment : %s" (ignore-errors (bts:ticket-get-unique-string ticket)))
  (let ((newcomm (or (plist-get ticket :new-comment) ""))
        (owner (plist-get ticket :owner))
        (repo (plist-get ticket :repo))
        (issue (plist-get ticket :number))
        (api (bts-github::get-api-instance :target 'comment :account account :token token)))
    (when (not (string= newcomm ""))
      (gh-issue-comments-new api owner repo issue
                             (make-instance 'gh-issue-comments-comment :body newcomm))
      (bts-github::clear-comment-cache owner repo issue))))

(defun* bts-github::update-comment (ticket diff &key account token)
  (bts--debug* "github start update comment : %s" (ignore-errors (bts:ticket-get-unique-string ticket)))
  (let ((owner (plist-get ticket :owner))
        (repo (plist-get ticket :repo))
        (issue (plist-get ticket :number))
        updatep)
    (dolist (diffnm (mapcar 'symbol-name (mapcar 'car diff)))
      (when (string-match "\\`comment-\\([0-9]+\\)" diffnm)
        (let ((comm-id (match-string-no-properties 1 diffnm))
              (comm-body (cdr (assoc-default (intern diffnm) diff)))
              (api (bts-github::get-api-instance :target 'comment :account account :token token)))
          (gh-issue-comments-update api owner repo comm-id
                                    (make-instance 'gh-issue-comments-comment :body comm-body))
          (setq updatep t))))
    (when updatep
      (bts-github::clear-comment-cache owner repo issue))))

(defun bts-github:regist-issue (project ticket diff)
  "Function for `bts:system-ticket-register'."
  (yaxception:$~
    (yaxception:try
      (let ((account (plist-get project :account))
            (token (plist-get project :token))
            issue)
        (if (not diff)
            (setq issue (bts-github::add-issue ticket :account account :token token))
          (setq issue (bts-github::update-issue ticket diff :account account :token token))
          (bts-github::update-comment ticket diff :account account :token token)
          (bts-github::add-comment ticket :account account :token token))
        ;; issue returned from gh.el has trouble. It's not updated the labels to latest.
        ;; (nth 0 (bts-github::make-tickets (list issue)))))
        t))
    (yaxception:catch 'error e
      (bts--error "github failed regist issue : %s" (yaxception:get-text e))
      (yaxception:throw e))))


;;;;;;;;;;;;;;;;;;;;
;; Ticket Summary

(defun bts-github::summary-face (ticket)
  (cond ((eq (plist-get ticket :state) 'closed)
         'bts:summary-closed-ticket-face)
        ((loop for lbl in (plist-get ticket :labels)
               if (member lbl bts-github:ignore-labels) return t)
         'bts:summary-ignored-ticket-face)))

(defun bts-github::summary-label-sort (tic1 tic2)
  (string< (mapconcat 'identity (plist-get tic1 :labels) ",")
           (mapconcat 'identity (plist-get tic2 :labels) ",")))

(defvar bts-github::label-format-hash (make-hash-table :test 'equal))
(defun bts-github::summary-label-decorate (ticket)
  (yaxception:$~
    (yaxception:try
      (bts--trace* "github start summary label decorate : %s"
                   (ignore-errors (bts:ticket-get-unique-string ticket)))
      (let ((lbls (plist-get ticket :labels)))
        (or (gethash lbls bts-github::label-format-hash)
            (puthash
             lbls
             (let* ((max-size bts-github:summary-label-width)
                    (sum-size (loop for lbl in lbls sum (length lbl)))
                    (size (if (> sum-size max-size)
                              (1+ (/ max-size (length lbls)))
                            max-size))
                    (proj (bts:ticket-get-project ticket))
                    (owner (plist-get ticket :owner))
                    (repo (plist-get ticket :repo))
                    (lbl-defs (bts-github::fetch-label :account (plist-get proj :account)
                                                       :token (plist-get proj :token)
                                                       :owner owner
                                                       :repo repo))
                    (face-pairs (loop for def in lbl-defs
                                      for name = (oref def :name)
                                      for bg-color = (oref def :color)
                                      for facesym = (intern (format "bts-github::face-%s-%s-%s" owner repo name))
                                      if (not (facep facesym))
                                      do (let* ((bg-highly (loop for e in (list (substring bg-color 0 2)
                                                                                (substring bg-color 2 4)
                                                                                (substring bg-color 4 6))
                                                                 if (not (string-match "\\`[def]" e))
                                                                 collect e))
                                                (fg-color (if (> (length bg-highly) 1) "white" "black")))
                                           (make-face facesym)
                                           (copy-face 'bts-github:summary-label-face facesym)
                                           (set-face-foreground facesym fg-color)
                                           (set-face-background facesym (concat "#" bg-color)))
                                      collect (cons name facesym)))
                    (fmt-lbls (loop for lbl in lbls
                                    for lblstr = (truncate-string-to-width lbl size nil nil nil)
                                    for face = (or (assoc-default lbl face-pairs)
                                                   'bts-github:summary-label-face)
                                    collect (propertize lblstr 'face face)))
                    (ret (mapconcat 'identity fmt-lbls "")))
               (bts--debug "github made decorated label value : %s" ret)
               ret)
             bts-github::label-format-hash))))
    (yaxception:catch 'error e
      (bts--error "github failed format summary labels : %s" (yaxception:get-text e))
      (propertize "*FormatErrored*" 'face 'font-lock-warning-face))))

(defun bts-github::summary-label-nodecorate (ticket)
  (mapconcat 'identity (plist-get ticket :labels) ","))

(defun bts-github:make-summary-format (project query)
  "Function for `bts:system-summary-format'."
  (bts--debug* "github start make summary format. project[%s] query[%s]"
               (bts:project-get-config-name project)
               (bts:query-get-config-name query))
  (setq bts-github::label-format-hash (make-hash-table :test 'equal))
  (let* ((lbl-cargs `(:name labels :label "Labels" :size ,bts-github:summary-label-width :ellipsis ""
                            :sort bts-github::summary-label-sort))
         (lbl-lo (if bts-github:summary-label-decorating
                     `(,@lbl-cargs :format bts-github::summary-label-decorate)
                   `(,@lbl-cargs :format bts-github::summary-label-nodecorate
                                 :face bts-github::summary-face))))
    `((:name number :label "ID" :size ,bts-github:summary-id-width :sort t
             :face bts-github::summary-face)
      (:name repo :label "Repository" :size 10 :sort t :face bts-github::summary-face)
      (:name title :label "Title" :size 40 :sort t :face bts-github::summary-face)
      ,lbl-lo
      (:name body :label "Body" :face bts-github::summary-face))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Other BTS Function

(defun bts-github:make-ticket-unique-string (ticket)
  "Function for `bts:system-ticket-unique-string'."
  (format "%s/%s:%i"
          (plist-get ticket :owner)
          (plist-get ticket :repo)
          (plist-get ticket :number)))


;;;;;;;;;;;;;;;;;;;
;; Regist to BTS

(bts:system-regist
 (make-bts:system
  :name                 'github
  :project-view         'bts-github:make-project-view
  :query-view           'bts-github:make-query-view
  :ticket-single-view   'bts-github:make-ticket-single-view
  :ticket-fetcher       'bts-github:fetch-issue
  :ticket-register      'bts-github:regist-issue
  :ticket-unique-string 'bts-github:make-ticket-unique-string
  :ticket-latest        'bts-github:fetch-latest-issue
  :summary-format       'bts-github:make-summary-format
  :conflict-checker     'any))


(provide 'bts-github)
;;; bts-github.el ends here
