;;; ein-process.el --- Notebook list buffer

;; Copyright (C) 2018- John M. Miller

;; Authors: Takafumi Arakaki <aka.tkf at gmail.com>
;;          John M. Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-process.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-process.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-process.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(eval-when-compile (require 'cl))

(require 'ein-core)
(require 'ein-jupyter)
(require 'ein-file)
(require 'f)

(defcustom ein:process-jupyter-regexp "\\(jupyter\\|ipython\\)\\(-\\|\\s-+\\)note"
  "Regexp by which we recognize notebook servers."
  :type 'string
  :group 'ein)


(defcustom ein:process-lsof "lsof"
  "Executable for lsof command."
  :type 'string
  :group 'ein)

(defun ein:process-divine-dir (pid args &optional error-buffer)
  "Returns notebook-dir or cwd of PID.  Supply ERROR-BUFFER to capture stderr"
  (if (string-match "\\bnotebook-dir\\(=\\|\\s-+\\)\\(\\S-+\\)" args)
      (directory-file-name (match-string 2 args))
    (if (executable-find ein:process-lsof)
        (ein:trim-right 
         (with-output-to-string 
           (shell-command (format "%s -p %d -a -d cwd -Fn | grep ^n | tail -c +2"
                                  ein:process-lsof pid) 
                          standard-output error-buffer))))))

(defun ein:process-divine-port (pid args &optional error-buffer)
  "Returns port on which PID is listening or 0 if none.  Supply ERROR-BUFFER to capture stderr"
  (if (string-match "\\bport\\(=\\|\\s-+\\)\\(\\S-+\\)" args)
      (string-to-number (match-string 2 args))
    (if (executable-find ein:process-lsof)
        (string-to-number
         (ein:trim-right 
          (with-output-to-string
            (shell-command (format "%s -p %d -a -iTCP -sTCP:LISTEN -Fn | grep ^n | sed \"s/[^0-9]//g\""
                                   ein:process-lsof pid) 
                           standard-output error-buffer)))))))

(defun ein:process-divine-ip (pid args)
  "Returns notebook-ip of PID"
  (if (string-match "\\bip\\(=\\|\\s-+\\)\\(\\S-+\\)" args)
      (match-string 2 args)
    "localhost"))

(defstruct ein:$process
  "Hold process variables.

`ein:$process-pid' : integer
  PID.

`ein:$process-port': integer
  Arg of --port or .

`ein:$process-ip' : string
  Arg of --notebook-ip or 'localhost'.

`ein:$process-dir' : string
  Arg of --notebook-dir or 'readlink -e /proc/<pid>/cwd'."
  pid
  port
  ip
  dir
)

(ein:deflocal ein:%processes% (make-hash-table :test #'equal)
  "Process table of `ein:$process' keyed on dir.")

(defun ein:process-processes ()
  (ein:hash-vals ein:%processes%))

(defun ein:process-alive-p (proc)
  (not (null (process-attributes (ein:$process-pid proc)))))

(defun ein:process-suitable-notebook-dir (filename)
  "Return the uppermost parent dir of DIR that contains ipynb files."
  (let ((fn (expand-file-name filename)))
    (loop with directory = (directory-file-name 
                            (if (f-file? fn) (f-parent fn) fn))
          with suitable = directory
          until (string= (file-name-nondirectory directory) "")
          do (if (directory-files directory nil "\\.ipynb$")
                 (setq suitable directory))
          (setq directory (directory-file-name (file-name-directory directory)))
          finally return suitable)))

(defun ein:process-refresh-processes ()
  (loop for pid in (list-system-processes)
        for attrs = (process-attributes pid)
        for args = (alist-get 'args attrs)
        with seen = (mapcar #'ein:$process-pid (ein:hash-vals ein:%processes%))
        if (and (null (member pid seen))
                (string-match ein:process-jupyter-regexp (alist-get 'comm attrs)))
          do (ein:and-let* ((dir (ein:process-divine-dir pid args))
                            (port (ein:process-divine-port pid args))
                            (ip (ein:process-divine-ip pid args)))
               (puthash dir (make-ein:$process :pid pid
                                               :port port
                                               :ip ip
                                               :dir dir)
                        ein:%processes%))
        end))

(defun ein:process-dir-match (filename)
  "Return ein:process whose directory is prefix of FILENAME."
  (loop for dir in (ein:hash-keys ein:%processes%)
        when (search dir filename)
        return (gethash dir ein:%processes%)))

(defsubst ein:process-url-or-port (proc)
  "Naively construct url-or-port from ein:process PROC's port and ip fields"
  (format "http://%s:%s" (ein:$process-ip proc) (ein:$process-port proc)))

(defsubst ein:process-path (proc filename)
  "Construct path by eliding PROC's dir from filename"
  (subseq filename (length (file-name-as-directory (ein:$process-dir proc)))))

;;;###autoload
(defun ein:process-open-file (filename)
  "Open FILENAME as a notebook and start a notebook server if necessary."
  (interactive (list (completing-read "Notebook file: " 
                                      (f-files default-directory 
                                               (lambda (file) (and (not (f-hidden? file))
                                                                   (f-ext? file "ipynb"))))
                                      nil 'confirm)))
  (ein:process-refresh-processes)
  (let* ((proc (ein:process-dir-match filename)))
    (when (and proc (not (ein:process-alive-p proc)))
      (ein:log 'warn "Server pid=%s dir=%s no longer running" (ein:$process-pid proc) (ein:$process-dir proc))
      (remhash (ein:$process-dir proc) ein:%processes%)
      (setq proc nil))
    (if (null proc)
        (let ((nbdir (read-directory-name "Notebook directory: " 
                                          (ein:process-suitable-notebook-dir filename))))
          (apply #'ein:jupyter-server-start
                 (append (ein:jupyter-server-start--arguments nbdir) 
                         (list nil t (lambda ()
                                       (ein:file-open (car (ein:jupyter-server-conn-info))
                                                      (subseq filename (length (file-name-as-directory nbdir)))))))))
      (ein:file-open (ein:process-url-or-port proc) (ein:process-path proc filename)))))


(provide 'ein-process)

;;; ein-process.el ends here
