;;; php-mode.el --- major mode for editing PHP code

;; Author:       Vincent DEBOUT <deboutv@free.fr>
;; Maintainer:	Vincent DEBOUT <deboutv@free.fr>
;; Keywords:	languages php
;; WWW:		http://deboutv.free.fr/lisp/php/

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; History

;; $Log: php-mode.el,v $
;; Revision 1.6  2007/03/17 09:57:32  vincent
;; Fix bugs and add APC templates
;;
;; Revision 1.5  2006/09/24 19:15:51  vincent
;; Release the 0.0.3 version
;;
;; Revision 1.4  2006/08/21 16:42:07  vincent
;; Add files and commit a 0.0.3pre release
;;
;; Revision 1.3  2006/07/30 08:31:13  vincent
;; Add templates, license; Fix highlight and missing definition
;;
;; Revision 1.2  2006/07/23 15:47:18  vincent
;; Add Template, Fontification, Indentation
;;
;; Revision 1.1.1.1  2006/07/14 10:16:37  vincent
;; First import
;;

(defconst php-version "0.0.4"
  "PHP Mode version number.")

(defconst php-time-stamp "2007-03-17"
  "PHP Mode time stamp for last update.")

;; From Emacs Php Mode

(require 'font-lock)
(require 'speedbar)
(require 'cc-mode)
(require 'custom)
(require 'etags)
(eval-when-compile
  (require 'regexp-opt))

(defcustom php-speedbar-config t
  "*When set to true automatically configures Speedbar to observe PHP files.\
Ignores php-file patterns option; fixed to expression \"\\.\\(inc\\|php[s34]?\\)\""
  :type 'boolean
  :group 'php)

(defcustom php-mode-speedbar-open nil
  "Normally php-mode starts with the speedbar closed.\
Turning this on will open it whenever php-mode is loaded."
  :type 'boolean
  :group 'php)

(defcustom php-manual-url "http://www.php.net/manual/en/"
  "*URL at which to find PHP manual.\
You can replace \"en\" with your ISO language code."
  :type 'string
  :group 'php)

(defcustom php-search-url "http://www.php.net/"
  "*URL at which to search for documentation on a word"
  :type 'string
  :group 'php)

(defcustom php-manual-path ""
  "*Path to the directory which contains the PHP manual"
  :type 'string
  :group 'php)

(defcustom php-mode-force-pear nil
  "Normally PEAR coding rules are enforced only when the filename contains \"PEAR\"\
Turning this on will force PEAR rules on all PHP files."
  :type 'boolean
  :group 'php)



;;   ;; Do not force newline at end of file.  Such newlines can cause
;;   ;; trouble if the PHP file is included in another file before calls
;;   ;; to header() or cookie().

;;   ;; PEAR coding standards
;;   (make-local-hook 'php-mode-pear-hook)
;;   (add-hook 'php-mode-pear-hook
;;             (lambda nil (set (make-local-variable 'tab-width) 4)) nil t)
;;   (add-hook 'php-mode-pear-hook
;;             (lambda nil (set (make-local-variable 'c-basic-offset) 4)) nil t)
;;   (add-hook 'php-mode-pear-hook
;;             (lambda nil (set (make-local-variable 'c-hanging-comment-ender-p) nil)) nil t)
;;   (add-hook 'php-mode-pear-hook
;;             (lambda nil (set (make-local-variable 'indent-tabs-mode) nil)) nil t)
;;   (add-hook 'php-mode-pear-hook
;;             (lambda nil (c-set-offset 'block-open' - )) nil t)
;;   (add-hook 'php-mode-pear-hook
;;             (lambda nil (c-set-offset 'block-close' 0 )) nil t)

;;   (if (or php-mode-force-pear
;;           (and (stringp buffer-file-name)
;;                (string-match "PEAR\\|pear"
;;                              (buffer-file-name))
;;                (string-match "\\.php$" (buffer-file-name))))
;;       (run-hooks 'php-mode-pear-hook))

;;   (run-hooks 'php-mode-user-hook))

;; Handle Speedbar
(if php-mode-speedbar-open
    (speedbar 1))
(if (and php-speedbar-config (symbolp 'speedbar))
    (speedbar-add-supported-extension "\\.\\(inc\\|php[s34]?\\)"))



;; Define function documentation function
(defun php-search-documentation ()
  "Search PHP documentation for the word at the point."
  (interactive)
  (browse-url (concat php-search-url (current-word t))))

;; Define function for browsing manual
(defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url php-manual-url))

;; Syntax check the current buffer
;; using CLI php -l
;; (Buffer has to be saved)
(defun php-check-syntax ()
  "Check for Syntax errors using CLI php"
  (interactive)
  (let ((msg nil))
    (if (not (buffer-modified-p))
        (setq msg (shell-command-to-string (format "php -l %s" (buffer-file-name))))
      (let ((tmp-name (make-temp-name "/tmp/phpsyntax"))
            (content (buffer-string)))
        (with-temp-file tmp-name
          (insert content))
        (setq msg (shell-command-to-string (format "php -l %s" tmp-name)))
        (delete-file tmp-name)))
    (if (string= (substring msg 0 2) "No")
        (message "No Errors found")
;      (string-match (format "in '%s'" 
      (message msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tempalte variable

(defvar php-template-menu (list '(".NET"
				  ["dotnet_load" php-template-dotnet-load t])
				'("Apache"
				  ["apache_child_terminate" php-template-apache-child-terminate t]
				  ["apache_get_modules" php-template-apache-get-modules t]
				  ["apache_get_version" php-template-apache-get-version t]
				  ["apache_getenv" php-template-apache-getenv t]
				  ["apache_lookup_uri" php-template-apache-lookup-uri t]
				  ["apache_note" php-template-apache-note t]
				  ["apache_request_headers" php-template-apache-request-headers t]
				  ["apache_reset_timeout" php-template-apache-reset-timeout t]
				  ["apache_response_headers" php-template-apache-response-headers t]
				  ["apache_setenv" php-template-apache-setenv t]
				  ["ascii2ebcdic" php-template-ascii2ebcdic t]
				  ["ebcdic2ascii" php-template-ebcdic2ascii t]
				  ["getallheaders" php-template-getallheaders t]
				  ["virtual" php-template-virtual t])
				'("APC"
				  ["apc_cache_info" php-template-apc-cache-info t]
				  ["apc_clear_cache" php-template-apc-clear-cache t]
				  ["apc_define_constants" php-template-apc-define-constants t]
				  ["apc_delete" php-template-apc-delete t]
				  ["apc_fetch" php-template-apc-fetch t]
				  ["apc_load_constants" php-template-apc-load-constants t]
				  ["apc_sma_info" php-template-apc-sma-info t]
				  ["apc_store" php-template-apc-store t])
				'("Array functions"
				  ("Array (array_change --> array_map)"
				   ["array_change_key_case" php-template-array-change-key-case t]
				   ["array_chunk" php-template-array-chunk t]
				   ["array_combine" php-template-array-combine t]
				   ["array_count_values" php-template-array-count-values t]
				   ["array_diff_assoc" php-template-array-diff-assoc t]
				   ["array_diff_key" php-template-array-diff-key t]
				   ["array_diff_uassoc" php-template-array-diff-uassoc t]
				   ["array_diff_ukey" php-template-array-diff-ukey t]
				   ["array_diff" php-template-array-diff t]
				   ["array_fill" php-template-array-fill t]
				   ["array_filter" php-template-array-filter t]
				   ["array_flip" php-template-array-flip t]
				   ["array_intersect_assoc" php-template-array-intersect-assoc t]
				   ["array_intersect_key" php-template-array-intersect-key t]
				   ["array_intersect_uassoc" php-template-array-intersect-uassoc t]
				   ["array_intersect_ukey" php-template-array-intersect-ukey t]
				   ["array_intersect" php-template-array-intersect t]
				   ["array_key_exists" php-template-array-exists t]
				   ["array_keys" php-template-array-keys t]
				   ["array_map" php-template-array-map t])
				  ("Array (array_merge --> array_uintersect_uassoc)"
				   ["array_merge_recursive" php-template-array-merge-recursive t]
				   ["array_merge" php-template-array-merge t]
				   ["array_multisort" php-template-array-multisort t]
				   ["array_pad" php-template-array-pad t]
				   ["array_pop" php-template-array-pop t]
				   ["array_product" php-template-array-product t]
				   ["array_push" php-template-array-push t]
				   ["array_rand" php-template-array-rand t]
				   ["array_reduce" php-template-array-reduce t]
				   ["array_reverse" php-template-array-reverse t]
				   ["array_search" php-template-array-search t]
				   ["array_shift" php-template-array-shift t]
				   ["array_slice" php-template-array-slice t]
				   ["array_splice" php-template-array-splice t]
				   ["array_sum" php-template-array-sum t]
				   ["array_udiff_assoc" php-template-array-udiff-assoc t]
				   ["array_udiff_uassoc" php-template-array-udiff-uassoc t]
				   ["array_udiff" php-template-array-udiff t]
				   ["array_uintersect_assoc" php-template-array-uintersect-assoc t]
				   ["array_uintersect_uassoc" php-template-array-uintersect-uassoc t])
				  ("Array (array_uintersect --> list)"
				   ["array_uintersect" php-template-array-uintersect t]
				   ["array_unique" php-template-array-unique t]
				   ["array_unshift" php-template-array-unshift t]
				   ["array_values" php-template-array-values t]
				   ["array_walk_recursive" php-template-array-walk-recursive t]
				   ["array_walk" php-template-array-walk t]
				   ["array" php-template-array t]
				   ["arsort" php-template-arsort t]
				   ["asort" php-template-asort t]
				   ["compact" php-template-compact t]
				   ["count" php-template-count t]
				   ["current" php-template-current t]
				   ["each" php-template-each t]
				   ["end" php-template-end t]
				   ["extract" php-template-extract t]
				   ["in_array" php-template-in-array t]
				   ["key" php-template-key t]
				   ["krsort" php-template-krsort t]
				   ["ksort" php-template-ksort t]
				   ["list" php-template-list t])
				  ("Array (natcasesort --> usort)"
				   ["natcasesort" php-template-natcasesort t]
				   ["natsort" php-template-natsort t]
				   ["next" php-template-next t]
				   ["pos" php-template-pos t]
				   ["prev" php-template-prev t]
				   ["range" php-template-range t]
				   ["reset" php-template-reset t]
				   ["rsort" php-template-rsort t]
				   ["shuffle" php-template-shuffle t]
				   ["sizeof" php-template-sizeof t]
				   ["sort" php-template-sort t]
				   ["uasort" php-template-uasort t]
				   ["uksort" php-template-uksort t]
				   ["usort" php-template-usort t]))
				'("Control Structures"
				  ["break" php-template-break t]
				  ["continue" php-template-continue t]
				  ;;["declare" php-template-declare t]
				  ["do-while" php-template-do-while t]
				  ["else" php-template-else t]
				  ["elseif" php-template-elseif t]
				  ["for" php-template-for t]
				  ["foreach" php-template-foreach t]
				  ["if" php-template-if t]
				  ["include" php-template-include t]
				  ["include_once" php-template-include-once t]
				  ["require" php-template-require t]
				  ["require_once" php-template-require-once t]
				  ["return" php-template-return t]
				  ["switch" php-template-switch t]
				  ["while" php-template-while t])
				'("Date/Time Functions"
				  ["checkdate" php-template-checkdate t]
				  ["date_default_timezone_get" php-template-date-default-timezone-get t]
				  ["date_default_timezone_set" php-template-date-default-timezone-set t]
				  ["date_sunrise" php-template-date-sunrise t]
				  ["date_sunset" php-template-date-sunset t]
				  ["date" php-template-date t]
				  ["getdate" php-template-getdate t]
				  ["gettimeofday" php-template-gettimeofday t]
				  ["gmdate" php-template-gmdate t]
				  ["gmmktime" php-template-gmmktime t]
				  ["gmstrftime" php-template-gmstrftime t]
				  ["idate" php-template-idate t]
				  ["localtime" php-template-localtime t]
				  ["microtime" php-template-microtime t]
				  ["mktime" php-template-mktime t]
				  ["strftime" php-template-strftime t]
				  ["strptime" php-template-strptime t]
				  ["strtotime" php-template-strtotime t]
				  ["time" php-template-time t])
				'("Directory Functions"
				  ["chdir" php-template-chdir t]
				  ["chroot" php-template-chroot t]
				  ["dir" php-template-dir t]
				  ["closedir" php-template-closedir t]
				  ["getcwd" php-template-getcwd t]
				  ["opendir" php-template-opendir t]
				  ["readdir" php-template-readdir t]
				  ["rewinddir" php-template-rewinddir t]
				  ["scandir" php-template-scandir t])
				'("Error and Logging"
				  ["debug_backtrace" php-template-debug-backtrace t]
				  ["debug_print_backtrace" php-template-debug-print-backtrace t]
				  ["error_log" php-template-error-log t]
				  ["error_reporting" php-template-error-reporting t]
				  ["restore_error_handler" php-template-restore-error-handler t]
				  ["restore_exception_handler" php-template-restore-exception-handler t]
				  ["set_error_handler" php-template-set-error-handler t]
				  ["set_exception_handler" php-template-set-exception-handler t]
				  ["trigger_error" php-template-trigger-error t]
				  ["user_error" php-template-user-error t])
				'("File System"
				  ("File System (Basename --> File_get_contents)"
				   ["basename" php-template-basename t]
				   ["chgrp" php-template-chgrp t]
				   ["chmod" php-template-chmod t]
				   ["chown" php-template-chown t]
				   ["clearstatcache" php-template-clearstatcache t]
				   ["copy" php-template-copy t]
				   ["delete" php-template-delete t]
				   ["dirname" php-template-dirname t]
				   ["disk_free_space" php-template-disk-free-space t]
				   ["disk_total_space" php-template-disk-total-space t]
				   ["diskfreespace" php-template-diskfreespace t]
				   ["fclose" php-template-fclose t]
				   ["feof" php-template-feof t]
				   ["fflush" php-template-fflush t]
				   ["fgetc" php-template-fgetc t]
				   ["fgetcsv" php-template-fgetcsv t]
				   ["fgets" php-template-fgets t]
				   ["fgetss" php-template-fgetss t]
				   ["file_exists" php-template-file-exists t]
				   ["file_get_contents" php-template-file-get-contents t])
				  ("File System (File_put_contents --> Fseek)"
				   ["file_put_contents" php-template-file-put-contents t]
				   ["file" php-template-file t]
				   ["fileatime" php-template-fileatime t]
				   ["filectime" php-template-filectime t]
				   ["filegroup" php-template-filegroup t]
				   ["fileinode" php-template-fileinode t]
				   ["filemtime" php-template-filemtime t]
				   ["fileowner" php-template-fileowner t]
				   ["fileperms" php-template-fileperms t]
				   ["filesize" php-template-filesize t]
				   ["filetype" php-template-filetype t]
				   ["flock" php-template-flock t]
				   ["fnmatch" php-template-fnmatch t]
				   ["fopen" php-template-fopen t]
				   ["fpassthru" php-template-fpassthru t]
				   ["fputcsv" php-template-fputcsv t]
				   ["fputs" php-template-fwrite t]
				   ["fread" php-template-fread t]
				   ;;["fscanf" php-template-fscanf t]
				   ["fseek" php-template-fseek t])
				  ("File System (Fstat --> Pathinfo)"
				   ["fstat" php-template-fstat t]
				   ["ftell" php-template-ftell t]
				   ["ftruncate" php-template-ftruncate t]
				   ["fwrite" php-template-fwrite t]
				   ["glob" php-template-glob t]
				   ["is_dir" php-template-is-dir t]
				   ["is_executable" php-template-is-executable t]
				   ["is_file" php-template-is-file t]
				   ["is_link" php-template-is-link t]
				   ["is_readable" php-template-is-readable t]
				   ["is_uploaded_file" php-template-is-uploaded-file t]
				   ["is_writable" php-template-is-writable t]
				   ["is_writeable" php-template-is-writable t]
				   ["link" php-template-link t]
				   ["linkinfo" php-template-linkinfo t]
				   ["lstat" php-template-lstat t]
				   ["mkdir" php-template-mkdir t]
				   ["move_uploaded_file" php-template-move-uploaded-file t]
				   ["parse_ini_file" php-template-parse-ini-file t]
				   ["pathinfo" php-template-pathinfo t])
				  ("File System (Pclose --> Unlink)"
				   ["pclose" php-template-pclose t]
				   ["popen" php-template-popen t]
				   ["readfile" php-template-readfile t]
				   ["readlink" php-template-readlink t]
				   ["realpath" php-template-realpath t]
				   ["rename" php-template-rename t]
				   ["rewind" php-template-rewind t]
				   ["rmdir" php-template-rmdir t]
				   ;;["set_file_buffer" php-template-set-file-buffer t]
				   ["stat" php-template-stat t]
				   ["symlink" php-template-symlink t]
				   ["tempnam" php-template-tempnam t]
				   ["tmpfile" php-template-tmpfile t]
				   ["touch" php-template-touch t]
				   ["umask" php-template-umask t]
				   ["unlink" php-template-unlink t]))
				'("Functions"
				  ["call_user_func_array" php-template-call-user-func-array t]
				  ["call_user_func" php-template-call-user-func t]
				  ["create_function" php-template-create-function t]
				  ["func_get_arg" php-template-func-get-arg t]
				  ["func_get_args" php-template-func-get-args t]
				  ["func_num_args" php-template-func-num-args t]
				  ["function_exists" php-template-function-exists t]
				  ["get_defined_functions" php-template-get-defined-functions t]
				  ["register_shutdown_function" php-template-register-shutdown-function t]
				  ["register_tick_function" php-template-register-tick-function t]
				  ["unregister_tick_function" php-template-unregister-tick-function t])
				'("Image"
				  ("Image (gd_info --> imagecolormatch)"
				   ["gd_info" php-template-gd-info t]
				   ["getimagesize" php-template-getimagesize t]
				   ["image_type_to_extension" php-template-image-type-to-extension t]
				   ["image_type_to_mime_type" php-template-image-type-to-mime-type t]
				   ["image2wbmp" php-template-image2wbmp t]
				   ["imagealphablending" php-template-imagealphablending t]
				   ["imageantialias" php-template-imageantialias t]
				   ["imagearc" php-template-imagearc t]
				   ["imagechar" php-template-imagechar t]
				   ["imagecharup" php-template-imagecharup t]
				   ["imagecolorallocate" php-template-imagecolorallocate t]
				   ["imagecolorallocatealpha" php-template-imagecolorallocatealpha t]
				   ["imagecolorat" php-template-imagecolorat t]
				   ["imagecolorclosest" php-template-imagecolorclosest t]
				   ["imagecolorclosestalpha" php-template-imagecolorclosestalpha t]
				   ["imagecolorclosesthwb" php-template-imagecolorclosesthwb t]
				   ["imagecolordeallocate" php-template-imagecolordeallocate t]
				   ["imagecolorexact" php-template-imagecolorexact t]
				   ["imagecolorexactalpha" php-template-imagecolorexactalpha t]
				   ["imagecolormatch" php-template-imagecolormatch t])
				  ("Image (imagecolorresolve --> imagecreatefromstring)"
				   ["imagecolorresolve" php-template-imagecolorresolve t]
				   ["imagecolorresolvealpha" php-template-imagecolorresolvealpha t]
				   ["imagecolorset" php-template-imagecolorset t]
				   ["imagecolorsforindex" php-template-imagecolorsforindex t]
				   ["imagecolorstotal" php-template-imagecolorstotal t]
				   ["imagecolortransparent" php-template-imagecolortransparent t]
				   ["imageconvolution" php-template-imageconvolution t]
				   ["imagecopy" php-template-imagecopy t]
				   ["imagecopymerge" php-template-imagecopymerge t]
				   ["imagecopymergegray" php-template-imagecopymergegray t]
				   ["imagecopyresampled" php-template-imagecopyresampled t]
				   ["imagecopyresized" php-template-imagecopyresized t]
				   ["imagecreate" php-template-imagecreate t]
				   ["imagecreatefromgd2" php-template-imagecreatefromgd2 t]
				   ["imagecreatefromgd2part" php-template-imagecreatefromgd2part t]
				   ["imagecreatefromgd" php-template-imagecreatefromgd t]
				   ["imagecreatefromgif" php-template-imagecreatefromgif t]
				   ["imagecreatefromjpeg" php-template-imagecreatefromjpeg t]
				   ["imagecreatefrompng" php-template-imagecreatefrompng t]
				   ["imagecreatefromstring" php-template-imagecreatefromstring t])
				  ("Image (imagecreatefromwbmp --> imagegd2)"
				   ["imagecreatefromwbmp" php-template-imagecreatefromwbmp t]
				   ["imagecreatefromxbm" php-template-imagecreatefromxbm t]
				   ["imagecreatefromxpm" php-template-imagecreatefromxpm t]
				   ["imagecreatetruecolor" php-template-imagecreatetruecolor t]
				   ["imagedashedline" php-template-imagedashedline t]
				   ["imagedestroy" php-template-imagedestroy t]
				   ["imageellipse" php-template-imageellipse t]
				   ["imagefill" php-template-imagefill t]
				   ["imagefilledarc" php-template-imagefilledarc t]
				   ["imagefilledellipse" php-template-imagefilledellipse t]
				   ["imagefilledpolygon" php-template-imagefilledpolygon t]
				   ["imagefilledrectangle" php-template-imagefilledrectangle t]
				   ["imagefilltoborder" php-template-imagefilltoborder t]
				   ["imagefilter" php-template-imagefilter t]
				   ["imagefontheight" php-template-imagefontheight t]
				   ["imagefontwidth" php-template-imagefontwidth t]
				   ["imageftbbox" php-template-imageftbbox t]
				   ["imagefttext" php-template-imagefttext t]
				   ["imagegammacorrect" php-template-imagegammacorrect t]
				   ["imagegd2" php-template-imagegd2 t])
				  ("Image (imagegd --> imagerotate)"
				   ["imagegd" php-template-imagegd t]
				   ["imagegif" php-template-imagegif t]
				   ["imageinterlace" php-template-imageinterlace t]
				   ["imageistruecolor" php-template-imageistruecolor t]
				   ["imagejpeg" php-template-imagejpeg t]
				   ["imagelayereffect" php-template-imagelayereffect t]
				   ["imageline" php-template-imageline t]
				   ["imageloadfont" php-template-imageloadfont t]
				   ["imagepalettecopy" php-template-imagepalettecopy t]
				   ["imagepng" php-template-imagepng t]
				   ["imagepolygon" php-template-imagepolygon t]
				   ["imagepsbbox" php-template-imagepsbbox t]
				   ["imagepsencodefont" php-template-imagepsencodefont t]
				   ["imagepsextendfont" php-template-imagepsextendfont t]
				   ["imagepsfreefont" php-template-imagepsfreefont t]
				   ["imagepsloadfont" php-template-imagepsloadfont t]
				   ["imagepsslantfont" php-template-imagepsslantfont t]
				   ["imagepstext" php-template-imagepstext t]
				   ["imagerectangle" php-template-imagerectangle t]
				   ["imagerotate" php-template-imagerotate t])
				  ("Image (imagesavealpha --> png2wbmp)"
				   ["imagesavealpha" php-template-imagesavealpha t]
				   ["imagesetbrush" php-template-imagesetbrush t]
				   ["imagesetpixel" php-template-imagesetpixel t]
				   ["imagesetstyle" php-template-imagesetstyle t]
				   ["imagesetthickness" php-template-imagesetthickness t]
				   ["imagesettile" php-template-imagesettile t]
				   ["imagestring" php-template-imagestring t]
				   ["imagestringup" php-template-imagestringup t]
				   ["imagesx" php-template-imagesx t]
				   ["imagesy" php-template-imagesy t]
				   ["imagetruecolortopalette" php-template-imagetruecolortopalette t]
				   ["imagettfbbox" php-template-imagettfbbox t]
				   ["imagettftext" php-template-imagettftext t]
				   ["imagetypes" php-template-imagetypes t]
				   ["imagewbmp" php-template-imagewbmp t]
				   ["imagexbm" php-template-imagexbm t]
				   ["iptcembed" php-template-iptcembed t]
				   ["iptcparse" php-template-iptcparse t]
				   ["jpeg2wbmp" php-template-jpeg2wbmp t]
				   ["png2wbmp" php-template-png2wbmp t]))
				'("Mail"
				  ["ezmlm_hash" php-template-ezmlm-hash t]
				  ["mail" php-template-mail t])
				'("Mathematical"
				  ("Mathematical (abs --> floor)"
				   ["abs" php-template-abs t]
				   ["acos" php-template-acos t]
				   ["acosh" php-template-acosh t]
				   ["asin" php-template-asin t]
				   ["asinh" php-template-asinh t]
				   ["atan2" php-template-atan2 t]
				   ["atan" php-template-atan t]
				   ["atanh" php-template-atanh t]
				   ["base_convert" php-template-base-convert t]
				   ["bindec" php-template-bindec t]
				   ["ceil" php-template-ceil t]
				   ["cos" php-template-cos t]
				   ["cosh" php-template-cosh t]
				   ["decbin" php-template-decbin t]
				   ["dechex" php-template-dechex t]
				   ["decoct" php-template-decoct t]
				   ["deg2rad" php-template-deg2rad t]
				   ["exp" php-template-exp t]
				   ["expm1" php-template-expm1 t]
				   ["floor" php-template-floor t])
				  ("Mathematical (fmod --> rad2deg)"
				   ["fmod" php-template-fmod t]
				   ["getrandmax" php-template-getrandmax t]
				   ["hexdec" php-template-hexdec t]
				   ["hypot" php-template-hypot t]
				   ["is_finite" php-template-is-finite t]
				   ["is_infinite" php-template-is-infinite t]
				   ["is_nan" php-template-is-nan t]
				   ["lcg_value" php-template-lcg-value t]
				   ["log10" php-template-log10 t]
				   ["log1p" php-template-log1p t]
				   ["log" php-template-log t]
				   ["max" php-template-max t]
				   ["min" php-template-min t]
				   ["mt_getrandmax" php-template-mt-getrandmax t]
				   ["mt_rand" php-template-mt-rand t]
				   ["mt_srand" php-template-mt-srand t]
				   ["octdec" php-template-octdec t]
				   ["pi" php-template-pi t]
				   ["pow" php-template-pow t]
				   ["rad2deg" php-template-rad2deg t])
				  ("Mathematical (rand --> tanh)"
				   ["rand" php-template-rand t]
				   ["round" php-template-round t]
				   ["sin" php-template-sin t]
				   ["sinh" php-template-sinh t]
				   ["sqrt" php-template-sqrt t]
				   ["srand" php-template-srand t]
				   ["tan" php-template-tan t]
				   ["tanh" php-template-tanh t]))
				'("Miscellaneous Functions"
				  ("Misc (connection_aborted --> sys_getloadavg)"
				   ["connection_aborted" php-template-connection-aborted t]
				   ["connection_status" php-template-connection-status t]
				   ["connection_timeout" php-template-connection-timeout t]
				   ["constant" php-template-constant t]
				   ["define" php-template-define t]
				   ["defined" php-template-defined t]
				   ["die" php-template-die t]
				   ["eval" php-template-eval t]
				   ["exit" php-template-exit t]
				   ["get_browser" php-template-get-browser t]
				   ["__halt_compiler" php-template-halt-compiler t]
				   ["highlight_file" php-template-highlight-file t]
				   ["highlight_string" php-template-highlight-string t]
				   ["ignore_user_abort" php-template-ignore-user-abort t]
				   ["pack" php-template-pack t]
				   ["php_check_syntax" php-template-php-check-syntax t]
				   ["php_strip_whitespace" php-template-php-strip-whitespace t]
				   ["show_source" php-template-show-source t]
				   ["sleep" php-template-sleep t]
				   ["sys_getloadavg" php-template-sys-getloadavg t])
				  ("Misc (time_nanosleep --> usleep)"
				   ["time_nanosleep" php-template-time-nanosleep t]
				   ["time_sleep_until" php-template-time-sleep-until t]
				   ["uniqid" php-template-uniqid t]
				   ["unpack" php-template-unpack t]
				   ["usleep" php-template-usleep t])) 
				'("MySQL"
				  ("MySQL (Aff --> Field_flags)"
				   ["mysql_affected_rows" php-template-mysql-affected-rows t]
				   ["mysql_change_user" php-template-mysql-change-user t]
				   ["mysql_client_encoding" php-template-mysql-client-encoding t]
				   ["mysql_close" php-template-mysql-close t]
				   ["mysql_connect" php-template-mysql-connect t]
				   ["mysql_create_db" php-template-mysql-create-db t]
				   ["mysql_data_seek" php-template-mysql-data-seek t]
				   ["mysql_db_name" php-template-mysql-db-name t]
				   ["mysql_db_query" php-template-mysql-db-query t]
				   ["mysql_drop_db" php-template-mysql-drop-db t]
				   ["mysql_errno" php-template-mysql-errno t]
				   ["mysql_error" php-template-mysql-error t]
				   ["mysql_escape_string" php-template-mysql-escape-string t]
				   ["mysql_fetch_array" php-template-mysql-fetch-array t]
				   ["mysql_fetch_assoc" php-template-mysql-fetch-assoc t]
				   ["mysql_fetch_field" php-template-mysql-fetch-field t]
				   ["mysql_fetch_lengths" php-template-mysql-fetch-lengths t]
				   ["mysql_fetch_object" php-template-mysql-fetch-object t]
				   ["mysql_fetch_row" php-template-mysql-fetch-row t]
				   ["mysql_field_flags" php-template-mysql-field-flags t])
				  ("MySQL (Field_lan --> Ping"
				   ["mysql_field_len" php-template-mysql-field-len t]
				   ["mysql_field_name" php-template-mysql-field-name t]
				   ["mysql_field_seek" php-template-mysql-field-seek t]
				   ["mysql_field_table" php-template-mysql-field-table t]
				   ["mysql_field_type" php-template-mysql-field-type t]
				   ["mysql_free_result" php-template-mysql-free-result t]
				   ["mysql_get_client_info" php-template-mysql-get-client-info t]
				   ["mysql_get_host_info" php-template-mysql-get-host-info t]
				   ["mysql_get_proto_info" php-template-mysql-get-proto-info t]
				   ["mysql_get_server_info" php-template-mysql-get-server-info t]
				   ["mysql_info" php-template-mysql-info t]
				   ["mysql_insert_id" php-template-mysql-insert-id t]
				   ["mysql_list_dbs" php-template-mysql-list-dbs t]
				   ["mysql_list_fields" php-template-mysql-list-fields t]
				   ["mysql_list_processes" php-template-mysql-list-processes t]
				   ["mysql_list_tables" php-template-mysql-list-tables t]
				   ["mysql_num_fields" php-template-mysql-num-fields t]
				   ["mysql_num_rows" php-template-mysql-num-rows t]
				   ["mysql_pconnect" php-template-mysql-pconnect t]
				   ["mysql_ping" php-template-mysql-ping t])
				  ("MySQL (Query --> Unbuff)"
				   ["mysql_query" php-template-mysql-query t]
				   ["mysql_real_escape_string" php-template-mysql-real-escape-string t]
				   ["mysql_result" php-template-mysql-result t]
				   ["mysql_select_db" php-template-mysql-select-db t]
				   ["mysql_stat" php-template-mysql-stat t]
				   ["mysql_tablename" php-template-mysql-tablename t]
				   ["mysql_thread_id" php-template-mysql-thread-id t]
				   ["mysql_unbuffered_query" php-template-mysql-unbuffered-query t]))
				'("Others"
				  ["class" php-template-class t]
				  ["function" php-template-function t])
				'("Regular expressions"
				  ["ereg_replace" php-template-ereg-replace t]
				  ["ereg" php-template-ereg t]
				  ["eregi_replace" php-template-eregi-replace t]
				  ["eregi" php-template-eregi t]
				  ["split" php-template-split t]
				  ["spliti" php-template-spliti t]
				  ["sql_regcase" php-template-sql-regcase t])
				'("Session"
				  ["session_cache_expire" php-template-session-cache-expire t]
				  ["session_cache_limiter" php-template-session-cache-limiter t]
				  ["session_commit" php-template-session-commit t]
				  ["session_decode" php-template-session-decode t]
				  ["session_destroy" php-template-session-destroy t]
				  ["session_encode" php-template-session-encode t]
				  ["session_get_cookie_params" php-template-session-get-cookie-params t]
				  ["session_id" php-template-session-id t]
				  ["session_is_registered" php-template-session-is-registered t]
				  ["session_module_name" php-template-session-module-name t]
				  ["session_name" php-template-session-name t]
				  ["session_regenerate_id" php-template-session-regenerate-id t]
				  ["session_register" php-template-session-register t]
				  ["session_save_path" php-template-session-save-path t]
				  ["session_set_cookie_params" php-template-session-set-cookie-params t]
				  ["session_set_save_handler" php-template-session-set-save-handler t]
				  ["session_start" php-template-session-start t]
				  ["session_unregister" php-template-session-unregister t]
				  ["session_unset" php-template-session-unset t]
				  ["session_write_close" php-template-session-write-close t])
				'("String"
				  ("String (addcslashes --> htmlentities)"
				   ["addcslashes" php-template-addcslashes t]
				   ["addslashes" php-template-addslashes t]
				   ["bin2hex" php-template-bin2hex t]
				   ["chop" php-template-chop t]
				   ["chr" php-template-chr t]
				   ["chunk_split" php-template-chunk-split t]
				   ["convert_cyr_string" php-template-convert-cyr-string t]
				   ["convert_uudecode" php-template-convert-uudecode t]
				   ["convert_uuencode" php-template-convert-uuencode t]
				   ["count_chars" php-template-count-chars t]
				   ["crc32" php-template-crc32 t]
				   ["crypt" php-template-crypt t]
				   ["echo" php-template-echo t]
				   ["explode" php-template-explode t]
				   ["fprintf" php-template-fprintf t]
				   ["get_html_translation_table" php-template-get-html-translation-table t]
				   ["hebrev" php-template-hebrev t]
				   ["hebrevc" php-template-hebrevc t]
				   ["html_entity_decode" php-template-html-entity-decode t]
				   ["htmlentities" php-template-htmlentities t])
				  ("String (htmlspecialchars_decode --> quotemeta)"
				   ["htmlspecialchars_decode" php-template-htmlspecialchars-decode t]
				   ["htmlspecialchars" php-template-htmlspecialchars t]
				   ["implode" php-template-implode t]
				   ["join" php-template-join t]
				   ["levenshtein" php-template-levenshtein t]
				   ["localeconv" php-template-localeconv t]
				   ["ltrim" php-template-ltrim t]
				   ["md5_file" php-template-md5-file t]
				   ["md5" php-template-md5 t]
				   ["metaphone" php-template-metaphone t]
				   ["money_format" php-template-money-format t]
				   ["nl_langinfo" php-template-nl-langinfo t]
				   ["nl2br" php-template-nl2br t]
				   ["number_format" php-template-number-format t]
				   ["ord" php-template-ord t]
				   ["parse_str" php-template-parse-str t]
				   ["print" php-template-print t]
				   ["printf" php-template-printf t]
				   ["quoted_printable_decode" php-template-quoted-printable-decode t]
				   ["quotemeta" php-template-quotemeta t])
				  ("String (rtrim --> strcoll)"
				   ["rtrim" php-template-rtrim t]
				   ["setlocale" php-template-setlocale t]
				   ["sha1_file" php-template-sha1-file t]
				   ["sha1" php-template-sha1 t]
				   ["similar_text" php-template-similar-text t]
				   ["soundex" php-template-soundex t]
				   ["sprintf" php-template-sprintf t]
				   ["sscanf" php-template-sscanf t]
				   ["str_ireplace" php-template-str-ireplace t]
				   ["str_pad" php-template-str-pad t]
				   ["str_repeat" php-template-str-repeat t]
				   ["str_replace" php-template-str-replace t]
				   ["str_rot13" php-template-str-rot13 t]
				   ["str_shuffle" php-template-str-shuffle t]
				   ["str_split" php-template-str-split t]
				   ["str_word_count" php-template-str-word-count t]
				   ["strcasecmp" php-template-strcasecmp t]
				   ["strchr" php-template-strchr t]
				   ["strcmp" php-template-strcmp t]
				   ["strcoll" php-template-strcoll t])
				  ("String (strcspn --> strtok)"
				   ["strcspn" php-template-strcspn t]
				   ["strip_tags" php-template-strip-tags t]
				   ["stripcslashes" php-template-stripcslashes t]
				   ["stripos" php-template-stripos t]
				   ["stripslashes" php-template-stripslashes t]
				   ["stristr" php-template-stristr t]
				   ["strlen" php-template-strlen t]
				   ["strnatcasecmp" php-template-strnatcasecmp t]
				   ["strnatcmp" php-template-strnatcmp t]
				   ["strncasecmp" php-template-strncasecmp t]
				   ["strncmp" php-template-strncmp t]
				   ["strpbrk" php-template-strpbrk t]
				   ["strpos" php-template-strpos t]
				   ["strrchr" php-template-strrchr t]
				   ["strrev" php-template-strrev t]
				   ["strripos" php-template-strripos t]
				   ["strrpos" php-template-strrpos t]
				   ["strspn" php-template-strspn t]
				   ["strstr" php-template-strstr t]
				   ["strtok" php-template-strtok t])
				  ("String (strtolower --> wordwrap)"
				   ["strtolower" php-template-strtolower t]
				   ["strtoupper" php-template-strtoupper t]
				   ["strtr" php-template-strtr t]
				   ["substr_compare" php-template-substr-compare t]
				   ["substr_count" php-template-substr-count t]
				   ["substr_replace" php-template-substr-replace t]
				   ["substr" php-template-substr t]
				   ["trim" php-template-trim t]
				   ["ucfirst" php-template-ucfirst t]
				   ["ucwords" php-template-ucwords t]
				   ["vfprintf" php-template-vfprintf t]
				   ["vprintf" php-template-vprintf t]
				   ["vsprintf" php-template-vsprintf t]
				   ["wordwrap" php-template-wordwrap t]))
				'("Variable"
				  ["debug_zval_dump" php-template-debug-zval-dump t]
				  ["doubleval" php-template-doubleval t]
				  ["empty" php-template-empty t]
				  ["floatval" php-template-floatval t]
				  ["get_defined_vars" php-template-get-defined-vars t]
				  ["get_resource_type" php-template-get-resource-type t]
				  ["gettype" php-template-gettype t]
				  ["import_request_variables" php-template-import-request-variables t]
				  ["intval" php-template-intval t]
				  ["is_array" php-template-is-array t]
				  ["is_bool" php-template-is-bool t]
				  ["is_callable" php-template-is-callable t]
				  ["is_double" php-template-is-double t]
				  ["is_float" php-template-is-float t]
				  ["is_int" php-template-is-int t]
				  ["is_integer" php-template-is-integer t]
				  ["is_long" php-template-is-long t]
				  ["is_null" php-template-is-null t]
				  ["is_numeric" php-template-is-numeric t]
				  ["is_object" php-template-is-object t]
				  ["is_real" php-template-is-real t]
				  ["is_resource" php-template-is-resource t]
				  ["is_scalar" php-template-is-scalar t]
				  ["is_string" php-template-is-string t]
				  ["isset" php-template-isset t]
				  ["print_r" php-template-print-r t]
				  ["serialize" php-template-serialize t]
				  ["settype" php-template-settype t]
				  ["strval" php-template-strval t]
				  ["unserialize" php-template-unserialize t]
				  ["unset" php-template-unset t]
				  ["var_dump" php-template-var-dump t]
				  ["var_export" php-template-var-export t])
				'("XML"
				  ("XML (utf8_decode --> xml_set_notation_decl_handler)"
				   ["utf8_decode" php-template-utf8-decode t]
				   ["utf8_encode" php-template-utf8-encode t]
				   ["xml_error_string" php-template-xml-error-string t]
				   ["xml_get_current_byte_index" php-template-xml-get-current-byte-index t]
				   ["xml_get_current_column_number" php-template-xml-get-current-column-number t]
				   ["xml_get_current_line_number" php-template-xml-get-current-line-number t]
				   ["xml_get_error_code" php-template-xml-get-error-code t]
				   ["xml_parse_into_struct" php-template-xml-parse-into-struct t]
				   ["xml_parse" php-template-xml-parse t]
				   ["xml_parser_create_ns" php-template-xml-parser-create-ns t]
				   ["xml_parser_create" php-template-xml-parser-create t]
				   ["xml_parser_free" php-template-xml-parser-free t]
				   ["xml_parser_get_option" php-template-xml-parser-get-option t]
				   ["xml_parser_set_option" php-template-xml-parser-set-option t]
				   ["xml_set_character_data_handler" php-template-xml-set-character-data-handler t]
				   ["xml_set_default_handler" php-template-xml-set-default-handler t]
				   ["xml_set_element_handler" php-template-xml-set-element-handler t]
				   ["xml_set_end_namespace_decl_handler" php-template-xml-set-end-namespace-decl-handler t]
				   ["xml_set_external_entity_ref_handler" php-template-xml-set-external-entity-ref-handler t]
				   ["xml_set_notation_decl_handler" php-template-xml-set-notation-decl-handler t])
				  ("XML (xml_set_object --> xml_set_unparsed_entity_decl_handler)"
				   ["xml_set_object" php-template-xml-set-object t]
				   ["xml_set_processing_instruction_handler" php-template-xml-set-processing-instruction-handler t]
				   ["xml_set_start_namespace_decl_handler" php-template-xml-set-start-namespace-decl-handler t]
				   ["xml_set_unparsed_entity_decl_handler" php-template-xml-set-unparsed-entity-decl-handler t]))
				"--"
				["Insert Header" php-template-header t]
				["Insert Footer" php-template-footer t]
				["Insert Date" php-template-insert-date t]
				["Modify Date" php-template-modify t])
  "The Template menu")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup php nil
  "Customizations for PHP mode."
  :prefix "php-"
  :group 'languages)

(defgroup php-mode nil
  "Customizations for modes."
  :group 'php)

(defcustom php-electric-mode t
  "*Non-nil enables electrification (automatic template generation).
If nil, template generators can still be invoked through key bindings and
menu.  Is indicated in the modeline by \"/e\" after the mode name and can be
toggled by `\\[php-electric-mode]'."
  :type 'boolean
  :group 'php-mode)

(defcustom php-stutter-mode t
  "*Non-nil enables stuttering.
Is indicated in the modeline by \"/s\" after the mode name and can be toggled
by `\\[php-stutter-mode]'."
  :type 'boolean
  :group 'php-mode)

(defcustom php-indent-tabs-mode nil
  "*Non-nil means indentation can insert tabs.
Overrides local variable `indent-tabs-mode'."
  :type 'boolean
  :group 'php-mode)

(defgroup php-menu nil
  "Customizations for menues."
  :group 'php)

(defcustom php-index-menu t
  "*Non-nil means add an index menu for a source file when loading.
Note that the index menu scans a file
when it is opened."
  :type 'boolean
  :group 'php-menu)

(defcustom php-index-menu-auto-rescan t
  "*Non-nil means that the Index menu will updated during buffer changes."
  :type 'boolean
  :group 'php-menu)

(defcustom php-source-file-menu t
  "*Non-nil means add a menu of all source files in current directory."
  :type 'boolean
  :group 'php-menu)

(defgroup php-template nil
  "Template group."
  :group 'php)

(defcustom php-include-in-parenthesis t
  "*Non-nil means place parenthesis around include file."
  :type 'boolean
  :group 'php-template)

(defcustom php-add-fclose-with-fopen t
  "*Non-nil means that fclose statement will be added during the fopen template."
  :type 'boolean
  :group 'php-template)

(defcustom php-add-mysql-close-when-connect t
  "*Non-nil means that mysql_close statement will be added during the mysql_connect template."
  :type 'boolean
  :group 'php-template)

(defgroup php-style nil
  "Customizations for coding styles."
  :group 'php)

(defcustom php-basic-offset 4
  "*Amount of basic offset used for indentation.
This value is used by + and - symbols in `php-offsets-alist'."
  :type 'integer
  :group 'php-style)

(defgroup php-header nil
  "Customizations for file header."
  :group 'php-template)

(defcustom php-file-header "\
/**
 * <description string>
 *
 *
 * Created: <date>
 * Last update: <date>
 *
 * @link <link string>
 * @copyright <copyright>
 * @author <author>
 * @package <package string>
 * @version <version string>
 */

<cursor>
"
  "*String or file to insert as file header.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted as file header.
Type `C-j' for newlines.
If the header contains RCS keywords, they may be written as <RCS>Keyword<RCS>
if the header needs to be version controlled.

The following keywords for template generation are supported:
  <filename>    : replaced by the name of the buffer
  <author>      : replaced by the user name and email address
                  \(`user-full-name',`mail-host-address', `user-mail-address')
  <login>       : replaced by user login name (`user-login-name')
  <company>     : replaced by contents of option `php-company-name'
  <date>        : replaced by the current date
  <year>        : replaced by the current year
  <copyright>   : replaced by copyright string (`php-copyright-string')
  <cursor>      : final cursor position."
  :type 'string
  :group 'php-header)

(defcustom php-file-footer ""
  "*String or file to insert as file footer.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted as file footer (i.e. at
the end of the file).
Type `C-j' for newlines.
The same keywords as in option `php-file-header' can be used."
  :type 'string
  :group 'php-header)

(defcustom php-company-name ""
  "*Name of company to insert in file header.
See option `php-file-header'."
  :type 'string
  :group 'php-header)

(defcustom php-copyright-string ""
  "*Copyright string to insert in file header.
Can be multi-line string (type `C-j' for newline) and contain other file
header keywords (see option `php-file-header')."
  :type 'string
  :group 'php-header)

(defcustom php-date-format "%Y-%m-%d"
  "*Specifies the date format to use in the header.
This string is passed as argument to the command `format-time-string'.
For more information on format strings, see the documentation for the
`format-time-string' command (C-h f `format-time-string')."
  :type 'string
  :group 'php-header)

(defcustom php-modify-date-prefix-string " * Last update: "
  "*Prefix string of modification date in PHP file header.
If actualization of the modification date is called (menu,
`\\[php-template-modify]'), this string is searched and the rest
of the line replaced by the current date."
  :type 'string
  :group 'php-header)

(defcustom php-modify-date-on-saving t
  "*Non-nil means update the modification date when the buffer is saved.
Calls function `\\[php-template-modify]').

NOTE: Activate the new setting in a PHP buffer by using the menu entry
      \"Activate Options\"."
  :type 'boolean
  :group 'php-header)

(defgroup php-phpdocumentor nil
  "PHP-Documentor customizations."
  :group 'php)

(defcustom php-enable-phpdocumentor-tags t
  "*Non-nil means add PHP-Documentor tag in the comments."
  :type 'boolean
  :group 'php-phpdocumentor)

(defcustom php-class-tags '("package")
  "List of PHP-Documentor tag placed in the class comment."
  :type '(repeat (string :tag "Tag" ""))
  :group 'php-phpdocumentor)

(defcustom php-function-tags '()
  "List of PHP-Documentor tag placed in the function comment."
  :type '(repeat (string :tag "Tag" ""))
  :group 'php-phpdocumentor)

(defgroup php-misc nil
  "Miscellaneous customizations."
  :group 'php)

(defcustom php-intelligent-tab t
  "*Non-nil means `TAB' does indentation, word completion and tab insertion.
That is, if preceding character is part of a word then complete word,
else if not at beginning of line then insert tab,
else if last command was a `TAB' or `RET' then dedent one step,
else indent current line (i.e. `TAB' is bound to `php-electric-tab').
If nil, TAB always indents current line (i.e. `TAB' is bound to
`indent-according-to-mode').

NOTE: Activate the new setting in a PHP buffer by using the menu entry
      \"Activate Options\"."
  :type 'boolean
  :group 'php-misc)

(defcustom php-word-completion-in-minibuffer t
  "*Non-nil enables word completion in minibuffer (for template prompts).

NOTE: Activate the new setting by restarting Emacs."
  :type 'boolean
  :group 'php-misc)

(defcustom php-word-completion-case-sensitive nil
  "*Non-nil means word completion using `TAB' is case sensitive.
That is, `TAB' completes words that start with the same letters and case.
Otherwise, case is ignored."
  :type 'boolean
  :group 'php-misc)

(defun php-custom-set (variable value &rest functions)
  "Set variables as in `custom-set-default' and call FUNCTIONS afterwards."
  (if (fboundp 'custom-set-default)
      (custom-set-default variable value)
    (set-default variable value))
  (while functions
    (when (fboundp (car functions)) (funcall (car functions)))
    (setq functions (cdr functions))))

(defun php-customize ()
  "Call the customize function with `php' as argument."
  (interactive)
  (customize-browse 'php))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu tools functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar php-menu-max-size 20
  "*Specifies the maximum size of a menu before splitting it into submenues.")

(defun php-menu-split (list title)
  "Split menu LIST into several submenues, if number of
elements > `php-menu-max-size'."
  (if (> (length list) php-menu-max-size)
      (let ((remain (nreverse list))
	    (result '())
	    (sublist '())
	    (menuno 1)
	    (i 0))
	(while remain
	  (setq sublist (cons (car remain) sublist))
	  (setq remain (cdr remain))
	  (setq i (+ i 1))
	  (if (= i php-menu-max-size)
	      (progn
		(setq result (cons (cons (format "%s %s" title menuno)
					 (nreverse sublist)) result))
		(setq i 0)
		(setq menuno (+ menuno 1))
		(setq sublist '()))))
	(and sublist
	     (setq result (cons (cons (format "%s %s" title menuno)
				      (nreverse sublist)) result)))
	(nreverse result))
    (nreverse list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Index menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst php-imenu-generic-expression
  '(
    ("Functions"
     "^\\s-*\\(function\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Class"
     "^\\s-*\\(class\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    )
  "Imenu generic expression for PHP Mode.  See `imenu-generic-expression'.")

(defun php-index-menu-init ()
  "Initialize index menu."
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression) php-imenu-generic-expression)
  (set (make-local-variable 'imenu-auto-rescan) php-index-menu-auto-rescan)
  (set (make-local-variable 'imenu-sort-function) 'imenu--sort-by-name)
  (when (and php-index-menu (fboundp 'imenu))
    (if (or (not (boundp 'font-lock-maximum-size))
	    (> font-lock-maximum-size (buffer-size)))
	(imenu-add-to-menubar "Index")
      (message "Scanning buffer for index...buffer too big"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source file menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar php-sources-menu nil)

;; Create the source menu
(defun php-add-source-files-menu ()
  "Scan directory for all PHP source files and generate menu.
The directory of the current source file is scanned."
  (interactive)
  (message "Scanning directory for source files ...")
  (let ((newmap (current-local-map))
	(file-list (php-get-source-files))
	menu-list found)
    ;; Create list for menu
    (setq found nil)
    (while file-list
      (setq found t)
      (setq menu-list (cons (vector (car file-list)
				   (list 'find-file (car file-list)) t)
			   menu-list))
      (setq file-list (cdr file-list)))
    (setq menu-list (php-menu-split menu-list "Sources"))
    (when found (setq menu-list (cons "--" menu-list)))
    (setq menu-list (cons ["*Rescan*" php-add-source-files-menu t] menu-list))
    (setq menu-list (cons "Sources" menu-list))
    ;; Create menu
    (easy-menu-add menu-list)
    (easy-menu-define php-sources-menu newmap
		      "PHP source files menu" menu-list))
  (message ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun php-create-mode-menu ()
  "Create PHP Mode menu."
  `("PHP"
    ,(append
      '("Templates")
      php-template-menu)
    ("Indent"
     ["Line"			indent-according-to-mode :keys "C-c C-i C-l"]
     ["Region"			php-indent-region (mark)]
     ["Buffer"			php-indent-buffer :keys "C-c C-i C-b"])
    "--"
    ["Show Messages"		php-show-messages :keys "C-c M-m"]
    ["PHP Mode Documentation"   php-doc-mode :keys "C-c C-h"]
    ["Version"			php-version :keys "C-c C-v"]
    ["Check Syntax"		php-check-syntax :keys "f12"]
    ["Browse Manual"	php-browse-manual :keys "shift-f12"]
    ["Search Documentation"	php-search-documentation :keys "M-f12"]



    "--"
    ("Options"
     ("Mode"
      ["Electric Mode"
       (progn (customize-set-variable 'php-electric-mode
				      (not php-electric-mode))
	      (php-mode-line-update))
       :style toggle :selected php-electric-mode :keys "C-c C-m C-e"]
      ["Stutter Mode"
       (progn (customize-set-variable 'php-stutter-mode
				      (not php-stutter-mode))
	      (php-mode-line-update))
       :style toggle :selected php-stutter-mode :keys "C-c C-m C-s"]
      ["Indent Tabs Mode"
       (progn (customize-set-variable 'php-indent-tabs-mode
				      (not php-indent-tabs-mode))
	      (setq indent-tabs-mode php-indent-tabs-mode))
       :style toggle :selected php-indent-tabs-mode]
      "--"
      ["Customize Group..." (customize-group 'php-mode) t])
     ("Menu"
      ["Index Menu"
       (customize-set-variable 'php-index-menu
			       (not php-index-menu))
       :style toggle :selected php-index-menu]
      ["Auto-Rescan (Index Menu)"
       (customize-set-variable 'php-index-menu-auto-rescan
			       (not php-index-menu-auto-rescan))
       :style toggle :selected php-index-menu-auto-rescan]
      ["Source Menu"
       (customize-set-variable 'php-source-file-menu
			       (not php-source-file-menu))
       :style toggle :selected php-source-file-menu]
      "--"
      ["Customize Group..." (customize-group 'php-menu) t])
     ("Template"
      ["Parenthesis in include"
       (customize-set-variable 'php-include-in-parenthesis
			       (not php-include-in-parenthesis))
       :style toggle :selected php-include-in-parenthesis]
      ["fopen -> fclose"
       (customize-set-variable 'php-add-fclose-with-fopen
			       (not php-add-fclose-with-fopen))
       :style toggle :selected php-add-fclose-with-fopen]
      ["mysql_connect -> mysql_close"
       (customize-set-variable 'php-add-mysql-close-when-connect
			       (not php-add-mysql-close-when-connect))
       :style toggle :selected php-add-mysql-close-when-connect]
      "--"
      ["Customize Group..." (customize-group 'php-template) t])
     ("Style"
      ["Indent offset..."
       (customize-option 'php-basic-offset) t]
      "--"
      ["Customize Group..." (customize-group 'php-style) t])
     ("Header"
      ["Header template..."
       (customize-option 'php-file-header) t]
      ["Footer template..."
       (customize-option 'php-file-footer) t]
      ["Company..."
       (customize-option 'php-company-name) t]
      ["Copyright..."
       (customize-option 'php-copyright-string) t]
      ["Date format..."
       (customize-option 'php-date-format) t]
      ["Modify date prefix..."
       (customize-option 'php-modify-date-prefix-string) t]
      ["Modify date on saving"
       (customize-set-variable 'php-modify-date-on-saving
			       (not php-modify-date-on-saving))
       :style toggle :selected php-modify-date-on-saving]
      "--"
      ["Customize Group..." (customize-group 'php-header) t])
     ("PHP-Documentor"
      ["Enable tag"
       (progn (customize-set-variable 'php-enable-phpdocumentor-tags
				      (not php-enable-phpdocumentor-tags))
	      (php-activate-customizations))
       :style toggle :selected php-enable-phpdocumentor-tags]
      ["Class tags..."
       (customize-option 'php-class-tags) t]
      ["Function tags..."
       (customize-option 'php-function-tags) t]
      "--"
      ["Customize Group..." (customize-group 'php-phpdocumentor) t])
     ("Miscellaneous"
      ["Use Intelligent Tab"
       (progn (customize-set-variable 'php-intelligent-tab
				      (not php-intelligent-tab))
	      (php-activate-customizations))
       :style toggle :selected php-intelligent-tab]
      ["Word Completion in Minibuffer"
       (progn (customize-set-variable 'php-word-completion-in-minibuffer
				      (not php-word-completion-in-minibuffer))
	      (message "Activate new setting by saving options and restarting Emacs"))
       :style toggle :selected php-word-completion-in-minibuffer]
      ["Completion is case sensitive"
       (customize-set-variable 'php-word-completion-case-sensitive
			       (not php-word-completion-case-sensitive))
       :style toggle :selected php-word-completion-case-sensitive]
      "--"
      ["Customize Group..." (customize-group 'php-misc) t])
     "--"
     ["Save Options" customize-save-customized t]
     ["Activate Options" php-activate-customizations t]
     ["Browse Options..." php-customize t])))

(defvar php-mode-menu-list (php-create-mode-menu)
  "PHP Mode menu.")

(defvar php-mode-map nil
  "Keymap for PHP Mode.")

(defun php-update-mode-menu ()
  "Update PHP Mode menu."
  (interactive)
  (easy-menu-remove php-mode-menu-list)
  (setq php-mode-menu-list (php-create-mode-menu))
  (easy-menu-add php-mode-menu-list)
  (easy-menu-define php-mode-menu php-mode-map
		    "Menu keymap for PHP Mode." php-mode-menu-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Progress reporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar php-progress-interval 1
  "*Interval used to update progress status during long operations.
If a number, percentage complete gets updated after each interval of
that many seconds.  To inhibit all messages, set this option to nil.")

(defvar php-progress-info nil
  "Array variable for progress information: 0 begin, 1 end, 2 time.")

(defun php-update-progress-info (string pos)
  "Update progress information."
  (when (and php-progress-info (not noninteractive)
	     (< php-progress-interval
		(- (nth 1 (current-time)) (aref php-progress-info 2))))
    (message (concat string "... (%2d%s)")
	     (/ (* 100 (- pos (aref php-progress-info 0)))
		(- (aref php-progress-info 1)
		   (aref php-progress-info 0))) "%")
    (aset php-progress-info 2 (nth 1 (current-time)))))

(defmacro php-point (position)
  "Return the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

bol  -- beginning of line
eol  -- end of line
bod  -- beginning of defun
boi  -- back to indentation
eoi  -- last whitespace on line
ionl -- indentation of next line
iopl -- indentation of previous line
bonl -- beginning of next line
bopl -- beginning of previous line

This function does not modify point or mark."
  (or (and (eq 'quote (car-safe position))
	   (null (cddr position)))
      (error "ERROR:  Bad buffer position requested: %s" position))
  (setq position (nth 1 position))
  `(let ((here (point)))
     ,@(cond
	((eq position 'bol)  '((beginning-of-line)))
	((eq position 'eol)  '((end-of-line)))
	((eq position 'bod)  '((save-match-data
				 (php-beginning-of-defun))))
	((eq position 'boi)  '((back-to-indentation)))
	((eq position 'eoi)  '((end-of-line) (skip-chars-backward " \t")))
	((eq position 'bonl) '((forward-line 1)))
	((eq position 'bopl) '((forward-line -1)))
	((eq position 'iopl)
	 '((forward-line -1)
	   (back-to-indentation)))
	((eq position 'ionl)
	 '((forward-line 1)
	   (back-to-indentation)))
	(t (error "ERROR:  Unknown buffer position requested: %s" position))
	)
     (prog1
	 (point)
       (goto-char here))
     ;; workaround for an Emacs18 bug -- blech! Well, at least it
     ;; doesn't hurt for v19
     ,@nil
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File/directory manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun php-directory-files (directory &optional full match)
  "Call `directory-files' if DIRECTORY exists, otherwise generate error
message."
  (if (not (file-directory-p directory))
      (php-warning-when-idle "No such directory: \"%s\"" directory)
    (let ((dir (directory-files directory full match)))
      (setq dir (delete "." dir))
      (setq dir (delete ".." dir))
      dir)))

(defun php-get-source-files (&optional full directory)
  "Get list of PHP source files in DIRECTORY or current directory."
  (let ((mode-alist auto-mode-alist)
	filename-regexp)
    ;; create regular expressions for matching file names
    (setq filename-regexp "\\`[^.].*\\(")
    (while mode-alist
      (when (eq (cdar mode-alist) 'php-mode)
	(setq filename-regexp
	      (concat filename-regexp (caar mode-alist) "\\|")))
      (setq mode-alist (cdr mode-alist)))
    (setq filename-regexp
	  (concat (substring filename-regexp 0
			     (string-match "\\\\|$" filename-regexp)) "\\)"))
    ;; find files
    (php-directory-files
     (or directory default-directory) full filename-regexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages reporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar php-warnings nil
  "Warnings to tell the user during start up.")

(defun php-run-when-idle (secs repeat function)
  "Wait until idle, then run FUNCTION."
  (if (fboundp 'start-itimer)
      (start-itimer "php-mode" function secs repeat t)
;    (run-with-idle-timer secs repeat function)))
    ;; explicitely activate timer (necessary when Emacs is already idle)
    (aset (run-with-idle-timer secs repeat function) 0 nil)))

(defun php-warning-when-idle (&rest args)
  "Wait until idle, then print out warning STRING and beep."
  (if noninteractive
      (php-warning (apply 'format args) t)
    (unless php-warnings
      (php-run-when-idle .1 nil 'php-print-warnings))
    (setq php-warnings (cons (apply 'format args) php-warnings))))

(defun php-warning (string &optional nobeep)
  "Print out warning STRING and beep."
  (message (concat "WARNING:  " string))
  (unless (or nobeep noninteractive) (beep)))

(defun php-print-warnings ()
  "Print out messages in variable `php-warnings'."
  (let ((no-warnings (length php-warnings)))
    (setq php-warnings (nreverse php-warnings))
    (while php-warnings
      (message (concat "WARNING:  " (car php-warnings)))
      (setq php-warnings (cdr php-warnings)))
    (beep)
    (when (> no-warnings 1)
      (message "WARNING:  See warnings in message buffer (type `C-c M-m')."))))

(defun php-show-messages ()
  "Get *Messages* buffer to show recent messages."
  (interactive)
  (display-buffer " *Message-Log*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update the date field when saving file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun php-write-file-hooks-init ()
  "Add/remove hooks when buffer is saved."
  (if php-modify-date-on-saving
      (add-hook 'local-write-file-hooks 'php-template-modify-noerror)
    (remove-hook 'local-write-file-hooks 'php-template-modify-noerror))
  (make-local-variable 'after-save-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Regular expression used internally to recognize labels in switch
;;; statements.
(defvar php-switch-label-regexp (purecopy "case[ \t'/(]\\|default[ \t]*:"))

(defvar php-label-offset -2
  "*Offset of PHP label lines and case statements relative to usual indentation.")

(defvar php-indent-level 4
  "*Indentation of PHP statements with respect to containing block.")

(defvar php-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")

(defvar php-argdecl-indent 5
  "*Indentation level of declarations of PHP function arguments.")

(defvar php-continued-statement-offset 2
  "*Extra indent for lines not starting new statements.")

(defvar php-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.")

(defvar php-brace-imaginary-offset 0
  "*Imagined indentation of a PHP open brace that actually follows a statement.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun php-comment-indent ()
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (let ((opoint (point)))
      (save-excursion
	(beginning-of-line)
	(cond ((looking-at "[ \t]*}[ \t]*\\($\\|/\\*\\)")
	       ;; A comment following a solitary close-brace
	       ;; should have only one space.
	       (search-forward "}")
	       (1+ (current-column)))
	      ((or (looking-at "^#[ \t]*endif[ \t]*")
		   (looking-at "^#[ \t]*else[ \t]*"))
	       7)			;2 spaces after #endif
	      ((progn
		 (goto-char opoint)
		 (skip-chars-backward " \t")
		 (and (= comment-column 0) (bolp)))
	       ;; If comment-column is 0, and nothing but space
	       ;; before the comment, align it at 0 rather than 1.
	       0)
	      (t
	       (max (1+ (current-column))	;Else indent at comment column
		    comment-column)))))))	; except leave at least one space.

(defun php-indent-line-2 ()
  "Indent current line as PHP code.
Return the amount the indentation changed by."
  (let ((indent (php-calculate-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (php-calculate-indent-within-comment)))
	  ((looking-at "[ \t]*#")
	   (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((or (looking-at php-switch-label-regexp)
		      (and (looking-at "[A-Za-z]")
			   (save-excursion
			     (forward-sexp 1)
			     (looking-at ":"))))
		  (setq indent (max 1 (+ indent php-label-offset))))
		 ;; Indenting the else statement
		 ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (php-backward-to-start-of-if)
				 (current-indentation))))
		 ((and (looking-at "}[ \t]*else\\b")
		       (not (looking-at "}[ \t]*else\\s_")))
		  (setq indent (save-excursion
				 (forward-char)
				 (backward-sexp)
				 (php-backward-to-start-of-if)
				 (current-indentation))))
		 ((and (looking-at "while\\b")
		       (not (looking-at "while\\s_"))
		       (save-excursion
			 (php-backward-to-start-of-do)))
		  ;; This is a `while' that ends a do-while.
		  (setq indent (save-excursion
				 (php-backward-to-start-of-do)
				 (current-indentation))))
		 ((= (following-char) ?})
		  (setq indent (- indent php-indent-level)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent php-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      ;;(setq indent (+ php-basic-offset indent))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    ;;(indent-to php-basic-offset)
    shift-amt))

(defun php-indent-line ()
  "Indent the current line as PHP code.  Returns the amount of
indentation change."
  (interactive)
  (let* (indent current-line)
    (save-excursion
      (beginning-of-line)
      (setq current-line (point-marker))
      ;(while (and (not (eq (point-marker) (beginning-of-buffer)))
		  ;(
	(forward-line -1);)
      (if (eq current-line (point-marker))
	  (setq indent 0)
	(setq indent (current-indentation))
	(if (and (re-search-forward "^\\s-*\\/\\*" (php-point 'eol) t) (not (re-search-forward "\\*\\/\\s-*$" (php-point 'eol) t)))
	    (setq indent (+ indent 1)))
	(if (and (re-search-forward "\\*\\/\\s-*$" (php-point 'eol) t) (not (re-search-forward "^\\s-*\\/\\*" (php-point 'eol) t)))
	    (setq indent (- indent 1)))
	(when (and (re-search-forward "\\({\\|(\\)\\s-*$" (php-point 'eol) t ) (not (re-search-forward "^\\(}\\|)\\)\\s-*" (php-point 'eol) t)))
	  (setq indent (+ indent php-basic-offset)))))
    (delete-region (php-point 'bol) (php-point 'boi))
    (beginning-of-line)
    (when (re-search-forward "^\\(}\\|)\\)\\s-*" (php-point 'eol) t)
      (setq indent (- indent php-basic-offset)))
    (indent-to indent)
    indent))

(defun php-indent-region (beg end column)
  "Indent region as PHP code.
Adds progress reporting to `indent-region'."
  (interactive "r\nP")
  (when php-progress-interval
    (setq php-progress-info (vector (count-lines (point-min) beg)
				     (count-lines (point-min) end) 0)))
  (indent-region beg end column)
  (when php-progress-interval (message "Indenting...done"))
  (setq php-progress-info nil))

(defun php-indent-buffer ()
  "Indent whole buffer as PHP code.
Calls `indent-region' for whole buffer and adds progress reporting."
  (interactive)
  (php-indent-region (point-min) (point-max) nil))

(defun php-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as PHP code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     ;; or may be function argument declaration.
	     ;; Indent like the previous top level line
	     ;; unless that ends in a closeparen without semicolon,
	     ;; in which case this line is the first argument decl.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (if (= (following-char) ?{)
		 0   ; Unless it starts a function body
	       (php-backward-to-noncomment (or parse-start (point-min)))
	       ;; Look at previous line that's at column 0
	       ;; to determine whether we are in top-level decls
	       ;; or function's arg decls.  Set basic-indent accordingly.
	       (let (found (basic-indent
		      (save-excursion
			(re-search-backward "^[^ \^L\t\n#]" nil 'move)
			(let (comment lim)
			  ;; Recognize the DEFUN macro in Emacs.
			  (if (save-excursion
				;; Move down to the (putative) argnames line.
				(while (and (not (eobp))
					    (not (looking-at " *[({}#/]")))
				  (forward-line 1))
				;; Go back to the DEFUN, if it is one.
				(condition-case nil
				    (backward-sexp 1)
				  (error))
				(beginning-of-line)
				(looking-at "DEFUN\\b"))
			      php-argdecl-indent
			    (if (and (looking-at "\\sw\\|\\s_")
				     ;; This is careful to stop at the first
				     ;; paren if we have
				     ;; int foo Proto ((int, int));
				     (looking-at "[^\"\n=(]*(")
				     (progn
				       (goto-char (1- (match-end 0)))
				       ;; Skip any number of paren-groups.
				       ;; Consider typedef int (*fcn) (int);
				       (while (= (following-char) ?\()
					 (setq lim (point))
					 (condition-case nil
					     (forward-sexp 1)
					   (error))
					 (skip-chars-forward " \t\f"))
				       ;; Have we reached something
				       ;; that shows this isn't a function
				       ;; definition?
				       (and (< (point) indent-point)
					    (not (memq (following-char)
						       '(?\, ?\;)))))
				     ;; Make sure the "function decl" we found
				     ;; is not inside a comment.
				     (progn
				       ;; Move back to the `(' starting arglist
				       (goto-char lim)
				       (beginning-of-line)
				       (while (and (not comment)
						   (search-forward "/*" lim t))
					 (setq comment
					       (not (search-forward "*/" lim t))))
				       (not comment)))
				(progn
				  (setq found nil)
				  (while (and (not found) (re-search-forward ";" indent-point t))
				    (if (and (not (php-in-comment-p))
					     (save-excursion
					       (setq start (point-marker))
					       (setq state (parse-partial-sexp start indent-point))
					       (if (= 0 (nth 0 state))
						   t
						 nil)))
					(setq found t)))
				  (if found
				      0
				    php-argdecl-indent))
			      (progn
				(if (re-search-forward "else" indent-point t)
				    (progn
				      (setq found nil)
				      (while (and (not found) (re-search-forward ";" indent-point t))
					(if (and (not (php-in-comment-p))
						 (save-excursion
						   (setq start (point-marker))
						   (setq state (parse-partial-sexp start indent-point))
						   (if (= 0 (nth 0 state))
						       t
						     nil)))
					    (setq found t)))
				      (if found
					  0
					php-argdecl-indent))
				  0))))))))
		 basic-indent)))

;; 		 ;; Now add a little if this is a continuation line.
;; 		 (+ basic-indent (if (or (bobp)
;; 					 (memq (preceding-char) '(?\) ?\; ?\}))
;; 					 ;; Line with zero indentation
;; 					 ;; is probably the return-type
;; 					 ;; of a function definition,
;; 					 ;; so following line is function name.
;; 					 (= (current-indentation) 0))
;;				     0 php-continued-statement-offset))

	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (php-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (or (eq (preceding-char) ?\,)
			(and (eq (preceding-char) ?:)
			     (or (eq (char-after (- (point) 2)) ?\')
				 (memq (char-syntax (char-after (- (point) 2)))
				       '(?w ?_)))))
	       (if (eq (preceding-char) ?\,)
		   (progn (forward-char -1)
			  (php-backward-to-start-of-continued-exp containing-sexp)))
	       (beginning-of-line)
	       (php-backward-to-noncomment containing-sexp))
	     ;; Check for a preprocessor statement or its continuation lines.
	     ;; Move back to end of previous non-preprocessor line,
	     ;; or possibly beginning of buffer.
	     (let ((found (point)) stop)
	       (while (not stop)
		 (beginning-of-line)
		 (cond ((bobp)
			(setq found (point)
			      stop t))
		       ((save-excursion (forward-char -1)
					(= (preceding-char) ?\\))
			(forward-char -1))
		       ;; This line is not preceded by a backslash.
		       ;; So either it starts a preprocessor command
		       ;; or any following continuation lines
		       ;; should not be skipped.
		       ((= (following-char) ?#)
			(forward-char -1)
			(setq found (point)))
		       (t (setq stop t))))
	       (goto-char found))
	     ;; Now we get the answer.
	     (if (and (not (memq (preceding-char) '(0 ?\, ?\; ?\} ?\{)))
		      ;; But don't treat a line with a close-brace
		      ;; as a continuation.  It is probably the
		      ;; end of an enum type declaration.
		      (save-excursion
			(goto-char indent-point)
			(skip-chars-forward " \t")
			(not (= (following-char) ?}))))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  php-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (php-backward-to-start-of-continued-exp containing-sexp)
		   (+ php-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?{))
			  php-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (let ((colon-line-end 0))
		     (while (progn (skip-chars-forward " \t\n")
				   (looking-at "#\\|/\\*\\|case[ \t\n'/(].*:\\|[a-zA-Z0-9_$]*:"))
		       ;; Skip over comments and labels following openbrace.
		       (cond ((= (following-char) ?\#)
			      (forward-line 1))
			     ((= (following-char) ?\/)
			      (forward-char 2)
			      (search-forward "*/" nil 'move))
			     ;; case or label:
			     (t
			      (save-excursion (end-of-line)
					      (setq colon-line-end (point)))
			      (search-forward ":"))))
		     ;; The first following code counts
		     ;; if it is before the line we want to indent.
		     (and (< (point) indent-point)
			  (- 
			   (if (> colon-line-end (point))
			       (- (current-indentation) php-label-offset)
			     (current-column))
			   ;; If prev stmt starts with open-brace, that
			   ;; open brace was offset by php-brace-offset.
			   ;; Compensate to get the column where
			   ;; an ordinary statement would start.
			   (if (= (following-char) ?\{) php-brace-offset 0)))))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 (php-calculate-indent-after-brace))))))))

(defun php-calculate-indent-within-comment (&optional after-star)
  "Return the indentation amount for line inside a block comment.
Optional arg AFTER-STAR means, if lines in the comment have a leading star,
return the indentation of the text that would follow this star."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if after-star
	  (and (looking-at "\\*")
	       (re-search-forward "\\*[ \t]*")))
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (not after-star)
	   (goto-char (1+ (match-beginning 0))))
      (if (and (looking-at "[ \t]*$") (= (preceding-char) ?\*))
	  (1+ (current-column))
	(current-column)))))

(defun php-backward-to-noncomment (lim)
  (let (stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*/")))
	  (search-backward "/*" lim 'move)
	(setq stop (or (<= (point) lim)
		       (save-excursion
			 (while (progn
				  (beginning-of-line)
				  (eq ?\\ (char-after (- (point) 2))))
			   (forward-char -1)
			   (beginning-of-line))
			 (skip-chars-forward " \t")
			 (not (looking-at "#")))))
	(or stop (beginning-of-line))))))

(defun php-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) '(?\) ?\"))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun php-backward-to-start-of-if (&optional limit)
  "Move to the start of the last \"unbalanced\" `if'."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (and (not (bobp)) (not (zerop if-level)))
      (backward-sexp 1)
      (cond ((and (looking-at "else\\b")
		  (not (looking-at "else\\s_")))
	     (setq if-level (1+ if-level)))
	    ((and (looking-at "if\\b")
		  (not (looking-at "if\\s_")))
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun php-backward-to-start-of-do (&optional limit)
  "If point follows a `do' statement, move to beginning of it and return t.
Otherwise return nil and don't move point."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((first t)
	(startpos (point))
	(done nil))
    (while (not done)
      (let ((next-start (point)))
	(condition-case nil
	    ;; Move back one token or one brace or paren group.
	    (backward-sexp 1)
	  ;; If we find an open-brace, we lose.
	  (error (setq done 'fail)))
	(if done
	    nil
	  ;; If we reached a `do', we win.
	  (if (looking-at "do\\b")
	      (setq done 'succeed)
	    ;; Otherwise, if we skipped a semicolon, we lose.
	    ;; (Exception: we can skip one semicolon before getting
	    ;; to a the last token of the statement, unless that token
	    ;; is a close brace.)
	    (if (save-excursion
		  (forward-sexp 1)
		  (or (and (not first) (= (preceding-char) ?}))
		      (search-forward ";" next-start t
				      (if (and first
					       (/= (preceding-char) ?}))
					  2 1))))
		(setq done 'fail)
	      (setq first nil)
	      ;; If we go too far back in the buffer, we lose.
	      (if (< (point) limit)
		  (setq done 'fail)))))))
    (if (eq done 'succeed)
	t
      (goto-char startpos)
      nil)))

(defun php-calculate-indent-after-brace ()
  "Return the proper PHP indent for the first line after an open-brace.
This function is called with point before the brace."
  ;; For open brace in column zero, don't let statement
  ;; start there too.  If php-indent-level is zero,
  ;; use php-brace-offset + php-continued-statement-offset instead.
  ;; For open-braces not the first thing in a line,
  ;; add in php-brace-imaginary-offset.
  (+ (if (and (bolp) (zerop php-indent-level))
	 (+ php-brace-offset php-continued-statement-offset)
       php-indent-level)
     ;; Move back over whitespace before the openbrace.
     ;; If openbrace is not first nonwhite thing on the line,
     ;; add the c-brace-imaginary-offset.
     (progn (skip-chars-backward " \t")
	    (if (bolp) 0 php-brace-imaginary-offset))
     ;; If the openbrace is preceded by a parenthesized exp,
     ;; move to the beginning of that;
     ;; possibly a different line
     (progn
       (if (eq (preceding-char) ?\))
	   (forward-sexp -1))
       ;; Get initial indentation of the line we are on.
       (current-indentation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun php-function-name (prefix string &optional postfix)
  "Generate a Lisp function name.
PREFIX, STRING and optional POSTFIX are concatenated by '-' and spaces in
STRING are replaced by `-' and substrings are converted to lower case."
  (let ((name prefix))
    (while (string-match "\\(\\w+\\)\\s-*\\(.*\\)" string)
      (setq name
	    (concat name "-" (downcase (substring string 0 (match-end 1)))))
      (setq string (substring string (match-beginning 2))))
    (when postfix (setq name (concat name "-" postfix)))
    (intern name)))

(defvar php-mode-hook nil
  "*Hook called by `php-mode'.")

(defvar php-constants nil
  "List of PHP constants.")

(defvar php-constants-regexp nil
  "Regexp for PHP constants.")

(defconst php-01-constants
  '(;; core constants
    "__LINE__" "__FILE__"
    "PHP_OS" "PHP_VERSION"
    "TRUE" "FALSE" "NULL"
    "E_ERROR" "E_NOTICE" "E_PARSE" "E_WARNING" "E_ALL"
    "E_USER_ERROR" "E_USER_WARNING" "E_USER_NOTICE"
    "DEFAULT_INCLUDE_PATH" "PEAR_INSTALL_DIR" "PEAR_EXTENSION_DIR"
    "PHP_BINDIR" "PHP_LIBDIR" "PHP_DATADIR" "PHP_SYSCONFDIR"
    "PHP_LOCALSTATEDIR" "PHP_CONFIG_FILE_PATH")
  "PHP constants.")

(defvar php-keywords nil
  "List of PHP keywords.")

(defvar php-keywords-regexp nil
  "Regexp for PHP keywords.")

(defconst php-01-keywords
  ;; "class", "new" and "extends" get special treatment
  ;; "case" and  "default" get special treatment elsewhere
  '("and" "as" "break" "continue" "declare" "do" "echo" "else" "elseif"
    "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit"
    "extends" "for" "foreach" "global" "if" "include" "include_once"
    "next" "or" "require" "require_once" "return" "static" "switch"
    "then" "var" "while" "xor" "private" "throw" "catch" "try"
    "instanceof" "catch all" "finally")
  "PHP keywords.")

(defconst php-identifier
  (eval-when-compile
    '"[a-zA-Z\_\x7f-\xff][a-zA-Z0-9\_\x7f-\xff]*")
  "Characters in a PHP identifier.")

(defvar php-types nil
  "List of PHP types.")

(defvar php-types-regexp nil
  "Regexp for PHP types.")

(defconst php-01-types
  '("array" "bool" "boolean" "char" "const" "double" "float"
    "int" "integer" "long" "mixed" "object" "real"
    "string")
  "PHP types.")

(defvar php-superglobals nil
  "List of PHP superglobals.")

(defvar php-superglobals-regexp nil
  "Regexp for PHP superglobals.")

(defconst php-01-superglobals
  '("_GET" "_POST" "_COOKIE" "_SESSION" "_ENV" "GLOBALS"
    "_SERVER" "_FILES" "_REQUEST")
  "PHP superglobal variables.")

(defvar php-functions nil
  "List of PHP functions.")

(defvar php-functions-regexp nil
  "Regexp for PHP functions.")

(defconst php-01-functions
  '(
    ;; .NET
    "dotnet_load"
    ;; Apache
    "apache_child_terminate" "apache_get_modules" "apache_get_version"
    "apache_getenv" "apache_lookup_uri" "apache_note" "apache_request_headers"
    "apache_reset_timeout" "apache_response_headers" "apache_setenv"
    "ascii2ebcdic" "ebcdic2ascii" "getallheaders" "virtual"
    ;; APC
    "apc_cache_info" "apc_clear_cache" "apc_define_constants" "apc_delete"
    "apc_fetch" "apc_load_constants" "apc_sma_info" "apc_store"
    ;; Array
    "array_change_key_case" "array_chunk" "array_combine" "array_count_values"
    "array_diff_assoc" "array_diff_key" "array_diff_uassoc" "array_diff_ukey"
    "array_diff" "array_fill" "array_filter" "array_flip" "array_intersect_assoc"
    "array_intersect_key" "array_intersect_uassoc" "array_intersect_ukey"
    "array_intersect" "array_key_exists" "array_keys" "array_map" 
    "array_merge_recursive" "array_merge" "array_multisort" "array_pad"
    "array_pop" "array_product" "array_push" "array_rand" "array_reduce"
    "array_reverse" "array_search" "array_shift" "array_slice" "array_splice"
    "array_sum" "array_udiff_assoc" "array_udiff_uassoc" "array_udiff"
    "array_uintersect_assoc" "array_uintersect_uassoc" "array_uintersect"
    "array_unique" "array_unshift" "array_values" "array_walk_recursive"
    "array_walk" "array" "arsort" "asort" "compact" "count" "current" "each"
    "end" "extract" "in_array" "key" "krsort" "ksort" "list" "natcasesort"
    "natsort" "next" "pos" "prev" "range" "reset" "rsort" "shuffle" "sizeof"
    "sort" "uasort" "uksort" "usort"
    ;; Date/Time
    "checkdate" "date_default_timezone_get" "date_default_timezone_set"
    "date_sunrise" "date_sunset" "date" "getdate" "gettimeofday" "gmdate"
    "gmmktime" "gmstrftime" "idate" "localtime" "microtime" "mktime"
    "strftime" "strptime" "strtotime" "time"
    ;; Directory
    "chdir" "chroot" "dir" "closedir" "getcwd" "opendir" "readdir"
    "rewinddir" "scandir"
    ;; Error and Logging
    "debug_backtrace" "debug_print_backtrace" "error_log" "error_reporting"
    "restore_error_handler" "restore_exception_handler" "set_error_handler"
    "set_exception_handler" "trigger_error" "user_error"
    ;; File system
    "basename" "chgrp" "chmod" "chown" "clearstatcache" "copy" "delete"
    "dirname" "disk_free_space" "disk_total_space" "diskfreespace" "fclose"
    "feof" "fflush" "fgetc" "fgetcsv" "fgets" "fgetss" "file_exists"
    "file_get_contents" "file_put_contents" "file" "fileatime" "filectime"
    "filegroup" "fileinode" "filemtime" "fileowner" "fileperms" "filesize"
    "filetype" "flock" "fnmatch" "fopen" "fpassthru" "fputcsv" "fputs"
    "fread" "fscanf" "fseek" "fstat" "ftell" "ftruncate" "fwrite" "glob"
    "is_dir" "is_executable" "is_file" "is_link" "is_readable" "is_uploaded_file"
    "is_writable" "is_writeable" "link" "linkinfo" "lstat" "mkdir" "move_uploaded_file"
    "parse_ini_file" "pathinfo" "pclose" "popen" "readfile" "readlink" "realpath"
    "rename" "rewind" "rmdir" "set_file_buffer" "stat" "symlink" "tempnam"
    "tmpfile" "touch" "umask" "unlink"
    ;; Functions
    "call_user_func_array" "call_user_func" "create_function" "func_get_arg"
    "func_get_args" "func_num_args" "function_exists" "get_defined_functions"
    "register_shutdown_function" "register_tick_function" "unregister_tick_function"
    ;; Image
    "gd_info" "getimagesize" "image_type_to_extension" "image_type_to_mime_type"
    "image2wbmp" "imagealphablending" "imageantialias" "imagearc" "imagechar"
    "imagecharup" "imagecolorallocate" "imagecolorallocatealpha" "imagecolorat"
    "imagecolorclosest" "imagecolorclosestalpha" "imagecolorclosesthwb" 
    "imagecolordeallocate" "imagecolorexact" "imagecolorexactalpha" "imagecolormatch"
    "imagecolorresolve" "imagecolorresolvealpha" "imagecolorset" "imagecolorsforindex"
    "imagecolorstotal" "imagecolortransparent" "imageconvolution" "imagecopy"
    "imagecopymerge" "imagecopymergegray" "imagecopyresampled" "imagecopyresized"
    "imagecreate" "imagecreatefromgd2" "imagecreatefromgd2part" "imagecreatefromgd"
    "imagecreatefromgif" "imagecreatefromjpeg" "imagecreatefrompng" 
    "imagecreatefromstring" "imagecreatefromwbmp" "imagecreatefromxbm" "imagecreatefromxpm"
    "imagecreatetruecolor" "imagedashedline" "imagedestroy" "imageellipse" 
    "imagefill" "imagefilledarc" "imagefilledellipse" "imagefilledpolygon" 
    "imagefilledrectangle" "imagefilltoborder" "imagefilter" "imagefontheight"
    "imagefontwidth" "imageftbbox" "imagefttext" "imagegammacorrect" "imagegd2"
    "imagegd" "imagegif" "imageinterlace" "imageistruecolor" "imagejpeg" "imagelayereffect"
    "imageline" "imageloadfont" "imagepalettecopy" "imagepng" "imagepolygon" "imagepsbbox"
    "imagepsencodefont" "imagepsextendfont" "imagepsfreefont" "imagepsloadfont"
    "imagepsslantfont" "imagepstext" "imagerectangle" "imagerotate" "imagesavealpha"
    "imagesetbrush" "imagesetpixel" "imagesetstyle" "imagesetthickness" "imagesettile"
    "imagestring" "imagestringup" "imagesx" "imagesy" "imagetruecolortopalette" "imagettfbbox"
    "imagettftext" "imagetypes" "imagewbmp" "imagexbm" "iptcembed" "iptcparse"
    "jpeg2wbmp" "png2wbmp"
    ;; Mail
    "ezmlm_hash" "mail"
    ;; Mathematical
    "abs" "acos" "acosh" "asin" "asinh" "atan2" "atan" "atanh" "base_convert" "bindec"
    "ceil" "cos" "cosh" "decbin" "dechex" "decoct" "deg2rad" "exp" "expm1" "floor"
    "fmod" "getrandmax" "hexdec" "hypot" "is_finite" "is_infinite" "is_nan" "lcg_value"
    "log10" "log1p" "log" "max" "min" "mt_getrandmax" "mt_rand" "mt_srand" "octdec"
    "pi" "pow" "rad2deg" "rand" "round" "sin" "sinh" "sqrt" "srand" "tan" "tanh"
    ;; Miscellaneous
    "connection_aborted" "connection_status" "connection_timeout" "constant"
    "define" "defined" "die" "eval" "exit" "get_browser" "__halt_compiler"
    "highlight_file" "highlight_string" "ignore_user_abort" "pack" 
    "php_check_syntax" "php_strip_whitespace" "show_source" "sleep" 
    "sys_getloadavg" "time_nanosleep" "time_sleep_until" "uniqid"
    "unpack" "usleep"  
    ;; MySQL
    "mysql_affected_rows" "mysql_change_user" "mysql_client_encoding"
    "mysql_close" "mysql_connect" "mysql_create_db" "mysql_data_seek"
    "mysql_db_name" "mysql_db_query" "mysql_drop_db" "mysql_errno"
    "mysql_error" "mysql_escape_string" "mysql_fetch_array" "mysql_fetch_assoc"
    "mysql_fetch_field" "mysql_fetch_lengths" "mysql_fetch_object"
    "mysql_fetch_row" "mysql_field_flags" "mysql_field_len"
    "mysql_field_name" "mysql_field_seek" "mysql_field_table" "mysql_field_type"
    "mysql_free_result" "mysql_get_client_info" "mysql_get_host_info"
    "mysql_get_proto_info" "mysql_get_server_info" "mysql_info" "mysql_insert_id"
    "mysql_list_dbs" "mysql_list_fields" "mysql_list_processes" "mysql_list_tables"
    "mysql_num_fields" "mysql_num_rows" "mysql_pconnect" "mysql_ping"
    "mysql_query" "mysql_real_escape_string" "mysql_result" "mysql_select_db"
    "mysql_stat" "mysql_tablename" "mysql_thread_id" "mysql_unbuffered_query"
    ;; Regular expressions
    "ereg_replace" "ereg" "eregi_replace" "eregi" "split" "spliti" "sql_regcase"
    ;; Session
    "session_cache_expire" "session_cache_limiter" "session_commit" "session_decode"
    "session_destroy" "session_encode" "session_get_cookie_params" "session_id"
    "session_is_registered" "session_module_name" "session_name" "session_regenerate_id"
    "session_register" "session_save_path" "session_set_cookie_params"
    "session_set_save_handler" "session_start" "session_unregister"
    "session_unset" "session_write_close"
    ;; Strings
    "addcslashes" "addslashes" "bin2hex" "chop" "chr" "chunk_split" "convert_cyr_string"
    "convert_uudecode" "convert_uuencode" "count_chars" "crc32" "crypt" "echo"
    "explode" "fprintf" "get_html_translation_table" "hebrev" "hebrevc" 
    "html_entity_decode" "htmlentities" "htmlspecialchars_decode" "htmlspecialchars"
    "implode" "join" "levenshtein" "localeconv" "ltrim" "md5_file" "md5" "metaphone"
    "money_format" "nl_langinfo" "nl2br" "number_format" "ord" "parse_str" "print"
    "printf" "quoted_printable_decode" "quotemeta" "rtrim" "setlocale" "sha1_file"
    "sha1" "similar_text" "soundex" "sprintf" "sscanf" "str_ireplace" "str_pad"
    "str_repeat" "str_replace" "str_rot13" "str_shuffle" "str_split" "str_word_count"
    "strcasecmp" "strchr" "strcmp" "strcoll" "strcspn" "strip_tags" "stripcslashes"
    "stripos" "stripslashes" "stristr" "strlen" "strnatcasecmp" "strnatcmp" "strncasecmp"
    "strncmp" "strpbrk" "strpos" "strrchr" "strrev" "strripos" "strrpos" "strspn"
    "strstr" "strtok" "strtolower" "strtoupper" "strtr" "substr_compare" "substr_count"
    "substr_replace" "substr" "trim" "ucfirst" "ucwords" "vfprintf" "vprintf"
    "vsprintf" "wordwrap"
    ;; Variable
    "debug_zval_dump" "doubleval" "empty" "floatval" "get_defined_vars" "get_resource_type"
    "gettype" "import_request_variables" "intval" "is_array" "is_bool" "is_callable" 
    "is_double" "is_float" "is_int" "is_integer" "is_long" "is_null" "is_numeric" 
    "is_object" "is_real" "is_resource" "is_scalar" "is_string" "isset" "print_r"
    "serialize" "settype" "strval" "unserialize" "unset" "var_dump" "var_export"
    ;; XML
    "utf8_decode" "utf8_encode" "xml_error_string" "xml_get_current_byte_index"
    "xml_get_current_column_number" "xml_get_current_line_number" "xml_get_error_code"
    "xml_parse_into_struct" "xml_parse" "xml_parser_create_ns" "xml_parser_create"
    "xml_parser_free" "xml_parser_get_option" "xml_parser_set_option"
    "xml_set_character_data_handler" "xml_set_default_handler" "xml_set_element_handler"
    "xml_set_end_namespace_decl_handler" "xml_set_external_entity_ref_handler"
    "xml_set_notation_decl_handler" "xml_set_object" "xml_set_processing_instruction_handler"
    "xml_set_start_namespace_decl_handler" "xml_set_unparsed_entity_decl_handler"
    )
  "PHP functions list")

(defconst php-others '("class" "function")
  "PHP functions list that have a special highlight")

(defun php-version ()
  "Echo the current version of PHP Mode in the minibuffer."
  (interactive)
  (message "PHP Mode %s (%s)" php-version php-time-stamp)
  (php-keep-region-active))

;; active regions
(defun php-keep-region-active ()
  "Do whatever is necessary to keep the region active in XEmacs.
Ignore byte-compiler warnings you might see."
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defmacro php-prepare-search-1 (&rest body)
  "Enable case insensitive search and switch to syntax table that includes '_',
then execute BODY, and finally restore the old environment.  Used for
consistent searching."
  `(let ((case-fold-search t)		; case insensitive search
	 (current-syntax-table (syntax-table))
	 result
	 (restore-prog			; program to restore enviroment
	  '(progn
	     ;; restore syntax table
	     (set-syntax-table current-syntax-table))))
     ;; use extended syntax table
     (set-syntax-table php-mode-ext-syntax-table)
     ;; execute BODY safely
     (setq result
	   (condition-case info
	       (progn ,@body)
	     (error (eval restore-prog)	; restore environment on error
		    (error (cadr info))))) ; pass error up
     ;; restore environment
     (eval restore-prog)
     result))

(defmacro php-prepare-search-2 (&rest body)
  "Enable case insensitive search, switch to syntax table that includes '_',
and remove `intangible' overlays, then execute BODY, and finally restore the
old environment.  Used for consistent searching."
  `(let ((case-fold-search t)		; case insensitive search
	 (current-syntax-table (syntax-table))
	 result overlay-all-list overlay-intangible-list overlay
	 (restore-prog			; program to restore enviroment
	  '(progn
	     ;; restore syntax table
	     (set-syntax-table current-syntax-table)
	     ;; restore `intangible' overlays
	     (when (fboundp 'overlay-lists)
	       (while overlay-intangible-list
		 (overlay-put (car overlay-intangible-list) 'intangible t)
		 (setq overlay-intangible-list
		       (cdr overlay-intangible-list)))))))
     ;; use extended syntax table
     (set-syntax-table php-mode-ext-syntax-table)
     ;; remove `intangible' overlays
     (when (fboundp 'overlay-lists)
       (setq overlay-all-list (overlay-lists))
       (setq overlay-all-list
	     (append (car overlay-all-list) (cdr overlay-all-list)))
       (while overlay-all-list
	 (setq overlay (car overlay-all-list))
	 (when (memq 'intangible (overlay-properties overlay))
	   (setq overlay-intangible-list
		 (cons overlay overlay-intangible-list))
	   (overlay-put overlay 'intangible nil))
	 (setq overlay-all-list (cdr overlay-all-list))))
     ;; execute BODY safely
     (setq result
	   (condition-case info
	       (progn ,@body)
	     (error (eval restore-prog)	; restore environment on error
		    (error (cadr info))))) ; pass error up
     ;; restore environment
     (eval restore-prog)
     result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Words to expand

(defun php-words-init ()
  "Initialize reserved words."
  (setq php-keywords php-01-keywords)
  (setq php-types php-01-types)
  (setq php-constants php-01-constants)
  (setq php-functions php-01-functions)
  (setq php-superglobals php-01-superglobals)
  (setq php-keywords-regexp (concat "\\<\\(" (regexp-opt php-keywords) "\\)\\>"))
  (setq php-types-regexp (concat "\\<\\(" (regexp-opt php-types) "\\)\\>"))
  (setq php-constants-regexp (concat "\\<\\(" (regexp-opt php-constants) "\\)\\>"))
  (setq php-functions-regexp (concat "\\<\\(" (regexp-opt php-functions) "\\)\\>"))
  (setq php-superglobals-regexp (concat "\\<\\(" (regexp-opt php-superglobals) "\\)\\>"))
  (php-abbrev-list-init))

(defvar php-abbrev-list nil
  "Predefined abbreviations for PHP.")

(defun php-abbrev-list-init ()
  (setq php-abbrev-list
	(append
	 (list nil) php-keywords
	 (list nil) php-types
	 (list nil) php-constants
	 (list nil) php-functions
	 (list nil) php-superglobals
	 (list nil) php-others)))

(defvar php-expand-upper-case nil)

(defun php-try-expand-abbrev (old)
  "Try expanding abbreviations from `php-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
	  (let ((abbrev-list php-abbrev-list)
		(sel-abbrev-list '()))
	    (while abbrev-list
	   ;   (if (stringp (car abbrev-list))
		;  (insert (concat " " (car abbrev-list))))
	      (when (or (not (stringp (car abbrev-list)))
			(string-match
			 (concat "^" he-search-string) (car abbrev-list)))
		(setq sel-abbrev-list
		      (cons (car abbrev-list) sel-abbrev-list)))
	      (setq abbrev-list (cdr abbrev-list)))
	    (nreverse sel-abbrev-list))))
  (while (and he-expand-list
	      (or (not (stringp (car he-expand-list)))
		  (he-string-member (car he-expand-list) he-tried-table t)))
    (unless (stringp (car he-expand-list))
      (setq php-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
	     nil)
    (he-substitute-string
     (if php-expand-upper-case
	 (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

;; initialize reserved words for PHP Mode
(php-words-init)

;; function for expanding abbrevs and dabbrevs
(defun php-expand-abbrev (arg))
(fset 'php-expand-abbrev (make-hippie-expand-function
			   '(try-expand-dabbrev
			     try-expand-dabbrev-all-buffers
			     php-try-expand-abbrev)))

;; function for expanding parenthesis
(defun php-expand-paren (arg))
(fset 'php-expand-paren (make-hippie-expand-function
			  '(try-expand-list
			    try-expand-list-all-buffers)))

;; Syntactic support functions:

(defun php-in-comment-p ()
  "Check if point is in a comment."
  (let (result (here (point)) begin)
    (save-excursion
      (beginning-of-line)
      (setq begin (point))
      (goto-char here)
      (if (or (or (re-search-backward "^\\s-*\#" begin t)
		  (re-search-backward "^\\s-*\/\/" begin t))
	      (php-in-comment-p2))
	  (setq result t)
	(setq result nil)))
    result))
  ;(eq (php-in-literal) 'comment))

(defun php-in-comment-p2 ()
  "Check if point is in a comment '/* */' type"
  (let (result start-comment (here (point)))
    (save-excursion
      (setq result t)
      (setq start-comment (re-search-backward "\\/\\*" nil t))
      (if (and start-comment (re-search-forward "\\*\\/" here t))
	  (setq result nil))
      (if (not start-comment)
	  (setq result nil)))
    result))

(defun php-in-string-p ()
  "Check if point is in a string."
  (eq (php-in-literal) 'string))

(defun php-in-quote-p ()
  "Check if point is in a quote ('x')."
  (or (and (> (point) (point-min))
	   (< (1+ (point)) (point-max))
	   (= (char-before (point)) ?\')
	   (= (char-after (1+ (point))) ?\'))
      (and (> (1- (point)) (point-min))
	   (< (point) (point-max))
	   (= (char-before (1- (point))) ?\')
	   (= (char-after (point)) ?\'))))

(defun php-in-literal ()
  "Determine if point is in a PHP literal."
  (save-excursion
    (let ((here (point))
	  start state)
      (beginning-of-line)
      (setq start (point))
      (goto-char here)
      (setq state (parse-partial-sexp start (point)))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       ((php-beginning-of-macro) 'pound)
       (t nil)))))

;; Macro definitions:

(defun php-beginning-of-macro (&optional lim)
  "Go to the beginning of a cpp macro definition (nicked from `cc-engine')."
  (let ((here (point)))
    (beginning-of-line)
    (while (eq (char-before (1- (point))) ?\\)
      (forward-line -1))
    (back-to-indentation)
    (if (and (<= (point) here)
	     (eq (char-after) ?#))
	t
      (goto-char here)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Electrification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst php-template-prompt-syntax "[^ =<>][^<>@.\n]*[^ =<>]"
  "Syntax of prompt inserted by template generators.")

(defvar php-template-invoked-by-hook nil
  "Indicates whether a template has been invoked by a hook or by key or menu.
Used for undoing after template abortion.")

(defun php-minibuffer-tab (&optional prefix-arg)
  "If preceding character is part of a word or a paren then hippie-expand,
else insert tab (used for word completion in PHP minibuffer)."
  (interactive "P")
  (cond
   ;; expand word
   ((= (char-syntax (preceding-char)) ?w)
    (let ((case-fold-search (not php-word-completion-case-sensitive))
	  (case-replace nil)
	  (hippie-expand-only-buffers
	   (or (and (boundp 'hippie-expand-only-buffers)
		    hippie-expand-only-buffers)
	       '(php-mode))))
      (php-expand-abbrev prefix-arg)))
   ;; expand parenthesis
   ((or (= (preceding-char) ?\() (= (preceding-char) ?\)))
    (let ((case-fold-search (not php-word-completion-case-sensitive))
	  (case-replace nil))
      (php-expand-paren prefix-arg)))
   ;; insert tab
   (t (insert-tab))))

;; correct different behavior of function `unread-command-events' in XEmacs
(defun php-character-to-event (arg))
(defalias 'php-character-to-event
  (if (fboundp 'character-to-event) 'character-to-event 'identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar php-mode-syntax-table nil
  "Syntax table used in `php-mode' buffers.")

(defvar php-mode-ext-syntax-table nil
  "Syntax table extended by `_' used in `php-mode' buffers.")

(defun php-mode-syntax-table-init ()
  "Initialize `php-mode-syntax-table'."
  (setq php-mode-syntax-table (make-syntax-table))
  ;; define punctuation
  (modify-syntax-entry ?\# "< b"  php-mode-syntax-table)
  (modify-syntax-entry ?\$ "."    php-mode-syntax-table)
  (modify-syntax-entry ?\% "."    php-mode-syntax-table)
  (modify-syntax-entry ?\& "."    php-mode-syntax-table)
  (modify-syntax-entry ?\' "."    php-mode-syntax-table)
  (modify-syntax-entry ?\+ "."    php-mode-syntax-table)
  (modify-syntax-entry ?\. "."    php-mode-syntax-table)
  (modify-syntax-entry ?\: "."    php-mode-syntax-table)
  (modify-syntax-entry ?\; "."    php-mode-syntax-table)
  (modify-syntax-entry ?\< "."    php-mode-syntax-table)
  (modify-syntax-entry ?\= "."    php-mode-syntax-table)
  (modify-syntax-entry ?\> "."    php-mode-syntax-table)
  ;;(modify-syntax-entry ?\\ "w"    php-mode-syntax-table)
  (modify-syntax-entry ?\| "."    php-mode-syntax-table)
  ;; define string
  (modify-syntax-entry ?\" "\""   php-mode-syntax-table)
  (modify-syntax-entry ?\' "\""   php-mode-syntax-table)
  ;; define underscore
  (modify-syntax-entry ?\_ "w"   php-mode-syntax-table)
  ;; a single hyphen is punctuation, but a double hyphen starts a comment
  (modify-syntax-entry ?\/ ". 1456" php-mode-syntax-table)
  (modify-syntax-entry ?\* ". 23" php-mode-syntax-table)
  ;; and \n and \^M end a comment
  (modify-syntax-entry ?\n "> b"    php-mode-syntax-table)
  (modify-syntax-entry ?\^M "> b"   php-mode-syntax-table)
  ;; define parentheses to match
  (modify-syntax-entry ?\( "()"   php-mode-syntax-table)
  (modify-syntax-entry ?\) ")("   php-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]"   php-mode-syntax-table)
  (modify-syntax-entry ?\] ")["   php-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}"   php-mode-syntax-table)
  (modify-syntax-entry ?\} "){"   php-mode-syntax-table)
  ;; extended syntax table including '_' (for simpler search regexps)
  (setq php-mode-ext-syntax-table (copy-syntax-table php-mode-syntax-table))
  (modify-syntax-entry ?_ "w" php-mode-ext-syntax-table))

;; initialize syntax table for PHP Mode
(php-mode-syntax-table-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun php-mode-map-init ()
  "Initialize `php-mode-map'."
  (setq php-mode-map (make-sparse-keymap))
  ;; template key bindings
  ;;(define-key php-mode-map "\C-c\C-t"	   php-template-map)
  ;; mode specific key bindings
  (define-key php-mode-map "\C-c\C-m\C-e"  'php-electric-mode)
  (define-key php-mode-map "\C-c\C-m\C-s"  'php-stutter-mode)
  (define-key php-mode-map "\C-c\C-s\C-u"  'php-add-source-files-menu)
  (define-key php-mode-map "\M-\C-\\"	   'php-indent-region)
  (define-key php-mode-map "\C-c\C-i\C-b"  'php-indent-buffer)
  (define-key php-mode-map "\C-c\M-m"	   'php-show-messages)
  (define-key php-mode-map "\C-c\C-h"	   'php-doc-mode)
  (define-key php-mode-map "\C-c\C-v"	   'php-version)
  (define-key php-mode-map "\M-\t"	   'insert-tab)
  ;; electric key bindings
  (define-key php-mode-map " "		   'php-electric-space)
  (when php-intelligent-tab
    (define-key php-mode-map "\t"	   'php-electric-tab))
  (define-key php-mode-map "\r"	           'php-electric-return)
  (define-key php-mode-map "*"             'php-electric-star)
  (define-key php-mode-map "?"             'php-electric-interrogation-point)
  (define-key php-mode-map "("		   'php-electric-open-bracket)
  (define-key php-mode-map ")"		   'php-electric-close-bracket)
  (define-key php-mode-map "}"		   'php-electric-close-bracket2)
  ;(define-key php-mode-map "'"		   'php-electric-quote)
  (define-key php-mode-map ";"		   'php-electric-semicolon))
  ;(define-key php-mode-map ","		   'php-electric-comma)
  ;(define-key php-mode-map "."		   'php-electric-period))

;; initialize mode map for PHP Mode
(php-mode-map-init)

;; define special minibuffer keymap for enabling word completion in minibuffer
;; (useful in template generator prompts)
(defvar php-minibuffer-local-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (when php-word-completion-in-minibuffer
      (define-key map "\t" 'php-minibuffer-tab))
    map)
  "Keymap for minibuffer used in PHP Mode.")

;; set up electric character functions to work with
;; `delete-selection-mode' (Emacs) and `pending-delete-mode' (XEmacs)
(mapcar
 (function
  (lambda (sym)
    (put sym 'delete-selection t)	; for `delete-selection-mode' (Emacs)
    (put sym 'pending-delete t)))	; for `pending-delete-mode' (XEmacs)
 '(php-electric-space
   php-electric-tab
   php-electric-return
   php-electric-star
   php-electric-interrogation-point
   ;php-electric-dash
   php-electric-open-bracket
   php-electric-close-bracket
   php-electric-close-bracket2
   ;php-electric-quote
   php-electric-semicolon))
   ;php-electric-comma
   ;php-electric-period
   ;php-electric-equal))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation commands

(defun php-electric-tab (&optional prefix-arg)
  "If preceding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then insert tab,
else if last command was a tab or return then dedent one step or if a comment
toggle between normal indent and inline comment indent,
else indent `correctly'."
  (interactive "*P")
  (php-prepare-search-2
   (cond
    ;; expand word
    ((= (char-syntax (preceding-char)) ?w)
     (let ((case-fold-search (not php-word-completion-case-sensitive))
	   (case-replace nil)
	   (hippie-expand-only-buffers
	    (or (and (boundp 'hippie-expand-only-buffers)
		     hippie-expand-only-buffers)
		'(php-mode))))
       (php-expand-abbrev prefix-arg)))
    ;; expand parenthesis
    ((or (= (preceding-char) ?\() (= (preceding-char) ?\)))
     (let ((case-fold-search (not php-word-completion-case-sensitive))
	   (case-replace nil))
       (php-expand-paren prefix-arg)))
    ;; insert tab
    ((> (current-column) (current-indentation))
     (insert-tab))
    ;; toggle comment indent
    ((and (looking-at "//")
	  (or (eq last-command 'php-electric-tab)
	      (eq last-command 'php-electric-return)))
     (cond ((= (current-indentation) 0) ; no indent
	    (indent-to 1)
	    (indent-according-to-mode))
	   ((< (current-indentation) comment-column) ; normal indent
	    (indent-to comment-column)
	    (indent-according-to-mode))
	   (t				; inline comment indent
	    (kill-line -0))))
    ;; dedent
    ((and (>= (current-indentation) php-basic-offset)
	  (or (eq last-command 'php-electric-tab)
	      (eq last-command 'php-electric-return)))
     (backward-delete-char-untabify php-basic-offset nil))
    ;; indent line
    (t (indent-according-to-mode)))
   (setq this-command 'php-electric-tab)))

(defun php-electric-return ()
  "newline-and-indent or indent-new-comment-line if in comment and preceding
character is a space."
  (interactive)
  (if (php-in-comment-p2)
      (php-indent-new-comment-line)
    (newline-and-indent)))

(defun php-electric-space (count)
  "Expand abbreviations and self-insert space(s), do indent-new-comment-line
if in comment and past end-comment-column."
  (interactive "p")
  (cond ((php-in-comment-p)
	 (self-insert-command count)
	 (cond ((>= (current-column) (+ 2 end-comment-column))
		(backward-char 1)
		(skip-chars-backward "^ \t\n")
		(indent-new-comment-line)
		(skip-chars-forward "^ \t\n")
		(forward-char 1))
	       ((>= (current-column) end-comment-column)
		(indent-new-comment-line))
	       (t nil)))
	((or (and (>= (preceding-char) ?a) (<= (preceding-char) ?z))
	     (and (>= (preceding-char) ?A) (<= (preceding-char) ?Z)))
	 (php-prepare-search-1 (expand-abbrev))
	  ;(or (expand-abbrev) (php-fix-case-word -1)))
	 (self-insert-command count))
	(t (self-insert-command count))))

(defun php-electric-star (count)
  "/** start a comment paragraph"
  (interactive "p")
  (if (and php-stutter-mode (not (php-in-literal)))
      (if (= (preceding-char) ?\/)
	  (progn
	    (insert "*")
	    (message "Enter '*' for paragraph, 'SP' for commeting-out code")
	    (let ((next-input (read-char))
		  here)
	      (if (= next-input ?\*)
		  (progn
		    (insert "* \n")
		    (insert "* ")
		    (php-indent-line-2)
		    (end-of-line)
		    (setq here (point))
		    (insert "\n*/")
		    (php-indent-line-2)
		    (goto-char here))
		(insert " ")
		(setq here (point))
		(insert " */")
		(goto-char here))))
	(self-insert-command count))
    (self-insert-command count)))

(defun php-electric-interrogation-point (count)
  "Add '?>' and 'php' to the '<?' tag"
  (interactive "p")
  (let (here beg-pos)
    (if (and php-stutter-mode (= count 1) (not (php-in-literal)))
	(if (= (preceding-char) ?\<)
	    (progn 
	      (backward-char 1)
	      (setq beg-pos (point-marker))
	      (beginning-of-buffer)
	      (if (= (point-marker) beg-pos)
		  (progn
		    (forward-char 1)
		    (insert "?php\n")
		    (newline-and-indent)
		    (setq here (point-marker))
		    (insert "\n\n?>\n"))
		(progn
		  (goto-char beg-pos)
		  (forward-char 1)
		  (insert "?php ")
		  (setq here (point-marker))
		  (insert " ?>")))
	      (goto-char here))
	  (self-insert-command count))
      (self-insert-command count))))

(defun php-electric-semicolon (count)
  "Add a new line after a ';'"
  (interactive "p")
  (setq start (point-marker))
  (setq for-loop (re-search-backward "[;]*[ \t\n]for\s*(\s*" nil t))
  (if for-loop
      (progn
	(setq for-point (point-marker))
	(goto-char start)
	(setq for-loop (not (re-search-backward "[;]*[ \t\n]for\s*(\s*.*;.*;.*)" for-point t)))))
  (goto-char start)
  (if (and php-stutter-mode (not (php-in-comment-p)) (not (php-in-literal)) (not for-loop))
      (progn
	(insert ";")
	(newline-and-indent))
    (self-insert-command count)))

(defun php-electric-open-bracket (count) 
  "'(' --> '(', '((' --> '[', '[(' --> '{'"
  (interactive "p")
  (if (and php-stutter-mode (= count 1) (not (php-in-literal)))
      (if (= (preceding-char) ?\()
	  (progn (delete-char -1) (insert-char ?\[ 1))
	(if (= (preceding-char) ?\[)
	    (progn (delete-char -1) (insert-char ?\{ 1))
	  (insert-char ?\( 1)))
    (self-insert-command count)))

(defun php-electric-close-bracket (count) 
  "')' --> ')', '))' --> ']', '])' --> '}'"
  (interactive "p")
  (if (and php-stutter-mode (= count 1) (not (php-in-literal)))
      (progn
	(if (= (preceding-char) ?\))
	    (progn 
	      (delete-char -1) 
	      (insert-char ?\] 1))
	  (if (= (preceding-char) ?\])
	      (progn 
		(delete-char -1) 
		(insert-char ?} 1) 
		(save-excursion 
		  (php-indent-line-2)))
	    (insert-char ?\) 1)))
	(blink-matching-open))
    (self-insert-command count)))

(defun php-electric-close-bracket2 (count)
  "Automatic indent"
  (interactive "p")
  (if (and php-stutter-mode (not (php-in-literal)))
      (progn
	(insert "}")
	(save-excursion
	  (php-indent-line-2)))
    (self-insert-command count)))

(defun php-indent-new-comment-line ()
  "Add a new line, indent it and comment it"
  (interactive)
  (let ((here (point)) found)
    (setq found (re-search-backward "^\\s-*\\*" (php-point 'bol) t))
    (goto-char here)
    (newline-and-indent)
    (if found
	(insert "* "))))
  








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Enabling/disabling

(defun php-mode-line-update ()
  "Update the modeline string for PHP major mode."
  (setq mode-name (concat "PHP"
			  (and (or php-electric-mode php-stutter-mode) "/")
			  (and php-electric-mode "e")
			  (and php-stutter-mode "s")))
  (force-mode-line-update t))

(defun php-electric-mode (arg)
  "Toggle PHP electric mode.
Turn on if ARG positive, turn off if ARG negative, toggle if ARG zero or nil."
  (interactive "P")
  (setq php-electric-mode
	(cond ((or (not arg) (zerop arg)) (not php-electric-mode))
	      ((> arg 0) t) (t nil)))
  (php-mode-line-update))

(defun php-stutter-mode (arg)
  "Toggle PHP stuttering mode.
Turn on if ARG positive, turn off if ARG negative, toggle if ARG zero or nil."
  (interactive "P")
  (setq php-stutter-mode
	(cond ((or (not arg) (zerop arg)) (not php-stutter-mode))
	      ((> arg 0) t) (t nil)))
  (php-mode-line-update))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fontification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up font locking
(defconst php-font-lock-keywords-1
  (list
   ;; Fontify constants
   (cons
    (concat "\\<\\(" php-constants-regexp "\\)\\>")
    'font-lock-constant-face)

   ;; Fontify keywords
   (cons
    (concat "\\<\\(" php-keywords-regexp "\\)\\>")
    'font-lock-keyword-face)

   (cons
    (concat "\\<\\(" php-functions-regexp "\\)\\>")
    'font-lock-keyword-face)

   ;; Fontify keywords and targets, and case default tags.
   (list "\\<\\(break\\|case\\|continue\\)\\>[ \t]*\\(-?\\(?:\\sw\\|\\s_\\)+\\)?"
	 '(1 font-lock-keyword-face) '(2 font-lock-constant-face t t))
   ;; This must come after the one for keywords and targets.
   '(":" ("^[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)[ \t]*:[ \t]*$"
	  (beginning-of-line) (end-of-line)
	  (1 font-lock-constant-face)))

   ;; treat 'print' as keyword only when not used like a function name
   '("\\<print\\s-*(" . default)
   '("\\<print\\>" . font-lock-keyword-face)

   ;; Fontify PHP tag
   '("<\\?\\(php\\)?" . font-lock-constant-face)
   '("\\?>" . font-lock-constant-face)

   ;; Fontify ASP-style tag
   '("<\\%\\(=\\)?" . font-lock-constant-face)
   '("\\%>" . font-lock-constant-face)

   ;;'("\/\/\\([A-z ;:.,?!-יאט']*\\)$" . font-lock-comment-face)
   ;;'("\/\/.*" . font-lock-comment-face)

   )
  "Subdued level highlighting for PHP mode.")

(defconst php-font-lock-keywords-2
  (append
   php-font-lock-keywords-1
   (list

    ;; class declaration
    '("\\<\\(class\\|interface\\)[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
    ;; handle several words specially, to include following word,
    ;; thereby excluding it from unknown-symbol checks later
    ;; FIX to handle implementing multiple
    ;; currently breaks on "class Foo implements Bar, Baz"
    '("\\<\\(new\\|extends\\|implements\\)\\s-+\\$?\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; function declaration
    '("\\<\\(function\\)\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))

    ;; class hierarchy
    '("\\(self\\|parent\\)\\W" (1 font-lock-constant-face nil nil))

    ;; method and variable features
    '("\\<\\(private\\|protected\\|public\\)\\s-+\\$?\\(?:\\sw\\|\\s_\\)+"
      (1 font-lock-keyword-face))

    ;; method features
    '("^[ \t]*\\(abstract\\|static\\|final\\)\\s-+\\$?\\(?:\\sw\\|\\s_\\)+"
      (1 font-lock-keyword-face))

    ;; variable features
    '("^[ \t]*\\(static\\|const\\)\\s-+\\$?\\(?:\\sw\\|\\s_\\)+"
      (1 font-lock-keyword-face))
    ))
  "Medium level highlighting for PHP mode.")

(defconst php-font-lock-keywords-3
  (append
   php-font-lock-keywords-2
   (list

    ;; <word> or </word> for HTML
    '("</?\\sw+[^>]*>" . font-lock-constant-face)

    ;; HTML entities
    '("&\\w+;" . font-lock-variable-name-face)

    ;; warn about '$' immediately after ->
    '("\\$\\(?:\\sw\\|\\s_\\)+->\\s-*\\(\\$\\)\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-warning-face) (2 default))

    ;; warn about $word.word -- it could be a valid concatenation,
    ;; but without any spaces we'll assume $word->word was meant.
    '("\\$\\(?:\\sw\\|\\s_\\)+\\(\\.\\)\\sw"
      1 font-lock-warning-face)

    ;; Warn about ==> instead of =>
    '("==+>" . font-lock-warning-face)

    ;; exclude casts from bare-word treatment (may contain spaces)
    `(,(concat "(\\s-*\\(" php-types-regexp "\\)\\s-*)")
      1 font-lock-type-face)

    ;; PHP5: function declarations may contain classes as parameters type
    `(,(concat "[(,]\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-+\\$\\(?:\\sw\\|\\s_\\)+\\>")
      1 font-lock-type-face)

    ;; Fontify variables and function calls
    '("\\$\\(this\\|that\\)\\W" (1 font-lock-constant-face nil nil))
    `(,(concat "\\$\\(" php-superglobals-regexp "\\)\\W")
      (1 font-lock-constant-face nil nil)) ; $_GET & co
    '("\\$\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face)) ; $variable
    '("->\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face t t)) ; ->variable
    '("->\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" . (1 default t t)) ; ->function_call
    '("\\(?:\\sw\\|\\s_\\)+::\\(?:\\sw\\|\\s_\\)+\\s-*(" . default) ; class::method call
    '("\\<\\(?:\\sw\\|\\s_\\)+\\s-*[[(]" . default)	; word( or word[
    '("\\<[0-9]+" . default)		; number (also matches word)

    ;; Warn on any words not already fontified
    '("\\<\\(?:\\sw\\|\\s_\\)+\\>" . font-lock-warning-face)
    ))
  "Gauchy level highlighting for PHP mode.")
  
(defconst php-font-lock-syntactic-keywords
  ;;(if xemacsp nil
    ;; Mark shell-style comments.  font-lock handles this in a
    ;; separate pass from normal syntactic scanning (somehow), so we
    ;; get a chance to mark these in addition to C and C++ style
    ;; comments.  This only works in GNU Emacs, not XEmacs 21 which
    ;; seems to ignore this same code if we try to use it.
    (list
     ;; Mark _all_ # chars as being comment-start.  That will be
     ;; ignored when inside a quoted string.
     '("\\(\#\\)" (1 (11 . nil)))
     ;;'("\\(//\\)" (1 (11 . nil)))
     ;; Mark all newlines ending a line with # as being comment-end.
     ;; This causes a problem, premature end-of-comment, when '#'
     ;; appears inside a multiline C-style comment.  Oh well.
     '("#.*\\([\n]\\)" (1 (12 . nil)))
     ;;'("//.*\\([\n]\\)" (1 (12 . nil)))
     ));;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev ook bindings

(defvar php-mode-abbrev-table nil
  "Abbrev table to use in `php-mode' buffers.")

(defun php-mode-abbrev-table-init ()
  "Initialize `php-mode-abbrev-table'."
  (when php-mode-abbrev-table (clear-abbrev-table php-mode-abbrev-table))
  (define-abbrev-table 'php-mode-abbrev-table
    (append
     '(
       ;; .NET
       ("dotnet_load" "" php-template-dotnet-load-hook 0)
       ;; Apache
       ("apache_child_terminate" "" php-template-apache-child-terminate-hook 0)
       ("apache_get_modules" "" php-template-apache-get-modules-hook 0)
       ("apache_get_version" "" php-template-apache-get-version-hook 0)
       ("apache_getenv" "" php-template-apache-getenv-hook 0)
       ("apache_lookup_uri" "" php-template-apache-lookup-uri-hook 0)
       ("apache_note" "" php-template-apache-note-hook 0)
       ("apache_request_headers" "" php-template-apache-request-headers-hook 0)
       ("apache_reset_timeout" "" php-template-apache-reset-timeout-hook 0)
       ("apache_response_headers" "" php-template-apache-response-headers-hook 0)
       ("apache_setenv" "" php-template-apache-setenv-hook 0)
       ("ascii2ebcdic" "" php-template-ascii2ebcdic-hook 0)
       ("ebcdic2ascii" "" php-template-ebcdic2ascii-hook 0)
       ("getallheaders" "" php-template-getallheaders-hook 0)
       ("virtual" "" php-template-virtual-hook 0)
       ;; APC
       ("apc_cache_info" "" php-template-apc-cache-info-hook 0)
       ("apc_clear_cache" "" php-template-apc-clear-cache-hook 0)
       ("apc_define_constants" "" php-template-apc-define-constants-hook 0)
       ("apc_delete" "" php-template-apc-delete-hook 0)
       ("apc_fetch" "" php-template-apc-fetch-hook 0)
       ("apc_load_constants" "" php-template-apc-load-constants-hook 0)
       ("apc_sma_info" "" php-template-apc-sma-info-hook 0)
       ("apc_store" "" php-template-apc-store-hook 0)
       ;; Array
       ("array_change_key_case" "" php-template-array-change-key-case-hook 0)
       ("array_chunk" "" php-template-array-chunk-hook 0)
       ("array_combine" "" php-template-array-combine-hook 0)
       ("array_count_values" "" php-template-array-count-values-hook 0)
       ("array_diff_assoc" "" php-template-array-diff-assoc-hook 0)
       ("array_diff_key" "" php-template-array-diff-key-hook 0)
       ("array_diff_uassoc" "" php-template-array-diff-uassoc-hook 0)
       ("array_diff_ukey" "" php-template-array-diff-ukey-hook 0)
       ("array_diff" "" php-template-array-diff-hook 0)
       ("array_fill" "" php-template-array-fill-hook 0)
       ("array_filter" "" php-template-array-filter-hook 0)
       ("array_flip" "" php-template-array-flip-hook 0)
       ("array_intersect_assoc" "" php-template-array-intersect-assoc-hook 0)
       ("array_intersect_key" "" php-template-array-intersect-key-hook 0)
       ("array_intersect_uassoc" "" php-template-array-intersect-uassoc-hook 0)
       ("array_intersect_ukey" "" php-template-array-intersect-ukey-hook 0)
       ("array_intersect" "" php-template-array-intersect-hook 0)
       ("array_key_exists" "" php-template-array-exists-hook 0)
       ("array_keys" "" php-template-array-keys-hook 0)
       ("array_map" "" php-template-array-map-hook 0)
       ("array_merge_recursive" "" php-template-array-merge-recursive-hook 0)
       ("array_merge" "" php-template-array-merge-hook 0)
       ("array_multisort" "" php-template-array-multisort-hook 0)
       ("array_pad" "" php-template-array-pad-hook 0)
       ("array_pop" "" php-template-array-pop-hook 0)
       ("array_product" "" php-template-array-product-hook 0)
       ("array_push" "" php-template-array-push-hook 0)
       ("array_rand" "" php-template-array-rand-hook 0)
       ("array_reduce" "" php-template-array-reduce-hook 0)
       ("array_reverse" "" php-template-array-reverse-hook 0)
       ("array_search" "" php-template-array-search-hook 0)
       ("array_shift" "" php-template-array-shift-hook 0)
       ("array_slice" "" php-template-array-slice-hook 0)
       ("array_splice" "" php-template-array-splice-hook 0)
       ("array_sum" "" php-template-array-sum-hook 0)
       ("array_udiff_assoc" "" php-template-array-udiff-assoc-hook 0)
       ("array_udiff_uassoc" "" php-template-array-udiff-uassoc-hook 0)
       ("array_udiff" "" php-template-array-udiff-hook 0)
       ("array_uintersect_assoc" "" php-template-array-uintersect-assoc-hook 0)
       ("array_uintersect_uassoc" "" php-template-array-uintersect-uassoc-hook 0)
       ("array_uintersect" "" php-template-array-uintersect-hook 0)
       ("array_unique" "" php-template-array-unique-hook 0)
       ("array_unshift" "" php-template-array-unshift-hook 0)
       ("array_values" "" php-template-array-values-hook 0)
       ("array_walk_recursive" "" php-template-array-walk-recursive-hook 0)
       ("array_walk" "" php-template-array-walk-hook 0)
       ("array" "" php-template-array-hook 0)
       ("arsort" "" php-template-arsort-hook 0)
       ("asort" "" php-template-asort-hook 0)
       ("compact" "" php-template-compact-hook 0)
       ("count" "" php-template-count-hook 0)
       ("current" "" php-template-current-hook 0)
       ("each" "" php-template-each-hook 0)
       ("end" "" php-template-end-hook 0)
       ("extract" "" php-template-extract-hook 0)
       ("in_array" "" php-template-in-array-hook 0)
       ("key" "" php-template-key-hook 0)
       ("krsort" "" php-template-krsort-hook 0)
       ("ksort" "" php-template-ksort-hook 0)
       ("list" "" php-template-list-hook 0)
       ("natcasesort" "" php-template-natcasesort-hook 0)
       ("natsort" "" php-template-natsort-hook 0)
       ("next" "" php-template-next-hook 0)
       ("pos" "" php-template-pos-hook 0)
       ("prev" "" php-template-prev-hook 0)
       ("range" "" php-template-range-hook 0)
       ("reset" "" php-template-reset-hook 0)
       ("rsort" "" php-template-rsort-hook 0)
       ("shuffle" "" php-template-shuffle-hook 0)
       ("sizeof" "" php-template-sizeof-hook 0)
       ("sort" "" php-template-sort-hook 0)
       ("uasort" "" php-template-uasort-hook 0)
       ("uksort" "" php-template-uksort-hook 0)
       ("usort" "" php-template-usort-hook 0)
       ("break" "" php-template-break-hook 0)
       ("continue" "" php-template-continue-hook 0)
       ("do" "" php-template-do-while-hook 0)
       ("else" "" php-template-else-hook 0)
       ("elseif" "" php-template-elseif-hook 0)
       ("for" "" php-template-for-hook 0)
       ("foreach" "" php-template-foreach-hook 0)
       ("if" "" php-template-if-hook 0)
       ("include" "" php-template-include-hook 0)
       ("include_once" "" php-template-include-once-hook 0)
       ("require" "" php-template-require-hook 0)
       ("require_once" "" php-template-require-once-hook 0)
       ("return" "" php-template-return-hook 0)
       ("switch" "" php-template-switch-hook 0)
       ("while" "" php-template-while-hook 0)
       ("checkdate" "" php-template-checkdate-hook 0)
       ("date_default_timezone_get" "" php-template-date-default-timezone-get-hook 0)
       ("date_default_timezone_set" "" php-template-date-default-timezone-set-hook 0)
       ("date_sunrise" "" php-template-date-sunrise-hook 0)
       ("date_sunset" "" php-template-date-sunset-hook 0)
       ("date" "" php-template-date-hook 0)
       ("getdate" "" php-template-getdate-hook 0)
       ("gettimeofday" "" php-template-gettimeofday-hook 0)
       ("gmdate" "" php-template-gmdate-hook 0)
       ("gmmktime" "" php-template-gmmktime-hook 0)
       ("gmstrftime" "" php-template-gmstrftime-hook 0)
       ("idate" "" php-template-idate-hook 0)
       ("localtime" "" php-template-localtime-hook 0)
       ("microtime" "" php-template-microtime-hook 0)
       ("mktime" "" php-template-mktime-hook 0)
       ("strftime" "" php-template-strftime-hook 0)
       ("strptime" "" php-template-strptime-hook 0)
       ("strtotime" "" php-template-strtotime-hook 0)
       ("time" "" php-template-time-hook 0)
       ("chdir" "" php-template-chdir-hook 0)
       ("chroot" "" php-template-chroot 0)
       ("dir" "" php-template-dir-hook 0)
       ("closedir" "" php-template-closedir-hook 0)
       ("getcwd" "" php-template-getcwd-hook 0)
       ("opendir" "" php-template-opendir-hook 0)
       ("readdir" "" php-template-readdir-hook 0)
       ("rewinddir" "" php-template-rewinddir-hook 0)
       ("scandir" "" php-template-scandir-hook 0)
       ;; Error and Logging
       ("debug_backtrace" "" php-template-debug-backtrace-hook 0)
       ("debug_print_backtrace" "" php-template-debug-print-backtrace-hook 0)
       ("error_log" "" php-template-error-log-hook 0)
       ("error_reporting" "" php-template-error-reporting-hook 0)
       ("restore_error_handler" "" php-template-restore-error-handler-hook 0)
       ("restore_exception_handler" "" php-template-restore-exception-handler-hook 0)
       ("set_error_handler" "" php-template-set-error-handler-hook 0)
       ("set_exception_handler" "" php-template-set-exception-handler-hook 0)
       ("trigger_error" "" php-template-trigger-error-hook 0)
       ("user_error" "" php-template-user-error-hook 0)
       ;; File System
       ("basename" "" php-template-basename-hook 0)
       ("chgrp" "" php-template-chgrp-hook 0)
       ("chmod" "" php-template-chmod-hook 0)
       ("chown" "" php-template-chown-hook 0)
       ("clearstatcache" "" php-template-clearstatcache-hook 0)
       ("copy" "" php-template-copy-hook 0)
       ("delete" "" php-template-delete-hook 0)
       ("dirname" "" php-template-dirname-hook 0)
       ("disk_free_space" "" php-template-disk-free-space-hook 0)
       ("disk_total_space" "" php-template-disk-total-space-hook 0)
       ("diskfreespace" "" php-template-disk-free-space-hook 0)
       ("fclose" "" php-template-fclose-hook 0)
       ("feof" "" php-template-feof-hook 0)
       ("fflush" "" php-template-fflush-hook 0)
       ("fgetc" "" php-template-fgetc-hook 0)
       ("fgetcsv" "" php-template-fgetcsv-hook 0)
       ("fgets" "" php-template-fgets-hook 0)
       ("fgetss" "" php-template-fgetss-hook 0)
       ("file_exists" "" php-template-file-exists-hook 0)
       ("file_get_contents" "" php-template-file-get-contents-hook 0)
       ("file_put_contents" "" php-template-file-put-contents-hook 0)
       ("file" "" php-template-file-hook 0)
       ("fileatime" "" php-template-fileatime-hook 0)
       ("filectime" "" php-template-filectime-hook 0)
       ("filegroup" "" php-template-filegroup-hook 0)
       ("fileinode" "" php-template-fileinode-hook 0)
       ("filemtime" "" php-template-filemtime-hook 0)
       ("fileowner" "" php-template-fileowner-hook 0)
       ("fileperms" "" php-template-fileperms-hook 0)
       ("filesize" "" php-template-filesize-hook 0)
       ("filetype" "" php-template-filetype-hook 0)
       ("flock" "" php-template-flock-hook 0)
       ("fnmatch" "" php-template-fnmatch-hook 0)
       ("fopen" "" php-template-fopen-hook 0)
       ("fpassthru" "" php-template-fpassthru-hook 0)
       ("fputcsv" "" php-template-fputcsv-hook 0)
       ("fput" "" php-template-fwrite-hook 0)
       ("fread" "" php-template-fread-hook 0)
       ;("fscanf" "" php-template-fscanf-hook 0)
       ("fseek" "" php-template-fseek-hook 0)
       ("fstat" "" php-template-fstat-hook 0)
       ("ftell" "" php-template-ftell-hook 0)
       ("ftruncate" "" php-template-ftruncate-hook 0)
       ("fwrite" "" php-template-fwrite-hook 0)
       ("glob" "" php-template-glob-hook 0)
       ("is_dir" "" php-template-is-dir-hook 0)
       ("is_executable" "" php-template-is-executable-hook 0)
       ("is_file" "" php-template-is-file-hook 0)
       ("is_link" "" php-template-is-link-hook 0)
       ("is_readable" "" php-template-is-readable-hook 0)
       ("is_uploaded_file" "" php-template-is-uploaded-file-hook 0)
       ("is_writable" "" php-template-is-writable-hook 0)
       ("is_writeable" "" php-template-is-writable-hook 0)
       ("link" "" php-template-link-hook 0)
       ("linkinfo" "" php-template-linkinfo-hook 0)
       ("lstat" "" php-template-lstat-hook 0)
       ("mkdir" "" php-template-mkdir-hook 0)
       ("move_uploaded_file" "" php-template-move-uploaded-file-hook 0)
       ("parse_ini_file" "" php-template-parse-ini-file-hook 0)
       ("pathinfo" "" php-template-pathinfo-hook 0)
       ("pclose" "" php-template-pclose-hook 0)
       ("popen" "" php-template-popen-hook 0)
       ("readfile" "" php-template-readfile-hook 0)
       ("readlink" "" php-template-readlink-hook 0)
       ("realpath" "" php-template-realpath-hook 0)
       ("rename" "" php-template-rename-hook 0)
       ("rewind" "" php-template-rewind-hook 0)
       ("rmdir" "" php-template-rmdir-hook 0)
       ;("set_file_buffer" "" php-template-set-file-buffer-hook 0)
       ("stat" "" php-template-stat-hook 0)
       ("symlink" "" php-template-symlink-hook 0)
       ("tempnam" "" php-template-tempnam-hook 0)
       ("tmpfile" "" php-template-tmpfile-hook 0)
       ("touch" "" php-template-touch-hook 0)
       ("umask" "" php-template-umask-hook 0)
       ("unlink" "" php-template-unlink-hook 0)
       ;; Functions
       ("call_user_func_array" "" php-template-call-user-func-array-hook 0)
       ("call_user_func" "" php-template-call-user-func-hook 0)
       ("create_function" "" php-template-create-function-hook 0)
       ("func_get_arg" "" php-template-func-get-arg-hook 0)
       ("func_get_args" "" php-template-func-get-args-hook 0)
       ("func_num_args" "" php-template-func-num-args-hook 0)
       ("function_exists" "" php-template-function-exists-hook 0)
       ("get_defined_functions" "" php-template-get-defined-functions-hook 0)
       ("register_shutdown_function" "" php-template-register-shutdown-function-hook 0)
       ("register_tick_function" "" php-template-register-tick-function-hook 0)
       ("unregister_tick_function" "" php-template-unregister-tick-function-hook 0)
       ;; Image
       ("gd_info" "" php-template-gd-info-hook 0)
       ("getimagesize" "" php-template-getimagesize-hook 0)
       ("image_type_to_extension" "" php-template-image-type-to-extension-hook 0)
       ("image_type_to_mime_type" "" php-template-image-type-to-mime-type-hook 0)
       ("image2wbmp" "" php-template-image2wbmp-hook 0)
       ("imagealphablending" "" php-template-imagealphablending-hook 0)
       ("imageantialias" "" php-template-imageantialias-hook 0)
       ("imagearc" "" php-template-imagearc-hook 0)
       ("imagechar" "" php-template-imagechar-hook 0)
       ("imagecharup" "" php-template-imagecharup-hook 0)
       ("imagecolorallocate" "" php-template-imagecolorallocate-hook 0)
       ("imagecolorallocatealpha" "" php-template-imagecolorallocatealpha-hook 0)
       ("imagecolorat" "" php-template-imagecolorat-hook 0)
       ("imagecolorclosest" "" php-template-imagecolorclosest-hook 0)
       ("imagecolorclosestalpha" "" php-template-imagecolorclosestalpha-hook 0)
       ("imagecolorclosesthwb" "" php-template-imagecolorclosesthwb-hook 0)
       ("imagecolordeallocate" "" php-template-imagecolordeallocate-hook 0)
       ("imagecolorexact" "" php-template-imagecolorexact-hook 0)
       ("imagecolorexactalpha" "" php-template-imagecolorexactalpha-hook 0)
       ("imagecolormatch" "" php-template-imagecolormatch-hook 0)
       ("imagecolorresolve" "" php-template-imagecolorresolve-hook 0)
       ("imagecolorresolvealpha" "" php-template-imagecolorresolvealpha-hook 0)
       ("imagecolorset" "" php-template-imagecolorset-hook 0)
       ("imagecolorsforindex" "" php-template-imagecolorsforindex-hook 0)
       ("imagecolorstotal" "" php-template-imagecolorstotal-hook 0)
       ("imagecolortransparent" "" php-template-imagecolortransparent-hook 0)
       ("imageconvolution" "" php-template-imageconvolution-hook 0)
       ("imagecopy" "" php-template-imagecopy-hook 0)
       ("imagecopymerge" "" php-template-imagecopymerge-hook 0)
       ("imagecopymergegray" "" php-template-imagecopymergegray-hook 0)
       ("imagecopyresampled" "" php-template-imagecopyresampled-hook 0)
       ("imagecopyresized" "" php-template-imagecopyresized-hook 0)
       ("imagecreate" "" php-template-imagecreate-hook 0)
       ("imagecreatefromgd2" "" php-template-imagecreatefromgd2-hook 0)
       ("imagecreatefromgd2part" "" php-template-imagecreatefromgd2part-hook 0)
       ("imagecreatefromgd" "" php-template-imagecreatefromgd-hook 0)
       ("imagecreatefromgif" "" php-template-imagecreatefromgif-hook 0)
       ("imagecreatefromjpeg" "" php-template-imagecreatefromjpeg-hook 0)
       ("imagecreatefrompng" "" php-template-imagecreatefrompng-hook 0)
       ("imagecreatefromstring" "" php-template-imagecreatefromstring-hook 0)
       ("imagecreatefromwbmp" "" php-template-imagecreatefromwbmp-hook 0)
       ("imagecreatefromxbm" "" php-template-imagecreatefromxbm-hook 0)
       ("imagecreatefromxpm" "" php-template-imagecreatefromxpm-hook 0)
       ("imagecreatetruecolor" "" php-template-imagecreatetruecolor-hook 0)
       ("imagedashedline" "" php-template-imagedashedline-hook 0)
       ("imagedestroy" "" php-template-imagedestroy-hook 0)
       ("imageellipse" "" php-template-imageellipse-hook 0)
       ("imagefill" "" php-template-imagefill-hook 0)
       ("imagefilledarc" "" php-template-imagefilledarc-hook 0)
       ("imagefilledellipse" "" php-template-imagefilledellipse-hook 0)
       ("imagefilledpolygon" "" php-template-imagefilledpolygon-hook 0)
       ("imagefilledrectangle" "" php-template-imagefilledrectangle-hook 0)
       ("imagefilltoborder" "" php-template-imagefilltoborder-hook 0)
       ("imagefilter" "" php-template-imagefilter-hook 0)
       ("imagefontheight" "" php-template-imagefontheight-hook 0)
       ("imagefontwidth" "" php-template-imagefontwidth-hook 0)
       ("imageftbbox" "" php-template-imageftbbox-hook 0)
       ("imagefttext" "" php-template-imagefttext-hook 0)
       ("imagegammacorrect" "" php-template-imagegammacorrect-hook 0)
       ("imagegd2" "" php-template-imagegd2-hook 0)
       ("imagegd" "" php-template-imagegd-hook 0)
       ("imagegif" "" php-template-imagegif-hook 0)
       ("imageinterlace" "" php-template-imageinterlace-hook 0)
       ("imageistruecolor" "" php-template-imageistruecolor-hook 0)
       ("imagejpeg" "" php-template-imagejpeg-hook 0)
       ("imagelayereffect" "" php-template-imagelayereffect-hook 0)
       ("imageline" "" php-template-imageline-hook 0)
       ("imageloadfont" "" php-template-imageloadfont-hook 0)
       ("imagepalettecopy" "" php-template-imagepalettecopy-hook 0)
       ("imagepng" "" php-template-imagepng-hook 0)
       ("imagepolygon" "" php-template-imagepolygon-hook 0)
       ("imagepsbbox" "" php-template-imagepsbbox-hook 0)
       ("imagepsencodefont" "" php-template-imagepsencodefont-hook 0)
       ("imagepsextendfont" "" php-template-imagepsextendfont-hook 0)
       ("imagepsfreefont" "" php-template-imagepsfreefont-hook 0)
       ("imagepsloadfont" "" php-template-imagepsloadfont-hook 0)
       ("imagepsslantfont" "" php-template-imagepsslantfont-hook 0)
       ("imagepstext" "" php-template-imagepstext-hook 0)
       ("imagerectangle" "" php-template-imagerectangle-hook 0)
       ("imagerotate" "" php-template-imagerotate-hook 0)
       ("imagesavealpha" "" php-template-imagesavealpha-hook 0)
       ("imagesetbrush" "" php-template-imagesetbrush-hook 0)
       ("imagesetpixel" "" php-template-imagesetpixel-hook 0)
       ("imagesetstyle" "" php-template-imagesetstyle-hook 0)
       ("imagesetthickness" "" php-template-imagesetthickness-hook 0)
       ("imagesettile" "" php-template-imagesettile-hook 0)
       ("imagestring" "" php-template-imagestring-hook 0)
       ("imagestringup" "" php-template-imagestringup-hook 0)
       ("imagesx" "" php-template-imagesx-hook 0)
       ("imagesy" "" php-template-imagesy-hook 0)
       ("imagetruecolortopalette" "" php-template-imagetruecolortopalette-hook 0)
       ("imagettfbbox" "" php-template-imagettfbbox-hook 0)
       ("imagettftext" "" php-template-imagettftext-hook 0)
       ("imagetypes" "" php-template-imagetypes-hook 0)
       ("imagewbmp" "" php-template-imagewbmp-hook 0)
       ("imagexbm" "" php-template-imagexbm-hook 0)
       ("iptcembed" "" php-template-iptcembed-hook 0)
       ("iptcparse" "" php-template-iptcparse-hook 0)
       ("jpeg2wbmp" "" php-template-jpeg2wbmp-hook 0)
       ("png2wbmp" "" php-template-png2wbmp-hook 0)
       ;; Mail
       ("ezmlm_hash" "" php-template-ezmlm-hash-hook 0)
       ("mail" "" php-template-mail-hook 0)
       ;; Mathematical
       ("abs" "" php-template-abs-hook 0)
       ("acos" "" php-template-acos-hook 0)
       ("acosh" "" php-template-acosh-hook 0)
       ("asin" "" php-template-asin-hook 0)
       ("asinh" "" php-template-asinh-hook 0)
       ("atan2" "" php-template-atan2-hook 0)
       ("atan" "" php-template-atan-hook 0)
       ("atanh" "" php-template-atanh-hook 0)
       ("base_convert" "" php-template-base-convert-hook 0)
       ("bindec" "" php-template-bindec-hook 0)
       ("ceil" "" php-template-ceil-hook 0)
       ("cos" "" php-template-cos-hook 0)
       ("cosh" "" php-template-cosh-hook 0)
       ("decbin" "" php-template-decbin-hook 0)
       ("dechex" "" php-template-dechex-hook 0)
       ("decoct" "" php-template-decoct-hook 0)
       ("deg2rad" "" php-template-deg2rad-hook 0)
       ("exp" "" php-template-exp-hook 0)
       ("expm1" "" php-template-expm1-hook 0)
       ("floor" "" php-template-floor-hook 0)
       ("fmod" "" php-template-fmod-hook 0)
       ("getrandmax" "" php-template-getrandmax-hook 0)
       ("hexdec" "" php-template-hexdec-hook 0)
       ("hypot" "" php-template-hypot-hook 0)
       ("is_finite" "" php-template-is-finite-hook 0)
       ("is_infinite" "" php-template-is-infinite-hook 0)
       ("is_nan" "" php-template-is-nan-hook 0)
       ("lcg_value" "" php-template-lcg-value-hook 0)
       ("log10" "" php-template-log10-hook 0)
       ("log1p" "" php-template-log1p-hook 0)
       ("log" "" php-template-log-hook 0)
       ("max" "" php-template-max-hook 0)
       ("min" "" php-template-min-hook 0)
       ("mt_getrandmax" "" php-template-mt-getrandmax-hook 0)
       ("mt_rand" "" php-template-mt-rand-hook 0)
       ("mt_srand" "" php-template-mt-srand-hook 0)
       ("octdec" "" php-template-octdec-hook 0)
       ("pi" "" php-template-pi-hook 0)
       ("pow" "" php-template-pow-hook 0)
       ("rad2deg" "" php-template-rad2deg-hook 0)
       ("rand" "" php-template-rand-hook 0)
       ("round" "" php-template-round-hook 0)
       ("sin" "" php-template-sin-hook 0)
       ("sinh" "" php-template-sinh-hook 0)
       ("sqrt" "" php-template-sqrt-hook 0)
       ("srand" "" php-template-srand-hook 0)
       ("tan" "" php-template-tan-hook 0)
       ("tanh" "" php-template-tanh-hook 0)
       ;; Miscellaneous
       ("connection_aborted" "" php-template-connection-aborted-hook 0)
       ("connection_status" "" php-template-connection-status-hook 0)
       ("connection_timeout" "" php-template-connection-timeout-hook 0)
       ("constant" "" php-template-constant-hook 0)
       ("define" "" php-template-define-hook 0)
       ("defined" "" php-template-defined-hook 0)
       ("die" "" php-template-die-hook 0)
       ("eval" "" php-template-eval-hook 0)
       ("exit" "" php-template-exit-hook 0)
       ("get_browser" "" php-template-get-browser-hook 0)
       ("__halt_compiler" "" php-template-halt-compiler-hook 0)
       ("highlight_file" "" php-template-highlight-file-hook 0)
       ("highlight_string" "" php-template-highlight-string-hook 0)
       ("ignore_user_abort" "" php-template-ignore-user-abort-hook 0)
       ("pack" "" php-template-pack-hook 0)
       ("php_check_syntax" "" php-template-php-check-syntax-hook 0)
       ("php_strip_whitespace" "" php-template-php-strip-whitespace-hook 0)
       ("show_source" "" php-template-show-source-hook 0)
       ("sleep" "" php-template-sleep-hook 0)
       ("sys_getloadavg" "" php-template-sys-getloadavg-hook 0)
       ("time_nanosleep" "" php-template-time-nanosleep-hook 0)
       ("time_sleep_until" "" php-template-time-sleep-until-hook 0)
       ("uniqid" "" php-template-uniqid-hook 0)
       ("unpack" "" php-template-unpack-hook 0)
       ("usleep" "" php-template-usleep-hook 0)
       ;; MySQL
       ("mysql_affected_rows" "" php-template-mysql-affected-rows-hook 0)
       ("mysql_change_user" "" php-template-mysql-change-user-hook 0)
       ("mysql_client_encoding" "" php-template-mysql-client-encoding-hook 0)
       ("mysql_close" "" php-template-mysql-close-hook 0)
       ("mysql_connect" "" php-template-mysql-connect-hook 0)
       ("mysql_create_db" "" php-template-mysql-create-db-hook 0)
       ("mysql_data_seek" "" php-template-mysql-data-seek-hook 0)
       ("mysql_db_name" "" php-template-mysql-db-name-hook 0)
       ("mysql_db_query" "" php-template-mysql-db-query-hook 0)
       ("mysql_drop_db" "" php-template-mysql-drop-db-hook 0)
       ("mysql_errno" "" php-template-mysql-errno-hook 0)
       ("mysql_error" "" php-template-mysql-error-hook 0)
       ("mysql_escape_string" "" php-template-mysql-escape-string-hook 0)
       ("mysql_fetch_array" "" php-template-mysql-fetch-array-hook 0)
       ("mysql_fetch_assoc" "" php-template-mysql-fetch-assoc-hook 0)
       ("mysql_fetch_field" "" php-template-mysql-fetch-field-hook 0)
       ("mysql_fetch_lengths" "" php-template-mysql-fetch-lengths-hook 0)
       ("mysql_fetch_object" "" php-template-mysql-fetch-object-hook 0)
       ("mysql_fetch_row" "" php-template-mysql-fetch-row-hook 0)
       ("mysql_field_flags" "" php-template-mysql-field-flags-hook 0)
       ("mysql_field_len" "" php-template-mysql-field-len-hook 0)
       ("mysql_field_name" "" php-template-mysql-field-name-hook 0)
       ("mysql_field_seek" "" php-template-mysql-field-seek-hook 0)
       ("mysql_field_table" "" php-template-mysql-field-table-hook 0)
       ("mysql_field_type" "" php-template-mysql-field-type-hook 0)
       ("mysql_free_result" "" php-template-mysql-free-result-hook 0)
       ("mysql_get_client_info" "" php-template-mysql-get-client-info-hook 0)
       ("mysql_get_host_info" "" php-template-mysql-get-host-info-hook 0)
       ("mysql_get_proto_info" "" php-template-mysql-get-proto-info-hook 0)
       ("mysql_get_server_info" "" php-template-mysql-get-server-info-hook 0)
       ("mysql_info" "" php-template-mysql-info-hook 0)
       ("mysql_insert_id" "" php-template-mysql-insert-id-hook 0)
       ("mysql_list_dbs" "" php-template-mysql-list-dbs-hook 0)
       ("mysql_list_fields" "" php-template-mysql-list-fields-hook 0)
       ("mysql_list_processes" "" php-template-mysql-list-processes-hook 0)
       ("mysql_list_tables" "" php-template-mysql-list-tables-hook 0)
       ("mysql_num_fields" "" php-template-mysql-num-fields-hook 0)
       ("mysql_num_rows" "" php-template-mysql-num-rows-hook 0)
       ("mysql_pconnect" "" php-template-mysql-pconnect-hook 0)
       ("mysql_ping" "" php-template-mysql-ping-hook 0)
       ("mysql_query" "" php-template-mysql-query-hook 0)
       ("mysql_real_escape_string" "" php-template-mysql-real-escape-string-hook 0)
       ("mysql_result" "" php-template-mysql-result-hook 0)
       ("mysql_select_db" "" php-template-mysql-select-db-hook 0)
       ("mysql_stat" "" php-template-mysql-stat-hook 0)
       ("mysql_tablename" "" php-template-mysql-tablename-hook 0)
       ("mysql_thread_id" "" php-template-mysql-thread-id-hook 0)
       ("mysql_unbuffered_query" "" php-template-mysql-unbuffered-query-hook 0)
       ;; Others
       ("class" "" php-template-class-hook 0)
       ("function" "" php-template-function-hook 0)
       ;; Regular expressions
       ("ereg_replace" "" php-template-ereg-replace-hook 0)
       ("ereg" "" php-template-ereg-hook 0)
       ("eregi_replace" "" php-template-eregi-replace-hook 0)
       ("eregi" "" php-template-eregi-hook 0)
       ("split" "" php-template-split-hook 0)
       ("spliti" "" php-template-spliti-hook 0)
       ("sql_regcase" "" php-template-sql-regcase-hook 0)
       ;; Session
       ("session_cache_expire" "" php-template-session-cache-expire-hook 0)
       ("session_cache_limiter" "" php-template-session-cache-limiter-hook 0)
       ("session_commit" "" php-template-session-commit-hook 0)
       ("session_decode" "" php-template-session-decode-hook 0)
       ("session_destroy" "" php-template-session-destroy-hook 0)
       ("session_encode" "" php-template-session-encode-hook 0)
       ("session_get_cookie_params" "" php-template-session-get-cookie-params-hook 0)
       ("session_id" "" php-template-session-id-hook 0)
       ("session_is_registered" "" php-template-session-is-registered-hook 0)
       ("session_module_name" "" php-template-session-module-name-hook 0)
       ("session_name" "" php-template-session-name-hook 0)
       ("session_regenerate_id" "" php-template-session-regenerate-id-hook 0)
       ("session_register" "" php-template-session-register-hook 0)
       ("session_save_path" "" php-template-session-save-path-hook 0)
       ("session_set_cookie_params" "" php-template-session-set-cookie-params-hook 0)
       ("session_set_save_handler" "" php-template-session-set-save-handler-hook 0)
       ("session_start" "" php-template-session-start-hook 0)
       ("session_unregister" "" php-template-session-unregister-hook 0)
       ("session_unset" "" php-template-session-unset-hook 0)
       ("session_write_close" "" php-template-session-write-close-hook 0)
       ;; Strings
       ("addcslashes" "" php-template-addcslashes-hook 0)
       ("addslashes" "" php-template-addslashes-hook 0)
       ("bin2hex" "" php-template-bin2hex-hook 0)
       ("chop" "" php-template-chop-hook 0)
       ("chr" "" php-template-chr-hook 0)
       ("chunk_split" "" php-template-chunk-split-hook 0)
       ("convert_cyr_string" "" php-template-convert-cyr-string-hook 0)
       ("convert_uudecode" "" php-template-convert-uudecode-hook 0)
       ("convert_uuencode" "" php-template-convert-uuencode-hook 0)
       ("count_chars" "" php-template-count-chars-hook 0)
       ("crc32" "" php-template-crc32-hook 0)
       ("crypt" "" php-template-crypt-hook 0)
       ("echo" "" php-template-echo-hook 0)
       ("explode" "" php-template-explode-hook 0)
       ("fprintf" "" php-template-fprintf-hook 0)
       ("get_html_translation_table" "" php-template-get-html-translation-table-hook 0)
       ("hebrev" "" php-template-hebrev-hook 0)
       ("hebrevc" "" php-template-hebrevc-hook 0)
       ("html_entity_decode" "" php-template-html-entity-decode-hook 0)
       ("htmlentities" "" php-template-htmlentities-hook 0)
       ("htmlspecialchars_decode" "" php-template-htmlspecialchars-decode-hook 0)
       ("htmlspecialchars" "" php-template-htmlspecialchars-hook 0)
       ("implode" "" php-template-implode-hook 0)
       ("join" "" php-template-join-hook 0)
       ("levenshtein" "" php-template-levenshtein-hook 0)
       ("localeconv" "" php-template-localeconv-hook 0)
       ("ltrim" "" php-template-ltrim-hook 0)
       ("md5_file" "" php-template-md5-file-hook 0)
       ("md5" "" php-template-md5-hook 0)
       ("metaphone" "" php-template-metaphone-hook 0)
       ("money_format" "" php-template-money-format-hook 0)
       ("nl_langinfo" "" php-template-nl-langinfo-hook 0)
       ("nl2br" "" php-template-nl2br-hook 0)
       ("number_format" "" php-template-number-format-hook 0)
       ("ord" "" php-template-ord-hook 0)
       ("parse_str" "" php-template-parse-str-hook 0)
       ("print" "" php-template-print-hook 0)
       ("printf" "" php-template-printf-hook 0)
       ("quoted_printable_decode" "" php-template-quoted-printable-decode-hook 0)
       ("quotemeta" "" php-template-quotemeta-hook 0)
       ("rtrim" "" php-template-rtrim-hook 0)
       ("setlocale" "" php-template-setlocale-hook 0)
       ("sha1_file" "" php-template-sha1-file-hook 0)
       ("sha1" "" php-template-sha1-hook 0)
       ("similar_text" "" php-template-similar-text-hook 0)
       ("soundex" "" php-template-soundex-hook 0)
       ("sprintf" "" php-template-sprintf-hook 0)
       ("sscanf" "" php-template-sscanf-hook 0)
       ("str_ireplace" "" php-template-str-ireplace-hook 0)
       ("str_pad" "" php-template-str-pad-hook 0)
       ("str_repeat" "" php-template-str-repeat-hook 0)
       ("str_replace" "" php-template-str-replace-hook 0)
       ("str_rot13" "" php-template-str-rot13-hook 0)
       ("str_shuffle" "" php-template-str-shuffle-hook 0)
       ("str_split" "" php-template-str-split-hook 0)
       ("str_word_count" "" php-template-str-word-count-hook 0)
       ("strcasecmp" "" php-template-strcasecmp-hook 0)
       ("strchr" "" php-template-strchr-hook 0)
       ("strcmp" "" php-template-strcmp-hook 0)
       ("strcoll" "" php-template-strcoll-hook 0)
       ("strcspn" "" php-template-strcspn-hook 0)
       ("strip_tags" "" php-template-strip-tags-hook 0)
       ("stripcslashes" "" php-template-stripcslashes-hook 0)
       ("stripos" "" php-template-stripos-hook 0)
       ("stripslashes" "" php-template-stripslashes-hook 0)
       ("stristr" "" php-template-stristr-hook 0)
       ("strlen" "" php-template-strlen-hook 0)
       ("strnatcasecmp" "" php-template-strnatcasecmp-hook 0)
       ("strnatcmp" "" php-template-strnatcmp-hook 0)
       ("strncasecmp" "" php-template-strncasecmp-hook 0)
       ("strncmp" "" php-template-strncmp-hook 0)
       ("strpbrk" "" php-template-strpbrk-hook 0)
       ("strpos" "" php-template-strpos-hook 0)
       ("strrchr" "" php-template-strrchr-hook 0)
       ("strrev" "" php-template-strrev-hook 0)
       ("strripos" "" php-template-strripos-hook 0)
       ("strrpos" "" php-template-strrpos-hook 0)
       ("strspn" "" php-template-strspn-hook 0)
       ("strstr" "" php-template-strstr-hook 0)
       ("strtok" "" php-template-strtok-hook 0)
       ("strtolower" "" php-template-strtolower-hook 0)
       ("strtoupper" "" php-template-strtoupper-hook 0)
       ("strtr" "" php-template-strtr-hook 0)
       ("substr_compare" "" php-template-substr-compare-hook 0)
       ("substr_count" "" php-template-substr-count-hook 0)
       ("substr_replace" "" php-template-substr-replace-hook 0)
       ("substr" "" php-template-substr-hook 0)
       ("trim" "" php-template-trim-hook 0)
       ("ucfirst" "" php-template-ucfirst-hook 0)
       ("ucwords" "" php-template-ucwords-hook 0)
       ("vfprintf" "" php-template-vfprintf-hook 0)
       ("vprintf" "" php-template-vprintf-hook 0)
       ("vsprintf" "" php-template-vsprintf-hook 0)
       ("wordwrap" "" php-template-wordwrap-hook 0)
       ;; Variable
       ("debug_zval_dump" "" php-template-debug-zval-dump-hook 0)
       ("doubleval" "" php-template-doubleval-hook 0)
       ("empty" "" php-template-empty-hook 0)
       ("floatval" "" php-template-floatval-hook 0)
       ("get_defined_vars" "" php-template-get-defined-vars-hook 0)
       ("get_resource_type" "" php-template-get-resource-type-hook 0)
       ("gettype" "" php-template-gettype-hook 0)
       ("import_request_variables" "" php-template-import-request-variables-hook 0)
       ("intval" "" php-template-intval-hook 0)
       ("is_array" "" php-template-is-array-hook 0)
       ("is_bool" "" php-template-is-bool-hook 0)
       ("is_callable" "" php-template-is-callable-hook 0)
       ("is_double" "" php-template-is-double-hook 0)
       ("is_float" "" php-template-is-float-hook 0)
       ("is_int" "" php-template-is-int-hook 0)
       ("is_integer" "" php-template-is-integer-hook 0)
       ("is_long" "" php-template-is-long-hook 0)
       ("is_null" "" php-template-is-null-hook 0)
       ("is_numeric" "" php-template-is-numeric-hook 0)
       ("is_object" "" php-template-is-object-hook 0)
       ("is_real" "" php-template-is-real-hook 0)
       ("is_resource" "" php-template-is-resource-hook 0)
       ("is_scalar" "" php-template-is-scalar-hook 0)
       ("is_string" "" php-template-is-string-hook 0)
       ("isset" "" php-template-isset-hook 0)
       ("print_r" "" php-template-print-r-hook 0)
       ("serialize" "" php-template-serialize-hook 0)
       ("settype" "" php-template-settype-hook 0)
       ("strval" "" php-template-strval-hook 0)
       ("unserialize" "" php-template-unserialize-hook 0)
       ("unset" "" php-template-unset-hook 0)
       ("var_dump" "" php-template-var-dump-hook 0)
       ("var_export" "" php-template-var-export-hook 0)
       ;; XML
       ("utf8_decode" "" php-template-utf8-decode-hook 0)
       ("utf8_encode" "" php-template-utf8-encode-hook 0)
       ("xml_error_string" "" php-template-xml-error-string-hook 0)
       ("xml_get_current_byte_index" "" php-template-xml-get-current-byte-index-hook 0)
       ("xml_get_current_column_number" "" php-template-xml-get-current-column-number-hook 0)
       ("xml_get_current_line_number" "" php-template-xml-get-current-line-number-hook 0)
       ("xml_get_error_code" "" php-template-xml-get-error-code-hook 0)
       ("xml_parse_into_struct" "" php-template-xml-parse-into-struct-hook 0)
       ("xml_parse" "" php-template-xml-parse-hook 0)
       ("xml_parser_create_ns" "" php-template-xml-parser-create-ns-hook 0)
       ("xml_parser_create" "" php-template-xml-parser-create-hook 0)
       ("xml_parser_free" "" php-template-xml-parser-free-hook 0)
       ("xml_parser_get_option" "" php-template-xml-parser-get-option-hook 0)
       ("xml_parser_set_option" "" php-template-xml-parser-set-option-hook 0)
       ("xml_set_character_data_handler" "" php-template-xml-set-character-data-handler-hook 0)
       ("xml_set_default_handler" "" php-template-xml-set-default-handler-hook 0)
       ("xml_set_element_handler" "" php-template-xml-set-element-handler-hook 0)
       ("xml_set_end_namespace_decl_handler" "" php-template-xml-set-end-namespace-decl-handler-hook 0)
       ("xml_set_external_entity_ref_handler" "" php-template-xml-set-external-entity-ref-handler-hook 0)
       ("xml_set_notation_decl_handler" "" php-template-xml-set-notation-decl-handler-hook 0)
       ("xml_set_object" "" php-template-xml-set-object-hook 0)
       ("xml_set_processing_instruction_handler" "" php-template-xml-set-processing-instruction-handler-hook 0)
       ("xml_set_start_namespace_decl_handler" "" php-template-xml-set-start-namespace-decl-handler-hook 0)
       ("xml_set_unparsed_entity_decl_handler" "" php-template-xml-set-unparsed-entity-decl-handler-hook 0)
       ))))

;; initialize abbrev table for PHP Mode
(php-mode-abbrev-table-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev hooks

(defun php-hooked-abbrev (func)
  "Do function, if syntax says abbrev is a keyword, invoked by hooked abbrev,
but not if inside a comment or quote)."
  (if (or (php-in-literal)
	  (php-in-comment-p))
      (progn
	(insert " ")
	(unexpand-abbrev)
	(delete-char -1))
    (if (not php-electric-mode)
	(progn
	  (insert " ")
	  (unexpand-abbrev)
	  (backward-word 1)
	  (delete-char 1))
      (let ((invoke-char last-command-char)
	    (abbrev-mode -1)
	    (php-template-invoked-by-hook t))
	(let ((caught (catch 'abort
			(funcall func))))
	  (when (stringp caught) (message caught)))
	(when (= invoke-char ?-) (setq abbrev-start-location (point)))
	;; delete CR which is still in event queue
	(if (fboundp 'enqueue-eval-event)
	    (enqueue-eval-event 'delete-char -1)
	  (setq unread-command-events	; push back a delete char
		(list (php-character-to-event ?\177))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP-mode

;;;###autoload
(defun php-mode ()
  "PHP Mode
********

PHP Mode is a GNU XEmacs major mode for editing files containing PHP
code.

1 Introduction
**************

Inspired from PHP-Mode (http://php-mode.sourceforge.net/) and from
VHDL-Mode (http://www.iis.ee.ethz.ch/~zimmi/emacs/vhdl-mode.html),
this new PHP Mode combines the advantages of the two modes to
simplify the writing of PHP code: highlight, indentation, completion,
templates, navigation into the file (functions and class), navigation
into source files...



Features (new features in bold) :

   * Completion

   * Customizable

   * Highlight

   * Indentation

   * Menu

   * Stuttering

   * Templates
        - .NET

        - Apache

        - APC

        - Array

        - Control Structures (\"declare\" function excepted)

        - Date/Time

        - Directory

        - Email

        - Error and Logging

        - Filesystem (\"fscanf\" and \"set_file_buffer\" functions
          excepted)

        - Functions

        - Image

        - Mathematical

        - Miscellaneous

        - MySQL

        - Regular expressions

        - Sessions

        - Strings

        - Variables

        - XML



This manual describes PHP Mode version 0.0.4.

2 Installation
**************

2.1 Requirements
================

PHP Mode is a XEmacs major mode that needs the following
software/packages:

   * XEmacs (http://www.xemacs.org/).

   * `font-lock' mode generaly installed with XEmacs.

   * `assoc' mode generaly installed with XEmacs.

   * `easymenu' mode generaly installed with XEmacs.

   * `hippie-exp' mode generaly installed with XEmacs.

Before continuing, you must be sure to have all this packages
installed.

2.2 Download
============

Two internet address to download PHP Mode :

   * Principal: PHP-Mode 0.0.4
     (http://deboutv.free.fr/lisp/php/download/php-0.0.4.tar.gz)
     (http://deboutv.free.fr/lisp/php/)

   * Secondary: PHP-Mode 0.0.4
     (http://www.morinie.fr/lisp/php/download/php-0.0.4.tar.gz)
     (http://www.morinie.fr/lisp/php/)

   * Old releases: PHP-Mode
     (http://deboutv.free.fr/lisp/php/download.php)
     (http://deboutv.free.fr/lisp/php/)

2.3 Installation
================

2.3.1 Installation
------------------

To install PHP Mode you need to choose an installation directory (for
example `/usr/local/share/lisp' or `c:\lisp'). The administrator must
have the write rights on this directory.

With your favorite unzip software, unzip the archive in the
installation directory.

Example:
     cd /usr/local/share/lisp
     tar zxvf php-0.0.4.tar.gz
Now you have a `php' directory in the installation directory. This
directory contains 2 files `php-mode.el' and `php-mode.elc' and
another directory `docs' containing the documentation.

You need to configure XEmacs. open you initialization file `init.el'
(open the file or start XEmacs then choose the Options menu and Edit
Init File). Add the following lines (the installation directory in
this example is `/usr/local/share/lisp') :

     (setq load-path
           (append (list \"/usr/local/share/lisp/\") load-path))
     (autoload 'php-mode \"php-mode\" \"PHP mode\" t)

2.3.2 Update
------------

The update is easy. You need to unzip the archive in the installation
directory to remove the old release.

Example:
     cd /usr/local/share/lisp
     rm -rf php
     tar zxvf php-0.0.4.tar.gz

2.4 Invoke PHP-Mode
===================

You have two possibilities to invoke the PHP Mode.

   - Manually: At each file opening you need to launch PHP Mode with
     the following command:

     `M-x php-mode'

   - Automatically: Add the following linesin your initialization
     file `init.el' :

          (setq auto-mode-alist
                (append
                 '((\"\\.php$\" . php-mode)
                   (\"\\.php5$\" . php-mode))
          	 auto-mode-alist))


3 Customization
***************

This chapter describes the differents parameters and functions that
you can change to customize PHP Mode.  To do that, open a PHP file,
click on the PHP menu and choose Options then Browse Options....

3.1 Parameters
==============

3.1.1 Mode
----------

PHP Mode has 3 modes allowing to simplify the writing of PHP code.
You can enable/disable each mode individually.

`php-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `php-electric-mode'.

`php-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `php-stutter-mode'.

`php-indent-tabs-mode'
     Type: boolean
     Default value: `nil'
     Description: If `t'; indentation is made with tabs.  If `nil';
     indentation is made with spaces.

3.1.2 Menu
----------

PHP Mode has also 2 menus tha you can enable/disable. The menus Index
and Sources are specific to each PHP files opened.

`php-index-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Index menu is enabled. It shows the
     list of class and functions of the opened file. The Index menu
     scans the file when it is opened.

`php-index-menu-auto-rescan'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Index menu is updated when a function
     or a class is added or removed.

`php-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of PHP file located in the current directory.
     The Sources menu scans the directory when a file is opened.

3.1.3 Templates
---------------

`php-include-in-parenthesis'
     Type: boolean
     Default value: `t'
     Description: If `t'; the include et require functions are used
     with parenthesis (example : `require( 'required_file.php' );').
     If `nil'; the include and require functions are used without
     parenthesis (example : `include_once 'required_file.php';')

`php-add-fclose-with-fopen'
     Type: boolean
     Default value: `t'
     Description: If `t'; the fclose function is added when the
     template of the fopen function is executed.

`php-add-mysql-close-when-connect'
     Type: boolean
     Default value: `t'
     Description: If `t'; the mysql_close function is added when the
     template of mysql_connect function is executed.

3.1.3.1 Header
..............

`php-file-header'
     Type: string
     Default value: `\"
     /**
     * <description string>
     *
     *
     * Created: <date>
     * Last update: <date>
     *
     * @link <link string>
     * @copyright <copyright>
     * @author <author>
     * @package <package string>
     * @version <version string>
     */
     <cursor>\"'
     Description: String or file to insert as file header. If the
     string specifies an existing file name the contents of the file
     is inserted; otherwise the string itself is inserted as file
     header.
     Type `C-j' for newlines.
     The follonwing keywords are supported:
     <filename>: replaced by the file name.
     <author>: replaced by the user name and email address.
     <login>: replaced by `user-login-name'.
     <company>: replaced by `php-company-name' content.
     <date>: replaced by the current date.
     <year>: replaced by the current year.
     <copyright>: replaced by `php-copyright-string' content.
     <cursor>: final cursor position.

`php-file-footer'
     Type: string
     Default value: `\"\"'
     Description: String or file to insert as file footer.  See
     `php-file-header'

`php-company-name'
     Type: string
     Default value: `\"\"'
     Description: Name of the company to insert in file header.

`php-copyright-string'
     Type: string
     Default value: `\"\"'
     Description: Coryright string to insert in file header.

`php-date-format'
     Type: string
     Default value: `\"%Y-%m-%d\"'
     Description: Date format.

`php-modify-date-prefix-string'
     Type: string
     Default value: `\" * Last update: \"'
     Description: Prefix string of modification date in PHP file
     header.

`php-modify-date-on-saving'
     Type: bool
     Default value: `t'
     Description: If `t'; update the modification date when the
     buffer is saved.

3.1.4 Style
-----------

`php-basic-offset'
     Type: integer
     Default value: `4'
     Description: Amount of basic offset used for indentation.

3.1.5 PHPDocumentor
-------------------

`php-enable-phpdocumentor-tags'
     Type: bool
     Default value: `t'
     Description: If `t'; PHP-Documentor tags are added into the
     class and functions comments.

`php-class-tags'
     Type: list (string)
     Default value: `'(\"package\")'
     Description: List of PHP-Documentor tags to add into the class
     comments.

`php-function-tags'
     Type: list (string)
     Default value: `'()'
     Description: List of PHP-Documentor tags to add into the
     functions comments.

3.1.6 Miscellaneous
-------------------

`php-intelligent-tab'
     Type: bool
     Default value: `t'
     Description: If `t'; TAB does indentation; completion and insert
     tabulations. If `nil'; TAB does only indentation.

`php-word-completion-in-minibuffer'
     Type: bool
     Default value: `t'
     Description: If `t'; enable completion in the minibuffer.

`php-word-completion-case-sensitive'
     Type: bool
     Default value: `nil'
     Description: If `t'; completion is case sensitive.

3.2 Functions
=============

3.2.1 Mode
----------

`php-electric-mode'
     Menu: PHP -> Options -> Mode -> Electric Mode
     Keybinding: `C-c C-m C-e'
     Description: This functions is used to enable/disable the
     electric mode.

`php-stutter-mode'
     Menu: PHP -> Options -> Mode -> Stutter Mode
     Keybinding: `C-c C-m C-s'
     Description: This function is used to enable/disable the stutter
     mode.

4 Menus
*******

There are 3 menus: PHP, Index and Sources. All theses menus can be
accessed from the menubar or from the right click. This chapter
describes each menus.

4.1 PHP
=======

This is the main menu of PHP Mode. It allows an easy access to the
main features of the PHP Mode: Templates (see *Note Templates::),
Indentation (see *Note Indentation::) and Options (see *Note
Customization::).

This menu contains also 3 functions that are discussed in the next
part.

4.1.1 Functions
---------------

`php-show-messages'
     Menu: PHP -> Show Messages
     Keybinding: `C-c M-m'
     Description: This function opens the *Messages* buffer to
     display previous error messages.

`php-doc-mode'
     Menu: PHP -> PHP Mode Documentation
     Keybinding: `C-c C-h'
     Description: This function opens the *Help* buffer and prints in
     it the PHP Mode documentation.

`php-version'
     Menu: PHP -> Version
     Keybinding: `C-c C-v'
     Description: This function displays in the minibuffer the
     current PHP Mode version with the timestamp.

4.2 Index
=========

The Index menu allows you to navigate into the current buffer to find
functions and classes.  This menu is built during the buffer openning
and you need to refresh it if you add or remove functions or classes.
There is one Index menu per buffer.

4.2.1 Customization
-------------------

`php-index-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Index menu is enabled. It shows the
     list of class and functions of the opened file. The Index menu
     scans the file when it is opened.

`php-index-menu-auto-rescan'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Index menu is updated when a function
     or a class is added or removed.

4.3 Sources
===========

The Sources menu shows the PHP files in the current directory. If you
add or delete a file in the current directory, you need to refresh
the menu.

4.3.1 Customization
-------------------

`php-source-file-menu'
     Type: boolean
     Default value: `t'
     Description: If `t'; the Sources menu is enabled. This menu
     contains the list of PHP file located in the current directory.
     The Sources menu scans the directory when a file is opened.

4.3.2 Functions
---------------

`php-add-source-files-menu'
     Menu: Sources -> *Rescan*
     Keybinding: `C-c C-s C-u'
     Description: This function is used to refresh the Sources menu.

5 Completion
************

Completion allows you to write PHP code by reducing errors in
function names or variable names. It also suggests PHP function names.

To use completion, write the first letters and use <TAB> to complete
the word until you find the perfect completion. For example, in a
blank PHP buffer, type \"mysql_\" and complete the function name with
<TAB>: all mysql_* functions are proposed.

Completion can occurs with the supported PHP functions, the PHP code
written in the current buffer and the PHP code written in all other
files that are opened in the current XEmacs window.

Completion is customizable.

5.1 Customization
=================

`php-intelligent-tab'
     Type: bool
     Default value: `t'
     Description: If `t'; TAB does indentation; completion and insert
     tabulations. If `nil'; TAB does only indentation.

`php-word-completion-in-minibuffer'
     Type: bool
     Default value: `t'
     Description: If `t'; enable completion in the minibuffer.

`php-word-completion-case-sensitive'
     Type: bool
     Default value: `nil'
     Description: If `t'; completion is case sensitive.

6 Indentation
*************

Indentation can be done with spaces or TAB characters. A new line is
automtically indented. Tou can manually indent or remove indentation
by placing the cursor at the beginning of the first word and by using
the <TAB> key.

6.1 Customization
=================

`php-indent-tabs-mode'
     Type: boolean
     Default value: `nil'
     Description: If `t'; indentation is made with tabs.  If `nil';
     indentation is made with spaces.

`php-basic-offset'
     Type: integer
     Default value: `4'
     Description: Amount of basic offset used for indentation.

6.2 Functions
=============

`indent-according-to-mode'
     Menu: PHP -> Indent -> Line
     Keybinding: `C-c C-i C-l'
     Description: This function is used to indent the current line
     (the line where the cursor is).

`php-indent-region'
     Menu: PHP -> Indent -> Region
     Keybinding: `M-C-, This function is used to indent the selected
     region in the current buffer.'
     Description:

`php-indent-buffer'
     Menu: PHP -> Indent -> Buffer
     Keybinding: `C-c C-i C-b'
     Description: This function is used to indent the buffer.

7 Stuttering
************

The stutter mode is a mode that affects a function to a key. For
example, when you use the `ENTER' key, the associated function will
create a new line and indent it.

7.1 Customization
=================

`php-stutter-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable the stuttering. Is indicated in the
     modeline by \"/s\" after the mode name and can be toggled by
     `php-stutter-mode'.

7.2 Functions
=============

`SPACE'
     If in comment, indent the comment and add new line if necessary.
     In other case, add a space.

`ENTER'
     Insert a new line and indent it.

`}'
     Insert a new line and indent it.

`;'
     Insert a new line and indent it.

`*'
     If the previous character is a `/', a prompt will ask you for
     inserting a
     `/* */' comment type (with the `SPACE' key) or
     `/**
     *
     */' coment type (with the `*' key).

`('
     If the previous character is a `(', the `((' will be replaced by
     `['.
     If the previous character is a `[', the `[(' will be replaced by
     `{'.
     In other case, insert a `('.

`)'
     If the previous character is a `)', the `))' will be replaced by
     `]'.
     If the previous character is a `]', the `])' will be replaced by
     `}'.
     In other case, insert a `)'.

8 Templates
***********

In the PHP Mode, the PHP functions (like if, while, for, fopen,
fclose) are predefined in functions called \"Templates\".

Each template can be invoked by the function name or by using the
<SPACE> key after the PHP function name in the buffer (Note, using
`M-<SPACE>' disable the template).

A template can be aborted by using the `C-g' or by lefting empty the
tempate prompt (in the minibuffer).

8.1 Customization
=================

`php-electric-mode'
     Type: boolean
     Default value: `t'
     Description: If `t'; enable automatic generation of template.
     If `nil'; template generators can still be invoked through key
     bindings and menu. Is indicated in the modeline by \"/e\" after
     the mode name and can be toggled by `php-electric-mode'.

For a complete description of the template customizable variables,
see *Note Cu01-Pa01-Template::

8.2 Functions
=============

8.2.1 PHP Functions
-------------------

For PHP functions, see the PDF or HTML documentation.

8.2.2 Non-PHP Functions
-----------------------

`php-template-header'
     Menu: PHP -> Templates -> Insert Header
     Keybinding: `none'
     Description: This function is used to insert a header in the
     current buffer.

`php-template-footer'
     Menu: PHP -> Templates -> Insert Footer
     Keybinding: `none'
     Description: This function is used to insert a footer in the
     current buffer.

`php-template-insert-date'
     Menu: PHP -> Templates -> Insert Date
     Keybinding: `none'
     Description: This function is used to insert the date in the
     current buffer.

`php-template-modify'
     Menu: PHP -> Templates -> Modify Date
     Keybinding: `none'
     Description: This function is used to modify the last
     modification date in the current buffer.

9 Bugs, Help
************

   * To report bugs: Bugtracker (http://bugtracker.morinie.fr/lisp/)

   * To obtain help you can post on the dedicated forum: Forum
     (http://forum.morinie.fr/lisp/)

10 Key bindings
***************

\\{php-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'php-mode)
  (setq mode-name "PHP")

  ;; set maps and tables
  (use-local-map php-mode-map)
  (set-syntax-table php-mode-syntax-table)
  (setq local-abbrev-table php-mode-abbrev-table)

  ;; set local variables
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'php-indent-line-2)
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'comment-start-skip) "/\\*+ *")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'end-comment-column) 70)
  (set (make-local-variable 'comment-indent-function) 'php-comment-indent)
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'hippie-expand-verbose) nil)

;;  (set (make-local-variable 'font-lock-support-mode) 'lazy-lock-mode)
 ;; (set (make-local-variable 'lazy-lock-defer-contextually) nil)
 ;; (set (make-local-variable 'lazy-lock-defer-on-the-fly) t)
 ;; (set (make-local-variable 'lazy-lock-defer-on-scrolling) t)


  ;; add index menu
  (php-index-menu-init)
  ;; add source file menu
  (if php-source-file-menu (php-add-source-files-menu))
  ;; add PHP menu
  (easy-menu-add php-mode-menu-list)
  (easy-menu-define php-mode-menu php-mode-map
		    "Menu keymap for PHP Mode." php-mode-menu-list)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((php-font-lock-keywords-1
           php-font-lock-keywords-2
           ;; Comment-out the next line if the font-coloring is too
           ;; extreme/ugly for you.
           php-font-lock-keywords-3)
          nil                               ; KEYWORDS-ONLY
          t                                 ; CASE-FOLD
          nil                               ; SYNTAX-ALIST
          nil                               ; SYNTAX-BEGIN
          (font-lock-syntactic-keywords . php-font-lock-syntactic-keywords)))

  (php-write-file-hooks-init)
  ;; miscellaneous
  (message "PHP Mode %s.%s" php-version
	   (if noninteractive "" "  See menu for documentation and release notes."))
  (php-mode-line-update)

  ;; run hooks
  (run-hooks 'php-mode-hook))

(defun php-activate-customizations ()
  "Activate all customizations on local variables."
  (interactive)
  (php-mode-map-init)
  (use-local-map php-mode-map)
  (set-syntax-table php-mode-syntax-table)
  (php-update-mode-menu)
  (run-hooks 'menu-bar-update-hook)
  (php-mode-line-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .NET

(defun php-template-dotnet-load ()
  "Insert a dotnet_load statement."
  (interactive)
  (php-template-generic-function "dotnet_load" '("assembly_name" "datatype_name" "codepage") 1))

;; Apache

(defun php-template-apache-child-terminate ()
  "Insert a apache_child_terminate statement."
  (interactive)
  (php-template-generic-function "apache_child_terminate" '() 0))

(defun php-template-apache-get-modules ()
  "Insert a apache_get_modules statement."
  (interactive)
  (php-template-generic-function "apache_get_modules" '() 0))

(defun php-template-apache-get-version ()
  "Insert a apache_get_version statement."
  (interactive)
  (php-template-generic-function "apache_get_version" '() 0))

(defun php-template-apache-getenv ()
  "Insert a apache_getenv statement."
  (interactive)
  (php-template-generic-function "apache_getenv" '("variable" "walk_to_top") 1))

(defun php-template-apache-lookup-uri ()
  "Insert a apache_lookup_uri statement."
  (interactive)
  (php-template-generic-function "apache_lookup_uri" '("filename") 1))

(defun php-template-apache-note ()
  "Insert a apache_note statement."
  (interactive)
  (php-template-generic-function "apache_note" '("note_name" "note_value") 1))

(defun php-template-apache-request-headers ()
  "Insert a apache_request_headers statement."
  (interactive)
  (php-template-generic-function "apache_request_headers" '() 0))

(defun php-template-apache-reset-timeout ()
  "Insert a apache_reset_timeout statement."
  (interactive)
  (php-template-generic-function "apache_reset_timeout" '() 0))

(defun php-template-apache-response-headers ()
  "Insert a apache_response_headers statement."
  (interactive)
  (php-template-generic-function "apache_response_headers" '() 0))

(defun php-template-apache-setenv ()
  "Insert a apache_setenv statement."
  (interactive)
  (php-template-generic-function "apache_setenv" '("variable" "value" "walk_to_top") 2))

(defun php-template-ascii2ebcdic ()
  "Insert a ascii2ebcdic statement."
  (interactive)
  (php-template-generic-function "ascii2ebcdic" '("ascii_str") 1))

(defun php-template-ebcdic2ascii ()
  "Insert a ebcdic2ascii statement."
  (interactive)
  (php-template-generic-function "ebcdic2ascii" '("ebcdic_str") 1))

(defun php-template-getallheaders ()
  "Insert a getallheaders statement."
  (interactive)
  (php-template-generic-function "getallheaders" '() 0))

(defun php-template-virtual ()
  "Insert a virtual statement."
  (interactive)
  (php-template-generic-function "virtual" '("filename") 1))

;; APC

(defun php-template-apc-cache-info ()
  "Insert a apc_cache_info statement."
  (interactive)
  (php-template-generic-function "apc_cache_info" '("cache_type" "limited") 0))

(defun php-template-apc-clear-cache ()
  "Insert a apc_clear_cache statement."
  (interactive)
  (php-template-generic-function "apc_clear_cache" '("cache_type") 0))

(defun php-template-apc-define-constants ()
  "Insert a apc_define_constants statement."
  (interactive)
  (php-template-generic-function "apc_define_constants" '("key" "constants" "case_sensitive") 2))

(defun php-template-apc-delete ()
  "Insert a apc_delete statement."
  (interactive)
  (php-template-generic-function "apc_delete" '("key") 1))

(defun php-template-apc-fetch ()
  "Insert a apc_fetch statement."
  (interactive)
  (php-template-generic-function "apc_fetch" '("key") 1))

(defun php-template-apc-load-constants ()
  "Insert a apc_load_constants statement."
  (interactive)
  (php-template-generic-function "apc_load_constants" '("key" "case_sensitive") 1))

(defun php-template-apc-sma-info ()
  "Insert a apc_sma_info statement."
  (interactive)
  (php-template-generic-function "apc_sma_info" '() 0))

(defun php-template-apc-store ()
  "Insert a apc_store statement."
  (interactive)
  (php-template-generic-function "apc_store" '("key" "var" "ttl") 2))


;; Array functions

(defun php-template-array-generic-function (label field mandatory-count last-prompt)
  "Generic function template 'result = label( field1, field2...)"
  (interactive)
  (let ((start (point)) remove here result-value elt continue field-count stop prompt)
    (insert " = ")
    (setq remove (point))
    (insert (concat label "( "))
    (setq here (point-marker))
    (goto-char start)
    (setq result-value (php-template-field "result_value" nil t))
    (if (not result-value)
	(delete-region start remove))
    (goto-char here)
    (setq elt field)
    (setq continue t)
    (setq field-count 0)
    (setq stop nil)
    (while (and elt continue)
      (setq prompt (car elt))
      (setq result-value (php-template-field prompt nil t))
      (if (and (not result-value)
	       (< field-count mandatory-count))
	  (progn (setq continue nil)
		 (delete-region start (point))
		 (insert (concat label " "))
		 (setq stop t))
	(if (not result-value)
	    (setq continue nil)
	  (insert ", ")))
      (setq field-count (+ 1 field-count))
      (setq elt (cdr elt)))
    (when continue
      (while continue
	(setq result-value (php-template-field prompt nil t))
	(if (and (not result-value)
		 (< field-count mandatory-count))
	    (progn (setq continue nil)
		   (delete-region start (point))
		   (insert (concat label " "))
		   (setq stop t))
	  (if (not result-value)
	      (setq continue nil)
	    (insert ", ")))
	(setq field-count (+ 1 field-count))))
    (when (not stop)
      (setq elt last-prompt)
      (setq continue t)
      (while (and elt continue)
	(setq result-value (php-template-field (car elt) nil t))
	(if result-value
	    (insert ", ")
	  (setq continue nil))
	(setq elt (cdr elt)))
      (if (= 1 field-count)
	  (delete-char -1)
	(delete-char -2)
	(insert " "))
      (insert ");")
      (newline-and-indent))))

(defun php-template-array-change-key-case ()
  "Insert a array_change_key_case statement."
  (interactive)
  (php-template-generic-function "array_change_key_case" '("input" "case") 1))

(defun php-template-array-chunk ()
  "Insert a array_chunk statement."
  (interactive)
  (php-template-generic-function "array_chunk" '("input" "size" "preserve_keys") 2))

(defun php-template-array-combine ()
  "Insert a array_combine statement."
  (interactive)
  (php-template-generic-function "array_combine" '("keys" "values") 2))

(defun php-template-array-count-values ()
  "Insert a array_count statement."
  (interactive)
  (php-template-generic-function "array_count" '("input") 1))

(defun php-template-array-diff-assoc ()
  "Insert a array_diff_assoc statement."
  (interactive)
  (php-template-generic-function "array_diff_assoc" '("array1" "array2" "array...") 2 t))

(defun php-template-array-diff-key ()
  "Insert a array_diff_key statement."
  (interactive)
  (php-template-generic-function "array_diff_key" '("array1" "array2" "array...") 2 t))

(defun php-template-array-diff-uassoc ()
  "Insert a array_diff_uassoc statement."
  (interactive)
  (php-template-array-generic-function "array_diff_uassoc" '("array1" "array2" "array...") 2 '("key_compare_func")))

(defun php-template-array-diff-ukey ()
  "Insert a array_diff_ukey statement."
  (interactive)
  (php-template-array-generic-function "array_diff_ukey" '("array1" "array2" "array...") 2 '("key_compare_func")))

(defun php-template-array-diff ()
  "Insert a array_diff statement."
  (interactive)
  (php-template-generic-function "array_diff" '("array1" "array2" "array...") 2 t))

(defun php-template-array-fill ()
  "Insert a array_fill statement."
  (interactive)
  (php-template-generic-function "array_fill" '("start_index" "num" "value") 3))

(defun php-template-array-filter ()
  "Insert a array_filter statement."
  (interactive)
  (php-template-generic-function "array_filter" '("input" "callback") 1))

(defun php-template-array-flip ()
  "Insert a array_flip statement."
  (interactive)
  (php-template-generic-function "array_flip" '("trans") 1))

(defun php-template-array-intersect-assoc ()
  "Insert a array_intersect_assoc statement."
  (interactive)
  (php-template-generic-function "array_intersect_assoc" '("array1" "array2" "array...") 2 t))

(defun php-template-array-intersect-key ()
  "Insert a array_intersect_key statement."
  (interactive)
  (php-template-generic-function "array_intersect_key" '("array1" "array2" "array...") 2 t))

(defun php-template-array-intersect-uassoc ()
  "Insert a array_intersect_uassoc statement."
  (interactive)
  (php-template-array-generic-function "array_intersect_uassoc" '("array1" "array2" "array...") 2 '("key_compare_func")))

(defun php-template-array-intersect-ukey ()
  "Insert a array_intersect_ukey statement."
  (interactive)
  (php-template-array-generic-function "array_intersect_ukey" '("array1" "array2" "array...") 2 '("key_compare_func")))

(defun php-template-array-intersect ()
  "Insert a array_intersect statement."
  (interactive)
  (php-template-generic-function "array_intersect" '("array1" "array2" "array...") 2 t))

(defun php-template-array-exists ()
  "Insert a array_exists statement."
  (interactive)
  (php-template-generic-function "array_exists" '("key" "search") 2))

(defun php-template-array-keys ()
  "Insert a array_keys statement."
  (interactive)
  (php-template-generic-function "array_keys" '("input" "search_value" "strict") 1))

(defun php-template-array-map ()
  "Insert a array_map statement."
  (interactive)
  (php-template-generic-function "array_map" '("callback" "arr1" "array...") 2 t))

(defun php-template-array-merge-recursive ()
  "Insert a array_merge_recursive statement."
  (interactive)
  (php-template-generic-function "array_merge_recursive" '("array1", "array...") 1 t))

(defun php-template-array-merge ()
  "Insert a array_merge statement."
  (interactive)
  (php-template-generic-function "array_merge" '("array1", "array...") 1 t))

(defun php-template-array-multisort ()
  "Insert a array_multisort statement."
  (interactive)
  (php-template-generic-function "array_multisort" '("ar1" "arg") 1 t))

(defun php-template-array-pad ()
  "Insert a array_pad statement."
  (interactive)
  (php-template-generic-function "array_pad" '("input" "pad_size" "pad_value") 3))

(defun php-template-array-pop ()
  "Insert a array_pop statement."
  (interactive)
  (php-template-generic-function "array_pop" '("array") 1))

(defun php-template-array-product ()
  "Insert a array_product statement."
  (interactive)
  (php-template-generic-function "array_product" '("array") 1))

(defun php-template-array-push ()
  "Insert a array_push statement."
  (interactive)
  (php-template-generic-function "array_push" '("array" "var" "var...") 2 t))

(defun php-template-array-rand ()
  "Insert a array_rand statement."
  (interactive)
  (php-template-generic-function "array_rand" '("input" "num_req") 1))

(defun php-template-array-reduce ()
  "Insert a array_reduce statement."
  (interactive)
  (php-template-generic-function "array_reduce" '("input" "function" "initial") 2))

(defun php-template-array-reverse ()
  "Insert a array_reverse statement."
  (interactive)
  (php-template-generic-function "array_reverse" '("array" "preserve_key") 1))

(defun php-template-array-search ()
  "Insert a array_search statement."
  (interactive)
  (php-template-generic-function "array_search" '("needle" "haystack" "strict") 2))

(defun php-template-array-shift ()
  "Insert a array_shift statement."
  (interactive)
  (php-template-generic-function "array_shift" '("array") 1))

(defun php-template-array-slice ()
  "Insert a array_slice statement."
  (interactive)
  (php-template-generic-function "array_slice" '("array" "offset" "length" "preserve_keys") 2))

(defun php-template-array-splice ()
  "Insert a array_splice statement."
  (interactive)
  (php-template-generic-function "array_splice" '("input" "offset" "length" "replacement") 2))

(defun php-template-array-sum ()
  "Insert a array_sum statement."
  (interactive)
  (php-template-generic-function "array_sum" '("array") 1))

(defun php-template-array-udiff-assoc ()
  "Insert a array_udiff_assoc statement."
  (interactive)
  (php-template-array-generic-function "array_udiff_assoc" '("array1" "array2" "array...") 2 '("data_compare_func")))

(defun php-template-array-udiff-uassoc ()
  "Insert a array_udiff_uassoc statement."
  (interactive)
  (php-template-array-generic-function "array_udiff_uassoc" '("array1" "array2" "array...") 2 '("data_compare_func" "key_compare_func")))

(defun php-template-array-udiff ()
  "Insert a array_udiff statement."
  (interactive)
  (php-template-array-generic-function "array_udiff" '("array1" "array2" "array...") 2 '("data_compare_func")))

(defun php-template-array-uintersect-assoc ()
  "Insert a array_uintersect_assoc statement."
  (interactive)
  (php-template-array-generic-function "array_uintersect_assoc" '("array1" "array2" "array...") 2 '("data_compare_func")))

(defun php-template-array-uintersect-uassoc ()
  "Insert a array_uintersect_uassoc statement."
  (interactive)
  (php-template-array-generic-function "array_uintersect_uassoc" '("array1" "array2" "array...") 2 '("data_compare_func" "key_compare_func")))

(defun php-template-array-uintersect ()
  "Insert a array_uintersect statement."
  (interactive)
  (php-template-array-generic-function "array_uintersect" '("array1" "array2" "array...") 2 '("data_compare_func")))

(defun php-template-array-unique ()
  "Insert a array_unique statement."
  (interactive)
  (php-template-generic-function "array_unique" '("array") 1))

(defun php-template-array-unshift ()
  "Insert a array_unshift statement."
  (interactive)
  (php-template-generic-function "array_unshift" '("array" "var" "var...") 2 t))

(defun php-template-array-values ()
  "Insert a array_values statement."
  (interactive)
  (php-template-generic-function "array_values" '("input") 1))

(defun php-template-array-walk-recursive ()
  "Insert a array_walk_recursive statement."
  (interactive)
  (php-template-generic-function "array_walk_recursive" '("input" "funcname" "userdata") 2))

(defun php-template-array-walk ()
  "Insert a array_walk statement."
  (interactive)
  (php-template-generic-function "array_walk" '("array" "funcname" "userdata") 2))

(defun php-template-array ()
  "Insert a array statement."
  (interactive)
  (php-template-generic-function "array" '("...") 0 t))

(defun php-template-arsort ()
  "Insert a arsort statement."
  (interactive)
  (php-template-generic-function "arsort" '("array" "sort_flags") 1))

(defun php-template-asort ()
  "Insert a asort statement."
  (interactive)
  (php-template-generic-function "asort" '("array" "sort_flags") 1))

(defun php-template-compact ()
  "Insert a compact statement."
  (interactive)
  (php-template-generic-function "compact" '("varname" "...") 1 t))

(defun php-template-count ()
  "Insert a count statement."
  (interactive)
  (php-template-generic-function "count" '("var" "mode") 1))

(defun php-template-current ()
  "Insert a current statement."
  (interactive)
  (php-template-generic-function "current" '("array") 1))

(defun php-template-each ()
  "Insert a each statement."
  (interactive)
  (php-template-generic-function "each" '("array") 1))

(defun php-template-end ()
  "Insert a end statement."
  (interactive)
  (php-template-generic-function "end" '("array") 1))

(defun php-template-extract ()
  "Insert a extract statement."
  (interactive)
  (php-template-generic-function "extract" '("var_array" "extract_type" "prefix") 1))

(defun php-template-in-array ()
  "Insert a in_array statement."
  (interactive)
  (php-template-generic-function "in_array" '("needle" "haystack" "strict") 2))

(defun php-template-key ()
  "Insert a key statement."
  (interactive)
  (php-template-generic-function "key" '("array") 1))

(defun php-template-krsort ()
  "Insert a krsort statement."
  (interactive)
  (php-template-generic-function "krsort" '("array" "sort_flags") 1))

(defun php-template-ksort ()
  "Insert a ksort statement."
  (interactive)
  (php-template-generic-function "ksort" '("array" "sort_flags") 1))

(defun php-template-list ()
  "Insert a list statement."
  (interactive)
  (php-template-generic-function "list" '("varname" "...") 1 t))

(defun php-template-natcasesort ()
  "Insert a natcasesort statement."
  (interactive)
  (php-template-generic-function "natcasesort" '("array") 1))

(defun php-template-natsort ()
  "Insert a natsort statement."
  (interactive)
  (php-template-generic-function "natsort" '("array") 1))

(defun php-template-next ()
  "Insert a next statement."
  (interactive)
  (php-template-generic-function "next" '("array") 1))

(defun php-template-pos ()
  "Insert a pos statement."
  (interactive)
  (php-template-current))

(defun php-template-prev ()
  "Insert a prev statement."
  (interactive)
  (php-template-generic-function "prev" '("array") 1))

(defun php-template-range ()
  "Insert a range statement."
  (interactive)
  (php-template-generic-function "range" '("low" "high" "step") 2))

(defun php-template-reset ()
  "Insert a reset statement."
  (interactive)
  (php-template-generic-function "reset" '("array") 1))

(defun php-template-rsort ()
  "Insert a rsort statement."
  (interactive)
  (php-template-generic-function "rsort" '("array" "sort_flags") 1))

(defun php-template-shuffle ()
  "Insert a shuffle statement."
  (interactive)
  (php-template-generic-function "shuffle" '("array") 1))

(defun php-template-sizeof ()
  "Insert a sizeof statement."
  (interactive)
  (php-template-count))

(defun php-template-sort ()
  "Insert a sort statement."
  (interactive)
  (php-template-generic-function "sort" '("array" "sort_flags") 1))

(defun php-template-uasort ()
  "Insert a uasort statement."
  (interactive)
  (php-template-generic-function "uasort" '("array" "cmp_function") 2))

(defun php-template-uksort ()
  "Insert a uksort statement."
  (interactive)
  (php-template-generic-function "uksort" '("array" "cmp_function") 2))

(defun php-template-usort ()
  "Insert a usort statement."
  (interactive)
  (php-template-generic-function "usort" '("array" "cmp_function") 2))

;; Control Structures

(defun php-template-break ()
  "Insert a break statement."
  (interactive)
  (insert "break;")
  (php-indent-line-2))

(defun php-template-continue ()
  "Insert a continue statement."
  (interactive)
  (insert "continue;")
  (php-indent-line-2))

(defun php-template-do-while ()
  "Insert a do-while statement."
  (interactive)
  (let ((start (point)) here)
    (insert "do {")
    (php-indent-line-2)
    (insert "\n")
    (php-indent-line-2)
    (setq here (point-marker))
    (insert "\n")
    (insert "} while ( ")
    (when (php-template-field "condition" nil t start (point))
      (insert " );")
      (php-indent-line-2)
      (goto-char here))))

(defun php-template-else ()
  "Insert an else statement."
  (interactive)
  (let (here)
    (insert "else {\n")
    (setq here (point-marker))
    (newline-and-indent)
    (insert "}")
    (php-indent-line-2)
    (goto-char here)
    (php-indent-line-2)))

(defun php-template-elseif ()
  "Insert an elseif statement."
  (interactive)
  (let ((start (point)))
    (insert "elseif ")
    (insert "( ")
    (when (php-template-field "condition" nil t start (point))
      (insert " ) {")
      (newline-and-indent)
      (setq start (point-marker))
      (insert "\n}")
      (php-indent-line-2)
      (goto-char start))))

(defun php-template-for ()
  "Insert a for statement."
  (interactive)
  (let ((start (point)) here)
    (insert "for( ")
    (when (php-template-field "expr1" nil t start (point))
      (insert "; ")
      (php-template-field "expr2" nil t start (point))
      (insert "; ")
      (php-template-field "expr3" nil t start (point))
      (insert " ) {")
      (newline-and-indent)
      (setq here (point-marker))
      (insert "\n}")
      (php-indent-line-2)
      (goto-char here))))

(defun php-template-foreach ()
  "Insert a foreach statement."
  (interactive)
  (let ((start (point)) here)
    (insert "foreach( ")
    (when (php-template-field "array_expression" nil t start (point))
      (insert " as ")
      (php-template-field "value_expression" nil t start (point))
      (insert " ) {")
      (newline-and-indent)
      (setq here (point-marker))
      (insert "\n}")
      (php-indent-line-2)
      (goto-char here))))

(defun php-template-if ()
  "Insert an if statement."
  (interactive)
  (let ((start (point)))
    (insert "if ")
    (insert "( ")
    (when (php-template-field "condition" nil t start (point))
      (insert " ) {")
      (newline-and-indent)
      (setq start (point-marker))
      (insert "\n}")
      (php-indent-line-2)
      (goto-char start))))

(defun php-template-include-require (label)
  "Insert an include, include_once, require or require_once statement."
  (interactive)
  (let ((start (point)))
    (insert label)
    (if php-include-in-parenthesis 
	(insert "( ")
      (insert " "))
    (when (php-template-field "file" nil t start (point) t "'")
      (if php-include-in-parenthesis 
	  (insert " );")
	(insert ";"))
      (newline-and-indent))))

(defun php-template-include ()
  "Insert an include statement."
  (php-template-include-require "include"))

(defun php-template-include-once ()
  "Insert an include_once statement."
  (php-template-include-require "include_once"))

(defun php-template-require ()
  "Insert a require statement."
  (php-template-include-require "require"))

(defun php-template-require-once ()
  "Insert a require_once statement."
  (php-template-include-require "require_once"))

(defun php-template-return ()
  "Insert a return statement."
  (interactive)
  (let ((start (point)))
    (insert "return ")
    (when (php-template-field "value" nil t start (point))
      (insert ";"))))

(defun php-template-switch ()
  "Insert a switch statement."
  (interactive)
  (let ((start (point)) here)
    (insert "switch( ")
    (when (php-template-field "field" nil t start (point))
      (insert " ) {")
      (newline-and-indent)
      (setq here (point-marker))
      (insert "\n}")
      (php-indent-line-2)
      (goto-char here))))

(defun php-template-while ()
  "Insert a while statement."
  (interactive)
  (let ((start (point)) here)
    (insert "while( ")
    (when (php-template-field "condition" nil t start (point))
      (insert " ) {")
      (newline-and-indent)
      (setq here (point-marker))
      (insert "\n}")
      (php-indent-line-2)
      (goto-char here))))

;; Date and Time Functions

(defun php-template-checkdate ()
  "Insert a checkdate statement."
  (interactive)
  (php-template-generic-function "checkdate" '("month" "day" "year") 3))

(defun php-template-date-default-timezone-get ()
  "Insert a date_default_timezone_get statement."
  (interactive)
  (php-template-generic-function "date_default_timezone_get" '() 0))

(defun php-template-date-default-timezone-set ()
  "Insert a date_default_timezone_set statement."
  (interactive)
  (php-template-generic-function "date_default_timezone_set" '("timezone_identifier") 1))

(defun php-template-date-sunrise ()
  "Insert a date_sunrise statement."
  (interactive)
  (php-template-generic-function "date_sunrise" '("timestamp" "format" "latitude" "longitude" "zenith" "gmt_offset") 0))

(defun php-template-date-sunset ()
  "Insert a date_sunset statement."
  (interactive)
  (php-template-generic-function "date_sunset" '("timestamp" "format" "latitude" "longitude" "zenith" "gmt_offset") 0))

(defun php-template-date ()
  "Insert a date statement."
  (interactive)
  (php-template-generic-function "date" '("format" "timestamp") 1))

(defun php-template-getdate ()
  "Insert a getdate statement."
  (interactive)
  (php-template-generic-function "getdate" '("timestamp") 0))

(defun php-template-gettimeofday ()
  "Insert a gettimeofday statement."
  (interactive)
  (php-template-generic-function "gettimeofday" '("return_float") 0))

(defun php-template-gmdate ()
  "Insert a gmdate statement."
  (interactive)
  (php-template-generic-function "gmdate" '("format" "timestamp") 1))

(defun php-template-gmmktime ()
  "Insert a gmmktime statement."
  (interactive)
  (php-template-generic-function "gmmktime" '("hour" "minute" "second" "month" "day" "year" "is_dst") 0))

(defun php-template-gmstrftime ()
  "Insert a gmstrftime statement."
  (interactive)
  (php-template-generic-function "gmstrftime" '("format" "timestamp") 1))

(defun php-template-idate ()
  "Insert a idate statement."
  (interactive)
  (php-template-generic-function "idate" '("format" "timestamp") 1))

(defun php-template-localtime ()
  "Insert a localtime statement."
  (interactive)
  (php-template-generic-function "localtime" '("timestamp" "is_associative") 0))

(defun php-template-microtime ()
  "Insert a microtime statement."
  (interactive)
  (php-template-generic-function "microtime" '("get_as_float") 0))

(defun php-template-mktime ()
  "Insert a mktime statement."
  (interactive)
  (php-template-generic-function "mktime" '("hour" "minute" "second" "month" "day" "year" "is_dst") 0))

(defun php-template-strftime ()
  "Insert a strftime statement."
  (interactive)
  (php-template-generic-function "strftime" '("format" "timestamp") 1))

(defun php-template-strptime ()
  "Insert a strptime statement."
  (interactive)
  (php-template-generic-function "strptime" '("date" "format") 2))

(defun php-template-strtotime ()
  "Insert a strtotime statement."
  (interactive)
  (php-template-generic-function "strtotime" '("time" "now") 1))

(defun php-template-time ()
  "Insert a time statement."
  (interactive)
  (php-template-generic-function "time" '() 0))

;; Directory functions

(defun php-template-search-opendir ()
  "Search about a opendir statement."
  (let (result found)
    (save-excursion
      (setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\opendir" nil t))
      (if found
	  (setq result (concat "$" (match-string 1)))
	(setq result nil)))
    result))

(defun php-template-directory-generic-function (label field mandatory-count &optional not-result)
  "Generic function template 'result = label( handle, field1, field2...)"
  (interactive)
  (let ((start (point)) remove (here (point)) result-value elt continue field-count 
	stop handle file (stop-handle nil) (comment-end (point)))
    (setq file (php-template-search-opendir))
    (when (not file)
      (insert "/* WARNING: ")
      (insert "'opendir'")
      (insert " statement not found in this file */")
      (newline-and-indent)
      (setq comment-end (point)))
    (when (not not-result)
      (insert " = ")
      (setq remove (point)))
    (insert (concat label "( "))
    (setq here (point-marker))
    (when (not not-result)
      (goto-char comment-end)
      (setq result-value (php-template-field "result_value" nil t))
      (if (not result-value)
	  (delete-region comment-end remove)))
    (goto-char here)
    (if (not file)
	(progn
	  (setq handle (php-template-field "dir_handle" ", "))
	  (if (not handle)
	      (progn (delete-region start (point))
		     (insert (concat label " "))
		     (setq stop-handle t))))
      (insert (concat file ", ")))
    (when (not stop-handle)
      (setq elt field)
      (setq continue t)
      (setq field-count 0)
      (setq stop nil)
      (while (and elt continue)
	(setq result-value (php-template-field (car elt) nil t))
	(if (and (not result-value)
		 (< field-count mandatory-count))
	    (progn (setq continue nil)
		   (delete-region start (point))
		   (insert (concat label " "))
		   (setq stop t))
	  (if (not result-value)
	      (setq continue nil)
	    (insert ", ")))
	(setq field-count (+ 1 field-count))
	(setq elt (cdr elt)))
      (when (not stop)
	(delete-char -2)
	(insert " );")
	(newline-and-indent)))))

(defun php-template-chdir ()
  "Insert a chdir statement."
  (interactive)
  (php-template-generic-function "chdir" '("directory") 1))

(defun php-template-chroot ()
  "Insert a chroot statement."
  (interactive)
  (php-template-generic-function "chroot" '("directory") 1))

(defun php-template-dir ()
  "Insert a dir statement."
  (interactive)
  (php-template-generic-function "dir" '("directory") 1))

(defun php-template-closedir ()
  "Insert a closedir statement."
  (interactive)
  (php-template-directory-generic-function "closedir" '() 0))

(defun php-template-getcwd ()
  "Insert a getcwd statement."
  (interactive)
  (php-template-generic-function "getcwd" '() 0))

(defun php-template-opendir ()
  "Insert a opendir statement."
  (interactive)
  (php-template-generic-function "opendir" '("path" "context") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-closedir))))

(defun php-template-readdir ()
  "Insert a readdir statement."
  (interactive)
  (php-template-directory-generic-function "readdir" '() 0))

(defun php-template-rewinddir ()
  "Insert a rewinddir statement."
  (interactive)
  (php-template-directory-generic-function "rewinddir" '() 0 t))

(defun php-template-scandir ()
  "Insert a scandir statement."
  (interactive)
  (php-template-generic-function "scandir" '("directory" "sorting_order" "context") 1))

;; Error and Logging

(defun php-template-debug-backtrace ()
  "Insert a debug_backtrace statement."
  (interactive)
  (php-template-generic-function "debug_backtrace" '() 0))

(defun php-template-debug-print-backtrace ()
  "Insert a debug_print_backtrace statement."
  (interactive)
  (php-template-generic-function "debug_print_backtrace" '() 0 nil t))

(defun php-template-error-log ()
  "Insert a error_log statement."
  (interactive)
  (php-template-generic-function "error_log" '("message" "message_type" "destination" "extra_headers") 1))

(defun php-template-error-reporting ()
  "Insert a error_reporting statement."
  (interactive)
  (php-template-generic-function "error_reporting" '("level") 0))

(defun php-template-restore-error-handler ()
  "Insert a error_handler statement."
  (interactive)
  (php-template-generic-function "error_handler" '() 0))

(defun php-template-restore-exception-handler ()
  "Insert a restore_exception_handler statement."
  (interactive)
  (php-template-generic-function "restore_exception_handler" '() 0))

(defun php-template-set-error-handler ()
  "Insert a set_error_handler statement."
  (interactive)
  (php-template-generic-function "set_error_handler" '("error_handler" "error_types") 1))

(defun php-template-set-exception-handler ()
  "Insert a set_exception_handler statement."
  (interactive)
  (php-template-generic-function "set_exception_handler" '("exception_handler") 1))

(defun php-template-trigger-error ()
  "Insert a trigger_error statement."
  (interactive)
  (php-template-generic-function "trigger_error" '("error_msg" "error_type") 1))

(defun php-template-user-error ()
  "Insert a user_error statement."
  (interactive)
  (php-template-trigger-error))

;; File System

(defun php-template-basename ()
  "Insert a basename statement."
  (interactive)
  (php-template-generic-function "basename" '("path" "suffix") 1))

(defun php-template-res-f1-function (label field1)
  "Insert a 'res = label( field1 )' statement (like dirname)."
  (interactive)
  (let ((start (point)) here remove result-value)
    (insert " = ")
    (setq remove (point))
    (insert (concat label "( "))
    (setq here (point-marker))
    (goto-char start)
    (setq result-value (php-template-field "result_value" nil t))
    (if (not result-value)
	(delete-region start remove))
    (goto-char here)
    (when (php-template-field field1 nil t start (point))
      (insert " );")
      (newline-and-indent))))
      
(defun php-template-res-f1-f2-function (label field1 field2)
  "Insert a 'res = label( field1, field2 )' statement (like chgrp, chmod)."
  (interactive)
  (let ((start (point)) here remove result-value)
    (insert " = ")
    (setq remove (point))
    (insert (concat label "( "))
    (setq here (point-marker))
    (goto-char start)
    (setq result-value (php-template-field "result_value" nil t))
    (if (not result-value)
	(delete-region start remove))
    (goto-char here)
    (when (php-template-field field1 nil t start (point))
      (insert ", ")
      (php-template-field field2 nil t)
      (insert " );")
      (newline-and-indent))))
      
(defun php-template-chgrp ()
  "Insert a chgrp statement."
  (interactive)
  (php-template-res-f1-f2-function "chgrp" "filename" "group"))
  
(defun php-template-chmod ()
  "Insert a chgrp statement."
  (interactive)
  (php-template-res-f1-f2-function "chmod" "filename" "mode"))
  
(defun php-template-chown ()
  "Insert a chgrp statement."
  (interactive)
  (php-template-res-f1-f2-function "chown" "filename" "user"))

(defun php-template-void-function (label)
  "Insert a 'void label( void )' function type."
  (interactive)
  (insert label)
  (insert "();")
  (newline-and-indent))

(defun php-template-clearstatcache ()
  "Insert a clearstatcache statement."
  (interactive)
  (php-template-void-function "clearstatcache"))

(defun php-template-copy ()
  "Insert a copy statement."
  (interactive)
  (php-template-res-f1-f2-function "copy" "source" "destination"))

(defun php-template-delete ()
  "Insert a delete statement."
  (interactive)
  (php-template-generic-function "delete" '("file") 1 nil t))

(defun php-template-dirname ()
  "Insert a dirname statement."
  (interactive)
  (php-template-res-f1-function "dirname" "path"))

(defun php-template-disk-free-space ()
  "Insert a disk_free_space statement."
  (interactive)
  (php-template-res-f1-function "disk_free_space" "directory"))

(defun php-template-disk-total-space ()
  "Insert a disk_total_space statement."
  (interactive)
  (php-template-res-f1-function "disk_total_space" "directory"))

(defun php-template-search-fopen (&optional include-popen dont-include-fsockopen popen-only)
  "Search about a fopen/fsockopen statement."
  (let (result found)
    (save-excursion
      (if popen-only
	  (setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\popen" nil t))
	(if include-popen
	    (if dont-include-fsockopen
		(setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\(fopen\\|popen\\)" nil t))
	      (setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\(fsockopen\\|fopen\\|popen\\)" nil t)))
	  (if dont-include-fsockopen
	      (setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\fopen" nil t))
	    (setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\(fsockopen\\|fopen\\)" nil t)))))
      (if found
	  (setq result (concat "$" (match-string 1)))
	(setq result nil)))
    result))

(defun php-template-file-generic-function (label field mandatory-count &optional include-popen dont-include-fsockopen popen-only)
  "Generic function template 'result = label( handle, field1, field2...)"
  (interactive)
  (let ((start (point)) remove here result-value elt continue field-count 
	stop file handle (stop-handle nil) (comment-end (point)))
    (setq file (php-template-search-fopen include-popen dont-include-fsockopen popen-only))
    (when (not file)
      (insert "/* WARNING: ")
      (if popen-only
	  (insert "'popen'")
	(insert "'fopen'")
	(if (not dont-include-fsockopen)
	    (insert ", 'fsockopen'"))
	(if include-popen
	    (insert " or 'popen'")))
      (insert " statement not found in this file */")
      (newline-and-indent)
      (setq comment-end (point)))
    (insert " = ")
    (setq remove (point))
    (insert (concat label "( "))
    (setq here (point-marker))
    (goto-char comment-end)
    (setq result-value (php-template-field "result_value" nil t))
    (if (not result-value)
	(delete-region comment-end remove))
    (goto-char here)
    (if (not file)
	(progn
	  (setq handle (php-template-field "handle" ", "))
	  (if (not handle)
	      (progn (delete-region start (point))
		     (insert (concat label " "))
		     (setq stop-handle t))))
      (insert (concat file ", ")))
    (when (not stop-handle)
      (setq elt field)
      (setq continue t)
      (setq field-count 0)
      (setq stop nil)
      (while (and elt continue)
	(setq result-value (php-template-field (car elt) nil t))
	(if (and (not result-value)
		 (< field-count mandatory-count))
	    (progn (setq continue nil)
		   (delete-region start (point))
		   (insert (concat label " "))
		   (setq stop t))
	  (if (not result-value)
	      (setq continue nil)
	    (insert ", ")))
	(setq field-count (+ 1 field-count))
	(setq elt (cdr elt)))
      (when (not stop)
	(delete-char -2)
	(insert " );")
	(newline-and-indent)))))

(defun php-template-fclose ()
  "Insert a fclose statement."
  (interactive)
  (php-template-file-generic-function "fclose" '() 0))

(defun php-template-feof ()
  "Insert a feof statement."
  (interactive)
  (php-template-file-generic-function "feof" '() 0 t))

(defun php-template-fflush ()
  "Insert a fflush statement."
  (interactive)
  (php-template-file-generic-function "fflush" '() 0 t))

(defun php-template-fgetc ()
  "Insert a fgetc statement."
  (interactive)
  (php-template-file-generic-function "fgetc" '() 0 t))

(defun php-template-fgetcsv ()
  "Insert a fgetcsv statement."
  (interactive)
  (php-template-file-generic-function "fgetcsv" '("length" "delimiter" "enclosure") 0 t))

(defun php-template-fgets ()
  "Insert a fgets statement."
  (interactive)
  (php-template-file-generic-function "fgets" '("length") 0))

(defun php-template-fgetss ()
  "Insert a fgetss statement."
  (interactive)
  (php-template-file-generic-function "fgetss" '("length" "allowable_tags") 0))

(defun php-template-file-exists ()
  "Insert a file_exists statements."
  (interactive)
  (php-template-res-f1-function "file_exists" "filename"))

(defun php-template-generic-function (label field mandatory-count &optional infinite not-result)
  "Generic function template 'result = label( field1, field2...)"
  (interactive)
  (let ((start (point)) remove here result-value elt continue field-count stop prompt result-present
	(last-line (point)))
    (if (re-search-backward ";" nil t)
	(progn
	  (setq last-line (point-marker))
	  (goto-char start)
	  (setq result-present (re-search-backward "=" last-line t)))
      (setq result-present (re-search-backward "=" nil t)))
    (goto-char start)
    (when (and (not not-result) (not result-present))
      (insert " = ")
      (setq remove (point)))
    (insert (concat label "( "))
    (setq here (point-marker))
    (when (and (not not-result) (not result-present))
      (goto-char start)
      (setq result-value (php-template-field "result_value" nil t))
      (if (not result-value)
	  (delete-region start remove)))
    (goto-char here)
    (setq elt field)
    (setq continue t)
    (setq field-count 0)
    (setq stop nil)
    (while (and elt continue)
      (setq prompt (car elt))
      (setq result-value (php-template-field prompt nil t))
      (if (and (not result-value)
	       (< field-count mandatory-count))
	  (progn (setq continue nil)
		 (delete-region start (point))
		 (insert (concat label " "))
		 (setq stop t))
	(if (not result-value)
	    (setq continue nil)
	  (insert ", ")))
      (setq field-count (+ 1 field-count))
      (setq elt (cdr elt)))
    (when (and infinite continue)
      (while continue
	(setq result-value (php-template-field prompt nil t))
	(if (and (not result-value)
		 (< field-count mandatory-count))
	    (progn (setq continue nil)
		   (delete-region start (point))
		   (insert (concat label " "))
		   (setq stop t))
	  (if (not result-value)
	      (setq continue nil)
	    (insert ", ")))
	(setq field-count (+ 1 field-count))))
    (when (not stop)
      (if (or (< field-count 1)
	      (and (< field-count 2) (not continue))) 
	  (delete-char -1)
	(delete-char -2)
	(insert " "))
      (insert ");")
      (newline-and-indent))))

(defun php-template-file-get-contents ()
  "Insert a file_get_contents statement."
  (interactive)
  (php-template-generic-function "file_get_contents" '("filename" "use_include_path" "context" "offset" "maxlen") 1))

(defun php-template-file-put-contents ()
  "Insert a file_put_contents statement."
  (interactive)
  (php-template-generic-function "file_put_contents" '("filename" "data" "flags" "context") 2))

(defun php-template-file ()
  "Insert a file statement."
  (interactive)
  (php-template-generic-function "file" '("filename" "use_include_path" "context") 1))

(defun php-template-fileatime ()
  "Insert a fileatime statement."
  (interactive)
  (php-template-generic-function "fileatime" '("filename") 1))

(defun php-template-filectime ()
  "Insert a fileatime statement."
  (interactive)
  (php-template-generic-function "fileatime" '("filename") 1))

(defun php-template-filegroup ()
  "Insert a filegroup statement."
  (interactive)
  (php-template-generic-function "filegroup" '("filename") 1))

(defun php-template-fileinode ()
  "Insert a fileinode statement."
  (interactive)
  (php-template-generic-function "fileinode" '("filename") 1))

(defun php-template-filemtime ()
  "Insert a filemtime statement."
  (interactive)
  (php-template-generic-function "filemtime" '("filename") 1))

(defun php-template-fileowner ()
  "Insert a fileowner statement."
  (interactive)
  (php-template-generic-function "fileowner" '("filename") 1))

(defun php-template-fileperms ()
  "Insert a fileperms statement."
  (interactive)
  (php-template-generic-function "fileperms" '("filename") 1))

(defun php-template-filesize ()
  "Insert a filesize statement."
  (interactive)
  (php-template-generic-function "filesize" '("filename") 1))

(defun php-template-filetype ()
  "Insert a filetype statement."
  (interactive)
  (php-template-generic-function "filetype" '("filename") 1))

(defun php-template-flock ()
  "Insert a flock statement."
  (interactive)
  (php-template-file-generic-function "flock" '("LOCK_SH | LOCK_EX | LOCK_UN | LOCK_NB" "wouldblock") 1 nil t))

(defun php-template-fnmatch ()
  "Insert a fnmatch statement."
  (interactive)
  (php-template-generic-function "fnmatch" '("pattern" "string" "flags") 2))

(defun php-template-fopen ()
  "Inser an fopen statement."
  (interactive)
  (php-template-generic-function "fopen" '("filename" "mode" "use_include_path" "zcontext") 2)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-fclose))))

(defun php-template-fpassthru ()
  "Insert a fpassthru statement."
  (interactive)
  (php-template-file-generic-function "fpassthru" '() 0 nil t))

(defun php-template-fputcsv ()
  "Insert a fputcsv statement."
  (interactive)
  (php-template-file-generic-function "fputcsv" '("fields" "delimiter" "enclosure") 0 t))

(defun php-template-fread ()
  "Insert a fread statement."
  (interactive)
  (php-template-file-generic-function "fread" '("length") 1 t))

;(defun php-template-fscanf ()
;  "Insert a fscanf statement.")

(defun php-template-fseek ()
  "Insert a fseek statement."
  (interactive)
  (php-template-file-generic-function "fseek" '("offset" "whence") 1 nil t))

(defun php-template-fstat ()
  "Insert a fstat statement."
  (interactive)
  (php-template-file-generic-function "fstat" '() 0 nil t))

(defun php-template-ftell ()
  "Insert a ftell statement."
  (interactive)
  (php-template-file-generic-function "ftell" '() 0 nil t))

(defun php-template-ftruncate ()
  "Insert a ftruncate statement."
  (interactive)
  (php-template-file-generic-function "ftruncate" '("size") 1 nil t))

(defun php-template-fwrite ()
  "Insert a fwrite statement."
  (interactive)
  (php-template-file-generic-function "fwrite" '("string" "length") 1 nil t))

(defun php-template-glob ()
  "Insert a glob statement."
  (interactive)
  (php-template-generic-function "glob" '("pattern" "flags") 1))

(defun php-template-is-dir ()
  "Insert an is_dir statement."
  (interactive)
  (php-template-generic-function "is_dir" '("filename") 1))

(defun php-template-is-executable ()
  "Insert an is_executable statement."
  (interactive)
  (php-template-generic-function "is_executable" '("filename") 1))

(defun php-template-is-file ()
  "Insert an is_file statement."
  (interactive)
  (php-template-generic-function "is_file" '("filename") 1))

(defun php-template-is-link ()
  "Insert an is_link statement."
  (interactive)
  (php-template-generic-function "is_link" '("filename") 1))

(defun php-template-is-readable ()
  "Insert an is_readable statement."
  (interactive)
  (php-template-generic-function "is_readable" '("filename") 1))

(defun php-template-is-uploaded-file ()
  "Insert an is_uploaded_file statement."
  (interactive)
  (php-template-generic-function "is_uploaded_file" '("filename") 1))

(defun php-template-is-writable ()
  "Insert an is_writable statement."
  (interactive)
  (php-template-generic-function "is_writable" '("filename") 1))

(defun php-template-link ()
  "Insert a link statement."
  (interactive)
  (php-template-generic-function "link" '("target" "link") 2))

(defun php-template-linkinfo ()
  "Insert a linkinfo statement."
  (interactive)
  (php-template-generic-function "linkinfo" '("path") 1))

(defun php-template-lstat ()
  "Insert a lstat statement."
  (interactive)
  (php-template-generic-function "lstat" '("filename") 1))

(defun php-template-mkdir ()
  "Insert a mkdir statement."
  (interactive)
  (php-template-generic-function "mkdir" '("pathname" "mode" "recursive" "context") 1))

(defun php-template-move-uploaded-file ()
  "Insert a move_uploaded_file statement."
  (interactive)
  (php-template-generic-function "move_uploaded_file" '("filename" "destination") 2))

(defun php-template-parse-ini-file ()
  "Insert a parse_ini_file statement."
  (interactive)
  (php-template-generic-function "parse_ini_file" '("filename" "process_sections") 1))

(defun php-template-pathinfo ()
  "Insert a pathinfo statement."
  (interactive)
  (php-template-generic-function "pathinfo" '("path" "options") 1))

(defun php-template-pclose ()
  "Insert a pclose statement."
  (interactive)
  (php-template-file-generic-function "pclose" '() 0 nil nil t))

(defun php-template-popen ()
  "Inser a popen statement."
  (interactive)
  (php-template-generic-function "popen" '("command" "mode") 2)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-pclose))))

(defun php-template-readfile ()
  "Insert a readfile statement."
  (interactive)
  (php-template-generic-function "readfile" '("filename" "use_include_path" "context") 1))

(defun php-template-readlink ()
  "Insert a readlink statement."
  (interactive)
  (php-template-generic-function "readlink" '("path") 0))

(defun php-template-realpath ()
  "Insert a realpath statement."
  (interactive)
  (php-template-generic-function "realpath" '("path") 1))

(defun php-template-rename ()
  "Insert a rename statement."
  (interactive)
  (php-template-generic-function "rename" '("oldname" "newname" "context") 2))

(defun php-template-rewind ()
  "Insert a rewind statement."
  (interactive)
  (php-template-file-generic-function "rewind" '() 0 nil t))

(defun php-template-rmdir ()
  "Insert a rmdir statement."
  (interactive)
  (php-template-generic-function "rmdir" '("dirname" "context") 1))

(defun php-template-stat ()
  "Insert a stat statement."
  (interactive)
  (php-template-generic-function "stat" '("filename") 1))

(defun php-template-symlink ()
  "Insert a symlink statement."
  (interactive)
  (php-template-generic-function "symlink" '("target" "link") 2))

(defun php-template-tempnam ()
  "Insert a tempnam statement."
  (interactive)
  (php-template-generic-function "tempnam" '("dir" "prefix") 2))

(defun php-template-tmpfilet ()
  "Insert a tmpfile statement."
  (interactive)
  (php-template-generic-function "tmpfile" '() 0))

(defun php-template-touch ()
  "Insert a touch statement."
  (interactive)
  (php-template-generic-function "touch" '("filename" "time" "atime") 1))

(defun php-template-umask ()
  "Insert an umask statement."
  (interactive)
  (php-template-generic-function "umask" '("mask") 0))

(defun php-template-unlink ()
  "Insert an unlink statement."
  (interactive)
  (php-template-generic-function "unlink" '("filename" "context") 1))

;; Functions

(defun php-template-call-user-func-array ()
  "Insert an call_user_func_array statement."
  (interactive)
  (php-template-generic-function "call_user_func_array" '("function" "param_arr") 2))

(defun php-template-call-user-func ()
  "Insert an call_user_func statement."
  (interactive)
  (php-template-generic-function "call_user_func" '("function" "parameter" "...") 1 t))

(defun php-template-create-function ()
  "Insert an create_function statement."
  (interactive)
  (php-template-generic-function "create_function" '("args" "code") 2))

(defun php-template-func-get-arg ()
  "Insert an func_get_arg statement."
  (interactive)
  (php-template-generic-function "func_get_arg" '("arg_num") 1))

(defun php-template-func-get-args ()
  "Insert an func_get_args statement."
  (interactive)
  (php-template-generic-function "func_get_args" '() 0))

(defun php-template-func-num-args ()
  "Insert an func_num_args statement."
  (interactive)
  (php-template-generic-function "func_num_args" '() 0))

(defun php-template-function-exists ()
  "Insert an function_exists statement."
  (interactive)
  (php-template-generic-function "function_exists" '("function_name") 1))

(defun php-template-get-defined-functions ()
  "Insert an get_defined_functions statement."
  (interactive)
  (php-template-generic-function "get_defined_functions" '() 0))

(defun php-template-register-shutdown-function ()
  "Insert an register_shutdown_function statement."
  (interactive)
  (php-template-generic-function "register_shutdown_function" '("function" "parameter" "...") 1 t))

(defun php-template-register-tick-function ()
  "Insert an register_tick_function statement."
  (interactive)
  (php-template-generic-function "register_tick_function" '("function" "arg" "...") 1 t))

(defun php-template-unregister-tick-function ()
  "Insert an unregister_tick_function statement."
  (interactive)
  (php-template-generic-function "unregister_tick_function" '("function_name") 1))

;; Image

(defun php-template-search-imagecreate ()
  "Search about a mysl_connect statement."
  (let (result found)
    (save-excursion
      (setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\imagecreate" nil t))
      (if found
	  (setq result (concat "$" (match-string 1)))
	(setq result nil)))
    result))

(defun php-template-image-generic-function (label field mandatory-count)
  "Generic function template 'result = label( image, field1, field2...)"
  (interactive)
  (let ((start (point)) remove here result-value elt continue field-count 
	stop file image (stop-handle nil) (comment-end (point)))
    (setq file (php-template-search-imagecreate))
    (when (not file)
      (insert "/* WARNING: ")
      (insert "'imagedcreate...'")
      (insert " statement not found in this file */")
      (newline-and-indent)
      (setq comment-end (point)))
    (insert " = ")
    (setq remove (point))
    (insert (concat label "( "))
    (setq here (point-marker))
    (goto-char comment-end)
    (setq result-value (php-template-field "result_value" nil t))
    (if (not result-value)
	(delete-region comment-end remove))
    (goto-char here)
    (if (not file)
	(progn
	  (setq image (php-template-field "image" ", "))
	  (if (not image)
	      (progn (delete-region start (point))
		     (insert (concat label " "))
		     (setq stop-handle t))))
      (insert (concat file ", ")))
    (when (not stop-handle)
      (setq elt field)
      (setq continue t)
      (setq field-count 0)
      (setq stop nil)
      (while (and elt continue)
	(setq result-value (php-template-field (car elt) nil t))
	(if (and (not result-value)
		 (< field-count mandatory-count))
	    (progn (setq continue nil)
		   (delete-region start (point))
		   (insert (concat label " "))
		   (setq stop t))
	  (if (not result-value)
	      (setq continue nil)
	    (insert ", ")))
	(setq field-count (+ 1 field-count))
	(setq elt (cdr elt)))
      (when (not stop)
	(delete-char -2)
	(insert " );")
	(newline-and-indent)))))

(defun php-template-gd-info ()
  "Insert a gd_info statement."
  (interactive)
  (php-template-generic-function "gd_info" '() 0))

(defun php-template-getimagesize ()
  "Insert a getimagesize statement."
  (interactive)
  (php-template-generic-function "getimagesize" '("filename" "imageinfo") 1))

(defun php-template-image-type-to-extension ()
  "Insert a image_type_to_extension statement."
  (interactive)
  (php-template-generic-function "image_type_to_extension" '("imagetype" "include_dot") 1))

(defun php-template-image-type-to-mime-type ()
  "Insert a image_type_to_mime_type statement."
  (interactive)
  (php-template-generic-function "image_type_to_mime_type" '("imagetype") 1))

(defun php-template-image2wbmp ()
  "Insert a image2wbmp statement."
  (interactive)
  (php-template-generic-function "image2wbmp" '("filename" "threshold") 1))

(defun php-template-imagealphablending ()
  "Insert a imagealphablending statement."
  (interactive)
  (php-template-image-generic-function "imagealphablending" '("blendmode") 1))

(defun php-template-imageantialias ()
  "Insert a imageantialias statement."
  (interactive)
  (php-template-image-generic-function "imageantialias" '("on") 1))

(defun php-template-imagearc ()
  "Insert a imagearc statement."
  (interactive)
  (php-template-image-generic-function "imagearc" '("cx" "cy" "w" "h" "s" "e" "color") 7))

(defun php-template-imagechar ()
  "Insert a imagechar statement."
  (interactive)
  (php-template-image-generic-function "imagechar" '("font" "x" "y" "c" "color") 5))

(defun php-template-imagecharup ()
  "Insert a imagecharup statement."
  (interactive)
  (php-template-image-generic-function "imagecharup" '("font" "x" "y" "c" "color") 5))

(defun php-template-imagecolorallocate ()
  "Insert a imagecolorallocate statement."
  (interactive)
  (php-template-image-generic-function "imagecolorallocate" '("red" "green" "blue") 3))

(defun php-template-imagecolorallocatealpha ()
  "Insert a imagecolorallocatealpha statement."
  (interactive)
  (php-template-image-generic-function "imagecolorallocatealpha" '("red" "green" "blue" "alpha") 4))

(defun php-template-imagecolorat ()
  "Insert a imagecolorat statement."
  (interactive)
  (php-template-image-generic-function "imagecolorat" '("x" "y") 2))

(defun php-template-imagecolorclosest ()
  "Insert a imagecolorclosest statement."
  (interactive)
  (php-template-image-generic-function "imagecolorclosest" '("red" "green" "blue") 3))

(defun php-template-imagecolorclosestalpha ()
  "Insert a imagecolorclosestalpha statement."
  (interactive)
  (php-template-image-generic-function "imagecolorclosestalpha" '("red" "green" "blue" "alpha") 4))

(defun php-template-imagecolorclosesthwb ()
  "Insert a imagecolorclosesthwb statement."
  (interactive)
  (php-template-image-generic-function "imagecolorclosesthwb" '("red" "green" "blue") 3))

(defun php-template-imagecolordeallocate ()
  "Insert a imagecolordeallocate statement."
  (interactive)
  (php-template-image-generic-function "imagecolordeallocate" '("color") 1))

(defun php-template-imagecolorexact ()
  "Insert a imagecolorexact statement."
  (interactive)
  (php-template-image-generic-function "imagecolorexact" '("red" "green" "blue" ) 3))

(defun php-template-imagecolorexactalpha ()
  "Insert a imagecolorexactalpha statement."
  (interactive)
  (php-template-image-generic-function "imagecolorexactalpha" '("red" "green" "blue" "alpha") 4))

(defun php-template-imagecolormatch ()
  "Insert a imagecolormatch statement."
  (interactive)
  (php-template-image-generic-function "imagecolormatch" '("image2") 1))

(defun php-template-imagecolorresolve ()
  "Insert a imagecolorresolve statement."
  (interactive)
  (php-template-image-generic-function "imagecolorresolve" '("red" "green" "blue") 3))

(defun php-template-imagecolorresolvealpha ()
  "Insert a imagecolorresolvealpha statement."
  (interactive)
  (php-template-image-generic-function "imagecolorresolvealpha" '("red" "green" "blue" "alpha") 4))

(defun php-template-imagecolorset ()
  "Insert a imagecolorset statement."
  (interactive)
  (php-template-image-generic-function "imagecolorset" '("index" "red" "green" "blue") 4))

(defun php-template-imagecolorsforindex ()
  "Insert a imagecolorsforindex statement."
  (interactive)
  (php-template-image-generic-function "imagecolorsforindex" '("index") 1))

(defun php-template-imagecolorstotal ()
  "Insert a imagecolorstotal statement."
  (interactive)
  (php-template-image-generic-function "imagecolorstotal" '() 0))

(defun php-template-imagecolortransparent ()
  "Insert a imagecolortransparent statement."
  (interactive)
  (php-template-image-generic-function "imagecolortransparent" '("color") 1))

(defun php-template-imageconvolution ()
  "Insert a imageconvolution statement."
  (interactive)
  (php-template-image-generic-function "imageconvolution" '("matrix3x3" "div" "offset") 3))

(defun php-template-imagecopy ()
  "Insert a imagecopy statement."
  (interactive)
  (php-template-image-generic-function "imagecopy" '("src_im" "dst_x" "dst_y" "src_x" "src_y" "src_w" "src_h") 7))

(defun php-template-imagecopymerge ()
  "Insert a imagecopymerge statement."
  (interactive)
  (php-template-image-generic-function "imagecopymerge" '("src_im" "dst_x" "dst_y" "src_x" "src_y" "src_w" "src_h" "pct") 8))

(defun php-template-imagecopymergegray ()
  "Insert a imagecopymergegray statement."
  (interactive)
  (php-template-image-generic-function "imagecopymergegray" '("src_im" "dst_x" "dst_y" "src_x" "src_y" "src_w" "src_h" "pct") 8))

(defun php-template-imagecopyresampled ()
  "Insert a imagecopyresampled statement."
  (interactive)
  (php-template-image-generic-function "imagecopyresampled" '("src_image" "dst_x" "dst_y" "src_x" "src_y" "dst_w" "dst_h" "src_w" "src_h") 9))

(defun php-template-imagecopyresized ()
  "Insert a imagecopyresized statement."
  (interactive)
  (php-template-image-generic-function "imagecopyresized" '("src_image" "dst_x" "dst_y" "src_x" "src_y" "dst_w" "dst_h" "src_w" "src_h") 9))

(defun php-template-imagecreate ()
  "Insert a imagecreate statement."
  (interactive)
  (php-template-generic-function "imagecreate" '("x_size" "y_size") 2)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromgd2 ()
  "Insert a imagecreatefromgd2 statement."
  (interactive)
  (php-template-generic-function "imagecreatefromgd2" '("filename") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromgd2part ()
  "Insert a imagecreatefromgd2part statement."
  (interactive)
  (php-template-generic-function "imagecreatefromgd2part" '("filename" "srcX" "srcY" "width" "height") 5)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromgd ()
  "Insert a imagecreatefromgd statement."
  (interactive)
  (php-template-generic-function "imagecreatefromgd" '("filename") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromgif ()
  "Insert a imagecreatefromgif statement."
  (interactive)
  (php-template-generic-function "imagecreatefromgif" '("filename") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromjpeg ()
  "Insert a imagecreatefromjpeg statement."
  (interactive)
  (php-template-generic-function "imagecreatefromjpeg" '("filename") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefrompng ()
  "Insert a imagecreatefrompng statement."
  (interactive)
  (php-template-generic-function "imagecreatefrompng" '("filename") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromstring ()
  "Insert a imagecreatefromstring statement."
  (interactive)
  (php-template-generic-function "imagecreatefromstring" '("image") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromwbmp ()
  "Insert a imagecreatefromwbmp statement."
  (interactive)
  (php-template-generic-function "imagecreatefromwbmp" '("filename") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromxbm ()
  "Insert a imagecreatefromxbm statement."
  (interactive)
  (php-template-generic-function "imagecreatefromxbm" '("filename") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatefromxpm ()
  "Insert a imagecreatefromxpm statement."
  (interactive)
  (php-template-generic-function "imagecreatefromxpm" '("fileanme") 1)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagecreatetruecolor ()
  "Insert a imagecreatetruecolor statement."
  (interactive)
  (php-template-generic-function "imagecreatetruecolor" '("x_size" "y_size") 2)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-imagedestroy))))

(defun php-template-imagedashedline ()
  "Insert a imagedashedline statement."
  (interactive)
  (php-template-image-generic-function "imagedashedline" '("x1" "y1" "x2" "y2" "color") 5))

(defun php-template-imagedestroy ()
  "Insert a imagedestroy statement."
  (interactive)
  (php-template-image-generic-function "imagedestroy" '() 0))

(defun php-template-imageellipse ()
  "Insert a imageellipse statement."
  (interactive)
  (php-template-image-generic-function "imageellipse" '("cx" "cy" "w" "h" "color") 5))

(defun php-template-imagefill ()
  "Insert a imagefill statement."
  (interactive)
  (php-template-image-generic-function "imagefill" '("x" "y" "color") 3))

(defun php-template-imagefilledarc ()
  "Insert a imagefilledarc statement."
  (interactive)
  (php-template-image-generic-function "imagefilledarc" '("cx" "cy" "w" "h" "s" "e" "color" "style") 8))

(defun php-template-imagefilledellipse ()
  "Insert a imagefilledellipse statement."
  (interactive)
  (php-template-image-generic-function "imagefilledellipse" '("cx" "cy" "w" "h" "color") 5))

(defun php-template-imagefilledpolygon ()
  "Insert a imagefilledpolygon statement."
  (interactive)
  (php-template-image-generic-function "imagefilledpolygon" '("points" "num_points" "color") 3))

(defun php-template-imagefilledrectangle ()
  "Insert a imagefilledrectangle statement."
  (interactive)
  (php-template-image-generic-function "imagefilledrectangle" '("x1" "y1" "x2" "y2" "color") 5))

(defun php-template-imagefilltoborder ()
  "Insert a imagefilltoborder statement."
  (interactive)
  (php-template-image-generic-function "imagefilltoborder" '("x" "y" "border" "color") 4))

(defun php-template-imagefilter ()
  "Insert a imagefilter statement."
  (interactive)
  (php-template-image-generic-function "imagefilter" '("filtertype" "arg1" "arg2" "arg3") 1))

(defun php-template-imagefontheight ()
  "Insert a imagefontheight statement."
  (interactive)
  (php-template-generic-function "imagefontheight" '("font") 1))

(defun php-template-imagefontwidth ()
  "Insert a imagefontwidth statement."
  (interactive)
  (php-template-generic-function "imagefontwidth" '("font") 1))

(defun php-template-imageftbbox ()
  "Insert a imageftbbox statement."
  (interactive)
  (php-template-generic-function "imageftbbox" '("size" "angle" "font_file" "text" "extrainfo") 4))

(defun php-template-imagefttext ()
  "Insert a imagefttext statement."
  (interactive)
  (php-template-image-generic-function "imagefttext" '("size" "angle" "x" "y" "col" "font_file" "text" "extrainfo") 7))

(defun php-template-imagegammacorrect ()
  "Insert a imagegammacorrect statement."
  (interactive)
  (php-template-image-generic-function "imagegammacorrect" '("inputgamma" "outputgamma") 2))

(defun php-template-imagegd2 ()
  "Insert a imagegd2 statement."
  (interactive)
  (php-template-image-generic-function "imagegd2" '("filename" "chunk_size" "type") 0))

(defun php-template-imagegd ()
  "Insert a imagegd statement."
  (interactive)
  (php-template-image-generic-function "imagegd" '("filename") 0))

(defun php-template-imagegif ()
  "Insert a imagegif statement."
  (interactive)
  (php-template-image-generic-function "imagegif" '("filename") 0))

(defun php-template-imageinterlace ()
  "Insert a imageinterlace statement."
  (interactive)
  (php-template-image-generic-function "imageinterlace" '("interlace") 0))

(defun php-template-imageistruecolor ()
  "Insert a imageistruecolor statement."
  (interactive)
  (php-template-image-generic-function "imageistruecolor" '() 0))

(defun php-template-imagejpeg ()
  "Insert a imagejpeg statement."
  (interactive)
  (php-template-image-generic-function "imagejpeg" '("filename" "quality") 0))

(defun php-template-imagelayereffect ()
  "Insert a imagelayereffect statement."
  (interactive)
  (php-template-image-generic-function "imagelayereffect" '("effect") 1))

(defun php-template-imageline ()
  "Insert a imageline statement."
  (interactive)
  (php-template-image-generic-function "imageline" '("x1" "y1" "x2" "y2" "color") 5))

(defun php-template-imageloadfont ()
  "Insert a imageloadfont statement."
  (interactive)
  (php-template-generic-function "imageloadfont" '("file") 1))

(defun php-template-imagepalettecopy ()
  "Insert a imagepalettecopy statement."
  (interactive)
  (php-template-image-generic-function "imagepalettecopy" '("source") 1 t))

(defun php-template-imagepng ()
  "Insert a imagepng statement."
  (interactive)
  (php-template-image-generic-function "imagepng" '("filename") 0))

(defun php-template-imagepolygon ()
  "Insert a imagepolygon statement."
  (interactive)
  (php-template-image-generic-function "imagepolygon" '("points" "num_points" "color") 3))

(defun php-template-imagepsbbox ()
  "Insert a imagepsbbox statement."
  (interactive)
  (php-template-generic-function "imagepsbbox" '("text" "font" "size" "space" "tightness" "angle") 3))

(defun php-template-imagepsencodefont ()
  "Insert a imagepsencodefont statement."
  (interactive)
  (php-template-generic-function "imagepsencodefont" '("font_index" "encodingfile") 2))

(defun php-template-imagepsextendfont ()
  "Insert a imagepsextendfont statement."
  (interactive)
  (php-template-generic-function "imagepsextendfont" '("font_index" "extend") 2))

(defun php-template-imagepsfreefont ()
  "Insert a imagepsfreefont statement."
  (interactive)
  (php-template-generic-function "imagepsfreefont" '("fontindex") 1))

(defun php-template-imagepsloadfont ()
  "Insert a imagepsloadfont statement."
  (interactive)
  (php-template-generic-function "imagepsloadfont" '("filename") 1))

(defun php-template-imagepsslantfont ()
  "Insert a imagepsslantfont statement."
  (interactive)
  (php-template-generic-function "imagepsslantfont" '("font_index" "slant") 2))

(defun php-template-imagepstext ()
  "Insert a imagepstext statement."
  (interactive)
  (php-template-image-generic-function "imagepstext" '("text" "font" "size" "foreground" "background" "x" "y" "space" "tightness" "angle" "antialias_steps") 7))

(defun php-template-imagerectangle ()
  "Insert a imagerectangle statement."
  (interactive)
  (php-template-image-generic-function "imagerectangle" '("x1" "y1" "x2" "y2" "col") 5))

(defun php-template-imagerotate ()
  "Insert a imagerotate statement."
  (interactive)
  (php-template-image-generic-function "imagerotate" '("angle" "bgd_color" "ignore_transparent") 2))

(defun php-template-imagesavealpha ()
  "Insert a imagesavealpha statement."
  (interactive)
  (php-template-image-generic-function "imagesavealpha" '("saveflag") 1))

(defun php-template-imagesetbrush ()
  "Insert a imagesetbrush statement."
  (interactive)
  (php-template-image-generic-function "imagesetbrush" '("brush") 1))

(defun php-template-imagesetpixel ()
  "Insert a imagesetpixel statement."
  (interactive)
  (php-template-image-generic-function "imagesetpixel" '("x" "y" "color") 3))

(defun php-template-imagesetstyle ()
  "Insert a imagesetstyle statement."
  (interactive)
  (php-template-image-generic-function "imagesetstyle" '("style") 1))

(defun php-template-imagesetthickness ()
  "Insert a imagesetthickness statement."
  (interactive)
  (php-template-image-generic-function "imagesetthickness" '("thickness") 1))

(defun php-template-imagesettile ()
  "Insert a imagesettile statement."
  (interactive)
  (php-template-image-generic-function "imagesettile" '("tile") 1))

(defun php-template-imagestring ()
  "Insert a imagestring statement."
  (interactive)
  (php-template-image-generic-function "imagestring" '("font" "x" "y" "s" "col") 5))

(defun php-template-imagestringup ()
  "Insert a imagestringup statement."
  (interactive)
  (php-template-image-generic-function "imagestringup" '("font" "x" "y" "s" "col") 5))

(defun php-template-imagesx ()
  "Insert a imagesx statement."
  (interactive)
  (php-template-image-generic-function "imagesx" '() 0))

(defun php-template-imagesy ()
  "Insert a imagesy statement."
  (interactive)
  (php-template-image-generic-function "imagesy" '() 0))

(defun php-template-imagetruecolortopalette ()
  "Insert a imagetruecolortopalette statement."
  (interactive)
  (php-template-image-generic-function "imagetruecolortopalette" '("dither" "ncolors") 2))

(defun php-template-imagettfbbox ()
  "Insert a imagettfbbox statement."
  (interactive)
  (php-template-generic-function "imagettfbbox" '("size" "angle" "fontfile" "text") 4))

(defun php-template-imagettftext ()
  "Insert a imagettftext statement."
  (interactive)
  (php-template-image-generic-function "imagettftext" '("size" "angle" "x" "y" "color" "fontfile" "text") 7))

(defun php-template-imagetypes ()
  "Insert a imagetypes statement."
  (interactive)
  (php-template-image-generic-function "imagetypes" '() 0))

(defun php-template-imagewbmp ()
  "Insert a imagewbmp statement."
  (interactive)
  (php-template-image-generic-function "imagewbmp" '("filename" "foreground") 1))

(defun php-template-imagexbm ()
  "Insert a imagexbm statement."
  (interactive)
  (php-template-image-generic-function "imagexbm" '("filename" "foreground") 1))

(defun php-template-iptcembed ()
  "Insert a iptcembed statement."
  (interactive)
  (php-template-generic-function "iptcembed" '("iptcdata" "jpeg_file_name" "spool") 2))

(defun php-template-iptcparse ()
  "Insert a iptcparse statement."
  (interactive)
  (php-template-generic-function "iptcparse" '("iptcblock") 1))

(defun php-template-jpeg2wbmp ()
  "Insert a jpeg2wbmp statement."
  (interactive)
  (php-template-generic-function "jpeg2wbmp" '("jpegname" "wbmpname" "d_height" "d_width" "threshold") 5))

(defun php-template-png2wbmp ()
  "Insert a png2wbmp statement."
  (interactive)
  (php-template-generic-function "png2wbmp" '("pngname" "wbmpname" "d_height" "d_width" "threshold") 5))

;; Mail

(defun php-template-ezmlm-hash ()
  "Insert a ezmlm_hash statement."
  (interactive)
  (php-template-generic-function "ezmlm_hash" '("addr") 1))

(defun php-template-mail ()
  "Insert a mail statement."
  (interactive)
  (php-template-generic-function "mail" '("to" "subject" "message" "additional_headers" "additional_parameters") 3))

;; Mathematical

(defun php-template-abs ()
  "Insert a abs statement."
  (interactive)
  (php-template-generic-function "abs" '("number") 1))

(defun php-template-acos ()
  "Insert a acos statement."
  (interactive)
  (php-template-generic-function "acos" '("arg") 1))

(defun php-template-acosh ()
  "Insert a acosh statement."
  (interactive)
  (php-template-generic-function "acosh" '("arg") 1))

(defun php-template-asin ()
  "Insert a asin statement."
  (interactive)
  (php-template-generic-function "asin" '("arg") 1))

(defun php-template-asinh ()
  "Insert a asinh statement."
  (interactive)
  (php-template-generic-function "asinh" '("arg") 1))

(defun php-template-atan2 ()
  "Insert a atan2 statement."
  (interactive)
  (php-template-generic-function "atan2" '("y" "x") 2))

(defun php-template-atan ()
  "Insert a atan statement."
  (interactive)
  (php-template-generic-function "atan" '("arg") 1))

(defun php-template-atanh ()
  "Insert a atanh statement."
  (interactive)
  (php-template-generic-function "atanh" '("arg") 1))

(defun php-template-base-convert ()
  "Insert a base_convert statement."
  (interactive)
  (php-template-generic-function "base_convert" '("number" "frombase" "tobase") 3))

(defun php-template-bindec ()
  "Insert a bindec statement."
  (interactive)
  (php-template-generic-function "bindec" '("binary_string") 1))

(defun php-template-ceil ()
  "Insert a ceil statement."
  (interactive)
  (php-template-generic-function "ceil" '("value") 1))

(defun php-template-cos ()
  "Insert a cos statement."
  (interactive)
  (php-template-generic-function "cos" '("arg") 1))

(defun php-template-cosh ()
  "Insert a cosh statement."
  (interactive)
  (php-template-generic-function "cosh" '("arg") 1))

(defun php-template-decbin ()
  "Insert a decbin statement."
  (interactive)
  (php-template-generic-function "decbin" '("number") 1))

(defun php-template-dechex ()
  "Insert a dechex statement."
  (interactive)
  (php-template-generic-function "dechex" '("number") 1))

(defun php-template-decoct ()
  "Insert a decoct statement."
  (interactive)
  (php-template-generic-function "decoct" '("number") 1))

(defun php-template-deg2rad ()
  "Insert a deg2rad statement."
  (interactive)
  (php-template-generic-function "deg2rad" '("number") 1))

(defun php-template-exp ()
  "Insert a exp statement."
  (interactive)
  (php-template-generic-function "exp" '("arg") 1))

(defun php-template-expm1 ()
  "Insert a expm1 statement."
  (interactive)
  (php-template-generic-function "expm1" '("number") 1))

(defun php-template-floor ()
  "Insert a floor statement."
  (interactive)
  (php-template-generic-function "floor" '("value") 1))

(defun php-template-fmod ()
  "Insert a fmod statement."
  (interactive)
  (php-template-generic-function "fmod" '("x" "y") 2))

(defun php-template-getrandmax ()
  "Insert a getrandmax statement."
  (interactive)
  (php-template-generic-function "getrandmax" '() 0))

(defun php-template-hexdec ()
  "Insert a hexdec statement."
  (interactive)
  (php-template-generic-function "hexdec" '("hex_string") 1))

(defun php-template-hypot ()
  "Insert a hypot statement."
  (interactive)
  (php-template-generic-function "hypot" '("x" "y") 2))

(defun php-template-is-finite ()
  "Insert a is_finite statement."
  (interactive)
  (php-template-generic-function "is_finite" '("val") 1))

(defun php-template-is-infinite ()
  "Insert a is_infinite statement."
  (interactive)
  (php-template-generic-function "is_infinite" '("val") 1))

(defun php-template-is-nan ()
  "Insert a is_nan statement."
  (interactive)
  (php-template-generic-function "is_nan" '("val") 1))

(defun php-template-lcg-value ()
  "Insert a lcg_value statement."
  (interactive)
  (php-template-generic-function "lcg_value" '() 0))

(defun php-template-log10 ()
  "Insert a log10 statement."
  (interactive)
  (php-template-generic-function "log10" '("arg") 1))

(defun php-template-log1p ()
  "Insert a log1p statement."
  (interactive)
  (php-template-generic-function "log1p" '("number") 1))

(defun php-template-log ()
  "Insert a log statement."
  (interactive)
  (php-template-generic-function "log" '("arg" "base") 1))

(defun php-template-max ()
  "Insert a max statement."
  (interactive)
  (php-template-generic-function "max" '("arg1" "arg2" "...") 2 t))

(defun php-template-min ()
  "Insert a min statement."
  (interactive)
  (php-template-generic-function "min" '("arg1" "arg2" "...") 2 t))

(defun php-template-mt-getrandmax ()
  "Insert a mt_getrandmax statement."
  (interactive)
  (php-template-generic-function "mt_getrandmax" '() 0))

(defun php-template-mt-rand ()
  "Insert a mt_rand statement."
  (interactive)
  (php-template-generic-function "mt_rand" '("min" "max") 2))

(defun php-template-mt-srand ()
  "Insert a mt_srand statement."
  (interactive)
  (php-template-generic-function "mt_srand" '("seed") 0))

(defun php-template-octdec ()
  "Insert a octdec statement."
  (interactive)
  (php-template-generic-function "octdec" '("octal_string") 1))

(defun php-template-pi ()
  "Insert a pi statement."
  (interactive)
  (php-template-generic-function "pi" '() 0))

(defun php-template-pow ()
  "Insert a pow statement."
  (interactive)
  (php-template-generic-function "pow" '("base" "exp") 2))

(defun php-template-rad2deg ()
  "Insert a rad2deg statement."
  (interactive)
  (php-template-generic-function "rad2deg" '("number") 1))

(defun php-template-rand ()
  "Insert a rand statement."
  (interactive)
  (php-template-generic-function "rand" '("min" "max") 0))

(defun php-template-round ()
  "Insert a round statement."
  (interactive)
  (php-template-generic-function "round" '("val" "precision") 1))

(defun php-template-sin ()
  "Insert a sin statement."
  (interactive)
  (php-template-generic-function "sin" '("arg") 1))

(defun php-template-sinh ()
  "Insert a sinh statement."
  (interactive)
  (php-template-generic-function "sinh" '("arg") 1))

(defun php-template-sqrt ()
  "Insert a sqrt statement."
  (interactive)
  (php-template-generic-function "sqrt" '("arg") 1))

(defun php-template-srand ()
  "Insert a srand statement."
  (interactive)
  (php-template-generic-function "srand" '("seed") 0))

(defun php-template-tan ()
  "Insert a tan statement."
  (interactive)
  (php-template-generic-function "tan" '("arg") 1))

(defun php-template-tanh ()
  "Insert a tanh statement."
  (interactive)
  (php-template-generic-function "tanh" '("arg") 1))

;; Miscellaneous Functions

(defun php-template-connection-aborted ()
  "Insert an connection_aborted statement."
  (interactive)
  (php-template-generic-function "connection_aborted" '() 0))

(defun php-template-connection-status ()
  "Insert an connection_status statement."
  (interactive)
  (php-template-generic-function "connection_status" '() 0))

(defun php-template-connection-timeout ()
  "Insert an connection_timeout statement."
  (interactive)
  (php-template-generic-function "connection_timeout" '() 0))

(defun php-template-constant ()
  "Insert an constant statement."
  (interactive)
  (php-template-generic-function "constant" '("name") 0))

(defun php-template-define ()
  "Insert an define statement."
  (interactive)
  (php-template-generic-function "define" '("name" "value" "case_insensitive") 2))

(defun php-template-defined ()
  "Insert an defined statement."
  (interactive)
  (php-template-generic-function "defined" '("name") 1))

(defun php-template-die ()
  "Insert an die statement."
  (interactive)
  (php-template-generic-function "die" '("status") 0 nil t))

(defun php-template-eval ()
  "Insert an eval statement."
  (interactive)
  (php-template-generic-function "eval" '("code_str") 1))

(defun php-template-exit ()
  "Insert an exit statement."
  (interactive)
  (php-template-generic-function "exit" '("status") 0 nil t))

(defun php-template-get-browser ()
  "Insert an get_browser statement."
  (interactive)
  (php-template-generic-function "get_browser" '("user_agent" "eturn_array") 0))

(defun php-template-halt-compiler ()
  "Insert an __halt_compiler statement."
  (interactive)
  (php-template-generic-function "__halt_compiler" '() 0 nil t))

(defun php-template-highlight-file ()
  "Insert an highlight_file statement."
  (interactive)
  (php-template-generic-function "highlight_file" '("filename" "return") 1))

(defun php-template-highlight-string ()
  "Insert an highlight_string statement."
  (interactive)
  (php-template-generic-function "highlight_string" '("str" "return") 1))

(defun php-template-ignore-user-abort ()
  "Insert an ignore_user_abort statement."
  (interactive)
  (php-template-generic-function "ignore_user_abort" '("setting") 0))

(defun php-template-pack ()
  "Insert an pack statement."
  (interactive)
  (php-template-generic-function "pack" '("format" "args" "...") 1 t))

(defun php-template-php-check-syntax ()
  "Insert an php_check_syntax statement."
  (interactive)
  (php-template-generic-function "php_check_syntax" '("file_name" "error_message") 1))

(defun php-template-php-strip-whitespace ()
  "Insert an php_strip_whitespace statement."
  (interactive)
  (php-template-generic-function "php_strip_whitespace" '("filename") 1))

(defun php-template-show-source ()
  "Insert an show_source statement."
  (interactive)
  (php-template-highlight-file))

(defun php-template-sleep ()
  "Insert an sleep statement."
  (interactive)
  (php-template-generic-function "sleep" '("seconds") 1))

(defun php-template-sys-getloadavg ()
  "Insert an sys_getloadavg statement."
  (interactive)
  (php-template-generic-function "sys_getloadavg" '() 0))

(defun php-template-time-nanosleep ()
  "Insert an time_nanosleep statement."
  (interactive)
  (php-template-generic-function "time_nanosleep" '("seconds" "nanoseconds") 2))

(defun php-template-time-sleep-until ()
  "Insert an time_sleep_until statement."
  (interactive)
  (php-template-generic-function "time_sleep_until" '("timestamp") 1))

(defun php-template-uniqid ()
  "Insert an uniqid statement."
  (interactive)
  (php-template-generic-function "uniqid" '("prefix" "more_entropy") 1))

(defun php-template-unpack ()
  "Insert an unpack statement."
  (interactive)
  (php-template-generic-function "unpack" '("format" "data") 2))

(defun php-template-usleep ()
  "Insert an usleep statement."
  (interactive)
  (php-template-generic-function "usleep" '("micro_seconds") 1))

;; MySQL

(defun php-template-search-mysql-connect ()
  "Search about a mysl_connect statement."
  (let (result found)
    (save-excursion
      (setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\(mysql_connect\\|mysql_pconnect\\)" nil t))
      (if found
	  (setq result (concat "$" (match-string 1)))
	(setq result nil)))
    result))

(defun php-template-search-mysql-query (query)
  "Search about a mysl_query statement."
  (let (result found)
    (save-excursion
      (setq found (re-search-backward (concat "\$\\(\\w+\\)\\s-*=\\s-*\\" query) nil t))
      (if found
	  (setq result (concat "$" (match-string 1)))
	(setq result nil)))
    result))

(defun php-template-mysql-generic-function (label field mandatory-count)
  "Generic function for MySQL functions."
  (interactive)
  (let ((start (point)) remove here result-value elt continue field-count 
	stop file handle (comment-end (point)))
    (setq file (php-template-search-mysql-connect))
    (insert " = ")
    (setq remove (point))
    (insert (concat label "( "))
    (setq here (point-marker))
    (goto-char comment-end)
    (setq result-value (php-template-field "result_value" nil t))
    (if (not result-value)
	(delete-region comment-end remove))
    (goto-char here)
    (setq elt field)
    (setq continue t)
    (setq field-count 0)
    (setq stop nil)
    (while (and elt continue)
      (setq result-value (php-template-field (car elt) nil t))
      (if (and (not result-value)
	       (< field-count mandatory-count))
	  (progn (setq continue nil)
		 (delete-region start (point))
		 (insert (concat label " "))
		 (setq stop t))
	(if (not result-value)
	    (setq continue nil)
	  (insert ", ")))
      (setq field-count (+ 1 field-count))
      (setq elt (cdr elt)))
    (when (not stop)
      (if (and (not file) continue)
	  (progn
	    (setq here (point-marker))
	    (setq handle (php-template-field "link_identifier" "  "))
	    (if (not handle)
		(progn (delete-region here (point))
		       (insert " "))))
	(if continue
	    (insert (concat file "  "))))
      (delete-char -2)
      (if (= (preceding-char) ?\,)
	  (delete-char -1))
      (if (= (preceding-char) ?\()
	  (insert ");")
	(insert " );"))
      (newline-and-indent))))

(defun php-template-mysql-result-generic-function (query label field mandatory-count)
  "Generic function for MySQL functions."
  (interactive)
  (let ((start (point)) remove here result-value elt continue field-count 
	stop file handle (stop-handle nil) (comment-end (point)))
    (setq file (php-template-search-mysql-query query))
    (when (not file)
      (insert "/* WARNING: '")
      (insert query)
      (insert "' statement not found in this file */")
      (newline-and-indent)
      (setq comment-end (point)))
    (insert " = ")
    (setq remove (point))
    (insert (concat label "( "))
    (setq here (point-marker))
    (goto-char comment-end)
    (setq result-value (php-template-field "result_value" nil t))
    (if (not result-value)
	(delete-region comment-end remove))
    (goto-char here)
    (setq handle (php-template-field "result" ", " nil (point) (point) nil "" file))
    (if (not handle)
	(progn (delete-region start (point))
	       (insert (concat label " "))
	       (setq stop-handle t)))
    (when (not stop-handle)
      (setq elt field)
      (setq continue t)
      (setq field-count 0)
      (setq stop nil)
      (while (and elt continue)
	(setq result-value (php-template-field (car elt) nil t))
	(if (and (not result-value)
		 (< field-count mandatory-count))
	    (progn (setq continue nil)
		   (delete-region start (point))
		   (insert (concat label " "))
		   (setq stop t))
	  (if (not result-value)
	      (setq continue nil)
	    (insert ", ")))
	(setq field-count (+ 1 field-count))
	(setq elt (cdr elt)))
      (when (not stop)
	(delete-char -2)
	(insert " );")
	(newline-and-indent)))))

(defun php-template-mysql-affected-rows ()
  "Insert a mysql_affected_rows statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_affected_rows" '() 0))

(defun php-template-mysql-change-user ()
  "Insert a mysql_change_user statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_change_user" '("user" "password" "database") 2))

(defun php-template-mysql-client-encoding ()
  "Insert a mysql_client_encoding statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_client_encoding" '() 0))

(defun php-template-mysql-close ()
  "Insert a mysql_close statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_close" '() 0))

(defun php-template-mysql-connect ()
  "Insert a mysql_connect statement."
  (interactive)
  (php-template-generic-function "mysql_connect" '("server" "username" "password" "new_link" "client_flags") 0)
  (when php-add-mysql-close-when-connect
    (save-excursion
      (newline-and-indent)
      (php-template-mysql-close))))

(defun php-template-mysql-create-db ()
  "Insert a mysql_create_db statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_create_db" '("database_name") 1))

(defun php-template-mysql-data-seek ()
  "Insert a mysql_data_seek statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_data_seek" '("row_number") 1))

(defun php-template-mysql-db-name ()
  "Insert a mysql_db_name statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_list_dbs" "mysql_db_name" '("row" "field") 1))

(defun php-template-mysql-db-query ()
  "Insert a mysql_db_query statement."
  (interactive)
  (php-template-mysql-select-db)
  (php-template-mysql-query))

(defun php-template-mysql-drop-db ()
  "Insert a mysql_drop_db statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_drop_db" '("database_name") 1))

(defun php-template-mysql-errno ()
  "Insert a mysql_errno statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_errno" '() 0))

(defun php-template-mysql-error ()
  "Insert a mysql_error statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_error" '() 0))

(defun php-template-mysql-escape-string ()
  "Insert a mysql_escape_string statement."
  (interactive)
  (php-template-generic-function "mysql_escape_string" '("unescaped_string") 1))

(defun php-template-mysql-fetch-array ()
  "Insert a mysql_fetch_array statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_fetch_array" '("result_type") 0))

(defun php-template-mysql-fetch-assoc ()
  "Insert a mysql_fetch_assoc statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_fetch_assoc" '() 0))

(defun php-template-mysql-fetch-field ()
  "Insert a mysql_fetch_field statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_fetch_field" '("field_offset") 0))

(defun php-template-mysql-fetch-lengths ()
  "Insert a mysql_fetch_lengths statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_fetch_lengths" '() 0))

(defun php-template-mysql-fetch-object ()
  "Insert a mysql_fetch_object statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_fetch_object" '() 0))

(defun php-template-mysql-fetch-row ()
  "Insert a mysql_fetch_row statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_fetch_row" '() 0))

(defun php-template-mysql-field-flags ()
  "Insert a mysql_field_flags statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_field_flags" '("field_offset") 1))

(defun php-template-mysql-field-len ()
  "Insert a mysql_field_len statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_field_len" '("field_offset") 1))

(defun php-template-mysql-field-name ()
  "Insert a mysql_field_name statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_field_name" '("field_offset") 1))

(defun php-template-mysql-field-seeks ()
  "Insert a mysql_field_seek statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_field_seek" '("field_offset") 1))

(defun php-template-mysql-field-table ()
  "Insert a mysql_field_table statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_field_table" '("field_offset") 1))

(defun php-template-mysql-field-type ()
  "Insert a mysql_field_type statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_field_type" '("field_offset") 1))

(defun php-template-mysql-free-result ()
  "Insert a mysql_free_result statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_free_result" '() 0))

(defun php-template-mysql-get-client-info ()
  "Insert a mysql_get_client_info statement."
  (interactive)
  (php-template-generic-function "mysql_get_client_info" '() 0))

(defun php-template-mysql-get-host-info ()
  "Insert a mysql_get_host_info statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_get_host_info" '() 0))

(defun php-template-mysql-get-proto-info ()
  "Insert a mysql_get_ptoto_info statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_get_proto_info" '() 0))

(defun php-template-mysql-get-server-info ()
  "Insert a mysql_get_server_info statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_get_server_info" '() 0))

(defun php-template-mysql-info ()
  "Insert a mysql_info statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_info" '() 0))

(defun php-template-mysql-list-dbs ()
  "Insert a mysql_list_dbs statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_list_dbs" '() 0))

(defun php-template-mysql-list-fields ()
  "Insert a mysql_list_fields statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_list_fields" '("database_name" "table_name") 2))

(defun php-template-mysql-list-processes ()
  "Insert a mysql_list_processes statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_list_processes" '() 0))

(defun php-template-mysql-list-tables ()
  "Insert a mysql_list_tables statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_list_tables" '("database") 1))

(defun php-template-mysql-num-fields ()
  "Insert a mysql_num_fields statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_num_fields" '() 0))

(defun php-template-mysql-num-rows ()
  "Insert a mysql_num_rows statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_num_rows" '() 0))

(defun php-template-mysql-pconnect ()
  "Insert a mysql_pconnect statement."
  (interactive)
  (php-template-generic-function "mysql_pconnect" '("server" "username" "password" "client_flags") 0)
  (when php-add-mysql-close-when-connect
    (save-excursion
      (newline-and-indent)
      (php-template-mysql-close))))

(defun php-template-mysql-ping ()
  "Insert a mysql_ping statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_ping" '() 0))

(defun php-template-mysql-query ()
  "Insert a mysql_query statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_query" '("query") 1))

(defun php-template-mysql-real-escape-string ()
  "Insert a mysql_real_escape_string statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_real_escape_string" '("unescaped_string") 1))

(defun php-template-mysql-result ()
  "Insert a mysql_result statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_query" "mysql_result" '("row" "field") 1))

(defun php-template-mysql-select-db ()
  "Insert a mysql_select_db statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_select_db" '("database_name") 1))

(defun php-template-mysql-stat ()
  "Insert a mysql_stat statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_stat" '() 0))

(defun php-template-mysql-tablename ()
  "Insert a mysql_tablename statement."
  (interactive)
  (php-template-mysql-result-generic-function "mysql_list_tables" "mysql_tablename" '("i") 1))

(defun php-template-mysql-thread-id ()
  "Insert a mysql_thread_id statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_thread_id" '() 0))

(defun php-template-mysql-unbuffered-query ()
  "Insert a mysql_unbuffered_query statement."
  (interactive)
  (php-template-mysql-generic-function "mysql_unbuffered_query" '("query") 1))

;; Others

(defun php-template-class ()
  "Insert a class statement."
  (interactive)
  (let ((start (point)) remove here result-value elt comment (continue t))
    (insert "class ")
    (setq result-value (php-template-field "name" nil t start (point)))
    (when result-value
      (newline-and-indent)
      (insert "{")
      (newline-and-indent)
      (setq here (point-marker))
      (newline-and-indent)
      (insert "};")
      (php-indent-line-2)
      (goto-char start)
      (newline-and-indent)
      (goto-char start)
      (insert "/**")
      (newline-and-indent)
      (insert "*")
      (setq comment (point-marker))
      (newline-and-indent)
      (insert "*/")
      (forward-char 1)
      (setq remove (point-marker))
      (goto-char comment)
      (insert " ")
      (setq result-value (php-template-field "description"))
      (if (not result-value)
	  (delete-region start remove)
	(when php-enable-phpdocumentor-tags
	  (newline-and-indent)
	  (insert "*")
	  (setq elt php-class-tags)
	  (while (and continue elt)
	    (setq start (point))
	    (newline-and-indent)
	    (insert (concat "* @" (car elt) " "))
	    (setq result-value (php-template-field (car elt)))
	    (if (not result-value)
		(progn 
		  (setq continue nil)
		  (delete-region start (point))))
	    (setq elt (cdr elt)))))
      (goto-char here)
      (php-indent-line-2))))

(defun php-template-function ()
  "Insert a function statement."
  (interactive)
  (let ((start (point)) variable variable-comment remove here result-value elt comment (continue t))
    (insert "function ")
    (setq result-value (php-template-field "name" nil t start (point)))
    (when result-value
      (insert "( ")
      (setq variable (point-marker))
      (goto-char start)
      (newline-and-indent)
      (goto-char start)
      (insert "/**")
      (newline-and-indent)
      (insert "*")
      (setq comment (point-marker))
      (newline-and-indent)
      (setq variable-comment (point-marker))
      (insert "*/")
      (forward-char 1)
      (setq remove (point-marker))
      (goto-char comment)
      (insert " ")
      (setq result-value (php-template-field "description"))
      (if (not result-value)
	  (delete-region start remove)
	(when php-enable-phpdocumentor-tags
	  (newline-and-indent)
	  (insert "*")
	  (setq elt php-function-tags)
	  (while (and continue elt)
	    (setq start (point))
	    (newline-and-indent)
	    (insert (concat "* @" (car elt) " "))
	    (setq result-value (php-template-field (car elt)))
	    (if (not result-value)
		(progn 
		  (setq continue nil)
		  (delete-region start (point))))
	    (setq elt (cdr elt)))))
      (goto-char variable)
      (setq result-value (php-template-field "variable" nil t))
      (while result-value
	(insert ", ")
	(setq variable (point-marker))
	(goto-char variable-comment)
	(insert "* @param")
	(setq variable-comment (point-marker))
	(newline-and-indent)
	(goto-char variable-comment)
	(insert " ")
	(php-template-field "type" (concat " " result-value " "))
	(php-template-field "description" nil)
	(re-search-forward "*" nil t)
	(forward-char -1)
	(setq variable-comment (point-marker))
	(goto-char variable)
	(setq result-value (php-template-field "variable" nil t)))
      (delete-char -1)
      (if (= (preceding-char) ?\,)
	  (progn (delete-char -1)
		 (insert " ) {"))
	(insert ") {"))
      (newline-and-indent)
      (setq here (point-marker))
      (newline-and-indent)
      (insert "}")
      (php-indent-line-2)
      (goto-char variable-comment)
      (beginning-of-line)
      (forward-char -1)
      (setq remove (point))
      (goto-char variable-comment)
      (insert "* @return")
      (setq variable-comment (point))
      (newline-and-indent)
      (goto-char variable-comment)
      (insert " ")
      (php-template-field "type" nil t remove (point))
      (goto-char here)
      (php-indent-line-2))))

;; Regular expressions

(defun php-template-ereg-replace ()
  "Insert a ereg_replace statement."
  (interactive)
  (php-template-generic-function "ereg_replace" '("pattern" "replacement" "string") 3))

(defun php-template-ereg ()
  "Insert a ereg statement."
  (interactive)
  (php-template-generic-function "ereg" '("pattern" "string" "regs") 2))

(defun php-template-eregi-replace ()
  "Insert a eregi_replace statement."
  (interactive)
  (php-template-generic-function "eregi_replace" '("pattern" "replacement" "string") 3))

(defun php-template-eregi ()
  "Insert a eregi statement."
  (interactive)
  (php-template-generic-function "eregi" '("pattern" "string" "regs") 2))

(defun php-template-split ()
  "Insert a split statement."
  (interactive)
  (php-template-generic-function "split" '("pattern" "string" "limit") 2))

(defun php-template-spliti ()
  "Insert a spliti statement."
  (interactive)
  (php-template-generic-function "spliti" '("pattern" "string" "limit") 2))

(defun php-template-sql-regcase ()
  "Insert a sql_regcase statement."
  (interactive)
  (php-template-generic-function "sql_regcase" '("string") 1))

;; Session

(defun php-template-session-cache-expire ()
  "Insert a session_cache_expire statement."
  (interactive)
  (php-template-generic-function "session_cache_expire" '("new_cache_expire") 0))

(defun php-template-session-cache-limiter ()
  "Insert a session_cache_limiter statement."
  (interactive)
  (php-template-generic-function "session_cache_limiter" '("cache_limiter") 0))

(defun php-template-session-commit ()
  "Insert a session_commit statement."
  (interactive)
  (php-template-session-write-close))

(defun php-template-session-decode ()
  "Insert a session_decode statement."
  (interactive)
  (php-template-generic-function "session_decode" '("data") 1))

(defun php-template-session-destroy ()
  "Insert a session_destroy statement."
  (interactive)
  (php-template-generic-function "session_destroy" '() 0))

(defun php-template-session-encode ()
  "Insert a session_encode statement."
  (interactive)
  (php-template-generic-function "session_encode" '() 0))

(defun php-template-session-get-cookie-params ()
  "Insert a session_get_cookie_params statement."
  (interactive)
  (php-template-generic-function "session_get_cookie_params" '() 0))

(defun php-template-session-id ()
  "Insert a session_id statement."
  (interactive)
  (php-template-generic-function "session_id" '("id") 0))

(defun php-template-session-is-registered ()
  "Insert a session_is_registered statement."
  (interactive)
  (php-template-generic-function "session_is_registered" '("name") 1))

(defun php-template-session-module-name ()
  "Insert a session_module_name statement."
  (interactive)
  (php-template-generic-function "session_module_name" '("module") 0))

(defun php-template-session-name ()
  "Insert a session_name statement."
  (interactive)
  (php-template-generic-function "session_name" '("name") 0))

(defun php-template-session-regenerate-id ()
  "Insert a session_regenerate_id statement."
  (interactive)
  (php-template-generic-function "session_regenerate_id" '("delete_old_session") 0))

(defun php-template-session-register ()
  "Insert a session_register statement."
  (interactive)
  (php-template-generic-function "session_register" '("name" "...") 1 t))

(defun php-template-session-save-path ()
  "Insert a session_save_path statement."
  (interactive)
  (php-template-generic-function "session_save_path" '("path") 0))

(defun php-template-session-set-cookie-params ()
  "Insert a session_set_cookie_params statement."
  (interactive)
  (php-template-generic-function "session_set_cookie_params" '("lifetime" "path" "domain" "secure") 1))

(defun php-template-session-set-save-handler ()
  "Insert a session_set_save_handler statement."
  (interactive)
  (php-template-generic-function "session_set_save_handler" '("open" "close" "read" "write" "destroy" "gc") 6))

(defun php-template-session-start ()
  "Insert a session_start statement."
  (interactive)
  (php-template-generic-function "session_start" '() 0))

(defun php-template-session-unregister ()
  "Insert a session_unregister statement."
  (interactive)
  (php-template-generic-function "session_unregister" '("name") 1))

(defun php-template-session-unset ()
  "Insert a session_unset statement."
  (interactive)
  (php-template-generic-function "session_unset" '() 0 nil t))

(defun php-template-session-write-close ()
  "Insert a session_write_close statement."
  (interactive)
  (php-template-generic-function "session_write_close" '() 0 nil t))

;; Strings

(defun php-template-addcslashes ()
  "Insert a addcslashes statement."
  (interactive)
  (php-template-generic-function "addcslashes" '("str" "charlist") 2))

(defun php-template-addslashes ()
  "Insert a addslashes statement."
  (interactive)
  (php-template-generic-function "addslashes" '("str") 1))

(defun php-template-bin2hex ()
  "Insert a bin2hex statement."
  (interactive)
  (php-template-generic-function "bin2hex" '("str") 1))

(defun php-template-chop ()
  "Insert a chop statement."
  (interactive)
  (php-template-rtrim))

(defun php-template-chr ()
  "Insert a chr statement."
  (interactive)
  (php-template-generic-function "chr" '("ascii") 1))

(defun php-template-chunk-split ()
  "Insert a chunk-split statement."
  (interactive)
  (php-template-generic-function "chunk-split" '("body" "chunklen" "end") 1))

(defun php-template-convert-cyr-string ()
  "Insert a convert-cyr-string statement."
  (interactive)
  (php-template-generic-function "convert-cyr-string" '("str" "from" "to") 3))

(defun php-template-convert-uudecode ()
  "Insert a convert_uudecode statement."
  (interactive)
  (php-template-generic-function "convert_uudecode" '("data") 1))

(defun php-template-convert-uuencode ()
  "Insert a convert_uuencode statement."
  (interactive)
  (php-template-generic-function "convert_uuencode" '("data") 1))

(defun php-template-count-chars ()
  "Insert a count_chars statement."
  (interactive)
  (php-template-generic-function "count_chars" '("string" "mode") 1))

(defun php-template-crc32 ()
  "Insert a crc32 statement."
  (interactive)
  (php-template-generic-function "crc32" '("str") 1))

(defun php-template-crypt ()
  "Insert a crypt statement."
  (interactive)
  (php-template-generic-function "crypt" '("str" "salt") 1)) 

(defun php-template-echo ()
  "Insert a echo statement."
  (interactive)
  (php-template-generic-function "echo" '("arg1" "...") 1 t))

(defun php-template-explode ()
  "Insert a explode statement."
  (interactive)
  (php-template-generic-function "explode" '("separator" "string" "limit") 2))

(defun php-template-fprintf ()
  "Insert a fprintf statement."
  (interactive)
  (php-template-generic-function "fprintf" '("handle" "format" "args" "...") 2))

(defun php-template-get-html-translation-table ()
  "Insert a get_html_translation_table statement."
  (interactive)
  (php-template-generic-function "get_html_translation_table" '("table" "quote_style") 0))

(defun php-template-hebrev ()
  "Insert a hebrev statement."
  (interactive)
  (php-template-generic-function "hebrev" '("hebrew_text" "max_chars_per_line") 1))

(defun php-template-hebrevc ()
  "Insert a hebrevc statement."
  (interactive)
  (php-template-generic-function "hebrevc" '("hebrew_text" "max_chars_per_line") 1))

(defun php-template-html-entity-decode ()
  "Insert a html_entity_decode statement."
  (interactive)
  (php-template-generic-function "html_entity_decode" '("string" "quote_style" "string charset") 1))

(defun php-template-htmlentities ()
  "Insert a htmlentities statement."
  (interactive)
  (php-template-generic-function "htmlentities" '("string" "quote_style" "string charset") 1))

(defun php-template-htmlspecialchars-decode ()
  "Insert a htmlspecialchars_decode statement."
  (interactive)
  (php-template-generic-function "htmlspecialchars_decode" '("string" "quote_style") 1))

(defun php-template-htmlspecialchars ()
  "Insert a htmlspecialchars statement."
  (interactive)
  (php-template-generic-function "htmlspecialchars" '("string" "quote_style" "charset") 1))

(defun php-template-implode ()
  "Insert a implode statement."
  (interactive)
  (php-template-generic-function "implode" '("glue" "pieces") 2))

(defun php-template-join ()
  "Insert a join statement."
  (interactive)
  (php-template-implode))

(defun php-template-levenshtein ()
  "Insert a levenshtein statement."
  (interactive)
  (php-template-generic-function "levenshtein" '("str1" "str2" "cost_ins" "cost_rep" "cost_del") 5))

(defun php-template-localeconv ()
  "Insert a localeconv statement."
  (interactive)
  (php-template-generic-function "localeconv" '() 0))

(defun php-template-ltrim ()
  "Insert a ltrim statement."
  (interactive)
  (php-template-generic-function "ltrim" '("str" "charlist") 1))

(defun php-template-md5-file ()
  "Insert a md5_file statement."
  (interactive)
  (php-template-generic-function "md5_file" '("filename" "raw_output") 1))

(defun php-template-md5 ()
  "Insert a md5 statement."
  (interactive)
  (php-template-generic-function "md5" '("str" "raw_output") 1))

(defun php-template-metaphone ()
  "Insert a metaphone statement."
  (interactive)
  (php-template-generic-function "metaphone" '("str" "phones") 1))

(defun php-template-money-format ()
  "Insert a money_format statement."
  (interactive)
  (php-template-generic-function "money_format" '("format" "number") 2))

(defun php-template-nl-langinfo ()
  "Insert a nl_langinfo statement."
  (interactive)
  (php-template-generic-function "nl_langinfo" '("item") 1))

(defun php-template-nl2br ()
  "Insert a nl2br statement."
  (interactive)
  (php-template-generic-function "nl2br" '("string") 1))

(defun php-template-number-format ()
  "Insert a number_format statement."
  (interactive)
  (php-template-generic-function "number_format" '("number" "decimals" "dec_point" "thousands_sep") 1))

(defun php-template-ord ()
  "Insert a ord statement."
  (interactive)
  (php-template-generic-function "ord" '("string") 1))

(defun php-template-parse-str ()
  "Insert a parse_str statement."
  (interactive)
  (php-template-generic-function "parse_str" '("str" "arr") 1))

(defun php-template-print ()
  "Insert a print statement."
  (interactive)
  (php-template-generic-function "print" '("arg") 1))

(defun php-template-printf ()
  "Insert a printf statement."
  (interactive)
  (php-template-generic-function "printf" '("format" "args" "...") 1))

(defun php-template-quoted-printable-decode ()
  "Insert a quoted_printable_decode statement."
  (interactive)
  (php-template-generic-function "quoted_printable_decode" '("str") 1))

(defun php-template-quotemeta ()
  "Insert a quotemeta statement."
  (interactive)
  (php-template-generic-function "quotemeta" '("str") 1))

(defun php-template-rtrim ()
  "Insert a rtrim statement."
  (interactive)
  (php-template-generic-function "rtrim" '("str" "charlist") 1))

(defun php-template-setlocale ()
  "Insert a setlocale statement."
  (interactive)
  (php-template-generic-function "setlocale" '("category" "locale" "...") 2))

(defun php-template-sha1-file ()
  "Insert a sha1_file statement."
  (interactive)
  (php-template-generic-function "sha1_file" '("filename" "raw_output") 1))

(defun php-template-sha1 ()
  "Insert a sha1 statement."
  (interactive)
  (php-template-generic-function "sha1" '("str" "raw_output") 1))

(defun php-template-similar-text ()
  "Insert a similar_text statement."
  (interactive)
  (php-template-generic-function "similar_text" '("first" "second" "percent") 2))

(defun php-template-soundex ()
  "Insert a soundex statement."
  (interactive)
  (php-template-generic-function "soundex" '("str") 1))

(defun php-template-sprintf ()
  "Insert a sprintf statement."
  (interactive)
  (php-template-generic-function "sprintf" '("format" "args" "...") 1))

(defun php-template-sscanf ()
  "Insert a sscanf statement."
  (interactive)
  (php-template-generic-function "sscanf" '("str" "format" "...") 2))

(defun php-template-str-ireplace ()
  "Insert a str_ireplace statement."
  (interactive)
  (php-template-generic-function "str_ireplace" '("search" "replace" "subject" "count") 3))

(defun php-template-str-pad ()
  "Insert a str_pad statement."
  (interactive)
  (php-template-generic-function "str_pad" '("input" "pad_length" "pad_string" "pad_type") 2))

(defun php-template-str-repeat ()
  "Insert a str_repeat statement."
  (interactive)
  (php-template-generic-function "str_repeat" '("input" "multiplier") 2))

(defun php-template-str-replace ()
  "Insert a str_replace statement."
  (interactive)
  (php-template-generic-function "str_replace" '("search" "replace" "subject" "count") 3))

(defun php-template-str-rot13 ()
  "Insert a str_rot13 statement."
  (interactive)
  (php-template-generic-function "str_rot13" '("str") 1))

(defun php-template-str-shuffle ()
  "Insert a str_shuffle statement."
  (interactive)
  (php-template-generic-function "str_shuffle" '("str") 1))

(defun php-template-str-split ()
  "Insert a str_split statement."
  (interactive)
  (php-template-generic-function "str_split" '("string" "split_length") 1))

(defun php-template-str-word-count ()
  "Insert a str_word_count statement."
  (interactive)
  (php-template-generic-function "str_word_count" '("string" "format" "charlist") 1))

(defun php-template-strcasecmp ()
  "Insert a strcasecmp statement."
  (interactive)
  (php-template-generic-function "strcasecmp" '("str1" "str2") 2))

(defun php-template-strchr ()
  "Insert a strchr statement."
  (interactive)
  (php-template-strstr))

(defun php-template-strcmp ()
  "Insert a strcmp statement."
  (interactive)
  (php-template-generic-function "strcmp" '("str1" "str2") 2))

(defun php-template-strcoll ()
  "Insert a strcoll statement."
  (interactive)
  (php-template-generic-function "strcoll" '("str1" "str2") 2))

(defun php-template-strcspn ()
  "Insert a strcspn statement."
  (interactive)
  (php-template-generic-function "strcspn" '("str1" "str2" "start" "length") 2))

(defun php-template-strip-tags ()
  "Insert a strip_tags statement."
  (interactive)
  (php-template-generic-function "strip_tags" '("str" "allowable_tags") 1))

(defun php-template-stripcslashes ()
  "Insert a stripcslashes statement."
  (interactive)
  (php-template-generic-function "stripcslashes" '("str") 1))

(defun php-template-stripos ()
  "Insert a stripos statement."
  (interactive)
  (php-template-generic-function "stripos" '("haystack" "needle" "offset") 2))

(defun php-template-stripslashes ()
  "Insert a stripslashes statement."
  (interactive)
  (php-template-generic-function "stripslashes" '("str") 1))

(defun php-template-stristr ()
  "Insert a stristr statement."
  (interactive)
  (php-template-generic-function "stristr" '("haystack" "needle") 2))

(defun php-template-strlen ()
  "Insert a strlen statement."
  (interactive)
  (php-template-generic-function "strlen" '("string") 1))

(defun php-template-strnatcasecmp ()
  "Insert a strnatcasecmp statement."
  (interactive)
  (php-template-generic-function "strnatcasecmp" '("str1" "str2") 2))

(defun php-template-strnatcmp ()
  "Insert a strnatcmp statement."
  (interactive)
  (php-template-generic-function "strnatcmp" '("str1" "str2") 2))

(defun php-template-strncasecmp ()
  "Insert a strncasecmp statement."
  (interactive)
  (php-template-generic-function "strncasecmp" '("str1" "str2" "len") 3))

(defun php-template-strncmp ()
  "Insert a strncmp statement."
  (interactive)
  (php-template-generic-function "strncmp" '("str1" "str2" "len") 3))

(defun php-template-strpbrk ()
  "Insert a strpbrk statement."
  (interactive)
  (php-template-generic-function "strpbrk" '("haystack" "char_list") 2))

(defun php-template-strpos ()
  "Insert a strpos statement."
  (interactive)
  (php-template-generic-function "strpos" '("haystack" "needle" "offset") 2))

(defun php-template-strrchr ()
  "Insert a strrchr statement."
  (interactive)
  (php-template-generic-function "strrchr" '("haystack" "needle") 2))

(defun php-template-strrev ()
  "Insert a strrev statement."
  (interactive)
  (php-template-generic-function "strrev" '("string") 1))

(defun php-template-strripos ()
  "Insert a strripos statement."
  (interactive)
  (php-template-generic-function "strripos" '("haystack" "needle" "offset") 2))

(defun php-template-strrpos ()
  "Insert a strrpos statement."
  (interactive)
  (php-template-generic-function "strrpos" '("haystack" "needle" "offset") 2))

(defun php-template-strspn ()
  "Insert a strspn statement."
  (interactive)
  (php-template-generic-function "strspn" '("str1" "str2" "start" "length") 2))

(defun php-template-strstr ()
  "Insert a strstr statement."
  (interactive)
  (php-template-generic-function "strstr" '("haystack" "needle") 2))

(defun php-template-strtok ()
  "Insert a strtok statement."
  (interactive)
  (php-template-generic-function "strtok" '("str" "token") 2))

(defun php-template-strtolower ()
  "Insert a strtolower statement."
  (interactive)
  (php-template-generic-function "strtolower" '("str") 1))

(defun php-template-strtoupper ()
  "Insert a strtoupper statement."
  (interactive)
  (php-template-generic-function "strtoupper" '("string") 1))

(defun php-template-strtr ()
  "Insert a strtr statement."
  (interactive)
  (php-template-generic-function "strtr" '("str" "from" "to") 3))

(defun php-template-substr-compare ()
  "Insert a substr_compare statement."
  (interactive)
  (php-template-generic-function "substr_compare" '("main_str" "str" "offset" "length" "case_insensitivity") 3))

(defun php-template-substr-count ()
  "Insert a substr_count statement."
  (interactive)
  (php-template-generic-function "substr_count" '("haystack" "needle" "offset" "length") 3))

(defun php-template-substr-replace ()
  "Insert a substr_replace statement."
  (interactive)
  (php-template-generic-function "substr_replace" '("string" "replacement" "start" "length") 3))

(defun php-template-substr ()
  "Insert a substr statement."
  (interactive)
  (php-template-generic-function "substr" '("string" "start" "length") 2))

(defun php-template-trim ()
  "Insert a trim statement."
  (interactive)
  (php-template-generic-function "trim" '("str" "charlist") 1))

(defun php-template-ucfirst ()
  "Insert a ucfirst statement."
  (interactive)
  (php-template-generic-function "ucfirst" '("str") 1))

(defun php-template-ucwords ()
  "Insert a ucwords statement."
  (interactive)
  (php-template-generic-function "ucwords" '("str") 1))

(defun php-template-vfprintf ()
  "Insert a vfprintf statement."
  (interactive)
  (php-template-generic-function "vfprintf" '("handle" "format" "args") 3))

(defun php-template-vprintf ()
  "Insert a vprintf statement."
  (interactive)
  (php-template-generic-function "vprintf" '("format" "args") 2))

(defun php-template-vsprintf ()
  "Insert a vsprintf statement."
  (interactive)
  (php-template-generic-function "vsprintf" '("format" "args") 2))

(defun php-template-wordwrap ()
  "Insert a wordwrap statement."
  (interactive)
  (php-template-generic-function "wordwrap" '("str" "width" "break" "cut") 1))

;; Variable

(defun php-template-debug-zval-dump ()
  "Insert a debug_zval_dump statement."
  (interactive)
  (php-template-generic-function "debug_zval_dump" '("variable") 1))

(defun php-template-doubleval ()
  "Insert a doubleval statement."
  (interactive)
  (php-template-floatval))

(defun php-template-empty ()
  "Insert a empty statement."
  (interactive)
  (php-template-generic-function "empty" '("var") 1))

(defun php-template-floatval ()
  "Insert a floatval statement."
  (interactive)
  (php-template-generic-function "floatval" '("var") 1))

(defun php-template-get-defined-vars ()
  "Insert a get_defined_vars statement."
  (interactive)
  (php-template-generic-function "get_defined_vars" '() 0))

(defun php-template-get-resource-type ()
  "Insert a get_resource_type statement."
  (interactive)
  (php-template-generic-function "get_resource_type" '("handle") 1))

(defun php-template-gettype ()
  "Insert a gettype statement."
  (interactive)
  (php-template-generic-function "gettype" '("var") 1))

(defun php-template-import-request-variables ()
  "Insert a import_request_variables statement."
  (interactive)
  (php-template-generic-function "import_request_variables" '("types" "prefix") 1))

(defun php-template-intval ()
  "Insert a intval statement."
  (interactive)
  (php-template-generic-function "intval" '("var" "base") 1))

(defun php-template-is-array ()
  "Insert a is_array statement."
  (interactive)
  (php-template-generic-function "is_array" '("var") 1))

(defun php-template-is-bool ()
  "Insert a is_bool statement."
  (interactive)
  (php-template-generic-function "is_bool" '("var") 1))

(defun php-template-is-callable ()
  "Insert a is_callable statement."
  (interactive)
  (php-template-generic-function "is_callable" '("var" "syntax_only" "callable_name") 1))

(defun php-template-is-double ()
  "Insert a is_double statement."
  (interactive)
  (php-template-is-float))

(defun php-template-is-float ()
  "Insert a  is_floatstatement."
  (interactive)
  (php-template-generic-function "is_float" '("var") 1))

(defun php-template-is-int ()
  "Insert a is_int statement."
  (interactive)
  (php-template-generic-function "is_int" '("var") 1))

(defun php-template-is-integer ()
  "Insert a is_integer statement."
  (interactive)
  (php-template-is-int))

(defun php-template-is-long ()
  "Insert a is_long statement."
  (interactive)
  (php-template-is-int))

(defun php-template-is-null ()
  "Insert a is_null statement."
  (interactive)
  (php-template-generic-function "is_null" '("var") 1))

(defun php-template-is-numeric ()
  "Insert a is_numeric statement."
  (interactive)
  (php-template-generic-function "is_numeric" '("var") 1))

(defun php-template-is-object ()
  "Insert a is_object statement."
  (interactive)
  (php-template-generic-function "is_object" '("var") 1))

(defun php-template-is-real ()
  "Insert a is_real statement."
  (interactive)
  (php-template-is-float))

(defun php-template-is-resource ()
  "Insert a is_resource statement."
  (interactive)
  (php-template-generic-function "is_resource" '("var") 1))

(defun php-template-is-scalar ()
  "Insert a is_scalar statement."
  (interactive)
  (php-template-generic-function "is_scalar" '("var") 1))

(defun php-template-is-string ()
  "Insert a is_string statement."
  (interactive)
  (php-template-generic-function "is_string" '("var") 1))

(defun php-template-isset ()
  "Insert a isset statement."
  (interactive)
  (php-template-generic-function "isset" '("var" "...") 1 t))

(defun php-template-print-r ()
  "Insert a print_r statement."
  (interactive)
  (php-template-generic-function "print_r" '("expression" "return") 1))

(defun php-template-serialize ()
  "Insert a serialize statement."
  (interactive)
  (php-template-generic-function "serialize" '("value") 1))

(defun php-template-settype ()
  "Insert a settype statement."
  (interactive)
  (php-template-generic-function "settype" '("var" "type") 2))

(defun php-template-strval ()
  "Insert a strval statement."
  (interactive)
  (php-template-generic-function "strval" '("var") 1))

(defun php-template-unserialize ()
  "Insert a unserialize statement."
  (interactive)
  (php-template-generic-function "unserialize" '("str") 1))

(defun php-template-unset ()
  "Insert a unset statement."
  (interactive)
  (php-template-generic-function "unset" '("var" "...") 1 t))

(defun php-template-var-dump ()
  "Insert a var_dump statement."
  (interactive)
  (php-template-generic-function "var_dump" '("expression" "...") 1 t))

(defun php-template-var-export ()
  "Insert a var_export statement."
  (interactive)
  (php-template-generic-function "var_export" '("expression" "return") 1))

;; XML

(defun php-template-search-xml-parser ()
  "Search about a xml_parser_create or a xml_parser_create_ns statement."
  (let (result found)
    (save-excursion
      (setq found (re-search-backward "\$\\(\\w+\\)\\s-*=\\s-*\\(xml_parser_create\\|xml_parser_create_ns\\)" nil t))
      (if found
	  (setq result (concat "$" (match-string 1)))
	(setq result nil)))
    result))

(defun php-template-xml-generic-function (label field mandatory-count)
  "Generic function template 'result = label( handle, field1, field2...)"
  (interactive)
  (let ((start (point)) remove here result-value elt continue field-count 
	stop file handle (stop-handle nil) (comment-end (point)))
    (setq file (php-template-search-xml-parser))
    (when (not file)
      (insert "/* WARNING: ")
      (insert "'xml_parser_create'")
      (insert " or 'xml_parser_create_ns'")
      (insert " statement not found in this file */")
      (newline-and-indent)
      (setq comment-end (point)))
    (insert " = ")
    (setq remove (point))
    (insert (concat label "( "))
    (setq here (point-marker))
    (goto-char comment-end)
    (setq result-value (php-template-field "result_value" nil t))
    (if (not result-value)
	(delete-region comment-end remove))
    (goto-char here)
    (if (not file)
	(progn
	  (setq handle (php-template-field "parser" ", "))
	  (if (not handle)
	      (progn (delete-region start (point))
		     (insert (concat label " "))
		     (setq stop-handle t))))
      (insert (concat file ", ")))
    (when (not stop-handle)
      (setq elt field)
      (setq continue t)
      (setq field-count 0)
      (setq stop nil)
      (while (and elt continue)
	(setq result-value (php-template-field (car elt) nil t))
	(if (and (not result-value)
		 (< field-count mandatory-count))
	    (progn (setq continue nil)
		   (delete-region start (point))
		   (insert (concat label " "))
		   (setq stop t))
	  (if (not result-value)
	      (setq continue nil)
	    (insert ", ")))
	(setq field-count (+ 1 field-count))
	(setq elt (cdr elt)))
      (when (not stop)
	(delete-char -2)
	(insert " );")
	(newline-and-indent)))))

(defun php-template-utf8-decode ()
  "Insert a utf8_decode statement."
  (interactive)
  (php-template-generic-function "utf8_decode" '("data") 1))

(defun php-template-utf8-encode ()
  "Insert a utf8_encode statement."
  (interactive)
  (php-template-generic-function "utf8_encode" '("data") 1))

(defun php-template-xml-error-string ()
  "Insert a xml_error_string statement."
  (interactive)
  (php-template-generic-function "xml_error_string" '("code") 1))

(defun php-template-xml-get-current-byte-index ()
  "Insert a xml_get_current_byte_index statement."
  (interactive)
  (php-template-xml-generic-function "xml_get_current_byte_index" '() 0))

(defun php-template-xml-get-current-column-number ()
  "Insert a xml_get_current_column_number statement."
  (interactive)
  (php-template-xml-generic-function "xml_get_current_column_number" '() 0))

(defun php-template-xml-get-current-line-number ()
  "Insert a xml_get_current_line_number statement."
  (interactive)
  (php-template-xml-generic-function "xml_get_current_line_number" '() 0))

(defun php-template-xml-get-error-code ()
  "Insert a xml_get_error_code statement."
  (interactive)
  (php-template-xml-generic-function "xml_get_error_code" '() 0))

(defun php-template-xml-parse-into-struct ()
  "Insert a xml_parse_into_struct statement."
  (interactive)
  (php-template-xml-generic-function "xml_parse_into_struct" '("data" "values" "index") 2))

(defun php-template-xml-parse ()
  "Insert a xml_parse statement."
  (interactive)
  (php-template-xml-generic-function "xml_parse" '("data" "is_final") 1))

(defun php-template-xml-parser-create-ns ()
  "Insert a xml_parser_create_ns statement."
  (interactive)
  (php-template-generic-function "xml_parser_create_ns" '("encoding" "separator") 0)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-xml-parser-free))))

(defun php-template-xml-parser-create ()
  "Insert a xml_parser_create statement."
  (interactive)
  (php-template-generic-function "xml_parser_create" '("encoding") 0)
  (when php-add-fclose-with-fopen
    (save-excursion
      (newline-and-indent)
      (php-template-xml-parser-free))))

(defun php-template-xml-parser-free ()
  "Insert a xml_parser_free statement."
  (interactive)
  (php-template-xml-generic-function "xml_parser_free" '() 0))

(defun php-template-xml-parser-get-option ()
  "Insert a xml_parser_get_option statement."
  (interactive)
  (php-template-xml-generic-function "xml_parser_get_option" '("option") 1))

(defun php-template-xml-parser-set-option ()
  "Insert a xml_parser_set_option statement."
  (interactive)
  (php-template-xml-generic-function "xml_parser_set_option" '("option" "value") 2))

(defun php-template-xml-set-character-data-handler ()
  "Insert a xml_set_character_data_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_character_data_handler" '("handler") 1))

(defun php-template-xml-set-default-handler ()
  "Insert a xml_set_default_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_default_handler" '("handler") 1))

(defun php-template-xml-set-element-handler ()
  "Insert a xml_set_element_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_element_handler" '("start_element_handler" "end_element_handler") 2))

(defun php-template-xml-set-end-namespace-decl-handler ()
  "Insert a xml_set_end_namespace_decl_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_end_namespace_decl_handler" '("handler") 1))

(defun php-template-xml-set-external-entity-ref-handler ()
  "Insert a xml_set_external_entity_ref_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_external_entity_ref_handler" '("handler") 1))

(defun php-template-xml-set-notation-decl-handler ()
  "Insert a xml_set_notation_decl_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_notation_decl_handler" '("handler") 1))

(defun php-template-xml-set-object ()
  "Insert a xml_set_object statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_object" '("object") 1))

(defun php-template-xml-set-processing-instruction-handler ()
  "Insert a xml_set_processing_instruction_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_processing_instruction_handler" '("handler") 1))

(defun php-template-xml-set-start-namespace-decl-handler ()
  "Insert a xml_set_start_namespace_decl_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_start_namespace_decl_handler" '("handler") 1))

(defun php-template-xml-set-unparsed-entity-decl-handler ()
  "Insert a xml_set_unparsed_entity_decl_handler statement."
  (interactive)
  (php-template-xml-generic-function "xml_set_unparsed_entity_decl_handler" '("handler") 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .NET
(defun php-template-dotnet-load-hook ()
  (php-hooked-abbrev 'php-template-dotnet-load))

;; Apache
(defun php-template-apache-child-terminate-hook ()
  (php-hooked-abbrev 'php-template-apache-child-terminate))
(defun php-template-apache-get-modules-hook ()
  (php-hooked-abbrev 'php-template-apache-get-modules))
(defun php-template-apache-get-version-hook ()
  (php-hooked-abbrev 'php-template-apache-get-version))
(defun php-template-apache-getenv-hook ()
  (php-hooked-abbrev 'php-template-apache-getenv))
(defun php-template-apache-lookup-uri-hook ()
  (php-hooked-abbrev 'php-template-apache-lookup-uri))
(defun php-template-apache-note-hook ()
  (php-hooked-abbrev 'php-template-apache-note))
(defun php-template-apache-request-headers-hook ()
  (php-hooked-abbrev 'php-template-apache-request-headers))
(defun php-template-apache-reset-timeout-hook ()
  (php-hooked-abbrev 'php-template-apache-reset-timeout))
(defun php-template-apache-response-headers-hook ()
  (php-hooked-abbrev 'php-template-apache-response-headers))
(defun php-template-apache-setenv-hook ()
  (php-hooked-abbrev 'php-template-apache-setenv))
(defun php-template-ascii2ebcdic-hook ()
  (php-hooked-abbrev 'php-template-ascii2ebcdic))
(defun php-template-ebcdic2ascii-hook ()
  (php-hooked-abbrev 'php-template-ebcdic2ascii))
(defun php-template-getallheaders-hook ()
  (php-hooked-abbrev 'php-template-getallheaders))
(defun php-template-virtual-hook ()
  (php-hooked-abbrev 'php-template-virtual))

;; APC
(defun php-template-apc-cache-info-hook ()
  (php-hooked-abbrev 'php-template-apc-cache-info))
(defun php-template-apc-clear-cache-hook ()
  (php-hooked-abbrev 'php-template-apc-clear-cache))
(defun php-template-apc-define-constants-hook ()
  (php-hooked-abbrev 'php-template-apc-define-constants))
(defun php-template-apc-delete-hook ()
  (php-hooked-abbrev 'php-template-apc-delete))
(defun php-template-apc-fetch-hook ()
  (php-hooked-abbrev 'php-template-apc-fetch))
(defun php-template-apc-load-constants-hook ()
  (php-hooked-abbrev 'php-template-apc-load-constants))
(defun php-template-apc-sma-info-hook ()
  (php-hooked-abbrev 'php-template-apc-sma-info))
(defun php-template-apc-store-hook ()
  (php-hooked-abbrev 'php-template-apc-store))

;; Array functions
(defun php-template-array-change-key-case-hook ()
  (php-hooked-abbrev 'php-template-array-change-key-case))
(defun php-template-array-chunk-hook ()
  (php-hooked-abbrev 'php-template-array-chunk))
(defun php-template-array-combine-hook ()
  (php-hooked-abbrev 'php-template-array-combine))
(defun php-template-array-count-values-hook ()
  (php-hooked-abbrev 'php-template-array-count-values))
(defun php-template-array-diff-assoc-hook ()
  (php-hooked-abbrev 'php-template-array-diff-assoc))
(defun php-template-array-diff-key-hook ()
  (php-hooked-abbrev 'php-template-array-diff-key))
(defun php-template-array-diff-uassoc-hook ()
  (php-hooked-abbrev 'php-template-array-diff-uassoc))
(defun php-template-array-diff-ukey-hook ()
  (php-hooked-abbrev 'php-template-array-diff-ukey))
(defun php-template-array-diff-hook ()
  (php-hooked-abbrev 'php-template-array-diff))
(defun php-template-array-fill-hook ()
  (php-hooked-abbrev 'php-template-array-fill))
(defun php-template-array-filter-hook ()
  (php-hooked-abbrev 'php-template-array-filter))
(defun php-template-array-flip-hook ()
  (php-hooked-abbrev 'php-template-array-flip))
(defun php-template-array-intersect-assoc-hook ()
  (php-hooked-abbrev 'php-template-array-intersect-assoc))
(defun php-template-array-intersect-key-hook ()
  (php-hooked-abbrev 'php-template-array-intersect-key))
(defun php-template-array-intersect-uassoc-hook ()
  (php-hooked-abbrev 'php-template-array-intersect-uassoc))
(defun php-template-array-intersect-ukey-hook ()
  (php-hooked-abbrev 'php-template-array-intersect-ukey))
(defun php-template-array-intersect-hook ()
  (php-hooked-abbrev 'php-template-array-intersect))
(defun php-template-array-exists-hook ()
  (php-hooked-abbrev 'php-template-array-exists))
(defun php-template-array-keys-hook ()
  (php-hooked-abbrev 'php-template-array-keys))
(defun php-template-array-map-hook ()
  (php-hooked-abbrev 'php-template-array-map))
(defun php-template-array-merge-recursive-hook ()
  (php-hooked-abbrev 'php-template-array-merge-recursive))
(defun php-template-array-merge-hook ()
  (php-hooked-abbrev 'php-template-array-merge))
(defun php-template-array-multisort-hook ()
  (php-hooked-abbrev 'php-template-array-multisort))
(defun php-template-array-pad-hook ()
  (php-hooked-abbrev 'php-template-array-pad))
(defun php-template-array-pop-hook ()
  (php-hooked-abbrev 'php-template-array-pop))
(defun php-template-array-product-hook ()
  (php-hooked-abbrev 'php-template-array-product))
(defun php-template-array-push-hook ()
  (php-hooked-abbrev 'php-template-array-push))
(defun php-template-array-rand-hook ()
  (php-hooked-abbrev 'php-template-array-rand))
(defun php-template-array-reduce-hook ()
  (php-hooked-abbrev 'php-template-array-reduce))
(defun php-template-array-reverse-hook ()
  (php-hooked-abbrev 'php-template-array-reverse))
(defun php-template-array-search-hook ()
  (php-hooked-abbrev 'php-template-array-search))
(defun php-template-array-shift-hook ()
  (php-hooked-abbrev 'php-template-array-shift))
(defun php-template-array-slice-hook ()
  (php-hooked-abbrev 'php-template-array-slice))
(defun php-template-array-splice-hook ()
  (php-hooked-abbrev 'php-template-array-splice))
(defun php-template-array-sum-hook ()
  (php-hooked-abbrev 'php-template-array-sum))
(defun php-template-array-udiff-assoc-hook ()
  (php-hooked-abbrev 'php-template-array-udiff-assoc))
(defun php-template-array-udiff-uassoc-hook ()
  (php-hooked-abbrev 'php-template-array-udiff-uassoc))
(defun php-template-array-udiff-hook ()
  (php-hooked-abbrev 'php-template-array-udiff))
(defun php-template-array-uintersect-assoc-hook ()
  (php-hooked-abbrev 'php-template-array-uintersect-assoc))
(defun php-template-array-uintersect-uassoc-hook ()
  (php-hooked-abbrev 'php-template-array-uintersect-uassoc))
(defun php-template-array-uintersect-hook ()
  (php-hooked-abbrev 'php-template-array-uintersect))
(defun php-template-array-unique-hook ()
  (php-hooked-abbrev 'php-template-array-unique))
(defun php-template-array-unshift-hook ()
  (php-hooked-abbrev 'php-template-array-unshift))
(defun php-template-array-values-hook ()
  (php-hooked-abbrev 'php-template-array-values))
(defun php-template-array-walk-recursive-hook ()
  (php-hooked-abbrev 'php-template-array-walk-recursive))
(defun php-template-array-walk-hook ()
  (php-hooked-abbrev 'php-template-array-walk))
(defun php-template-array-hook ()
  (php-hooked-abbrev 'php-template-array))
(defun php-template-arsort-hook ()
  (php-hooked-abbrev 'php-template-arsort))
(defun php-template-asort-hook ()
  (php-hooked-abbrev 'php-template-asort))
(defun php-template-compact-hook ()
  (php-hooked-abbrev 'php-template-compact))
(defun php-template-count-hook ()
  (php-hooked-abbrev 'php-template-count))
(defun php-template-current-hook ()
  (php-hooked-abbrev 'php-template-current))
(defun php-template-each-hook ()
  (php-hooked-abbrev 'php-template-each))
(defun php-template-end-hook ()
  (php-hooked-abbrev 'php-template-end))
(defun php-template-extract-hook ()
  (php-hooked-abbrev 'php-template-extract))
(defun php-template-in-array-hook ()
  (php-hooked-abbrev 'php-template-in-array))
(defun php-template-key-hook ()
  (php-hooked-abbrev 'php-template-key))
(defun php-template-krsort-hook ()
  (php-hooked-abbrev 'php-template-krsort))
(defun php-template-ksort-hook ()
  (php-hooked-abbrev 'php-template-ksort))
(defun php-template-list-hook ()
  (php-hooked-abbrev 'php-template-list))
(defun php-template-natcasesort-hook ()
  (php-hooked-abbrev 'php-template-natcasesort))
(defun php-template-natsort-hook ()
  (php-hooked-abbrev 'php-template-natsort))
(defun php-template-next-hook ()
  (php-hooked-abbrev 'php-template-next))
(defun php-template-pos-hook ()
  (php-hooked-abbrev 'php-template-pos))
(defun php-template-prev-hook ()
  (php-hooked-abbrev 'php-template-prev))
(defun php-template-range-hook ()
  (php-hooked-abbrev 'php-template-range))
(defun php-template-reset-hook ()
  (php-hooked-abbrev 'php-template-reset))
(defun php-template-rsort-hook ()
  (php-hooked-abbrev 'php-template-rsort))
(defun php-template-shuffle-hook ()
  (php-hooked-abbrev 'php-template-shuffle))
(defun php-template-sizeof-hook ()
  (php-hooked-abbrev 'php-template-sizeof))
(defun php-template-sort-hook ()
  (php-hooked-abbrev 'php-template-sort))
(defun php-template-uasort-hook ()
  (php-hooked-abbrev 'php-template-uasort))
(defun php-template-uksort-hook ()
  (php-hooked-abbrev 'php-template-uksort))
(defun php-template-usort-hook ()
  (php-hooked-abbrev 'php-template-usort))

;; Control Structures
(defun php-template-break-hook ()
  (php-hooked-abbrev 'php-template-beak))
(defun php-template-continue-hook ()
  (php-hooked-abbrev 'php-template-continue))
(defun php-template-do-while-hook ()
  (php-hooked-abbrev 'php-template-do-while))
(defun php-template-else-hook ()
  (php-hooked-abbrev 'php-template-else))
(defun php-template-elseif-hook ()
  (php-hooked-abbrev 'php-template-elseif))
(defun php-template-for-hook ()
  (php-hooked-abbrev 'php-template-for))
(defun php-template-foreach-hook ()
  (php-hooked-abbrev 'php-template-foreach))
(defun php-template-if-hook ()
  (php-hooked-abbrev 'php-template-if))
(defun php-template-include-hook ()
  (php-hooked-abbrev 'php-template-include))
(defun php-template-include-once-hook ()
  (php-hooked-abbrev 'php-template-include-once))
(defun php-template-require-hook ()
  (php-hooked-abbrev 'php-template-require))
(defun php-template-require-once-hook ()
  (php-hooked-abbrev 'php-template-require-once))
(defun php-template-return-hook ()
  (php-hooked-abbrev 'php-template-return))
(defun php-template-switch-hook ()
  (php-hooked-abbrev 'php-template-switch))
(defun php-template-while-hook ()
  (php-hooked-abbrev 'php-template-while))

;; Date and Time functions
(defun php-template-checkdate-hook ()
  (php-hooked-abbrev 'php-template-checkdate))
(defun php-template-date-default-timezone-get-hook ()
  (php-hooked-abbrev 'php-template-date-default-timezone-get))
(defun php-template-date-default-timezone-set-hook ()
  (php-hooked-abbrev 'php-template-date-default-timezone-set))
(defun php-template-date-sunrise-hook ()
  (php-hooked-abbrev 'php-template-date-sunrise))
(defun php-template-date-sunset-hook ()
  (php-hooked-abbrev 'php-template-date-sunset))
(defun php-template-date-hook ()
  (php-hooked-abbrev 'php-template-date))
(defun php-template-getdate-hook ()
  (php-hooked-abbrev 'php-template-getdate))
(defun php-template-gettimeofday-hook ()
  (php-hooked-abbrev 'php-template-gettimeofday))
(defun php-template-gmdate-hook ()
  (php-hooked-abbrev 'php-template-gmdate))
(defun php-template-gmmktime-hook ()
  (php-hooked-abbrev 'php-template-gmmktime))
(defun php-template-gmstrftime-hook ()
  (php-hooked-abbrev 'php-template-gmstrftime))
(defun php-template-idate-hook ()
  (php-hooked-abbrev 'php-template-idate))
(defun php-template-localtime-hook ()
  (php-hooked-abbrev 'php-template-localtime))
(defun php-template-microtime-hook ()
  (php-hooked-abbrev 'php-template-microtime))
(defun php-template-mktime-hook ()
  (php-hooked-abbrev 'php-template-mktime))
(defun php-template-strftime-hook ()
  (php-hooked-abbrev 'php-template-strftime))
(defun php-template-strptime-hook ()
  (php-hooked-abbrev 'php-template-strptime))
(defun php-template-strtotime-hook ()
  (php-hooked-abbrev 'php-template-strtotime))
(defun php-template-time-hook ()
  (php-hooked-abbrev 'php-template-time))

;; Directory functions
(defun php-template-chdir-hook ()
  (php-hooked-abbrev 'php-template-chdir))
(defun php-template-chroot-hook ()
  (php-hooked-abbrev 'php-template-chroot))
(defun php-template-dir-hook ()
  (php-hooked-abbrev 'php-template-dir))
(defun php-template-closedir-hook ()
  (php-hooked-abbrev 'php-template-closedir))
(defun php-template-getcwd-hook ()
  (php-hooked-abbrev 'php-template-getcwd))
(defun php-template-opendir-hook ()
  (php-hooked-abbrev 'php-template-opendir))
(defun php-template-readdir-hook ()
  (php-hooked-abbrev 'php-template-readdir))
(defun php-template-rewinddir-hook ()
  (php-hooked-abbrev 'php-template-rewinddir))
(defun php-template-scandir-hook ()
  (php-hooked-abbrev 'php-template-scandir))

;; Error and Logging
(defun php-template-debug-backtrace-hook ()
  (php-hooked-abbrev 'php-template-debug-backtrace))
(defun php-template-debug-print-backtrace-hook ()
  (php-hooked-abbrev 'php-template-debug-print-backtrace))
(defun php-template-error-log-hook ()
  (php-hooked-abbrev 'php-template-error-log))
(defun php-template-error-reporting-hook ()
  (php-hooked-abbrev 'php-template-error-reporting))
(defun php-template-restore-error-handler-hook ()
  (php-hooked-abbrev 'php-template-restore-error-handler))
(defun php-template-restore-exception-handler-hook ()
  (php-hooked-abbrev 'php-template-restore-exception-handler))
(defun php-template-set-error-handler-hook ()
  (php-hooked-abbrev 'php-template-set-error-handler))
(defun php-template-set-exception-handler-hook ()
  (php-hooked-abbrev 'php-template-set-exception-handler))
(defun php-template-trigger-error-hook ()
  (php-hooked-abbrev 'php-template-trigger-error))
(defun php-template-user-error-hook ()
  (php-hooked-abbrev 'php-template-user-error))

;; File System
(defun php-template-basename-hook ()
  (php-hooked-abbrev 'php-template-basename))
(defun php-template-chgrp-hook ()
  (php-hooked-abbrev 'php-template-chgrp))
(defun php-template-chmod-hook ()
  (php-hooked-abbrev 'php-template-chmod))
(defun php-template-chown-hook ()
  (php-hooked-abbrev 'php-template-chown))
(defun php-template-clearstatcache-hook ()
  (php-hooked-abbrev 'php-template-clearstatcache))
(defun php-template-copy-hook ()
  (php-hooked-abbrev 'php-template-copy))
(defun php-template-delete-hook ()
  (php-hooked-abbrev 'php-template-delete))
(defun php-template-dirname-hook ()
  (php-hooked-abbrev 'php-template-dirname))
(defun php-template-disk-free-space-hook ()
  (php-hooked-abbrev 'php-template-disk-free-space))
(defun php-template-disk-total-space-hook ()
  (php-hooked-abbrev 'php-template-disk-total-space))
(defun php-template-fclose-hook ()
  (php-hooked-abbrev 'php-template-fclose))
(defun php-template-feof-hook ()
  (php-hooked-abbrev 'php-template-feof))
(defun php-template-fflush-hook ()
  (php-hooked-abbrev 'php-template-fflush))
(defun php-template-fgetc-hook ()
  (php-hooked-abbrev 'php-template-fgetc))
(defun php-template-fgetcsv-hook ()
  (php-hooked-abbrev 'php-template-fgetcsv))
(defun php-template-fgets-hook ()
  (php-hooked-abbrev 'php-template-fgets))
(defun php-template-fgetss-hook ()
  (php-hooked-abbrev 'php-template-fgetss))
(defun php-template-file-exists-hook ()
  (php-hooked-abbrev 'php-template-file-exists))
(defun php-template-file-get-contents-hook ()
  (php-hooked-abbrev 'php-template-file-get-contents))
(defun php-template-file-put-contents-hook ()
  (php-hooked-abbrev 'php-template-file-put-contents))
(defun php-template-file-hook ()
  (php-hooked-abbrev 'php-template-file))
(defun php-template-fileatime-hook ()
  (php-hooked-abbrev 'php-template-fileatime))
(defun php-template-filectime-hook ()
  (php-hooked-abbrev 'php-template-filectime))
(defun php-template-filegroup-hook ()
  (php-hooked-abbrev 'php-template-filegroup))
(defun php-template-fileinode-hook ()
  (php-hooked-abbrev 'php-template-fileinode))
(defun php-template-filemtime-hook ()
  (php-hooked-abbrev 'php-template-filemtime))
(defun php-template-fileowner-hook ()
  (php-hooked-abbrev 'php-template-fileowner))
(defun php-template-fileperms-hook ()
  (php-hooked-abbrev 'php-template-fileperms))
(defun php-template-filesize-hook ()
  (php-hooked-abbrev 'php-template-filesize))
(defun php-template-filetype-hook ()
  (php-hooked-abbrev 'php-template-filetype))
(defun php-template-flock-hook ()
  (php-hooked-abbrev 'php-template-flock))
(defun php-template-fnmatch-hook ()
  (php-hooked-abbrev 'php-template-fnmatch))
(defun php-template-fopen-hook ()
  (php-hooked-abbrev 'php-template-fopen))
(defun php-template-fpassthru-hook ()
  (php-hooked-abbrev 'php-template-fpassthru))
(defun php-template-fputcsv-hook ()
  (php-hooked-abbrev 'php-template-fputcsv))
(defun php-template-fread-hook ()
  (php-hooked-abbrev 'php-template-fread))
(defun php-template-fscanf-hook ()
  (php-hooked-abbrev 'php-template-fscanf))
(defun php-template-fseek-hook ()
  (php-hooked-abbrev 'php-template-fseek))
(defun php-template-fstat-hook ()
  (php-hooked-abbrev 'php-template-fstat))
(defun php-template-ftell-hook ()
  (php-hooked-abbrev 'php-template-ftell))
(defun php-template-ftruncate-hook ()
  (php-hooked-abbrev 'php-template-ftruncate))
(defun php-template-fwrite-hook ()
  (php-hooked-abbrev 'php-template-fwrite))
(defun php-template-glob-hook ()
  (php-hooked-abbrev 'php-template-glob))
(defun php-template-is-dir-hook ()
  (php-hooked-abbrev 'php-template-is-dir))
(defun php-template-is-executable-hook ()
  (php-hooked-abbrev 'php-template-is-executable))
(defun php-template-is-file-hook ()
  (php-hooked-abbrev 'php-template-is-file))
(defun php-template-is-link-hook ()
  (php-hooked-abbrev 'php-template-is-link))
(defun php-template-is-readable-hook ()
  (php-hooked-abbrev 'php-template-is-readable))
(defun php-template-is-uploaded-file-hook ()
  (php-hooked-abbrev 'php-template-is-uploaded-file))
(defun php-template-is-writable-hook ()
  (php-hooked-abbrev 'php-template-is-writable))
(defun php-template-link-hook ()
  (php-hooked-abbrev 'php-template-link))
(defun php-template-linkinfo-hook ()
  (php-hooked-abbrev 'php-template-linkinfo))
(defun php-template-lstat-hook ()
  (php-hooked-abbrev 'php-template-lstat))
(defun php-template-mkdir-hook ()
  (php-hooked-abbrev 'php-template-mkdir))
(defun php-template-move-uploaded-file-hook ()
  (php-hooked-abbrev 'php-template-move-uploaded-file))
(defun php-template-parse-ini-file-hook ()
  (php-hooked-abbrev 'php-template-parse-ini-file))
(defun php-template-pathinfo-hook ()
  (php-hooked-abbrev 'php-template-pathinfo))
(defun php-template-pclose-hook ()
  (php-hooked-abbrev 'php-template-pclose))
(defun php-template-popen-hook ()
  (php-hooked-abbrev 'php-template-popen))
(defun php-template-readfile-hook ()
  (php-hooked-abbrev 'php-template-readfile))
(defun php-template-readlink-hook ()
  (php-hooked-abbrev 'php-template-readlink))
(defun php-template-realpath-hook ()
  (php-hooked-abbrev 'php-template-realpath))
(defun php-template-rename-hook ()
  (php-hooked-abbrev 'php-template-rename))
(defun php-template-rewind-hook ()
  (php-hooked-abbrev 'php-template-rewind))
(defun php-template-rmdir-hook ()
  (php-hooked-abbrev 'php-template-rmdir))
(defun php-template-stat-hook ()
  (php-hooked-abbrev 'php-template-stat))
(defun php-template-symlink-hook ()
  (php-hooked-abbrev 'php-template-symlink))
(defun php-template-tempnam-hook ()
  (php-hooked-abbrev 'php-template-tempnam))
(defun php-template-tmpfile-hook ()
  (php-hooked-abbrev 'php-template-tmpfile))
(defun php-template-touch-hook ()
  (php-hooked-abbrev 'php-template-touch))
(defun php-template-umask-hook ()
  (php-hooked-abbrev 'php-template-umask))
(defun php-template-unlink-hook ()
  (php-hooked-abbrev 'php-template-unlink))

;; Functions
(defun php-template-call-user-func-array-hook ()
  (php-hooked-abbrev 'php-template-call-user-func-array))
(defun php-template-call-user-func-hook ()
  (php-hooked-abbrev 'php-template-call-user-func))
(defun php-template-create-function-hook ()
  (php-hooked-abbrev 'php-template-create-function))
(defun php-template-func-get-arg-hook ()
  (php-hooked-abbrev 'php-template-func-get-arg))
(defun php-template-func-get-args-hook ()
  (php-hooked-abbrev 'php-template-func-get-args))
(defun php-template-func-num-args-hook ()
  (php-hooked-abbrev 'php-template-func-num-args))
(defun php-template-function-exists-hook ()
  (php-hooked-abbrev 'php-template-function-exists))
(defun php-template-get-defined-functions-hook ()
  (php-hooked-abbrev 'php-template-get-defined-functions))
(defun php-template-register-shutdown-function-hook ()
  (php-hooked-abbrev 'php-template-register-shutdown-function))
(defun php-template-register-tick-function-hook ()
  (php-hooked-abbrev 'php-template-register-tick-function))
(defun php-template-unregister-tick-function-hook ()
  (php-hooked-abbrev 'php-template-unregister-tick-function))

;; Image
(defun php-template-gd-info-hook ()
  (php-hooked-abbrev 'php-template-gd-info))
(defun php-template-getimagesize-hook ()
  (php-hooked-abbrev 'php-template-getimagesize))
(defun php-template-image-type-to-extension-hook ()
  (php-hooked-abbrev 'php-template-image-type-to-extension))
(defun php-template-image-type-to-mime-type-hook ()
  (php-hooked-abbrev 'php-template-image-type-to-mime-type))
(defun php-template-image2wbmp-hook ()
  (php-hooked-abbrev 'php-template-image2wbmp))
(defun php-template-imagealphablending-hook ()
  (php-hooked-abbrev 'php-template-imagealphablending))
(defun php-template-imageantialias-hook ()
  (php-hooked-abbrev 'php-template-imageantialias))
(defun php-template-imagearc-hook ()
  (php-hooked-abbrev 'php-template-imagearc))
(defun php-template-imagechar-hook ()
  (php-hooked-abbrev 'php-template-imagechar))
(defun php-template-imagecharup-hook ()
  (php-hooked-abbrev 'php-template-imagecharup))
(defun php-template-imagecolorallocate-hook ()
  (php-hooked-abbrev 'php-template-imagecolorallocate))
(defun php-template-imagecolorallocatealpha-hook ()
  (php-hooked-abbrev 'php-template-imagecolorallocatealpha))
(defun php-template-imagecolorat-hook ()
  (php-hooked-abbrev 'php-template-imagecolorat))
(defun php-template-imagecolorclosest-hook ()
  (php-hooked-abbrev 'php-template-imagecolorclosest))
(defun php-template-imagecolorclosestalpha-hook ()
  (php-hooked-abbrev 'php-template-imagecolorclosestalpha))
(defun php-template-imagecolorclosesthwb-hook ()
  (php-hooked-abbrev 'php-template-imagecolorclosesthwb))
(defun php-template-imagecolordeallocate-hook ()
  (php-hooked-abbrev 'php-template-imagecolordeallocate))
(defun php-template-imagecolorexact-hook ()
  (php-hooked-abbrev 'php-template-imagecolorexact))
(defun php-template-imagecolorexactalpha-hook ()
  (php-hooked-abbrev 'php-template-imagecolorexactalpha))
(defun php-template-imagecolormatch-hook ()
  (php-hooked-abbrev 'php-template-imagecolormatch))
(defun php-template-imagecolorresolve-hook ()
  (php-hooked-abbrev 'php-template-imagecolorresolve))
(defun php-template-imagecolorresolvealpha-hook ()
  (php-hooked-abbrev 'php-template-imagecolorresolvealpha))
(defun php-template-imagecolorset-hook ()
  (php-hooked-abbrev 'php-template-imagecolorset))
(defun php-template-imagecolorsforindex-hook ()
  (php-hooked-abbrev 'php-template-imagecolorsforindex))
(defun php-template-imagecolorstotal-hook ()
  (php-hooked-abbrev 'php-template-imagecolorstotal))
(defun php-template-imagecolortransparent-hook ()
  (php-hooked-abbrev 'php-template-imagecolortransparent))
(defun php-template-imageconvolution-hook ()
  (php-hooked-abbrev 'php-template-imageconvolution))
(defun php-template-imagecopy-hook ()
  (php-hooked-abbrev 'php-template-imagecopy))
(defun php-template-imagecopymerge-hook ()
  (php-hooked-abbrev 'php-template-imagecopymerge))
(defun php-template-imagecopymergegray-hook ()
  (php-hooked-abbrev 'php-template-imagecopymergegray))
(defun php-template-imagecopyresampled-hook ()
  (php-hooked-abbrev 'php-template-imagecopyresampled))
(defun php-template-imagecopyresized-hook ()
  (php-hooked-abbrev 'php-template-imagecopyresized))
(defun php-template-imagecreate-hook ()
  (php-hooked-abbrev 'php-template-imagecreate))
(defun php-template-imagecreatefromgd2-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromgd2))
(defun php-template-imagecreatefromgd2part-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromgd2part))
(defun php-template-imagecreatefromgd-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromgd))
(defun php-template-imagecreatefromgif-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromgif))
(defun php-template-imagecreatefromjpeg-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromjpeg))
(defun php-template-imagecreatefrompng-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefrompng))
(defun php-template-imagecreatefromstring-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromstring))
(defun php-template-imagecreatefromwbmp-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromwbmp))
(defun php-template-imagecreatefromxbm-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromxbm))
(defun php-template-imagecreatefromxpm-hook ()
  (php-hooked-abbrev 'php-template-imagecreatefromxpm))
(defun php-template-imagecreatetruecolor-hook ()
  (php-hooked-abbrev 'php-template-imagecreatetruecolor))
(defun php-template-imagedashedline-hook ()
  (php-hooked-abbrev 'php-template-imagedashedline))
(defun php-template-imagedestroy-hook ()
  (php-hooked-abbrev 'php-template-imagedestroy))
(defun php-template-imageellipse-hook ()
  (php-hooked-abbrev 'php-template-imageellipse))
(defun php-template-imagefill-hook ()
  (php-hooked-abbrev 'php-template-imagefill))
(defun php-template-imagefilledarc-hook ()
  (php-hooked-abbrev 'php-template-imagefilledarc))
(defun php-template-imagefilledellipse-hook ()
  (php-hooked-abbrev 'php-template-imagefilledellipse))
(defun php-template-imagefilledpolygon-hook ()
  (php-hooked-abbrev 'php-template-imagefilledpolygon))
(defun php-template-imagefilledrectangle-hook ()
  (php-hooked-abbrev 'php-template-imagefilledrectangle))
(defun php-template-imagefilltoborder-hook ()
  (php-hooked-abbrev 'php-template-imagefilltoborder))
(defun php-template-imagefilter-hook ()
  (php-hooked-abbrev 'php-template-imagefilter))
(defun php-template-imagefontheight-hook ()
  (php-hooked-abbrev 'php-template-imagefontheight))
(defun php-template-imagefontwidth-hook ()
  (php-hooked-abbrev 'php-template-imagefontwidth))
(defun php-template-imageftbbox-hook ()
  (php-hooked-abbrev 'php-template-imageftbbox))
(defun php-template-imagefttext-hook ()
  (php-hooked-abbrev 'php-template-imagefttext))
(defun php-template-imagegammacorrect-hook ()
  (php-hooked-abbrev 'php-template-imagegammacorrect))
(defun php-template-imagegd2-hook ()
  (php-hooked-abbrev 'php-template-imagegd2))
(defun php-template-imagegd-hook ()
  (php-hooked-abbrev 'php-template-imagegd))
(defun php-template-imagegif-hook ()
  (php-hooked-abbrev 'php-template-imagegif))
(defun php-template-imageinterlace-hook ()
  (php-hooked-abbrev 'php-template-imageinterlace))
(defun php-template-imageistruecolor-hook ()
  (php-hooked-abbrev 'php-template-imageistruecolor))
(defun php-template-imagejpeg-hook ()
  (php-hooked-abbrev 'php-template-imagejpeg))
(defun php-template-imagelayereffect-hook ()
  (php-hooked-abbrev 'php-template-imagelayereffect))
(defun php-template-imageline-hook ()
  (php-hooked-abbrev 'php-template-imageline))
(defun php-template-imageloadfont-hook ()
  (php-hooked-abbrev 'php-template-imageloadfont))
(defun php-template-imagepalettecopy-hook ()
  (php-hooked-abbrev 'php-template-imagepalettecopy))
(defun php-template-imagepng-hook ()
  (php-hooked-abbrev 'php-template-imagepng))
(defun php-template-imagepolygon-hook ()
  (php-hooked-abbrev 'php-template-imagepolygon))
(defun php-template-imagepsbbox-hook ()
  (php-hooked-abbrev 'php-template-imagepsbbox))
(defun php-template-imagepsencodefont-hook ()
  (php-hooked-abbrev 'php-template-imagepsencodefont))
(defun php-template-imagepsextendfont-hook ()
  (php-hooked-abbrev 'php-template-imagepsextendfont))
(defun php-template-imagepsfreefont-hook ()
  (php-hooked-abbrev 'php-template-imagepsfreefont))
(defun php-template-imagepsloadfont-hook ()
  (php-hooked-abbrev 'php-template-imagepsloadfont))
(defun php-template-imagepsslantfont-hook ()
  (php-hooked-abbrev 'php-template-imagepsslantfont))
(defun php-template-imagepstext-hook ()
  (php-hooked-abbrev 'php-template-imagepstext))
(defun php-template-imagerectangle-hook ()
  (php-hooked-abbrev 'php-template-imagerectangle))
(defun php-template-imagerotate-hook ()
  (php-hooked-abbrev 'php-template-imagerotate))
(defun php-template-imagesavealpha-hook ()
  (php-hooked-abbrev 'php-template-imagesavealpha))
(defun php-template-imagesetbrush-hook ()
  (php-hooked-abbrev 'php-template-imagesetbrush))
(defun php-template-imagesetpixel-hook ()
  (php-hooked-abbrev 'php-template-imagesetpixel))
(defun php-template-imagesetstyle-hook ()
  (php-hooked-abbrev 'php-template-imagesetstyle))
(defun php-template-imagesetthickness-hook ()
  (php-hooked-abbrev 'php-template-imagesetthickness))
(defun php-template-imagesettile-hook ()
  (php-hooked-abbrev 'php-template-imagesettile))
(defun php-template-imagestring-hook ()
  (php-hooked-abbrev 'php-template-imagestring))
(defun php-template-imagestringup-hook ()
  (php-hooked-abbrev 'php-template-imagestringup))
(defun php-template-imagesx-hook ()
  (php-hooked-abbrev 'php-template-imagesx))
(defun php-template-imagesy-hook ()
  (php-hooked-abbrev 'php-template-imagesy))
(defun php-template-imagetruecolortopalette-hook ()
  (php-hooked-abbrev 'php-template-imagetruecolortopalette))
(defun php-template-imagettfbbox-hook ()
  (php-hooked-abbrev 'php-template-imagettfbbox))
(defun php-template-imagettftext-hook ()
  (php-hooked-abbrev 'php-template-imagettftext))
(defun php-template-imagetypes-hook ()
  (php-hooked-abbrev 'php-template-imagetypes))
(defun php-template-imagewbmp-hook ()
  (php-hooked-abbrev 'php-template-imagewbmp))
(defun php-template-imagexbm-hook ()
  (php-hooked-abbrev 'php-template-imagexbm))
(defun php-template-iptcembed-hook ()
  (php-hooked-abbrev 'php-template-iptcembed))
(defun php-template-iptcparse-hook ()
  (php-hooked-abbrev 'php-template-iptcparse))
(defun php-template-jpeg2wbmp-hook ()
  (php-hooked-abbrev 'php-template-jpeg2wbmp))
(defun php-template-png2wbmp-hook ()
  (php-hooked-abbrev 'php-template-png2wbmp))

;; Mail
(defun php-template-ezmlm-hash-hook ()
  (php-hooked-abbrev 'php-template-ezmlm-hash))
(defun php-template-mail-hook ()
  (php-hooked-abbrev 'php-template-mail))

;; Mathematical
(defun php-template-abs-hook ()
  (php-hooked-abbrev 'php-template-abs))
(defun php-template-acos-hook ()
  (php-hooked-abbrev 'php-template-acos))
(defun php-template-acosh-hook ()
  (php-hooked-abbrev 'php-template-acosh))
(defun php-template-asin-hook ()
  (php-hooked-abbrev 'php-template-asin))
(defun php-template-asinh-hook ()
  (php-hooked-abbrev 'php-template-asinh))
(defun php-template-atan2-hook ()
  (php-hooked-abbrev 'php-template-atan2))
(defun php-template-atan-hook ()
  (php-hooked-abbrev 'php-template-atan))
(defun php-template-atanh-hook ()
  (php-hooked-abbrev 'php-template-atanh))
(defun php-template-base-convert-hook ()
  (php-hooked-abbrev 'php-template-base-convert))
(defun php-template-bindec-hook ()
  (php-hooked-abbrev 'php-template-bindec))
(defun php-template-ceil-hook ()
  (php-hooked-abbrev 'php-template-ceil))
(defun php-template-cos-hook ()
  (php-hooked-abbrev 'php-template-cos))
(defun php-template-cosh-hook ()
  (php-hooked-abbrev 'php-template-cosh))
(defun php-template-decbin-hook ()
  (php-hooked-abbrev 'php-template-decbin))
(defun php-template-dechex-hook ()
  (php-hooked-abbrev 'php-template-dechex))
(defun php-template-decoct-hook ()
  (php-hooked-abbrev 'php-template-decoct))
(defun php-template-deg2rad-hook ()
  (php-hooked-abbrev 'php-template-deg2rad))
(defun php-template-exp-hook ()
  (php-hooked-abbrev 'php-template-exp))
(defun php-template-expm1-hook ()
  (php-hooked-abbrev 'php-template-expm1))
(defun php-template-floor-hook ()
  (php-hooked-abbrev 'php-template-floor))
(defun php-template-fmod-hook ()
  (php-hooked-abbrev 'php-template-fmod))
(defun php-template-getrandmax-hook ()
  (php-hooked-abbrev 'php-template-getrandmax))
(defun php-template-hexdec-hook ()
  (php-hooked-abbrev 'php-template-hexdec))
(defun php-template-hypot-hook ()
  (php-hooked-abbrev 'php-template-hypot))
(defun php-template-is-finite-hook ()
  (php-hooked-abbrev 'php-template-is-finite))
(defun php-template-is-infinite-hook ()
  (php-hooked-abbrev 'php-template-is-infinite))
(defun php-template-is-nan-hook ()
  (php-hooked-abbrev 'php-template-is-nan))
(defun php-template-lcg-value-hook ()
  (php-hooked-abbrev 'php-template-lcg-value))
(defun php-template-log10-hook ()
  (php-hooked-abbrev 'php-template-log10))
(defun php-template-log1p-hook ()
  (php-hooked-abbrev 'php-template-log1p))
(defun php-template-log-hook ()
  (php-hooked-abbrev 'php-template-log))
(defun php-template-max-hook ()
  (php-hooked-abbrev 'php-template-max))
(defun php-template-min-hook ()
  (php-hooked-abbrev 'php-template-min))
(defun php-template-mt-getrandmax-hook ()
  (php-hooked-abbrev 'php-template-mt-getrandmax))
(defun php-template-mt-rand-hook ()
  (php-hooked-abbrev 'php-template-mt-rand))
(defun php-template-mt-srand-hook ()
  (php-hooked-abbrev 'php-template-mt-srand))
(defun php-template-octdec-hook ()
  (php-hooked-abbrev 'php-template-octdec))
(defun php-template-pi-hook ()
  (php-hooked-abbrev 'php-template-pi))
(defun php-template-pow-hook ()
  (php-hooked-abbrev 'php-template-pow))
(defun php-template-rad2deg-hook ()
  (php-hooked-abbrev 'php-template-rad2deg))
(defun php-template-rand-hook ()
  (php-hooked-abbrev 'php-template-rand))
(defun php-template-round-hook ()
  (php-hooked-abbrev 'php-template-round))
(defun php-template-sin-hook ()
  (php-hooked-abbrev 'php-template-sin))
(defun php-template-sinh-hook ()
  (php-hooked-abbrev 'php-template-sinh))
(defun php-template-sqrt-hook ()
  (php-hooked-abbrev 'php-template-sqrt))
(defun php-template-srand-hook ()
  (php-hooked-abbrev 'php-template-srand))
(defun php-template-tan-hook ()
  (php-hooked-abbrev 'php-template-tan))
(defun php-template-tanh-hook ()
  (php-hooked-abbrev 'php-template-tanh))

;; Miscellaneous
(defun php-template-connection-aborted-hook ()
  (php-hooked-abbrev 'php-template-connection-aborted))
(defun php-template-connection-status-hook ()
  (php-hooked-abbrev 'php-template-connection-status))
(defun php-template-connection-timeout-hook ()
  (php-hooked-abbrev 'php-template-connection-timeout))
(defun php-template-constant-hook ()
  (php-hooked-abbrev 'php-template-constant))
(defun php-template-define-hook ()
  (php-hooked-abbrev 'php-template-define))
(defun php-template-defined-hook ()
  (php-hooked-abbrev 'php-template-defined))
(defun php-template-die-hook ()
  (php-hooked-abbrev 'php-template-die))
(defun php-template-eval-hook ()
  (php-hooked-abbrev 'php-template-eval))
(defun php-template-exit-hook ()
  (php-hooked-abbrev 'php-template-exit))
(defun php-template-get-browser-hook ()
  (php-hooked-abbrev 'php-template-get-browser))
(defun php-template-halt-compiler-hook ()
  (php-hooked-abbrev 'php-template-halt-compiler))
(defun php-template-highlight-file-hook ()
  (php-hooked-abbrev 'php-template-highlight-file))
(defun php-template-highlight-string-hook ()
  (php-hooked-abbrev 'php-template-highlight-string))
(defun php-template-ignore-user-abort-hook ()
  (php-hooked-abbrev 'php-template-ignore-user-abort))
(defun php-template-pack-hook ()
  (php-hooked-abbrev 'php-template-pack))
(defun php-template-php-check-syntax-hook ()
  (php-hooked-abbrev 'php-template-php-check-syntax))
(defun php-template-php-strip-whitespace-hook ()
  (php-hooked-abbrev 'php-template-php-strip-whitespace))
(defun php-template-show-source-hook ()
  (php-hooked-abbrev 'php-template-show-source))
(defun php-template-sleep-hook ()
  (php-hooked-abbrev 'php-template-sleep))
(defun php-template-sys-getloadavg-hook ()
  (php-hooked-abbrev 'php-template-sys-getloadavg))
(defun php-template-time-nanosleep-hook ()
  (php-hooked-abbrev 'php-template-time-nanosleep))
(defun php-template-time-sleep-until-hook ()
  (php-hooked-abbrev 'php-template-time-sleep-until))
(defun php-template-uniqid-hook ()
  (php-hooked-abbrev 'php-template-uniqid))
(defun php-template-unpack-hook ()
  (php-hooked-abbrev 'php-template-unpack))
(defun php-template-usleep-hook ()
  (php-hooked-abbrev 'php-template-usleep))

;; MySQL
(defun php-template-mysql-affected-rows-hook ()
  (php-hooked-abbrev 'php-template-mysql-affected-rows))
(defun php-template-mysql-change-user-hook ()
  (php-hooked-abbrev 'php-template-mysql-change-user))
(defun php-template-mysql-client-encoding-hook ()
  (php-hooked-abbrev 'php-template-mysql-client-encoding))
(defun php-template-mysql-close-hook ()
  (php-hooked-abbrev 'php-template-mysql-close))
(defun php-template-mysql-connect-hook ()
  (php-hooked-abbrev 'php-template-mysql-connect))
(defun php-template-mysql-create-db-hook ()
  (php-hooked-abbrev 'php-template-mysql-create-db))
(defun php-template-mysql-data-seek-hook ()
  (php-hooked-abbrev 'php-template-mysql-data-seek))
(defun php-template-mysql-db-name-hook ()
  (php-hooked-abbrev 'php-template-mysql-db-name))
(defun php-template-mysql-db-query-hook ()
  (php-hooked-abbrev 'php-template-mysql-db-query))
(defun php-template-mysql-drop-db-hook ()
  (php-hooked-abbrev 'php-template-mysql-drop-db))
(defun php-template-mysql-errno-hook ()
  (php-hooked-abbrev 'php-template-mysql-errno))
(defun php-template-mysql-error-hook ()
  (php-hooked-abbrev 'php-template-mysql-error))
(defun php-template-mysql-escape-string-hook ()
  (php-hooked-abbrev 'php-template-mysql-escape-string))
(defun php-template-mysql-fetch-array-hook ()
  (php-hooked-abbrev 'php-template-mysql-fetch-array))
(defun php-template-mysql-fetch-assoc-hook ()
  (php-hooked-abbrev 'php-template-mysql-fetch-assoc))
(defun php-template-mysql-fetch-field-hook ()
  (php-hooked-abbrev 'php-template-mysql-fetch-field))
(defun php-template-mysql-fetch-lengths-hook ()
  (php-hooked-abbrev 'php-template-mysql-fetch-lengths))
(defun php-template-mysql-fetch-object-hook ()
  (php-hooked-abbrev 'php-template-mysql-fetch-object))
(defun php-template-mysql-fetch-row-hook ()
  (php-hooked-abbrev 'php-template-mysql-fetch-row))
(defun php-template-mysql-field-flags-hook ()
  (php-hooked-abbrev 'php-template-mysql-field-flags))
(defun php-template-mysql-field-len-hook ()
  (php-hooked-abbrev 'php-template-mysql-field-len))
(defun php-template-mysql-field-name-hook ()
  (php-hooked-abbrev 'php-template-mysql-field-name))
(defun php-template-mysql-field-seek-hook ()
  (php-hooked-abbrev 'php-template-mysql-field-seek))
(defun php-template-mysql-field-table-hook ()
  (php-hooked-abbrev 'php-template-mysql-field-table))
(defun php-template-mysql-field-type-hook ()
  (php-hooked-abbrev 'php-template-mysql-field-type))
(defun php-template-mysql-free-result-hook ()
  (php-hooked-abbrev 'php-template-mysql-free-result))
(defun php-template-mysql-get-client-info-hook ()
  (php-hooked-abbrev 'php-template-mysql-get-client-info))
(defun php-template-mysql-get-host-info-hook ()
  (php-hooked-abbrev 'php-template-mysql-get-host-info))
(defun php-template-mysql-get-proto-info-hook ()
  (php-hooked-abbrev 'php-template-mysql-get-proto-info))
(defun php-template-mysql-get-server-info-hook ()
  (php-hooked-abbrev 'php-template-mysql-get-server-info))
(defun php-template-mysql-info-hook ()
  (php-hooked-abbrev 'php-template-mysql-info))
(defun php-template-mysql-insert-id-hook ()
  (php-hooked-abbrev 'php-template-mysql-insert-id))
(defun php-template-mysql-list-dbs-hook ()
  (php-hooked-abbrev 'php-template-mysql-list-dbs))
(defun php-template-mysql-list-fields-hook ()
  (php-hooked-abbrev 'php-template-mysql-list-fields))
(defun php-template-mysql-list-processes-hook ()
  (php-hooked-abbrev 'php-template-mysql-list-processes))
(defun php-template-mysql-list-tables-hook ()
  (php-hooked-abbrev 'php-template-mysql-list-tables))
(defun php-template-mysql-num-fields-hook ()
  (php-hooked-abbrev 'php-template-mysql-num-fields))
(defun php-template-mysql-num-rows-hook ()
  (php-hooked-abbrev 'php-template-mysql-num-rows))
(defun php-template-mysql-pconnect-hook ()
  (php-hooked-abbrev 'php-template-mysql-pconnect))
(defun php-template-mysql-ping-hook ()
  (php-hooked-abbrev 'php-template-mysql-ping))
(defun php-template-mysql-query-hook ()
  (php-hooked-abbrev 'php-template-mysql-query))
(defun php-template-mysql-real-escape-string-hook ()
  (php-hooked-abbrev 'php-template-mysql-real-escape-string))
(defun php-template-mysql-result-hook ()
  (php-hooked-abbrev 'php-template-mysql-result))
(defun php-template-mysql-select-db-hook ()
  (php-hooked-abbrev 'php-template-mysql-select-db))
(defun php-template-mysql-stat-hook ()
  (php-hooked-abbrev 'php-template-mysql-stat))
(defun php-template-mysql-tablename-hook ()
  (php-hooked-abbrev 'php-template-mysql-tablename))
(defun php-template-mysql-thread-id-hook ()
  (php-hooked-abbrev 'php-template-mysql-thread-id))
(defun php-template-mysql-unbuffered-query-hook ()
  (php-hooked-abbrev 'php-template-mysql-unbuffered-query))

;; Others
(defun php-template-class-hook ()
  (php-hooked-abbrev 'php-template-class))
(defun php-template-function-hook ()
  (php-hooked-abbrev 'php-template-function))

;; Regular expression
(defun php-template-ereg-replace-hook ()
  (php-hooked-abbrev 'php-template-ereg-replace))
(defun php-template-ereg-hook ()
  (php-hooked-abbrev 'php-template-ereg))
(defun php-template-eregi-replace-hook ()
  (php-hooked-abbrev 'php-template-eregi-replace))
(defun php-template-eregi-hook ()
  (php-hooked-abbrev 'php-template-eregi))
(defun php-template-split-hook ()
  (php-hooked-abbrev 'php-template-split))
(defun php-template-spliti-hook ()
  (php-hooked-abbrev 'php-template-spliti))
(defun php-template-sql-regcase-hook ()
  (php-hooked-abbrev 'php-template-sql-regcase))

;; Session
(defun php-template-session-cache-expire-hook ()
  (php-hooked-abbrev 'php-template-session-cache-expire))
(defun php-template-session-cache-limiter-hook ()
  (php-hooked-abbrev 'php-template-session-cache-limiter))
(defun php-template-session-commit-hook ()
  (php-hooked-abbrev 'php-template-session-commit))
(defun php-template-session-decode-hook ()
  (php-hooked-abbrev 'php-template-session-decode))
(defun php-template-session-destroy-hook ()
  (php-hooked-abbrev 'php-template-session-destroy))
(defun php-template-session-encode-hook ()
  (php-hooked-abbrev 'php-template-session-encode))
(defun php-template-session-get-cookie-params-hook ()
  (php-hooked-abbrev 'php-template-session-get-cookie-params))
(defun php-template-session-id-hook ()
  (php-hooked-abbrev 'php-template-session-id))
(defun php-template-session-is-registered-hook ()
  (php-hooked-abbrev 'php-template-session-is-registered))
(defun php-template-session-module-name-hook ()
  (php-hooked-abbrev 'php-template-session-module-name))
(defun php-template-session-name-hook ()
  (php-hooked-abbrev 'php-template-session-name))
(defun php-template-session-regenerate-id-hook ()
  (php-hooked-abbrev 'php-template-session-regenerate-id))
(defun php-template-session-register-hook ()
  (php-hooked-abbrev 'php-template-session-register))
(defun php-template-session-save-path-hook ()
  (php-hooked-abbrev 'php-template-session-save-path))
(defun php-template-session-set-cookie-params-hook ()
  (php-hooked-abbrev 'php-template-session-set-cookie-params))
(defun php-template-session-set-save-handler-hook ()
  (php-hooked-abbrev 'php-template-session-set-save-handler))
(defun php-template-session-start-hook ()
  (php-hooked-abbrev 'php-template-session-start))
(defun php-template-session-unregister-hook ()
  (php-hooked-abbrev 'php-template-session-unregister))
(defun php-template-session-unset-hook ()
  (php-hooked-abbrev 'php-template-session-unset))
(defun php-template-session-write-close-hook ()
  (php-hooked-abbrev 'php-template-session-write-close))

;; Strings
(defun php-template-addcslashes-hook ()
  (php-hooked-abbrev 'php-template-addcslashes))
(defun php-template-addslashes-hook ()
  (php-hooked-abbrev 'php-template-addslashes))
(defun php-template-bin2hex-hook ()
  (php-hooked-abbrev 'php-template-bin2hex))
(defun php-template-chop-hook ()
  (php-hooked-abbrev 'php-template-chop))
(defun php-template-chr-hook ()
  (php-hooked-abbrev 'php-template-chr))
(defun php-template-chunk-split-hook ()
  (php-hooked-abbrev 'php-template-chunk-split))
(defun php-template-convert-cyr-string-hook ()
  (php-hooked-abbrev 'php-template-convert-cyr-string))
(defun php-template-convert-uudecode-hook ()
  (php-hooked-abbrev 'php-template-convert-uudecode))
(defun php-template-convert-uuencode-hook ()
  (php-hooked-abbrev 'php-template-convert-uuencode))
(defun php-template-count-chars-hook ()
  (php-hooked-abbrev 'php-template-count-chars))
(defun php-template-crc32-hook ()
  (php-hooked-abbrev 'php-template-crc32))
(defun php-template-crypt-hook ()
  (php-hooked-abbrev 'php-template-crypt))
(defun php-template-echo-hook ()
  (php-hooked-abbrev 'php-template-echo))
(defun php-template-explode-hook ()
  (php-hooked-abbrev 'php-template-explode))
(defun php-template-fprintf-hook ()
  (php-hooked-abbrev 'php-template-fprintf))
(defun php-template-get-html-translation-table-hook ()
  (php-hooked-abbrev 'php-template-get-html-translation-table))
(defun php-template-hebrev-hook ()
  (php-hooked-abbrev 'php-template-hebrev))
(defun php-template-hebrevc-hook ()
  (php-hooked-abbrev 'php-template-hebrevc))
(defun php-template-html-entity-decode-hook ()
  (php-hooked-abbrev 'php-template-html-entity-decode))
(defun php-template-htmlentities-hook ()
  (php-hooked-abbrev 'php-template-htmlentities))
(defun php-template-htmlspecialchars-decode-hook ()
  (php-hooked-abbrev 'php-template-htmlspecialchars-decode))
(defun php-template-htmlspecialchars-hook ()
  (php-hooked-abbrev 'php-template-htmlspecialchars))
(defun php-template-implode-hook ()
  (php-hooked-abbrev 'php-template-implode))
(defun php-template-join-hook ()
  (php-hooked-abbrev 'php-template-join))
(defun php-template-levenshtein-hook ()
  (php-hooked-abbrev 'php-template-levenshtein))
(defun php-template-localeconv-hook ()
  (php-hooked-abbrev 'php-template-localeconv))
(defun php-template-ltrim-hook ()
  (php-hooked-abbrev 'php-template-ltrim))
(defun php-template-md5-file-hook ()
  (php-hooked-abbrev 'php-template-md5-file))
(defun php-template-md5-hook ()
  (php-hooked-abbrev 'php-template-md5))
(defun php-template-metaphone-hook ()
  (php-hooked-abbrev 'php-template-metaphone))
(defun php-template-money-format-hook ()
  (php-hooked-abbrev 'php-template-money-format))
(defun php-template-nl-langinfo-hook ()
  (php-hooked-abbrev 'php-template-nl-langinfo))
(defun php-template-nl2br-hook ()
  (php-hooked-abbrev 'php-template-nl2br))
(defun php-template-number-format-hook ()
  (php-hooked-abbrev 'php-template-number-format))
(defun php-template-ord-hook ()
  (php-hooked-abbrev 'php-template-ord))
(defun php-template-parse-str-hook ()
  (php-hooked-abbrev 'php-template-parse-str))
(defun php-template-print-hook ()
  (php-hooked-abbrev 'php-template-print))
(defun php-template-printf-hook ()
  (php-hooked-abbrev 'php-template-printf))
(defun php-template-quoted-printable-decode-hook ()
  (php-hooked-abbrev 'php-template-quoted-printable-decode))
(defun php-template-quotemeta-hook ()
  (php-hooked-abbrev 'php-template-quotemeta))
(defun php-template-rtrim-hook ()
  (php-hooked-abbrev 'php-template-rtrim))
(defun php-template-setlocale-hook ()
  (php-hooked-abbrev 'php-template-setlocale))
(defun php-template-sha1-file-hook ()
  (php-hooked-abbrev 'php-template-sha1-file))
(defun php-template-sha1-hook ()
  (php-hooked-abbrev 'php-template-sha1))
(defun php-template-similar-text-hook ()
  (php-hooked-abbrev 'php-template-similar-text))
(defun php-template-soundex-hook ()
  (php-hooked-abbrev 'php-template-soundex))
(defun php-template-sprintf-hook ()
  (php-hooked-abbrev 'php-template-sprintf))
(defun php-template-sscanf-hook ()
  (php-hooked-abbrev 'php-template-sscanf))
(defun php-template-str-ireplace-hook ()
  (php-hooked-abbrev 'php-template-str-ireplace))
(defun php-template-str-pad-hook ()
  (php-hooked-abbrev 'php-template-str-pad))
(defun php-template-str-repeat-hook ()
  (php-hooked-abbrev 'php-template-str-repeat))
(defun php-template-str-replace-hook ()
  (php-hooked-abbrev 'php-template-str-replace))
(defun php-template-str-rot13-hook ()
  (php-hooked-abbrev 'php-template-str-rot13))
(defun php-template-str-shuffle-hook ()
  (php-hooked-abbrev 'php-template-str-shuffle))
(defun php-template-str-split-hook ()
  (php-hooked-abbrev 'php-template-str-split))
(defun php-template-str-word-count-hook ()
  (php-hooked-abbrev 'php-template-str-word-count))
(defun php-template-strcasecmp-hook ()
  (php-hooked-abbrev 'php-template-strcasecmp))
(defun php-template-strchr-hook ()
  (php-hooked-abbrev 'php-template-strchr))
(defun php-template-strcmp-hook ()
  (php-hooked-abbrev 'php-template-strcmp))
(defun php-template-strcoll-hook ()
  (php-hooked-abbrev 'php-template-strcoll))
(defun php-template-strcspn-hook ()
  (php-hooked-abbrev 'php-template-strcspn))
(defun php-template-strip-tags-hook ()
  (php-hooked-abbrev 'php-template-strip-tags))
(defun php-template-stripcslashes-hook ()
  (php-hooked-abbrev 'php-template-stripcslashes))
(defun php-template-stripos-hook ()
  (php-hooked-abbrev 'php-template-stripos))
(defun php-template-stripslashes-hook ()
  (php-hooked-abbrev 'php-template-stripslashes))
(defun php-template-stristr-hook ()
  (php-hooked-abbrev 'php-template-stristr))
(defun php-template-strlen-hook ()
  (php-hooked-abbrev 'php-template-strlen))
(defun php-template-strnatcasecmp-hook ()
  (php-hooked-abbrev 'php-template-strnatcasecmp))
(defun php-template-strnatcmp-hook ()
  (php-hooked-abbrev 'php-template-strnatcmp))
(defun php-template-strncasecmp-hook ()
  (php-hooked-abbrev 'php-template-strncasecmp))
(defun php-template-strncmp-hook ()
  (php-hooked-abbrev 'php-template-strncmp))
(defun php-template-strpbrk-hook ()
  (php-hooked-abbrev 'php-template-strpbrk))
(defun php-template-strpos-hook ()
  (php-hooked-abbrev 'php-template-strpos))
(defun php-template-strrchr-hook ()
  (php-hooked-abbrev 'php-template-strrchr))
(defun php-template-strrev-hook ()
  (php-hooked-abbrev 'php-template-strrev))
(defun php-template-strripos-hook ()
  (php-hooked-abbrev 'php-template-strripos))
(defun php-template-strrpos-hook ()
  (php-hooked-abbrev 'php-template-strrpos))
(defun php-template-strspn-hook ()
  (php-hooked-abbrev 'php-template-strspn))
(defun php-template-strstr-hook ()
  (php-hooked-abbrev 'php-template-strstr))
(defun php-template-strtok-hook ()
  (php-hooked-abbrev 'php-template-strtok))
(defun php-template-strtolower-hook ()
  (php-hooked-abbrev 'php-template-strtolower))
(defun php-template-strtoupper-hook ()
  (php-hooked-abbrev 'php-template-strtoupper))
(defun php-template-strtr-hook ()
  (php-hooked-abbrev 'php-template-strtr))
(defun php-template-substr-compare-hook ()
  (php-hooked-abbrev 'php-template-substr-compare))
(defun php-template-substr-count-hook ()
  (php-hooked-abbrev 'php-template-substr-count))
(defun php-template-substr-replace-hook ()
  (php-hooked-abbrev 'php-template-substr-replace))
(defun php-template-substr-hook ()
  (php-hooked-abbrev 'php-template-substr))
(defun php-template-trim-hook ()
  (php-hooked-abbrev 'php-template-trim))
(defun php-template-ucfirst-hook ()
  (php-hooked-abbrev 'php-template-ucfirst))
(defun php-template-ucwords-hook ()
  (php-hooked-abbrev 'php-template-ucwords))
(defun php-template-vfprintf-hook ()
  (php-hooked-abbrev 'php-template-vfprintf))
(defun php-template-vprintf-hook ()
  (php-hooked-abbrev 'php-template-vprintf))
(defun php-template-vsprintf-hook ()
  (php-hooked-abbrev 'php-template-vsprintf))
(defun php-template-wordwrap-hook ()
  (php-hooked-abbrev 'php-template-wordwrap))

;; Variable
(defun php-template-debug-zval-dump-hook ()
  (php-hooked-abbrev 'php-template-debug-zval-dump))
(defun php-template-doubleval-hook ()
  (php-hooked-abbrev 'php-template-doubleval))
(defun php-template-empty-hook ()
  (php-hooked-abbrev 'php-template-empty))
(defun php-template-floatval-hook ()
  (php-hooked-abbrev 'php-template-floatval))
(defun php-template-get-defined-vars-hook ()
  (php-hooked-abbrev 'php-template-get-defined-vars))
(defun php-template-get-resource-type-hook ()
  (php-hooked-abbrev 'php-template-get-resource-type))
(defun php-template-gettype-hook ()
  (php-hooked-abbrev 'php-template-gettype))
(defun php-template-import-request-variables-hook ()
  (php-hooked-abbrev 'php-template-import-request-variables))
(defun php-template-intval-hook ()
  (php-hooked-abbrev 'php-template-intval))
(defun php-template-is-array-hook ()
  (php-hooked-abbrev 'php-template-is-array))
(defun php-template-is-bool-hook ()
  (php-hooked-abbrev 'php-template-is-bool))
(defun php-template-is-callable-hook ()
  (php-hooked-abbrev 'php-template-is-callable))
(defun php-template-is-double-hook ()
  (php-hooked-abbrev 'php-template-is-double))
(defun php-template-is-float-hook ()
  (php-hooked-abbrev 'php-template-is-float))
(defun php-template-is-int-hook ()
  (php-hooked-abbrev 'php-template-is-int))
(defun php-template-is-integer-hook ()
  (php-hooked-abbrev 'php-template-is-integer))
(defun php-template-is-long-hook ()
  (php-hooked-abbrev 'php-template-is-long))
(defun php-template-is-null-hook ()
  (php-hooked-abbrev 'php-template-is-null))
(defun php-template-is-numeric-hook ()
  (php-hooked-abbrev 'php-template-is-numeric))
(defun php-template-is-object-hook ()
  (php-hooked-abbrev 'php-template-is-object))
(defun php-template-is-real-hook ()
  (php-hooked-abbrev 'php-template-is-real))
(defun php-template-is-resource-hook ()
  (php-hooked-abbrev 'php-template-is-resource))
(defun php-template-is-scalar-hook ()
  (php-hooked-abbrev 'php-template-is-scalar))
(defun php-template-is-string-hook ()
  (php-hooked-abbrev 'php-template-is-string))
(defun php-template-isset-hook ()
  (php-hooked-abbrev 'php-template-isset))
(defun php-template-print-r-hook ()
  (php-hooked-abbrev 'php-template-print-r))
(defun php-template-serialize-hook ()
  (php-hooked-abbrev 'php-template-serialize))
(defun php-template-settype-hook ()
  (php-hooked-abbrev 'php-template-settype))
(defun php-template-strval-hook ()
  (php-hooked-abbrev 'php-template-strval))
(defun php-template-unserialize-hook ()
  (php-hooked-abbrev 'php-template-unserialize))
(defun php-template-unset-hook ()
  (php-hooked-abbrev 'php-template-unset))
(defun php-template-var-dump-hook ()
  (php-hooked-abbrev 'php-template-var-dump))
(defun php-template-var-export-hook ()
  (php-hooked-abbrev 'php-template-var-export))

;; XML
(defun php-template-utf8-decode-hook ()
  (php-hooked-abbrev 'php-template-utf8-decode))
(defun php-template-utf8-encode-hook ()
  (php-hooked-abbrev 'php-template-utf8-encode))
(defun php-template-xml-error-string-hook ()
  (php-hooked-abbrev 'php-template-xml-error-string))
(defun php-template-xml-get-current-byte-index-hook ()
  (php-hooked-abbrev 'php-template-xml-get-current-byte-index))
(defun php-template-xml-get-current-column-number-hook ()
  (php-hooked-abbrev 'php-template-xml-get-current-column-number))
(defun php-template-xml-get-current-line-number-hook ()
  (php-hooked-abbrev 'php-template-xml-get-current-line-number))
(defun php-template-xml-get-error-code-hook ()
  (php-hooked-abbrev 'php-template-xml-get-error-code))
(defun php-template-xml-parse-into-struct-hook ()
  (php-hooked-abbrev 'php-template-xml-parse-into-struct))
(defun php-template-xml-parse-hook ()
  (php-hooked-abbrev 'php-template-xml-parse))
(defun php-template-xml-parser-create-ns-hook ()
  (php-hooked-abbrev 'php-template-xml-parser-create-ns))
(defun php-template-xml-parser-create-hook ()
  (php-hooked-abbrev 'php-template-xml-parser-create))
(defun php-template-xml-parser-free-hook ()
  (php-hooked-abbrev 'php-template-xml-parser-free))
(defun php-template-xml-parser-get-option-hook ()
  (php-hooked-abbrev 'php-template-xml-parser-get-option))
(defun php-template-xml-parser-set-option-hook ()
  (php-hooked-abbrev 'php-template-xml-parser-set-option))
(defun php-template-xml-set-character-data-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-character-data-handler))
(defun php-template-xml-set-default-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-default-handler))
(defun php-template-xml-set-element-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-element-handler))
(defun php-template-xml-set-end-namespace-decl-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-end-namespace-decl-handler))
(defun php-template-xml-set-external-entity-ref-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-external-entity-ref-handler))
(defun php-template-xml-set-notation-decl-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-notation-decl-handler))
(defun php-template-xml-set-object-hook ()
  (php-hooked-abbrev 'php-template-xml-set-object))
(defun php-template-xml-set-processing-instruction-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-processing-instruction-handler))
(defun php-template-xml-set-start-namespace-decl-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-start-namespace-decl-handler))
(defun php-template-xml-set-unparsed-entity-decl-handler-hook ()
  (php-hooked-abbrev 'php-template-xml-set-unparsed-entity-decl-handler))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template functions utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun php-template-field (prompt &optional follow-string optional
				   begin end is-string string-char default)
  "Prompt for string and insert it in buffer with optional FOLLOW-STRING.
If OPTIONAL is nil, the prompt is left if an empty string is inserted.  If
an empty string is inserted, return nil and call `php-template-undo' for
the region between BEGIN and END.  IS-STRING indicates whether a string
with double-quotes is to be inserted.  DEFAULT specifies a default string."
  (let ((position (point))
	string)
    (insert "<" prompt ">")
    (if (not (> (length string-char) 0))
	(setq string-char "\""))
    (setq string
	  (condition-case ()
	      (read-from-minibuffer (concat prompt ": ")
				    (or (and is-string (cons (concat string-char string-char) 1)) default)
				    php-minibuffer-local-map)
	    (quit (if (and optional begin end)
		      (progn (beep) "")
		    (keyboard-quit)))))
    (when (or (not (equal string "")) optional)
      (delete-region position (point)))
    (when (and (equal string "") optional begin end)
      (php-template-undo begin end)
      (message "Template aborted"))
    (unless (equal string "")
      (insert string))
    (when (or (not (equal string "")) (not optional))
      (insert (or follow-string "")))
    (if (equal string "") nil string)))

(defun php-template-undo (begin end)
  "Undo aborted template by deleting region and unexpanding the keyword."
  (cond (php-template-invoked-by-hook
	 (goto-char end)
	 (insert " ")
	 (delete-region begin end)
	 (unexpand-abbrev))
	(t (delete-region begin end))))

(defun php-resolve-env-variable (string)
  "Resolve environment variables in STRING."
  (while (string-match "\\(.*\\)${?\\(\\(\\w\\|_\\)+\\)}?\\(.*\\)" string)
    (setq string (concat (match-string 1 string)
			 (getenv (match-string 2 string))
			 (match-string 4 string))))
  string)

(defun php-insert-string-or-file (string)
  "Insert STRING or file contents if STRING is an existing file name."
  (unless (equal string "")
    (let ((file-name
	   (progn (string-match "^\\([^\n]+\\)" string)
		  (php-resolve-env-variable (match-string 1 string)))))
      (if (file-exists-p file-name)
	   (forward-char (cadr (insert-file-contents file-name)))
	(insert string)))))

(defun php-template-modify (&optional noerror)
  "Actualize modification date."
  (interactive)
  (php-prepare-search-2
   (save-excursion
     (goto-char (point-min))
     (if (re-search-forward php-modify-date-prefix-string nil t)
	 (progn (delete-region (point) (progn (end-of-line) (point)))
		(php-template-insert-date))
       (unless noerror
	 (error (concat "ERROR:  Modification date prefix string \""
			php-modify-date-prefix-string "\" not found")))))))

(defun php-template-modify-noerror ()
  "Call `php-template-modify' with NOERROR non-nil."
  (php-template-modify t))

(defun php-template-insert-date ()
  "Insert date in appropriate format."
  (interactive)
  (insert
   (cond
    ;; 'american, 'european, 'scientific kept for backward compatibility
    ((eq php-date-format 'american) (format-time-string "%m/%d/%Y" nil))
    ((eq php-date-format 'european) (format-time-string "%d.%m.%Y" nil))
    ((eq php-date-format 'scientific) (format-time-string "%Y/%m/%d" nil))
    (t (format-time-string php-date-format nil)))))

(defun php-template-header (&optional file-title)
  "Insert a PHP file header."
  (interactive)
  (unless (equal php-file-header "")
    (let (pos)
      (save-excursion
	(php-insert-string-or-file php-file-header)
	(setq pos (point-marker)))
      (php-template-replace-header-keywords
       (point-min-marker) pos file-title))))

(defun php-template-footer ()
  "Insert a PHP file footer."
  (interactive)
  (unless (equal php-file-footer "")
    (let (pos)
      (save-excursion
	(setq pos (point-marker))
	(php-insert-string-or-file php-file-footer)
	(unless (= (preceding-char) ?\n)
	  (insert "\n")))
      (php-template-replace-header-keywords pos (point-max-marker)))))

(defun php-template-replace-header-keywords (beg end &optional file-title is-model)
  "Replace keywords in header and footer."
  (let ()
    (php-prepare-search-2
     (save-excursion
       (goto-char beg)
       (while (search-forward "<filename>" end t)
	 (replace-match (buffer-name) t t))
       (goto-char beg)
       (while (search-forward "<copyright>" end t)
	 (replace-match php-copyright-string t t))
       (goto-char beg)
       (while (search-forward "<author>" end t)
	 (replace-match "" t t)
	 (insert (user-full-name))
	 (when user-mail-address (insert "  <" user-mail-address ">")))
       (goto-char beg)
       (while (search-forward "<login>" end t)
	 (replace-match (user-login-name) t t))
       (goto-char beg)
       (while (search-forward "<company>" end t)
	 (replace-match php-company-name t t))
       (goto-char beg)
       ;; Replace <RCS> with $, so that RCS for the source is
       ;; not over-enthusiastic with replacements
       (while (search-forward "<RCS>" end t)
	 (replace-match "$" nil t))
       (goto-char beg)
       (while (search-forward "<date>" end t)
	 (replace-match "" t t)
	 (php-template-insert-date))
       (goto-char beg)
       (while (search-forward "<year>" end t)
	 (replace-match (format-time-string "%Y" nil) t t))
       (goto-char beg)
       (let (string)
	 (while
	     (re-search-forward "<\\(\\(\\w\\|\\s_\\)*\\) string>" end t)
	   (setq string (read-string (concat (match-string 1) ": ")))
	   (replace-match string t t)))
       (goto-char beg)
       (when (and (not is-model) (search-forward "<cursor>" end t))
	 (replace-match "" t t))))))

(defun php-doc-mode ()
  "Display PHP Mode documentation in *Help* buffer."
  (interactive)
  ;;(unless php-xemacs
  ;;  (help-setup-xref (list #'php-doc-mode) (interactive-p)))
  (with-output-to-temp-buffer
      (if (fboundp 'help-buffer) (help-buffer) "*Help*")
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation 'php-mode))
    (with-current-buffer standard-output
      (help-mode))
    (print-help-return-message)))

(provide 'php-mode)
