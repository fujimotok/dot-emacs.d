(when (and (eq system-type 'gnu/linux)
	     (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop"))

(defvar frame-wsl-powershell-path nil)
(defvar frame-wsl-w32-sys-cmd-path "C:\\\\Users\\\\fj\\\\work\\\\w32_wm_syscmd.ps1")
(defvar frame-wsl-wm-name "vcxsrv")

;; Toggle flag. Do not touch this.
(setq frame-wsl-maximize nil)

(defun frame-maximized-wsl (maximize)
  "If MAXIMIZE is not nil, frame maximize, or frame restore
This function works only with the Windows Subsystem for Linux.
And, use powershell script for win32api::SendMessage().

You can customize these variables for your enviroment.
`frame-wsl-powershell-path' Path to powershell.exe. Maybe not need.
`frame-wsl-w32-sys-cmd-path' Path to powershell script (w32_wm_syscmd.ps1).
`frame-wsl-wm-name' Process name (e.g. vcxsrv) to get window handle.
"
  (let ((powershell (or frame-wsl-powershell-path "powershell.exe"))
	(powershell-opt "-ExecutionPolicy RemoteSigned -File")
	(w32-wm-syscmd (or frame-wsl-w32-sys-cmd-path "w32_wm_syscmd.ps1"))
	(w32-wm-syscmd-arg1 (if maximize "maximize" "restore"))
	(w32-wm-syscmd-arg2 (or frame-wsl-wm-name "")))
    (setq frame-wsl-maximize maximize)
    (shell-command
     (format "%s %s %s %s %s"
	     powershell powershell-opt
	     w32-wm-syscmd w32-wm-syscmd-arg1 w32-wm-syscmd-arg2))
    )
  )

(defun toggle-frame-maximized-advice (orig-func &rest args)
  (if frame-wsl-maximize
      (frame-maximized-wsl nil)
      (frame-maximized-wsl t) ) )

;; override toggle-frame-maximized
(advice-add 'toggle-frame-maximized :around 'toggle-frame-maximized-advice)
)
