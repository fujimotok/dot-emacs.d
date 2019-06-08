(when (and (eq system-type 'gnu/linux)
	     (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop"))
(defun battery-linux-sysfs-wsl ()
  "Get ACPI status information from Linux kernel.
This function works only with the Windows Subsystem for Linux.

The following %-sequences are provided:
%c Current capacity (mAh)
%r Current rate
%B Battery status (verbose)
%b Battery status (charging:'+' discharging:'')
%d Temperature (in degrees Celsius)
%p Battery load percentage
%L AC line status (verbose)
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
  (let (charging-state ac-state temperature hours
		       energy-now energy-now-rate power-now current-now voltage-now
		       (dir "/sys/class/power_supply/battery"))
    (with-temp-buffer ;; このブロック限りのバッファを生成しcurrent_bufferとする
      (erase-buffer)  ;; バッファに読み込む前にバッファを空に
      (ignore-errors (insert-file-contents
		      (expand-file-name "capacity" dir)))
      (setq energy-now-rate (or (thing-at-point 'number) "N/A"))
       
      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "status" dir)))
      (setq charging-state (or (thing-at-point 'word) "N/A"))

      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "temp" dir)))
      (setq temperature (or (thing-at-point 'number) "N/A"))
      (setq temperature (if (numberp temperature) (* temperature 0.1)))

      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "charge_counter" dir)))
      (setq energy-now (or (thing-at-point 'number) "N/A"))

      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "current_now" dir)))
      (setq current-now (or (thing-at-point 'number) "N/A"))
      (unless (or	(stringp energy-now) (stringp current-now)
			(stringp energy-now-rate) (zerop current-now))
	;; 変数がN/Aのときパス。充電完了となるとcurrent-now=0。zero-dev回避のため0のときパス
	(if (string= charging-state "Discharging")
	    (setq hours (/ energy-now current-now))
	  (setq hours (/ (* energy-now (- 100.0 energy-now-rate))
			 energy-now-rate current-now ))))

      (erase-buffer)
      (ignore-errors (insert-file-contents
		      (expand-file-name "voltage_now" dir)))
      (setq voltage-now (or (thing-at-point 'number) "N/A"))
      (setq power-now (if (and (numberp current-now) (numberp voltage-now))
			  (* (/ current-now 1000.0) (/ voltage-now 1000000.0))))
      ;; current-now[mA]->[A] voltage-now[uV]->[V]
    
      (erase-buffer)
      (setq dir "/sys/class/power_supply/ac") ;; acはpathが変わるので変更
      (ignore-errors (insert-file-contents
		      (expand-file-name "online" dir)))
      (setq ac-state (cond ((eq (thing-at-point 'number) 1) "AC")
			   ((eq (thing-at-point 'number) 0) "BAT")
			   (t "N/A")))
      )
    ;; 戻り値(リスト型)を設定
    (list (cons ?c (number-to-string energy-now))
	  (cons ?r (if hours (number-to-string power-now) "N/A"))
	  (cons ?B charging-state)
	  (cons ?b (if (string= charging-state "Charging") "+" ""))
	  (cons ?d (number-to-string temperature))
	  (cons ?p (number-to-string energy-now-rate))
	  (cons ?L ac-state)
	  (cons ?m (if hours (format "%d" (* hours 60)) "N/A"))
	  (cons ?h (if hours (format "%d" hours) "N/A"))
	  (cons ?t (if hours (format "%d:%02d" hours (* (- hours (floor hours)) 60)) "N/A")))
    )
)

(setq battery-status-function #'battery-linux-sysfs-wsl))

