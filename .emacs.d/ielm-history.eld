  (add-hook 'find-file-hook (lambda () (message "find-file-hook fired, history: %s" consult--line-history)))
  (add-hook 'find-file-hook (lambda () (message "find-file-hook fired, history: %s" consult--line-history)))
