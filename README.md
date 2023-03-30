# aps-status
Get status update of manuscripts in APS journals

# Install with `straight.el`
```emacs-lisp
  (use-package aps-status
    :straight (aps-status :type git :host github :repo "md-arif-shaikh/aps-status"))
```

# Usage
- `M-x` `aps-status` to get status of manuscript by providing accession code and author last name.
- `M-x` `aps-status-short` to get a one line short update displayed in the minibuffer.

