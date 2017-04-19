(in-package #:sawyer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TOML TABLES
;;;
;;; A TOML table is a set of Bindings.
;;; A Binding is a pair (name, value), where the value can be:
;;;
;;; 1. A toml table
;;; 2. An integer
;;; 3. A datetime (offset-datetime, local-datetime, local-date, local-time)
;;; 4. A list of values
;;; 5. A toml table array
;;; 6. A boolean
;;; 7. A float

(defstruct
    (toml-table (:print-function
                 (lambda (struct stream depth)
                   (declare (ignore depth))
                   (print-unreadable-object (struct stream)
                     (format stream "TABLE (~A)" (length (toml-table-bindings struct)))
                     ))))
  (bindings '() :type list)
  (directly-defined nil)
  (parent-array-table nil)
  (inline nil))

(defstruct toml-table-array
  (tables '() :type list))

(defstruct toml-binding
  name
  value)

(defun find-binding (table name)
  "Return the binding for the given name in the given table."
  (find-if (lambda (binding) (string-equal (toml-binding-name binding) name)) (toml-table-bindings table)))


(defun find-value (table name)
  "Return the value bound to the given name in the given table, or nil if name
is not bound. Since a value can be nil, return in a second value t or nil
depending if it found a binding or not."
  (let ((binding (find-binding table name)))
    (if binding
        (values (toml-binding-value binding) t)
        (values nil nil))))

(defun add-binding (table name value)
  "Create a binding for name and value under the given table."
  (if (find-binding table name)
      (error "~A is already bound." name)
      (setf (toml-table-bindings table)
            (append (toml-table-bindings table)
                    (list (make-toml-binding :name name :value value))))))

(defun find-table (name parent)
  "Find the toml-table under parent named name. If its an array table, return
the last table of the array. If the name is not bound, return nil.  If name is
defined but its not a table, produce an error."
  (let ((binding (find-binding parent name)))
    (when binding
      (let ((value (toml-binding-value binding)))
        (cond ((toml-table-p value)
               value)
              ((toml-table-array-p value)
               (lastcar (toml-table-array-tables value)))
              (t (error "~A is defined but it is not a table" name)))))))

(defun find-or-create-super-table (name parent)
  "Find or create a super table under parent. (a super table is a table that is
being created implicitely when creating a deeper table)"
  (let ((table (find-table name parent)))
    (or table
        (let ((table (make-toml-table :directly-defined nil)))
          (add-binding parent name table)
          table))))

(defun find-or-create-super-tables (names parent)
  "Find or create the hierarchy of super tables defined by the list of names
under parent. Create the tables that do not exist. If array tables are found,
navigate through the last table of the array. Return the deepest table."
  (if names
      (find-or-create-super-tables
       (cdr names)
       (find-or-create-super-table (car names)
                                   parent))
      parent))

(defun create-table (names root)
  "Create a new regular table defined by the hierarchy of names"
  (if names
      ;; find or create the super tables
      (let ((parent (find-or-create-super-tables
                     (butlast names)
                     root)))
        ;; check if a binding for the last table exist
        (let ((table (find-table (lastcar names) parent)))
          ;; return table if it exists, otherwise create it
          (if table
              (cond ((toml-table-directly-defined table)
                     (error "you can't directly define a table twice"))
                    (t (setf (toml-table-directly-defined table) t)
                       table))
              (let ((table (make-toml-table :directly-defined t)))
                (add-binding parent (lastcar names) table)
                table))))
      (error "A table needs names!")))

(defun add-new-table-to-array-table (names root)
  "Create a new table as part of an array table, as defined by the hierarchy of
names. Create the new array table if necessary."
  (if names
      (let ((parent (find-or-create-super-tables
                     (butlast names)
                     root)))
        (let* ((new-table (make-toml-table :directly-defined t))
               (existing-table (find-table (lastcar names) parent)))
          (if existing-table
              (let ((existing-array-table (toml-table-parent-array-table existing-table)))
                (cond (existing-array-table
                       (setf (toml-table-parent-array-table new-table)
                             existing-array-table)
                       (setf (toml-table-array-tables existing-array-table)
                             (append
                              (toml-table-array-tables existing-array-table)
                              (list new-table)))
                       new-table)
                      (t (error "A regular table (not array) already exists under ~A" (lastcar names)))))
              (let ((new-array-table (make-toml-table-array
                                      :tables (list new-table)
                                      )))
                (setf (toml-table-parent-array-table new-table) new-array-table)
                (add-binding parent (lastcar names) new-array-table)
                new-table))))
      (error "A table needs names!")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Read a toml table from a list of toml entries
(defun load-toml-table (entries)
  "Build TOML tables from a toml document"
  (let* ((root (make-toml-table :directly-defined nil))
         (current-table root))
    ;; add the initial leafs
    (dolist (entry entries)
      (typecase entry
        (toml-key-value-entry (process-key-value entry current-table))
        (toml-table-header-entry
         (setf current-table (create-table
                              (mapcar #'toml-string-value (toml-table-header-entry-headers entry)) root)))
        (toml-array-table-header-entry
         (setf current-table (add-new-table-to-array-table
                              (mapcar #'toml-string-value (toml-array-table-header-entry-headers entry)) root)))))
    root))

(defun process-key-value (kv table)
  "Process a toml key value entry"
  (let ((key (toml-string-value (toml-key-value-entry-key kv)))
        (val (toml-key-value-entry-value kv)))
    (typecase val
      ((or toml-integer toml-float toml-boolean toml-string toml-offset-datetime toml-local-datetime toml-local-date toml-local-time) (add-binding table key val))
      (toml-array-entry (add-binding table key (toml-array-entry-to-value val)))
      (toml-inline-table-entry (add-binding table key (toml-inline-table-entry-to-value val))))))

(defun toml-inline-table-entry-to-value (inline-table-entry)
  (let ((table (make-toml-table :directly-defined t :parent-array-table nil :inline t)))
    (dolist (key-value-entry (toml-inline-table-entry-entries inline-table-entry))
      (process-key-value key-value-entry table))
    table))

(defun toml-array-entry-to-value (array-entry)
  "Return the value to be bound from an array entry. In particular, transform an array of inline tables to a toml-table-array"
  (let ((entries (toml-array-entry-entries array-entry)))
    (when entries
      (let ((type (type-of (first entries))))
        ;; first, check if all entries are of the same type
        (loop
           for element in (cdr entries)
           when (not (eq (type-of element) type)) do (Error "Values in an array must be of the same type ~A ~A" (type-of element) type))
        (typecase (first entries)
          ((or toml-integer toml-float toml-boolean toml-string toml-offset-datetime toml-local-datetime toml-local-date toml-local-time)
           entries)
          (toml-array-entry (mapcar #'toml-array-entry-to-value entries))
          (toml-inline-table-entry
           (let ((table-array (make-toml-table-array)))
             (let ((sub-tables
                    (mapcar (lambda (inline-table-entry)
                              (let ((table (make-toml-table :directly-defined t :parent-array-table table-array :inline t)))
                                (dolist (key-value-entry (toml-inline-table-entry-entries inline-table-entry))
                                  (process-key-value key-value-entry table))
                                table
                                ))
                            entries)))
               (setf (toml-table-array-tables table-array) sub-tables)
               table-array))))))))

(defun toml-object-to-lisp (value)
  (typecase value
    (toml-string (toml-string-value value))
    (toml-integer (toml-integer-value value))
    (toml-float (toml-float-value value))
    (toml-boolean (toml-boolean-value value))
    (toml-datetime (toml-datetime-value value))
    (list (let ((list (mapcar #'toml-object-to-lisp value)))
            (make-array (length list) :initial-contents list)))
    (toml-table
     `(:obj
       ,@(mapcar (lambda (binding)
                   (let ((name (toml-binding-name binding))
                         (value (toml-binding-value binding)))
                     (cons name (toml-object-to-lisp value))))
                 (toml-table-bindings value))))
    (toml-table-array
     (let ((list (mapcar #'toml-object-to-lisp (toml-table-array-tables value))))
       (make-array (length list) :initial-contents list)))))

(defun toml-object-to-jsownable (value)
  (typecase value
    (toml-string  `(:obj ("type" . "string") ("value" . ,(toml-string-value value))))
    (toml-integer `(:obj ("type" . "integer") ("value" . ,(write-to-string (toml-integer-value value)))) )
    (toml-float `(:obj ("type" . "float") ("value" . ,(format nil "~f" (toml-float-value value)))) )
    (toml-boolean `(:obj ("type" . "bool") ("value" .  ,(if (toml-boolean-value value) "true" "false"))))
    ((or toml-offset-datetime toml-local-datetime toml-local-date toml-local-time) `(:obj ("type" . "datetime") ("value" .  ,(toml-datetime-original-value value))))
    (list `(:obj ("type" . "array") ("value" . ,(mapcar #'toml-object-to-jsownable value))))
    (toml-table
     `(:obj
       ,@(mapcar (lambda (binding)
                   (let ((name (toml-binding-name binding))
                         (value (toml-binding-value binding)))
                     (cons name (toml-object-to-jsownable value))))
                 (toml-table-bindings value))))
    (toml-table-array (mapcar #'toml-object-to-jsownable (toml-table-array-tables value)))))

(defun parse-toml-file (file &key (jsown nil))
  (parse-toml-string (read-file-into-string file) :jsown jsown))

(defun parse-toml-string (string &key (jsown nil))
  (funcall (if jsown
               #'toml-object-to-jsownable
               #'toml-object-to-lisp
               )
           (load-toml-table (parse-toml-string-to-entries string))))
