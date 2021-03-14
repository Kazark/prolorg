;;; prolorg.el --- PROperty Logic for ORG-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Keith Pinson

;; Author: Keith Pinson <keith@t440p>
;; Keywords: data, abbrev

;; TODO other package headers?
;; TODO license?
;; TODO how to handle dependencies?
;; TODO document

(defun prolorg/try-read-constraint (s)
  "Read a property constraint, handling failure gracefully.

Issues a message with the failed form and returns nil on failure."
  (condition-case nil
      (read s)
    (error (message "Cannot parse constraint: %s" s))))

(defun prolorg/collect-constraints ()
  "Collect property constraints."
  (-keep #'prolorg/try-read-constraint
         (cdar (org-collect-keywords '("PROPERTY_CONSTRAINT")))))

;; for each xxx_ALL property, make sure the bare
;; xxx property is also included, not the _ALL
(defun prolorg/property-from-all-property (p)
  (if (string-match-p "._ALL\\'" p)
      (substring p 0 -4)
    p))

(defun prolorg/collect-keys ()
  "Get all data property keys in the current buffer."
  (let ((case-fold-search t)
        ;; Get property names from #+PROPERTY keywords
        (props (mapcar (lambda (s) (nth 0 (split-string s)))
                       (cdar (org-collect-keywords '("PROPERTY"))))))
    (mapcar #'intern
            (sort (-keep #'prolorg/property-from-all-property props)
                  (lambda (a b) (string< (upcase a) (upcase b)))))))

(defun prolorg/eval-expr (vars value)
  (pcase value

    (`(and . ,values)
     (-reduce-from (lambda (x y) (and x (prolorg/eval-expr vars y)))
                   t values))

    (`(not ,value) (not (prolorg/eval-expr vars value)))

    (`(member ,elt ,list)
     (member (prolorg/eval-expr vars elt)
             (prolorg/eval-expr vars list)))

    (`(> ,lhs ,rhs)
     (if-let* ((a (prolorg/eval-expr vars lhs))
               (x (cond ((stringp a) (string-to-number a))
                        ((integerp a) a)))
               (y (prolorg/eval-expr vars rhs)))
         (> x y)))

    (`(quote ,form) form)

    ((pred stringp) value)

    ((pred integerp) value)

    ((pred symbolp) (cdr (assq value vars)))

    (_ (throw 'prolorg/invalid-value value))))

(defun prolorg/excluded-keys (constraints values heading)
  (let* ((vars (mapcar (-lambda ((k . v)) (cons (intern k) v)) values))
         ;; As a baseline, exclude all keys that are already set
         (exclusions (mapcar #'car vars)))
    (dolist (constraint constraints)
      (pcase constraint

        (`(var ,name ,value)
         (unless (symbolp name)
           (throw 'prolorg/invalid-var/name name))
         (if-let (v (prolorg/eval-expr vars value))
             (add-to-list 'vars (cons name v))))

        (`(under ,heading-matcher ,key)
         (unless (symbolp key)
           (throw 'prolorg/invalid-under/key key))
         (unless (string-match-p heading-matcher heading)
           (add-to-list 'exclusions key)))

        (`(iff ,antecedent ,consequent)
         (unless (symbolp consequent)
           (throw 'prolorg/invalid-iff/consequent consequent))
         (unless (prolorg/eval-expr vars antecedent)
           (add-to-list 'exclusions consequent)))

        (`(nand ,key1 ,key2)
         (unless (symbolp key1) (throw 'prolorg/invalid-nand/1 key1))
         (unless (symbolp key2) (throw 'prolorg/invalid-nand/2 key2))
         (if (assq key1 vars) (add-to-list 'exclusions key2)
           (if (assq key2 vars) (add-to-list 'exclusions key1))))

        (`(never ,property)
         (unless (symbolp property) (throw 'prolorg/invalid-never property))
         (add-to-list 'exclusions property))

        (_ (throw 'prolorg/invalid-constraint constraint))))
    exclusions))

(defun prolorg/next-keys ()
  (-difference
   (prolorg/collect-keys)
   (prolorg/excluded-keys (prolorg/collect-constraints)
                          (org-entry-properties) ;; TODO need to inherit as well
                          (org-get-heading))))

(defun prolorg/read-key ()
  "Read a property name without specials, defaults or column properties."
  (let ((completion-ignore-case t)
	      (default-prop (or (and (org-at-property-p)
			                         (match-string-no-properties 2))
			                    org-last-set-property)))
    (org-completing-read
     (concat "Property"
	           (if default-prop (concat " [" default-prop "]") "")
	           ": ")
     (mapcar #'list (prolorg/next-keys))
     nil nil nil nil default-prop)))

(defun prolorg/set-data ()
  "In the current entry, set a new data property per constraints.

Prompts for a property name, offering completion on existing and default
properties; then prompts for a value, offering completion either on allowed
values (via an inherited xxx_ALL property) or on existing values in other
instances of this property in the current file.

A data property is one which the user employs to store data, i.e. user-defined
properties, not specials, defaults or columns. Constraints are explained below.

The data property must be new in the sense that property that are already set
are excluded. This menu is intended to operate something like a wizard. You can
use the fact that Org is textual, or its default property-setting mechanisms, to
change properties already set; or if you place the point on a property, you can
change it with this function.

Constraints are declared via #+PROPERTY_CONSTRAINT, e.g.:

  #+PROPERTY_CONSTRAINT (nand BAD GOOD)

The suggested idiom is to have a centralized properties file for a group of
related Org files, and to put into it your _ALL properties, constraints, and
(for the cases without an _ALL property) defaulted properties (defaulted so the
menu can be pre-populated with the options). Then in each Org file you use the
form:

  #+SETUPFILE: ../../properties.org

The constraint language's syntax is S-expressions. The following constraint
forms are supported:

  `(var ,name ,value)
     Declare a variable in the constraint language. This is a mere syntactic
     convenience.
     NAME must be a symbol.
     VALUE is an expression.

  `(under ,heading-matcher ,key)
     Only prompt for the property under a given header.
     HEADING-MATCHER is a regexp for matching the heading.
     KEY is the symbol corresponding to the property.

  `(iff ,antecedent ,consequent)
     Prompt for CONSEQUENT if and only if ANTECEDENT is already populated.
     ANTECEDENT is an expression.
     CONSEQUENT is a symbol corresponding to the property.

  `(nand ,key1 ,key2)
      If either property is populated, don't prompt for the other.
      KEY1 is a symbol corresponding to a property.
      KEY2 is a symbol corresponding to another property.

  `(never ,property)
      Unconditionally exclude PROPERTY.

The following expression forms are supported, and match their meaning in Lisp:

    `(and . ,values)
    `(not ,value)
    `(member ,elt ,list)
    `(> ,lhs ,rhs)
    `(quote ,form)

Strings, integers and symbols are also supported. Symbols are variables; the
environment is seeded with properties but can be extended with VAR constraints.

Throw an error when trying to set a property with an invalid name."
  (interactive)
  (let ((property (prolorg/read-key)))
    ;; `org-entry-put' also makes the following check, but this one
    ;; avoids polluting `org-last-set-property' and
    ;; `org-last-set-property-value' needlessly.
    (unless (org--valid-property-p property)
      (user-error "Invalid property name: \"%s\"" property))
    (let ((value (org-read-property-value property))
          (fn (cdr (assoc-string property org-properties-postprocess-alist t))))
      (setq org-last-set-property property)
      (setq org-last-set-property-value (concat property ": " value))
      ;; Possibly postprocess the inserted value:
      (when fn (setq value (funcall fn value)))
      (unless (equal (org-entry-get nil property) value)
        (org-entry-put nil property value)))))
