(vl-load-com)
;;;; constants and utils

(setq am:int16 1070)
(setq am:pi/2 (/ pi 2.0))
(setq am:2pi (* pi 2.0))

(defun am:default (value default)
  (if value value default))

(defun am:find-if (objects predicate)
  (if objects
      (if (predicate (car objects))
	  (car objects)
	  (am:find-if (cdr objects) predicate))))

;;;; model

(defun am:object-get (object field)
  (cdr (assoc field object)))

(defun am:property-find (properties name)
  (am:find-if properties
    	      (lambda (property) (= (am:object-get property ':name) name))))

(defun am:property-get (properties name)
  (am:object-get (am:property-find properties name) ':value))

(defun am:make-property (name display value)
  (list (cons ':name name)
	(cons ':display display)
	(cons ':value value)))

(defun am:make-model (model-name properties fields)
  (list (cons ':name model-name)
	(cons ':properties properties)
	(cons ':fields fields)))

(defun am:make-trench-model (main-line block block-ref)
  (am:make-model ':trench
		 (list (am:make-property ':width "Width" 1.0)
		       (am:make-property ':m "m" 0.66))
		 (list (cons ':main-line main-line)
		       (cons ':block block)
		       (cons ':block-ref block-ref))))

(defun am:make-success (data)
  (cons ':success data))

(defun am:make-error (data)
  (cons ':error data))

(defun am:successp (result)
  (= ':success (car result)))

(defun am:errorp (result)
  (= ':error (car result)))

;;;; persistence

(defun am:get-dictionary (doc
			  / dictionary-name dictionaries dictionary)
  (setq dictionary-name "amDictionary"
  	dictionaries (vla-get-Dictionaries doc)
  	dictionary (vl-catch-all-apply 'vla-Item (list dictionaries dictionary-name)))
  (if (= (type dictionary) 'vla-object)
    dictionary
    (vla-Add dictionaries dictionary-name)))

(defun am:get-xrecord (dictionary name
		       / record)
  (setq record (vl-catch-all-apply 'vla-GetObject (list dictionary name)))
  (if (= (type record) 'vla-object)
    record
    (vla-AddXRecord dictionary name)))

(defun am:get-variable (doc name
			/ dictionary record out-type out-value)
  (setq dictionary (am:get-dictionary doc)
	record (am:get-xrecord dictionary name))
  (vla-GetXRecordData record 'out-type 'out-value)
  (if (/= out-value nil)
    (vlax-variant-value (vlax-safearray-get-element out-value 0))))

(defun am:set-variable (doc name var-type var-value
			/ dictionary record record-type record-value)
  (setq dictionary (am:get-dictionary doc)
	record (am:get-xrecord dictionary name)
	record-type (vlax-make-safearray vlax-vbInteger '(0 . 0))
	record-value (vlax-make-safearray vlax-vbVariant '(0 . 0)))
  (vlax-safearray-put-element record-type 0 var-type)
  (vlax-safearray-put-element record-value 0 var-value)
  (vla-SetXRecordData record record-type record-value))

;;;; list manipulations

(defun am:variant->list (variant-array)
  (vlax-safearray->list (vlax-variant-value variant-array)))

(defun am:pairs (flat-list
		 / pair rest)
  (if flat-list
    (progn
      (setq pair (list (car flat-list) (cadr flat-list))
	    rest (cddr flat-list))
      (append (list pair) (am:pairs rest)))))

(defun am:flatten-list (list-of-lists) 
  (if list-of-lists
    (append (car list-of-lists) (am:flatten-list (cdr list-of-lists)))))

(defun am:generate-segments (points
			     / first second rest)
  (setq first (car points)
	second (cadr points)
	rest (cdr points))
  (if second
    (append (list first second) (am:generate-segments rest))))

;;;; Math

(defun am:scalar-op (operation arg1 arg2
		     / left-fn right-fn func list-arg)
  (setq left-fn '(lambda (x) (apply operation (list x arg2)))
	right-fn '(lambda (x) (apply operation (list arg1 x))))
  (cond ((listp arg1)
	 (setq func left-fn
	       list-arg arg1))
	((listp arg2)
	 (setq func right-fn
	       list-arg arg2)))
  (mapcar func list-arg))
	
(defun am:sign (number)
  (cond ((< number 0) -1)
	((> number 0) 1)
	(t 0)))

(defun am:vector (point1 point2)
  (mapcar '- point2 point1))

(defun am:line->vector (line)
  (am:vector (car line) (cadr line)))

(defun am:dot (vector1 vector2)
  (apply '+ (mapcar '* vector1 vector2)))

(defun am:length (vector)
  (sqrt (am:dot vector vector)))

(defun am:normalize (vector
		     / len)
  (setq len (am:length vector))
  (if (/= 0 len)
    (mapcar '(lambda (x) (/ x len)) vector)))

(defun am:flatten-point (3dpt)
  (list (car 3dpt) (cadr 3dpt)))

(defun am:rotate90 (vector)
  (list (- (cadr vector))
	(car vector)))
  
(defun am:side (reference vector)
  (am:sign (am:dot reference (am:rotate90 vector))))

(defun am:same-side (reference vector1 vector2 strict
		     / side1 side2)
  (setq side1 (am:side reference vector1)
	side2 (am:side reference vector2))
  (cond ((= side1 side2) t)
	((or (= side1 0) (= side2 0)) (not strict))
	(t ())))	 

(defun am:angle-between (v1 v2
			 / ang)
  (setq ang (- (atan (cadr v2) (car v2))
	       (atan (cadr v1) (car v1))))
  (if (< ang 0.0)
      (+ (* 2 pi) ang)
      ang))

(defun am:rotate-vector (v radians
			 / x y sina cosa)
  (setq x (car v)
	y (cadr v)
	sina (sin radians)
	cosa (cos radians))
  (list (- (* cosa x) (* sina y))
	(+ (* sina x) (* cosa y))))

;;;; User input

(defun am:fill-properties (table properties row
  			   / property display
			   table-display table-value tail-results)
  (if properties
      (progn
	(setq property (car properties)
	      display (am:object-get property ':display)
	      table-display (vla-GetText table row 0)
	      table-value (vla-GetCellValue table row 1))
	(if (/= display table-display)
	    (am:make-error "invalid table property name")
	    (progn
	      (setq tail-results (am:fill-properties
				  table
				  (cdr properties)
				  (1+ row)))
	      (if (am:errorp tail-results)
		  tail-results
		  (am:make-success
		   (cons (am:make-property (am:object-get property ':name)
					   display
					   (vlax-variant-value table-value))
			 (cdr tail-results)))))))
      (am:make-success ())))

(defun am:table-updated (owner reactor-object extra
			       / reactor-data callout model new-model new-properties)
  (setq reactor-data (vlr-data reactor-object)
	callout (car reactor-data)
	model (cadr reactor-data)
	new-properties (am:fill-properties owner (am:object-get model ':properties) 1))
  (if (am:successp new-properties)
      (progn
	(setq new-model (am:make-model (am:object-get model ':name)
				      (cdr new-properties)
				      (am:object-get model ':fields)))
	(callout new-model))))

(defun am:fill-table (table properties row
		      / property display value)
  (if properties
      (progn
	(setq property (car properties)
	      display (am:object-get property ':display)
	      value (am:object-get property ':value))
	(vla-SetText table row 0 display)
	(vla-SetCellValue table row 1 value)
	(am:fill-table table (cdr properties) (1+ row)))))

(defun am:create-table (title model modified-callout parent 
			/ point table properties wrapper)
  (setq point (getpoint "Table position")
	properties (am:object-get model ':properties)
	table (vla-AddTable parent
			    (vlax-3d-point point)
			    (1+ (length properties))
			    2 ;num-columns
			    1
			    5))
  (vla-SetText table 0 0 title)
  (am:fill-table table properties 1)
  (modified-callout model)
  (setq reactor (vlr-object-reactor (list table)
				    (list modified-callout model)
				    (list (cons :vlr-modified 'am:table-updated)))))

;;;; Drawing

(defun am:create-block (doc
			/ id-name id block-name block)
  (setq id-name "free-block-id"
	id (am:default (am:get-variable doc id-name) 1)
	block-name (strcat "Trench " (itoa id))
	block (vla-Add (vla-get-Blocks doc)
		       (vlax-3d-point 0 0 0)
		       block-name))
  (am:set-variable doc id-name am:int16 (1+ id))
  block)

(defun am:append-tail (polyline last-position len block-ref
		       / next-position point)
  (setq next-position (am:flatten-point (getpoint last-position "Next segment"))
	point (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-fill point next-position)
  (vla-AddVertex polyline len point)
  (vla-Update block-ref)
  (if (< len 1)
    (am:append-tail polyline next-position (1+ len) block-ref)))

(defun am:construct-main-line (doc block block-ref
			       / start first points polyline)
  (setq start (am:flatten-point (getpoint "Trench start"))
	first (am:flatten-point (getpoint start "First segment"))
	points (vlax-make-safearray vlax-vbDouble '(0 . 3)))
  (vlax-safearray-fill points (append start first))
  (setq polyline (vla-AddLightweightPolyline block points))
  (vla-Update block-ref)
  (am:append-tail polyline first 2 block-ref)
  polyline)

(defun am:add-width (main-line width block-ref
		     / offset offset+ offset-)
  (setq offset (/ width 2)
	offset+ (vlax-variant-value (vla-Offset main-line offset))
	offset- (vlax-variant-value (vla-Offset main-line (- offset))))
  (vla-Update block-ref)
  (list (vlax-safearray-get-element offset+ 0)
	(vlax-safearray-get-element offset- 0)))

(defun am:extrude-point (point block block-ref
			 / upper-point)
  (setq upper-point (getpoint point "Height")
	new-line (vla-AddLine block
		   	      (vlax-3d-point (append point '(0)))
		              (vlax-3d-point upper-point)))
  (vla-Update block-ref)
  upper-point)

(defun am:connect-extruded (points block block-ref
			    / array flat-list)
  (setq array (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length points)))))
	flat-list (am:flatten-list (mapcar 'am:flatten-point points)))
  (vlax-safearray-fill array flat-list)
  (vla-AddLightweightPolyline block array)
  (vla-Update block-ref))

(defun am:plan-extrusion (prev-point point next-point direction
			  / next-segment-v prev-segment-v extruded-vector
			  full-angle half-angle)
  (setq next-segment-v (am:vector point next-point)
	prev-segment-v (am:vector prev-point point))
  (cond ((= prev-point nil) (setq extrude-vector (am:rotate90 next-segment-v)))
	((= next-point nil) (setq extrude-vector (am:rotate90 prev-segment-v)))
	(t (progn
	     (setq full-angle (am:angle-between prev-segment-v
						(am:scalar-op '* -1 next-segment-v))
		   half-angle (/ full-angle 2)
		   extrude-vector (am:rotate-vector prev-segment-v half-angle)))))
  (list point (am:normalize (am:scalar-op '* direction extrude-vector))))

(defun am:plan-extrusions (edge direction
			   / segment segment-v next-segment next-segment-v
			   extrusion-v curve-inside point)
  (mapcar '(lambda (a b c) (am:plan-extrusion a b c direction))
	  (append (list nil) edge)
	  edge
	  (append (cdr edge) (list nil))))

(defun am:do-extrusion (extrusion block block-ref
			/ point direction dist upper-point)
  (setq point (car extrusion)
	direction (cadr extrusion)
	dist (getdist point "Height")
	upper-point (mapcar '+ point (mapcar '(lambda (x) (* dist x)) direction)))
  (vla-AddLine block
    (vlax-3d-point (append point '(0)))
    (vlax-3d-point upper-point))
  (vla-Update block-ref)
  upper-point)

(defun am:extrude-edge (edge direction block block-ref
			    / edge-points extrusions extruded-points extrusion)
  (setq edge-points (am:pairs (am:variant->list (vla-get-Coordinates edge)))
	extrusions (am:plan-extrusions edge-points direction)
	extruded-points (mapcar '(lambda (extrusion)
				   (am:do-extrusion extrusion block block-ref))
				extrusions))
  (am:connect-extruded extruded-points block block-ref)
  extruded-points)

(defun am:trench-updated (model)
  (setq acad-object (vlax-get-acad-object)
	doc (vla-get-ActiveDocument acad-object)

	fields (am:object-get model ':fields)
	main-line (am:object-get fields ':main-line)
	block (am:object-get fields ':block)
	block-ref (am:object-get fields ':block-ref)

	properties (am:object-get model ':properties)
	width (am:property-get properties ':width)

	bottom-edges (am:add-width main-line width block-ref)))

(defun am:trench (/ acad-object doc start end model-space
		  block block-ref start main-line	
		  bottom-edges right-slope-bottom left-slope-bottom
		  right-slope-top left-slope-top)
  (setq acad-object (vlax-get-acad-object)
	doc (vla-get-ActiveDocument acad-object)
	model-space (vla-get-ModelSpace doc)
	block (am:create-block doc)
	block-ref (vla-InsertBlock model-space
                  (vlax-3d-point 0 0 0)
                  (vla-get-Name block)
                  1 1 1 0)
	main-line (am:construct-main-line doc block block-ref)
;	start (am:variant->list (vla-get-Coordinate main-line 0))
;	bottom-edges (am:add-width main-line start block-ref)
;	right-slope-bottom (car bottom-edges)
;	left-slope-bottom (cadr bottom-edges)
;	right-slope-top (am:extrude-edge right-slope-bottom -1 block block-ref)
;	left-slope-top (am:extrude-edge left-slope-bottom 1 block block-ref)
	)
  (am:create-table "Trench parameters"
		   (am:make-trench-model main-line block block-ref)
		   am:trench-updated
		   model-space))

;allow closed main lines
;slope angles
;remove the first point in (getdist) and ask for full width
;delete block on object removal
;ghost images
;control points
;shade slopes
;the shading on slopes should stretch over the whole edge
