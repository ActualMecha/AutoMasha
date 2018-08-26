(vl-load-com)
;;;; constants and utils

(setq am:pointer 320)
(setq am:float 1040)
(setq am:distance 1041)
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

(defun am:aggregate (lst fn initial)
  (if lst
      (am:aggregate (cdr lst)
		    fn
		    (fn initial (car lst)))
      initial))

(defun am:max (lst)
  (am:aggregate lst
                (lambda (acc next) (if (> next acc) next acc))
                ()))

(defun am:seq (low high)
  (if (> low high)
      ()
      (cons low (am:seq (1+ low) high))))

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

(defun am:get-variable (dictionary name
			/ record out-type out-value)
  (setq record (am:get-xrecord dictionary name))
  (vla-GetXRecordData record 'out-type 'out-value)
  (if (/= out-value nil)
    (vlax-variant-value (vlax-safearray-get-element out-value 0))))

(defun am:get-object (dictionary name
		      / document value)
  (setq document (vla-get-Document dictionary)
	value (am:get-variable dictionary name))
  (if value (vla-HandleToObject document value)))


(defun am:set-variable (dictionary name var-type var-value
			/ record record-type record-value)
  (if var-value
      (progn
	(setq record (am:get-xrecord dictionary name)
	      record-type (vlax-make-safearray vlax-vbInteger '(0 . 0))
	      record-value (vlax-make-safearray vlax-vbVariant '(0 . 0)))
	(vlax-safearray-put-element record-type 0 var-type)
	(vlax-safearray-put-element record-value 0 var-value)
	(vla-SetXRecordData record record-type record-value))
      (vla-Remove dictionary name)))

(defun am:set-object (dictionary name object)
  (am:set-variable dictionary
		   name 
		   am:pointer (vla-get-Handle object)))

;;;; Model

(defun am:object-get (object field)
  (cdr (assoc field object)))

(defun am:make-property (name display value)
  (list (cons ':property-type ':property)
	(cons ':name name)
	(cons ':display display)
	(cons ':value value)
	(cons ':column-count 1)))

(defun am:make-column (name display)
  (list (cons ':name name)
	(cons ':display display)))

(defun am:make-composite-property (name display columns rows)
  (list (cons ':property-type ':composite-property)
	(cons ':name name)
	(cons ':display display)
	(cons ':columns columns)
	(cons ':rows rows)
	(cons ':column-count (length columns))))

(defun am:property-p (property)
  (= (am:object-get property ':property-type) ':property))

(defun am:composite-property-p (property)
  (= (am:object-get property ':property-type) ':composite-property))

(defun am:property-find (properties name)
  (am:find-if properties
    	      (lambda (property) (= (am:object-get property ':name) name))))

(defun am:property-get (properties name
			/ property)
  (setq property (am:property-find properties name))
  (if (am:property-p property)
      (am:object-get property ':value)
      (am:object-get property ':rows)))

(defun am:make-model (model-name properties fields)
  (list (cons ':name model-name)
	(cons ':properties properties)
	(cons ':fields fields)))

(defun am:make-trench-model (main-line block block-ref)
  (am:make-model ':trench
		 (append
		  (list (am:make-property ':width "Width" 1.0)
			(am:make-property ':m "m" 0.66)
			(am:make-composite-property
			 ':height-map
			 "Height"
			 (list (am:make-column ':bottom "Bottom")
			       (am:make-column ':top "Top"))
			 (mapcar '(lambda (_)
				    (list (cons ':bottom 0.0)
					  (cons ':top 3.0)))
				 (am:pairs (am:variant->list (vla-get-Coordinates main-line)))))))
		 (list (cons ':main-line main-line)
		       (cons ':block block)
		       (cons ':block-ref block-ref))))

(defun am:load-trench-height-map (dictionary)
  (mapcar '(lambda (index)
	     (list (cons ':top (am:get-variable dictionary
						(strcat "trench-heights-bottom-" (itoa index))))
		   (cons ':bottom (am:get-variable dictionary
						(strcat "trench-heights-top-" (itoa index))))))
	  (am:seq 1 (am:get-variable dictionary "trench-heights-length"))))

(defun am:load-trench (dictionary
		       / document)
  (am:make-model ':trench
		 (list (am:make-property ':width "Width" (am:get-variable dictionary "trench-width"))
		       (am:make-property ':m "m" (am:get-variable dictionary "trench-m"))
		       (am:make-composite-property ':height-map
						   "Heights"
						   (list (am:make-column ':top "Top")
							 (am:make-column ':bottom "Bottom"))
						   (am:load-trench-height-map dictionary)))
		 (list (cons ':main-line (am:get-object dictionary "trench-main-line"))
		       (cons ':block (am:get-object dictionary "trench-block"))
		       (cons ':block-ref (am:get-object dictionary "trench-block-ref")))))

(defun am:save-trench-height-map (dictionary height-map)
  (am:set-variable dictionary "trench-heights-length"
		   am:int16 (length height-map))
  (mapcar '(lambda (heights i)
	     (am:set-variable dictionary
			      (strcat "trench-heights-bottom-" (itoa i))
			      am:float
			      (am:object-get heights ':bottom))
	     (am:set-variable dictionary
			      (strcat "trench-heights-top-" (itoa i))
			      am:float
			      (am:object-get heights ':top)))
	  height-map
	  (am:seq 1 (length height-map))))

(defun am:save-trench (trench-model dictionary
		       / properties fields)
  (setq properties (am:object-get trench-model ':properties)
	fields (am:object-get trench-model ':fields))
  (am:set-variable dictionary "trench-width"
		   am:distance (am:property-get properties ':width))
  (am:set-variable dictionary "trench-m"
		   am:float (am:property-get properties ':m))
  (am:save-trench-height-map dictionary (am:property-get properties ':height-map))
  (am:set-object dictionary "trench-main-line" (am:object-get fields ':main-line))
  (am:set-object dictionary "trench-block" (am:object-get fields ':block))
  (am:set-object dictionary "trench-block-ref" (am:object-get fields ':block-ref)))

(defun am:make-success (data)
  (cons ':success data))

(defun am:make-error (data)
  (cons ':error data))

(defun am:successp (result)
  (= ':success (car result)))

(defun am:errorp (result)
  (= ':error (car result)))


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

(defun am:read-simple-property (table property row
			        / display table-display)
  (setq display (am:object-get property ':display)
	table-display (vla-GetText table row 0)
	table-value (vla-GetCellValue table row 1))
  (if (/= display table-display)
      (am:make-error "invalid table property name")
      (am:make-success (am:make-property (am:object-get property ':name)
					 display
					 (vlax-variant-value table-value)))))

(defun am:read-composite-property-row (table columns row-index)
  (mapcar '(lambda (column column-index)
	     (cons (am:object-get column ':name)
		   (vlax-variant-value (vla-GetCellValue table row-index column-index))))
	  columns
	  (am:seq 1 (length columns))))

(defun am:read-composite-property (table property row
				   / rows columns row-index)
  (setq rows (am:object-get property ':rows)
	columns (am:object-get property ':columns))
  (am:make-success
   (am:make-composite-property
    (am:object-get property ':name)
    (am:object-get property ':display)
    columns
    (mapcar '(lambda (row row-index)
	       (am:read-composite-property-row table columns row-index))
	    rows
	    (am:seq (1+ row) (+ 1 row (length rows)))))))

(defun am:read-table (table properties row
  		      / property display
		      table-display table-value property-result tail-results)
  (if properties
      (progn
	(setq property (car properties)
	      property-result (if (am:property-p property)
				  (am:read-simple-property table property row)
				  (am:read-composite-property table property row)))
	(if (am:errorp property-result)
	    property-result
	    (progn
	      (setq tail-results (am:read-table table (cdr properties) (1+ row)))
	      (if (am:errorp tail-results)
		  tail-results
		  (am:make-success (cons (cdr property-result)
					 (cdr tail-results)))))))
      (am:make-success ())))

(defun am:table-updated (owner reactor-object extra
			 / reactor-data load-callout callout save-callout model new-model new-properties)
  (setq reactor-data (vlr-data reactor-object)
	dictionary (am:object-get reactor-data ':dictionary)
	load-callout (am:object-get reactor-data ':load)
	callout (am:object-get reactor-data ':modified)
	save-callout (am:object-get reactor-data ':save)
	model (load-callout dictionary)
	new-properties (am:read-table owner (am:object-get model ':properties) 1))
  (if (am:successp new-properties)
      (progn
	(setq new-model (am:make-model (am:object-get model ':name)
				       (cdr new-properties)
				       (am:object-get model ':fields)))
	(callout new-model))))

(defun am:fill-simple-property (table property row)
  (vla-SetText table row 0 (am:object-get property ':display))
  (vla-SetCellValue table row 1 (am:object-get property ':value)))

(defun am:fill-composite-property-header (table columns row column)
  (if columns
      (progn
	(vla-SetText table row column (am:object-get (car columns) ':display))
	(am:fill-composite-property-header table
					   (cdr columns)
					   row
					   (1+ column)))))
(defun am:fill-composite-property-cells (table columns row row-index column-index
					/ column value)
  (if columns
      (progn
	(setq column (car columns)
	      value (am:object-get row (am:object-get column ':name)))
	(vla-SetCellValue table row-index column-index value)
	(am:fill-composite-property-cells table
					  (cdr columns)
					  row
					  row-index
					  (1+ column-index)))))

(defun am:fill-composite-property-rows (table columns rows header-index property-index
				       / row-index)
  (if rows
      (progn
	(setq row-index (+ header-index property-index))
	(vla-SetText table row-index 0 property-index)
	(am:fill-composite-property-cells table
					  columns
					  (car rows)
					  row-index
					  1)
	(am:fill-composite-property-rows table
					 columns
					 (cdr rows)
					 header-index
					 (1+ property-index)))))

(defun am:fill-composite-property (table property row)
  (vla-SetText table row 0 (am:object-get property ':display))
  (am:fill-composite-property-header table
				     (am:object-get property ':columns)
				     row
				     1)
  (am:fill-composite-property-rows table
				   (am:object-get property ':columns)
				   (am:object-get property ':rows)
				   row
				   1)
  (+ 1 row (length (am:object-get property ':rows))))

(defun am:fill-table (table properties row
		      / property)
  (if properties
      (progn
	(setq property (car properties))
	(if (am:property-p property)
	    (am:fill-simple-property table property row)
	    (am:fill-composite-property table property row))
	(am:fill-table table (cdr properties) (1+ row)))))

(defun am:calculate-table-rows (properties)
  (am:aggregate properties
		(lambda (acc property)
		  (if (am:property-p property)
		      (1+ acc)
		      (+ 1 acc (length (am:object-get property ':rows)))))
		1))

(defun am:calculate-table-columns (properties)
  (1+ (am:max (mapcar '(lambda (property)
			 (if (am:property-p property)
			     1
			     (am:object-get property ':column-count)))
		      properties))))

(defun am:create-table (title model load-callout modified-callout save-callout parent 
			/ point table properties wrapper)
  (setq point (getpoint "Table position")
	properties (am:object-get model ':properties)
	table (vla-AddTable parent
			    (vlax-3d-point point)
			    (am:calculate-table-rows properties)
			    (am:calculate-table-columns properties)
			    1
			    5)
	dictionary (vla-GetExtensionDictionary table))
  (vla-SetText table 0 0 title)
  (am:fill-table table properties 1)
  (modified-callout model)
  (save-callout model dictionary)
  (setq reactor (vlr-object-reactor (list table)
				    (list (cons ':dictionary dictionary)
					  (cons ':load load-callout)
					  (cons ':modified modified-callout)
					  (cons ':save save-callout))
				    (list (cons :vlr-modified 'am:table-updated)))))

;;;; Drawing

(defun am:create-block (doc
			/ id-name id block-name block)
  (setq id-name "free-block-id"
	id (am:default (am:get-variable (am:get-dictionary doc) id-name) 1)
	block-name (strcat "Trench " (itoa id))
	block (vla-Add (vla-get-Blocks doc)
		       (vlax-3d-point 0 0 0)
		       block-name))
  (am:set-variable (am:get-dictionary doc) id-name am:int16 (1+ id))
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
  (list (vlax-safearray-get-element offset+ 0)
	(vlax-safearray-get-element offset- 0)))

(defun am:connect-extruded (points block block-ref
			    / array flat-list)
  (setq array (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length points)))))
	flat-list (am:flatten-list (mapcar 'am:flatten-point points)))
  (vlax-safearray-fill array flat-list)
  (vla-AddLightweightPolyline block array))

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

(defun am:plan-extrusions (edge direction)
  (mapcar '(lambda (a b c) (am:plan-extrusion a b c direction))
	  (append (list nil) edge)
	  edge
	  (append (cdr edge) (list nil))))

(defun am:do-extrusion (extrusion m heights block block-ref
			/ point direction dist upper-point)
  (setq point (car extrusion)
	direction (cadr extrusion)
	dist (* m (- (am:object-get heights ':top)
		     (am:object-get heights ':bottom)))
	upper-point (mapcar '+
			    point
			    (mapcar '(lambda (x) (* dist x))
				    direction)))
  (vla-AddLine block
    (vlax-3d-point (append point '(0)))
    (vlax-3d-point upper-point))
  upper-point)

(defun am:extrude-edge (edge direction m height-map block block-ref
			/ edge-points extrusions extruded-points extrusion)
  (setq edge-points (am:pairs (am:variant->list (vla-get-Coordinates edge)))
	extrusions (am:plan-extrusions edge-points direction)
	extruded-points (mapcar '(lambda (extrusion heights)
				   (am:do-extrusion extrusion m heights block block-ref))
				extrusions
				height-map))
  (am:connect-extruded extruded-points block block-ref)
  extruded-points)

(defun am:clear-trench (main-line block block-ref
			/ i id item)
  (setq i (1- (vla-get-Count block))
	id (vla-get-ObjectId main-line))
  (while (>= i 0)
    (setq item (vla-Item block i))
    (if (/= (vla-get-ObjectId item) id)
	(vla-Delete item))
    (setq i (1- i))))

(defun am:trench-updated (model
			  / acad-object doc
			  fields main-line block block-ref dictionary
			  properties width m height-map
			  bottom-edges bottom-edge-right bottom-edge-left
			  top-edge-right top-edge-left)
  (setq acad-object (vlax-get-acad-object)
	doc (vla-get-ActiveDocument acad-object)

	fields (am:object-get model ':fields)
	main-line (am:object-get fields ':main-line)
	block (am:object-get fields ':block)
	block-ref (am:object-get fields ':block-ref)
        dictionary (vla-GetExtensionDictionary block)

	properties (am:object-get model ':properties)
	width (am:property-get properties ':width)
	m (am:property-get properties ':m)
	height-map (am:property-get properties ':height-map))

  (am:clear-trench main-line block block-ref)

  (setq bottom-edges (am:add-width main-line width block-ref)
	bottom-edge-right (car bottom-edges)
	bottom-edge-left (cadr bottom-edges)
        top-edge-right (am:extrude-edge bottom-edge-right 1 m height-map block block-ref)
	top-edge-left (am:extrude-edge bottom-edge-left -1 m height-map block block-ref))

  (vla-Update block-ref))

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
	main-line (am:construct-main-line doc block block-ref))
  (am:create-table "Trench parameters"
		   (am:make-trench-model main-line block block-ref)
		   am:load-trench
		   am:trench-updated
		   am:save-trench
		   model-space))

;allow save-loading
;allow closed main lines
;delete block on object removal
;shade slopes
;the shading on slopes should stretch over the whole edge
