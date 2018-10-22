;;;; constants and utils

(setq am:boolean 290)
(setq am:string 300)
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
		      / document value result)
  (setq document (vla-get-Document dictionary)
	value (am:get-variable dictionary name)
	result (vl-catch-all-apply 'vla-HandleToObject
				   (list document value)))
  (if (not (vl-catch-all-error-p result)) result))


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
  (if object
      (am:set-variable dictionary
		       name 
		       am:pointer (vla-get-Handle object))
      (vla-Remove dictionary name)))

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

(defun am:make-trench-model (main-line)
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
		 (list (cons ':main-line main-line))))

(defun am:load-trench-height-map (dictionary)
  (mapcar '(lambda (index)
	     (list (cons ':bottom (am:get-variable dictionary
						   (strcat "trench-heights-bottom-" (itoa index))))
		   (cons ':top (am:get-variable dictionary
						(strcat "trench-heights-top-" (itoa index))))))
	  (am:seq 1 (am:get-variable dictionary "trench-heights-length"))))

(defun am:load-trench (dictionary
		       / document)
  (am:make-model ':trench
		 (list (am:make-property ':width "Width" (am:get-variable dictionary "trench-width"))
		       (am:make-property ':m "m" (am:get-variable dictionary "trench-m"))
		       (am:make-composite-property ':height-map
						   "Heights"
						   (list (am:make-column ':bottom "Bottom")
							 (am:make-column ':top "Top"))
						   (am:load-trench-height-map dictionary)))
		 (list (cons ':main-line (am:get-object dictionary "trench-main-line")))))

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
  (am:set-object dictionary "trench-main-line" (am:object-get fields ':main-line)))

(defun am:height-diff (heights)
  (if heights
      (- (am:object-get heights ':top)
	 (am:object-get heights ':bottom))))

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

(defun am:table-updated (owner reactor-object
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
	(save-callout new-model dictionary)
	(callout new-model))))

(defun am:table-erased (reactor-object
			/ reactor-data delete-legend-callout dictionary)
  (setq reactor-data (vlr-data reactor-object)
	dictionary (am:object-get reactor-data ':dictionary)
	delete-legend-callout (am:object-get reactor-data ':delete-legend))
  (am:set-object dictionary "table" nil)
  (delete-legend-callout (am:object-get reactor-data ':dictionary)))

(defun am:table-modified (owner reactor-object extra)
  (if (vlax-erased-p owner)
      (am:table-erased reactor-object)
      (am:table-updated owner reactor-object)))

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

(defun am:bind-table-events (table dictionary load-callout modified-callout
			     save-callout delete-legend-block)
  (vlr-object-reactor (list table)
		      (list (cons ':dictionary dictionary)
			    (cons ':load load-callout)
			    (cons ':modified modified-callout)
			    (cons ':save save-callout)
			    (cons ':delete-legend delete-legend-block))
		      (list (cons :vlr-modified 'am:table-modified))))

(defun am:create-table (title model dictionary
			load-callout modified-callout save-callout
			draw-legend-block delete-legend-block
			parent
			/ doc point table properties wrapper block existing-table
			legend-block legend-block-name legend-block-ref)
  (setq existing-table (am:get-object dictionary "table"))
  (if (and existing-table
	   (not (vlax-erased-p existing-table)))
      (vla-Delete existing-table))
  (setq point (getpoint "Table position\n")
	properties (am:object-get model ':properties)
	table (vla-AddTable parent
			    (vlax-3d-point point)
			    (am:calculate-table-rows properties)
			    (am:calculate-table-columns properties)
			    1
			    5))
  (am:set-object dictionary "table" table)
  (vla-SetText table 0 0 title)
  (am:fill-table table properties 1)
  (save-callout model dictionary)
  (modified-callout model)
  (draw-legend-block model)
  (am:bind-table-events table
			dictionary
			load-callout
			modified-callout
			save-callout
			delete-legend-block))

;;;; Drawing

(setq *legend-joints-color*
      (vlax-create-object "AutoCAD.AcCmColor.20"))
(vla-SetRGB *legend-joints-color* 248 215 49)

(setq *legend-joints-transparency* 50)

(defun am:line->points (line)
  (am:pairs (am:variant->list (vla-get-Coordinates line))))

(defun am:create-named-block (doc name)
  (vla-Add (vla-get-Blocks doc)
	   (vlax-3d-point 0 0 0)
	   name))

(defun am:create-block (doc
			/ id-name id block-name block)
  (setq id-name "free-block-id"
	id (am:default (am:get-variable (am:get-dictionary doc) id-name) 1)
	block-name (strcat "Trench " (itoa id))
	block (am:create-named-block doc block-name))
  (am:set-variable (am:get-dictionary doc) id-name am:int16 (1+ id))
  block)

(defun am:create-subblock (parent-block child-name
			   / doc parent-name)
  (setq doc (vla-get-Document parent-block)
	parent-name (vla-get-Name parent-block)
	child-name (strcat parent-name "-" child-name))
  (vla-Add (vla-get-Blocks doc)
	   (vlax-3d-point 0 0 0)
	   child-name))

(defun am:append-tail (polyline first-position last-position len parent 
		       / next-position point)
  (initget 0 "Stop Close")
  (setq input (getpoint last-position "Next segment or [Stop/Close]\n")
	next-position (cond ((equal input "Stop") ())
			    ((equal input nil) ())
			    ((equal input "Close") first-position)
			    (t (am:flatten-point input))))
  (cond ((equal next-position first-position)
	 (vla-put-Closed polyline :vlax-true))
	(next-position
	 (setq point (vlax-make-safearray vlax-vbDouble '(0 . 1)))
	 (vlax-safearray-fill point next-position)
	 (vla-AddVertex polyline len point)
	 (am:append-tail polyline
			 first-position
			 next-position
			 (1+ len)
			 parent))))

(defun am:construct-main-line (doc
			       / model-space start first points polyline)
  (setq start (am:flatten-point (getpoint "Trench start\n"))
	first (am:flatten-point (getpoint start "First segment\n"))
	points (vlax-make-safearray vlax-vbDouble '(0 . 3))
	model-space (vla-get-ModelSpace doc))
  (vlax-safearray-fill points (append start first))
  (setq polyline (vla-AddLightweightPolyline model-space points))
  (am:append-tail polyline start first 2 model-space)
  polyline)

(defun am:connect-extruded (points block closed
			    / useful-points array flat-list polyline)
  (setq useful-points (if closed
			  (reverse (cdr (reverse points)))
			  points)
	array (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length useful-points)))))
	flat-list (am:flatten-list (mapcar 'am:flatten-point useful-points)))
  (vlax-safearray-fill array flat-list)
  (setq polyline (vla-AddLightweightPolyline block array))
  (vla-put-Closed polyline (if closed :vlax-true :vlax-false))
  polyline)

(defun am:extrude-point (point distance vector)
  (mapcar '+ point
	  (am:scalar-op '* distance (am:normalize vector))))

(defun am:plan-extrusion (prev-point point next-point
			  prev-distance distance next-distance 
			  / prev-vector next-vector prev-begin
			  prev-end next-begin next-end directed-distance)
  (setq next-segment-v (am:vector point next-point)
	prev-segment-v (am:vector prev-point point))
  (cond ((not prev-point) (am:extrude-point point distance (am:rotate90 next-segment-v)))
	((not next-point) (am:extrude-point point distance (am:rotate90 prev-segment-v)))
	(t (progn
	     (setq prev-begin (am:extrude-point prev-point
						prev-distance
						(am:rotate90 prev-segment-v))
		   prev-end (am:extrude-point point
					      distance
					      (am:rotate90 prev-segment-v))
		   next-begin (am:extrude-point point
						distance
						(am:rotate90 next-segment-v))
		   next-end (am:extrude-point next-point
					      next-distance
					      (am:rotate90 next-segment-v)))
	     (inters prev-begin prev-end next-begin next-end nil)))))


(defun am:plan-closed-extrusions (edge lengths)
  (mapcar 'am:plan-extrusion
	  (cons (last edge) edge)
	  edge
	  (append (cdr edge) (list (car edge)))
	  (cons (last lengths) lengths)
	  lengths
	  (append (cdr lengths) (list (car lengths)))))

(defun am:plan-opened-extrusions (edge lengths)
  (mapcar 'am:plan-extrusion
	  (cons 'nil edge)
	  edge
	  (append (cdr edge) '(nil))
	  (cons 'nil lengths)
	  lengths
	  (append (cdr lengths) '(nil))))

(defun am:offset (line lengths block
		  / line-points length-map closed extruded-points edge-points)
  (setq line-points (am:line->points line)
	length-map (if (numberp lengths)
		       (mapcar '(lambda (_) lengths) line-points)
		       lengths)

	closed (= :vlax-true (vla-get-Closed line))
	extruded-points (if closed
			    (am:plan-closed-extrusions line-points length-map)
			    (am:plan-opened-extrusions line-points length-map)))

  (if closed (setq extruded-points (append extruded-points (list (car extruded-points)))))

  (am:connect-extruded extruded-points block closed))

(defun am:shade-slope (bottom next-bottom top next-top block
		       / step top-vector top-unit-vector top-length
		       count)
  (setq step 0.3
	top-vector (am:vector top next-top)
	top-unit-vector (am:normalize top-vector)
	shading-unit-vector (am:rotate90 top-unit-vector)
	top-length (am:length top-vector)
	count (- (fix top-length) 1))
  (mapcar '(lambda (i
		    / shade-origin shade-end inters-line-1 inters-line-2)
	     (setq shade-origin (mapcar '+
					top
					(am:scalar-op '* top-unit-vector i))
		   shade-end (inters shade-origin
				     (mapcar '+ shade-origin shading-unit-vector)
				     bottom
				     next-bottom
				     nil))
	     (if (= 1 (rem i 2))
		 (setq shade-end (mapcar '+
					 shade-origin
					 (am:scalar-op '/
						       (am:vector shade-origin shade-end)
						       2.0))))
	     (setq inters-line-1 (inters shade-origin
					 shade-end
					 top
					 bottom)
		   inters-line-2 (inters shade-origin
					 shade-end
					 next-top
					 next-bottom)
		   shade-end (cond (inters-line-1 inters-line-1)
				   (inters-line-2 inters-line-2)
				   (t shade-end)))
	     (vla-AddLine block
			  (vlax-3d-point shade-origin)
			  (vlax-3d-point shade-end)))
	  (am:seq 1 (1+ count))))

(defun am:add-width (main-line width block
		     / offset)
  (setq offset (/ width 2))
  (list (am:offset main-line (- offset) block)
	(am:offset main-line offset block)))


(defun am:extrude-edge (edge direction m height-map shading-block block
			/ closed edge-points projected-direction height-diffs extruded-line extruded-points)
  (setq closed (= (vla-get-Closed edge) :vlax-true)
	edge-points (am:line->points edge)
	projected-direction (* m direction)
	height-diffs (mapcar 'am:height-diff height-map)
	extruded-line (am:offset edge (am:scalar-op '* projected-direction height-diffs) block)
	extruded-points (am:line->points extruded-line))

  (if closed (setq extruded-points (append extruded-points (list (car extruded-points)))
		   edge-points (append edge-points (list (car edge-points)))))

  (mapcar '(lambda (bottom top)
	     (vla-AddLine block (vlax-3d-point bottom) (vlax-3d-point top)))
	  edge-points
	  extruded-points)
  
  (mapcar '(lambda (point next-point extruded next-extruded)
	     (am:shade-slope point next-point extruded next-extruded shading-block))
	  edge-points
	  (cdr edge-points)
	  extruded-points
	  (cdr extruded-points))
  
  extruded-points)

(defun am:clear-trench (block block-ref
			/ i item block-name)
  (if (and block
	   block-ref
	   (not (vlax-erased-p block))
	   (not (vlax-erased-p block-ref)))
      (progn
	(setq i (1- (vla-get-Count block)))
	(while (>= i 0)
	  (setq item (vla-Item block i)
		block-name (if (= "AcDbBlockReference" (vla-get-ObjectName item))
			       (vla-get-EffectiveName item)))
	  (if (/= (vla-get-ObjectId item) id)
	      (vla-Delete item))
	  (if block-name
	      (vla-Delete (vla-Item
			   (vla-get-Blocks (vla-get-Document block))
			   block-name)))
	  (setq i (1- i)))
	(vla-Delete block-ref)
	(vla-Delete block))))

(defun am:delete-legend (dictionary)
  (setq legend-block (am:get-object dictionary "legend-block")
	legend-block-ref (am:get-object dictionary "legend-block-ref"))
  (am:clear-trench legend-block legend-block-ref)
  (am:set-object dictionary "legend-block" nil)
  (am:set-object dictionary "legend-block-ref" nil))

(defun am:redraw-legend (dictionary
			 / model fields main-line line-points block	
			 doc model-space dictionary legend-block legend-block-ref
			 legend-block-name legend-block legend-block-ref)

  (setq model (am:load-trench dictionary)
	fields (am:object-get model ':fields)
	main-line (am:object-get fields ':main-line)
	line-points (am:line->points main-line)
	block (am:get-object dictionary "block")
	doc (vla-get-Document main-line)
	model-space (vla-get-ModelSpace doc))

  (am:delete-legend dictionary)
  

  (setq legend-block-name (strcat (vla-get-Name block) "-legend")
	legend-block (am:create-named-block doc legend-block-name)) 
  (mapcar '(lambda (point number)
	     (setq label (vla-AddText legend-block
				       (itoa number)
				       (vlax-3d-Point point)
				       2))
	     (vla-put-TrueColor label *legend-joints-color*)
	     (vla-put-EntityTransparency label *legend-joints-transparency*))
	  line-points
	  (am:seq 1 (length line-points))) 
  (setq legend-block-ref (vla-InsertBlock model-space
					  (vlax-3d-point 0 0 0)
					  legend-block-name
					  1 1 1 0))
  (am:set-object dictionary "legend-block" legend-block)
  (am:set-object dictionary "legend-block-ref" legend-block-ref)
  nil)

(defun am:numerate-joints (model
			   / fields main-line main-line-dictionary)
  (setq fields (am:object-get model ':fields)
	main-line (am:object-get fields ':main-line)
	main-line-dictionary (vla-getExtensionDictionary main-line))
  (am:redraw-legend main-line-dictionary))

(defun am:redraw-trench (main-line-dictionary
			 / acad-object doc model-space
			 model fields main-line block block-ref
			 properties width m height-map
			 bottom-edges bottom-edge-right bottom-edge-left
			 top-edge-right top-edge-left)
  (setq acad-object (vlax-get-acad-object)
	doc (vla-get-ActiveDocument acad-object)
	model-space (vla-get-ModelSpace doc)

	model (am:load-trench main-line-dictionary)
	fields (am:object-get model ':fields)
	main-line (am:object-get fields ':main-line)
	
	block (am:get-object main-line-dictionary "block")
	block-ref (am:get-object main-line-dictionary "block-ref")

	properties (am:object-get model ':properties)
	width (am:property-get properties ':width)
	m (am:property-get properties ':m)
	height-map (am:property-get properties ':height-map))

  (am:clear-trench block block-ref)
  
  (setq block (am:create-block doc)
	bottom-edges (am:add-width main-line width block)
	bottom-edge-right (car bottom-edges)
	bottom-edge-left (cadr bottom-edges)
	shading-block (am:create-subblock block "shading")
        top-edge-right (am:extrude-edge bottom-edge-right -1 m height-map shading-block block)
	top-edge-left (am:extrude-edge bottom-edge-left 1 m height-map shading-block block))
  (vla-InsertBlock block
                   (vlax-3d-point 0 0 0)
                   (vla-get-Name shading-block)
                   1 1 1 0)
  (setq block-ref (vla-InsertBlock model-space
				   (vlax-3d-point 0 0 0)
				   (vla-get-Name block)
				   1 1 1 0)
	block-dictionary (vla-getExtensionDictionary block-ref))
  (am:set-object main-line-dictionary "block" block) 
  (am:set-object main-line-dictionary "block-ref" block-ref) 
  (am:set-variable main-line-dictionary "is-trench-main-line" am:boolean :vlax-true)
  (am:set-object block-dictionary "main-line" main-line)
  (am:set-variable block-dictionary "is-trench-block" am:boolean :vlax-true) )

(defun am:trench-updated (model
			  / fields main-line main-line-dictionary)
  (setq fields (am:object-get model ':fields)
	main-line (am:object-get fields ':main-line)
	main-line-dictionary (vla-getExtensionDictionary main-line))
  (am:redraw-trench main-line-dictionary))

(defun am:select-trench-main-line (/ selected selected-dictionary)
  (setq selected (vlax-ename->vla-object (car (entsel "Select a trench\n")))
	selected-dictionary (vla-getExtensionDictionary selected))
  (cond ((am:get-variable selected-dictionary "is-trench-main-line") selected)
	((am:get-variable selected-dictionary "is-trench-block")
	 (am:get-object selected-dictionary "main-line"))))

(defun am:restore-table (main-line model-space
			 / dictionary model)
  (setq dictionary (vla-GetExtensionDictionary main-line)
	model (am:load-trench dictionary)) 
  (am:create-table "Trench parameters"
		   model
		   dictionary
		   am:load-trench
		   am:trench-updated
		   am:save-trench
		   am:numerate-joints
		   am:delete-legend
		   model-space))

(defun am:main-line-changed (owner reactor-object extra)
 (setq main-line-dictionary (vlr-data reactor-object)) 
 (am:redraw-trench main-line-dictionary) 
 (if (am:get-object main-line-dictionary "legend-block-ref")
     (am:redraw-legend main-line-dictionary)))

(defun am:main-line-deleted (owner reactor-object extra
			     / main-line-dictionary table)
  (setq main-line-dictionary (vlr-data reactor-object))
  (am:clear-trench (am:get-object main-line-dictionary "block")
		   (am:get-object main-line-dictionary "block-ref"))
  (setq table (am:get-object main-line-dictionary "table"))
  (if table (vla-Delete table))
  (am:remove-trench main-line-dictionary (vla-get-Document main-line-dictionary)))

(defun am:main-line-modified (owner reactor-object extra
			      / main-line main-line-dictionary)
  (if (vlax-erased-p owner)
      (am:main-line-deleted owner reactor-object extra)
      (am:main-line-changed owner reactor-object extra)))

(defun am:add-trench (main-line document
		      / dictionary total-trenches trench-name)
  (setq dictionary (am:get-dictionary document)
	total-trenches (am:default (am:get-variable dictionary "total-trenches") 0)
	trench-name (strcat "trench-" (itoa total-trenches)))
  (am:set-object dictionary trench-name main-line)
  (am:set-variable (vla-GetExtensionDictionary main-line)
		   "trench-name"
		   am:string
		   trench-name)
  (am:set-variable dictionary
		   "total-trenches"
		   am:int16
		   (1+ total-trenches)))

(defun am:bind-trench-events (main-line)
  (vlr-object-reactor (list main-line)
		      (vla-GetExtensionDictionary main-line)
		      (list (cons :vlr-modified 'am:main-line-modified))))

(defun am:remove-trench (main-line-dictionary document
			 / trench-name dictionary total-trenches
			 new-total last-trench-name last-trench)
  (setq trench-name (am:get-variable main-line-dictionary "trench-name")
	dictionary (am:get-dictionary document)
	total-trenches (am:get-variable dictionary "total-trenches"))
  (if (and total-trenches
	   (and trench-name
		(am:get-variable dictionary trench-name)))
      (progn
	(setq new-total (1- total-trenches)
	      last-trench-name (strcat "trench-" (itoa new-total)))
	(if (not (equal last-trench-name trench-name))
	    (progn
	      (setq last-trench (am:get-object dictionary
					       last-trench-name))
	      (am:set-object dictionary trench-name last-trench)
	      (am:set-variable (vla-GetExtensionDictionary last-trench)
			       "trench-name"
			       am:string
			       trench-name)))
	(am:set-object dictionary last-trench-name nil)
	(am:set-variable dictionary
			 "total-trenches"
			 am:int16
			 new-total))))

(defun am:scan-trenches (/ acad-object doc dictionary trench-index total-trenches
			 trench-name trench trench-dictionary existing-trenches
			 table)
  (setq acad-object (vlax-get-acad-object)
	doc (vla-get-ActiveDocument acad-object)
	dictionary (am:get-dictionary doc)
	total-trenches (am:default
			(am:get-variable dictionary "total-trenches")
			0)
	existing-trenches ())
  (foreach trench-index (am:seq 0 (1- total-trenches))
	   (setq trench-name (strcat "trench-" (itoa trench-index))
		 trench (am:get-object dictionary trench-name))
	   (if trench
	       (setq existing-trenches (cons trench existing-trenches)))
	   (am:set-object dictionary trench-name nil))
  (am:set-object dictionary "total-trenches" nil)
  (foreach trench existing-trenches
	   (am:bind-trench-events trench)
	   (am:add-trench trench doc)
	   (setq trench-dictionary (vla-GetExtensionDictionary trench)
		 table (am:get-object trench-dictionary "table"))
	   (if table (am:bind-table-events table
					   trench-dictionary
					   am:load-trench
					   am:trench-updated
					   am:save-trench
					   am:delete-legend))))

(defun c:edit-trench (/ acad-object doc model-space selected-trench
			model block-ref dictionary)
  (setq acad-object (vlax-get-acad-object)
	doc (vla-get-ActiveDocument acad-object)
	model-space (vla-get-ModelSpace doc)
	main-line (am:select-trench-main-line))

  (if main-line
      (am:restore-table main-line model-space)
      (alert "This is not a trench")))

(defun c:trench (/ acad-object doc start end model-space
		   start main-line trench-model	
		   bottom-edges right-slope-bottom left-slope-bottom
		   right-slope-top left-slope-top dictionary)
  (setq acad-object (vlax-get-acad-object)
	doc (vla-get-ActiveDocument acad-object)
	model-space (vla-get-ModelSpace doc)
	main-line (am:construct-main-line doc)
	dictionary (vla-GetExtensionDictionary main-line)
	trench-model (am:make-trench-model main-line))

  (am:add-trench main-line doc)
  
  (am:create-table "Trench parameters"
		   trench-model
		   dictionary
		   am:load-trench
		   am:trench-updated
		   am:save-trench
		   am:numerate-joints
		   am:delete-legend
		   model-space)
  (am:bind-trench-events main-line))

(defun am:init ()
  (vl-load-com)
  (am:scan-trenches))

(am:init)

;;;;
;;allow save-loading
;;delete block on object removal
;;make the slope shading available as a separate command
;;move user input from table to the ribbon
;;input validation
;;optionally add slopes on trench's ends
;;make the main-line be separate from the block and allow it's edit
