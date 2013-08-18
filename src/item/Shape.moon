class Shape extends Item
	_applyMatrix: false
	
	new: (type, point, size) =>
    super(point)
    @_type = type
    @_size = size

  _draw: (ctx, param) =>
    style       = @_style
    size        = @_size
    width       = size.width
    height      = size.height
    fillColor   = style\getFillColor()
    strokeColor = style\getStrokeColor()

    if fillColor or strokeColor or param.clip
      ctx\beginPath()
      switch @_type
        when "rect"
          ctx.rect -width / 2, -height / 2, width, height
        when "circle"
          
          -- Average half of width & height for radius...
          ctx\arc(0, 0, (width + height) / 4, 0, Math.PI * 2, true)
        when "ellipse"
          
          -- Use four bezier curves and KAPPA value to aproximate ellipse
          mx    = width / 2
          my    = height / 2
          kappa = Numerical.KAPPA

          cx = mx * kappa
          cy = my * kappa

          ctx\moveTo -mx, 0
          ctx\bezierCurveTo -mx, -cy, -cx, -my, 0, -my
          ctx\bezierCurveTo cx, -my, mx, -cy, mx, 0
          ctx\bezierCurveTo mx, cy, cx, my, 0, my
          ctx\bezierCurveTo -cx, my, -mx, cy, -mx, 0

    if not param.clip and (fillColor or strokeColor)
      @_setStyles ctx
      ctx\fill()   if fillColor
      ctx\stroke() if strokeColor


  _getBounds: (getter, matrix) =>
    rect = Rectangle(@_size)\setCenter(0, 0)
    rect = rect\expand(@getStrokeWidth()) if getter isnt "getBounds" and @hasStroke()
    (if matrix then matrix\_transformBounds(rect) else rect)

  _contains: 
	  _contains = (point) ->
	    switch @_type
	      when "rect"
	        _contains.base(point)
	      when "circle", "ellipse"
	        point\divide(@_size).getLength() <= 0.5

  _hitTest: 
	  _hitTest = (point, options) ->
	    if @hasStroke()
	      type = @_type
	      strokeWidth = @getStrokeWidth()
	      switch type
	        when "rect"
	          rect  = Rectangle(@_size)\setCenter(0, 0)
	          outer = rect\expand(strokeWidth)
	          inner = rect\expand(-strokeWidth)
	          return new HitResult("stroke", this) if outer\_containsPoint(point) and not inner\_containsPoint(point)
	        when "circle", "ellipse"
	          size   = @_size
	          width  = size.width
	          height = size.height
	          radius = nil

	          if type is "ellipse"
	            
	            -- Calculate ellipse radius at angle
	            angle  = point\getAngleInRadians()
	            x      = width  * math.sin(angle)
	            y      = height * math.cos(angle)
	            radius = width  * height / (2 * Math.sqrt(x * x + y * y))
	          else
	            -- Average half of width & height for radius...
	            radius = (width + height) / 4

	          return  HitResult("stroke", @)  if 2 * math.abs(point\getLength() - radius) <= strokeWidth
	    
    _hitTest.base(arg)

		@createShape: (shapeType, point, size, args) =>
      shape = Shape(shapeType, point, size)
      named = Base\getNamed(args)
      shape\_set named if named
      shape

    Circle: -> 
      center = Point\readNamed(arg "center")
      radius = Base\readNamed(arg, "radius")
      @createShape "circle", center, Size(radius * 2), arg

    Rectangle: -> 
      rect = Rectangl\readNamed(arg, "rectangle")
      @createShape "rect", rect\getCenter(true), rect\getSize(true), arg

    Ellipse: ->
      rect = Rectangle\readNamed(arg, "rectangle")
      @createShape "ellipse", rect\getCenter(true), rect\getSize(true), arg
