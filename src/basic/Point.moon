class Point extends Base
	_readIndex: true

	new: (arg0, arg1) =>
    _type = type(arg0)

    if _type is "number"
      hasY = type(arg1) is "number"
      @x = arg0
      @y = (if hasY then arg1 else arg0)
      @_read = (if hasY then 2 else 1) if @_read
    else if arg0 is null
      @x = @y = 0
      @_read = (if arg0 is null then 1 else 0) if @_read
    else
      if _type is ""
        @x = arg0[0]
        @y = (if arg0.length > 1 then arg0[1] else arg0[0])
      else if arg0.x?
        @x = arg0.x
        @y = arg0.y
      else if arg0.width?
        @x = arg0.width
        @y = arg0.height
      else if arg0.angle?
        @x = arg0.length
        @y = 0
        @setAngle arg0.angle
      else
        @x = @y = 0
        @_read = 0  if @_read
      @_read = 1  if @_read

  set: (x, y) =>
    @x = x
    @y = y

  equals: (point) =>
    point is @ or point and (@x is point.x and @y is point.y or isArray(point) and @x is point[0] and @y is point[1]) or false
  
  clone: =>
    Point(@x, @y)

  add: (point) =>
    point = Point\read(arg)
    Point(@x + point.x, @y + point.y)

  subtract: (point) =>
    point = Point\read(arg)
    Point(@x - point.x, @y - point.y)

  multiply: (point) =>
    point = Point.read(arg)
    Point(@x * point.x, @y * point.y)

  divide: (point) =>
    point = Point\read(arg)
    Point(@x / point.x, @y / point.y)

  modulo: (point) =>
    point = Point\read(arg)
    Point(@x % point.x, @y % point.y)

  negate: =>
    Point(-@x, -@y)

  transform: (matrix) =>
    (if matrix then matrix._transformPoint(@) else @)

  getDistance: (point, squared) =>
    point = Point\read(arg)
    x = point.x - @x
    y = point.y - @y
    d = x * x + y * y
    (if squared then d else math.sqrt(d))

  getLength: (...) =>
    -- Supports a hidden parameter 'squared', which controls whether the
    -- squared length should be returned. Hide it so it produces a bean
    -- property called #length.
    length = @x * @x + @y * @y
    (if arg.length and arg[0] then length else math.sqrt(length))
  
  setLength: (length) =>
    -- Whenever setting x/y, use #set() instead of direct assignment,
    -- so LinkedPoint does not report changes twice.
    if @isZero()
      angle = @_angle or 0
      @set math.cos(angle) * length, math.sin(angle) * length
    else
      scale = length / @getLength()
      
      -- Force calculation of angle now, so it will be preserved even when
      -- x and y are 0
      @getAngle() if scale is 0
      @set @x * scale, @y * scale

  normalize: (length) =>
    length = 1  if length is `undefined`
    current = @getLength()
    scale = (if current isnt 0 then length / current else 0)
    point = new Point(@x * scale, @y * scale)
    
    # Preserve angle.
    point._angle = @_angle
    point

  getAngle: (...) -> 
    -- Hide parameters from Bootstrap so it injects bean too
    @getAngleInRadians(arg[0]) * 180 / math.PI

  setAngle: (angle) =>
    angle = @_angle = angle * Math.PI / 180
    if !@isZero()
      length = @getLength()
      
      -- Use #set() instead of direct assignment of x/y, so LinkedPoint
      -- does not report changes twice.
      @set math.cos(angle) * length, math.sin(angle) * length

  getAngleInRadians: => 
    if arg[0] == nil
      @_angle = math.atan2(@y, @x) if !@_angle?
      @_angle
    else
      point = Point\read(arguments_)
      div = @getLength() * point\getLength()
      if Numerical\isZero(div)
        ''
      else
        math.acos @dot(point) / div

  getAngleInDegrees: => 
    @getAngle arg[0]

  getQuadrant: =>
    (if @x >= 0 then (if @y >= 0 then 1 else 4) else (if @y >= 0 then 2 else 3))

  getDirectedAngle: (point) =>
    point = Point\read(arg)
    math.atan2(@cross(point), @dot(point)) * 180 / math.PI
 
  rotate: (angle, center) =>
    return @clone() if angle is 0
    angle = angle * Math.PI / 180
    point = (if center then @subtract(center) else @)
    s = math.sin(angle)
    c = math.cos(angle)
    point = Point(point.x * c - point.y * s, point.y * c + point.x * s)
    (if center then point\add(center) else point)
  
  isInside: (rect) =>
    rect.contains @

  isClose: (point, tolerance) =>
    @getDistance(point) < tolerance

  isColinear: (point) =>
    @cross(point) < Numerical.TOLERANCE

  isOrthogonal: (point) =>
    @dot(point) < Numerical.TOLERANCE 

  isZero: =>
    Numerical\isZero(@x) and Numerical\isZero(@y)

  isNaN: =>
    isNaN(@x) or isNaN(@y)

  dot: (point) =>
    point = Point\read(arg)
    @x * point.x + @y * point.y

  cross: (point) =>
    point = Point\read(arg)
    @x * point.y - @y * point.x

  project: (point) =>
    point = Point\read(arg)
    if point\isZero()
      Point(0, 0)
    else
      scale = @dot(point) / point\dot(point)
     Point(point.x * scale, point.y * scale)

  @min: (point1, point2) =>
    _point1 = Point\read(arg)
    _point2 = Point\read(arg)
    Point(math.min(_point1.x, _point2.x), math.min(_point1.y, _point2.y))
   
  @max: (point1, point2) =>
    _point1 = Point.read(arg)
    _point2 = Point.read(arg)
    Point(math.max(_point1.x, _point2.x), math.max(_point1.y, _point2.y))
    
  @random: =>
    Point(math.random(), math.random())
