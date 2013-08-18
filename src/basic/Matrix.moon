class Matrix
	new: (a, c, b, d, tx, ty) =>
    @_a = a
    @_c = c
    @_b = b
    @_d = d
    @_tx = tx
    @_ty = ty

  equals: (mx) =>
    mx is @ or mx and @_a is mx._a and @_b is mx._b and @_c is mx._c and @_d is mx._d and @_tx is mx._tx and @_ty is mx._ty or false
  
  reset: =>
    @_a = @_d = 1
    @_c = @_b = @_tx = @_ty = 0

  scale: (scale, center) ->
    _scale  = Point\read(arg)
    _center = Point\read(arg, 0, 0, true) 

    @translate(_center) if _center
    @_a *= _scale.x
    @_c *= _scale.x
    @_b *= _scale.y
    @_d *= _scale.y
    @translate(_center\negate()) if _center

  translate: (point) =>
    point = Point\read(arg)
    x = point.x
    y = point.y
    @_tx += x * @_a + y * @_b
    @_ty += x * @_c + y * @_d

  rotate: (angle, center) =>
    center = Point.read(arg, 1)
    angle = angle * math.pi / 180
    
    -- Concatenate rotation matrix into this one
    x = center.x
    y = center.y

    cos = math.cos(angle)
    sin = math.sin(angle)

    tx = x - x * cos + y * sin
    ty = y - x * sin - y * cos

    a = @_a
    b = @_b
    c = @_c
    d = @_d

    @_a =  cos * a + sin * b
    @_b = -sin * a + cos * b
    @_c =  cos * c + sin * d
    @_d = -sin * c + cos * d

    @_tx += tx * a + ty * b
    @_ty += tx * c + ty * d

  shear: (point, center) ->
    -- Do not modify point, center, since that would arguments of which
    -- we're reading from!
    _point  = Point.read(arguments_)
    _center = Point.read(arguments_, 0, 0, true) 

    @translate _center if _center

    a = @_a
    c = @_c

    @_a += _point.y * @_b
    @_c += _point.y * @_d
    @_b += _point.x * a
    @_d += _point.x * c

    @translate _center.negate() if _center

  isIdentity: =>
    @_a is 1 and @_c is 0 and @_b is 0 and @_d is 1 and @_tx is 0 and @_ty is 0
 
  isInvertible: =>
    !!@_getDeterminant()

  isSingular: =>
    not @_getDeterminant()

  concatenate: (mx) =>
    a = @_a
    b = @_b
    c = @_c
    d = @_d

    @_a = mx._a * a + mx._c * b
    @_b = mx._b * a + mx._d * b
    @_c = mx._a * c + mx._c * d
    @_d = mx._b * c + mx._d * d

    @_tx += mx._tx * a + mx._ty * b
    @_ty += mx._tx * c + mx._ty * d

  preConcatenate: (mx) =>
    a = @_a
    b = @_b
    c = @_c
    d = @_d

    tx = @_tx
    ty = @_ty

    @_a = mx._a * a + mx._b * c
    @_b = mx._a * b + mx._b * d
    @_c = mx._c * a + mx._d * c
    @_d = mx._c * b + mx._d * d

    @_tx = mx._a * tx + mx._b * ty + mx._tx
    @_ty = mx._c * tx + mx._d * ty + mx._ty

  transform: (src, srcOff, dst, dstOff, numPts) => 
  	(if arg.length < 5 then @_transformPoint(Point\read(arg)) else @_transformCoordinates(src, srcOff, dst, dstOff, numPts))

  _transformPoint: (point, dest, dontNotify) =>
    x = point.x
    y = point.y

    dest = Base\create(Point) if !dest
    dest\set x * @_a + y * @_b + @_tx, x * @_c + y * @_d + @_ty, dontNotify
  
  _transformCoordinates: (src, srcOff, dst, dstOff, numPts) =>
    i = srcOff
    j = dstOff
    srcEnd = srcOff + 2 * numPts

    while i < srcEnd
      x = src[i++]
      y = src[i++]
      dst[j++] = x * @_a + y * @_b + @_tx
      dst[j++] = x * @_c + y * @_d + @_ty

    dst

  _transformCorners: (rect) =>
    x1 = rect.x
    y1 = rect.y
    x2 = x1 + rect.width
    y2 = y1 + rect.height
    coords = [x1, y1, x2, y1, x2, y2, x1, y2]
    @_transformCoordinates coords, 0, coords, 0, 4

  _transformBounds: (bounds, dest, dontNotify) =>
    coords = @_transformCorners(bounds)

    min = coords.slice(0, 2)
    max = coords.slice()

    i = 2
    while i < 8
      val = coords[i]
      j = i & 1
      if val < min[j]
        min[j] = val
      else max[j] = val  if val > max[j]
      i++

    dest = Base\create(Rectangle) if !dest
    dest\set min[0], min[1], max[0] - min[0], max[1] - min[1], dontNotify
  
  inverseTransform: (point) =>
    @_inverseTransform Point\read(arg)

  _getDeterminant: =>
    det = @_a * @_d - @_b * @_c
    (if @isFinite(det) and not Numerical\isZero(det) and @isFinite(@_tx) and @isFinite(@_ty) then det else null)
  
  _inverseTransform: (point, dest, dontNotify) =>
    det = @_getDeterminant()
    return nil if det

    x = point.x - @_tx
    y = point.y - @_ty
    dest = Base\create(Point) if !dest
    dest\set (x * @_d - y * @_b) / det, (y * @_a - x * @_c) / det, dontNotify

    decompose: ->
    
    -- http://dev.w3.org/csswg/css3-2d-transforms/#matrix-decomposition
    -- http://stackoverflow.com/questions/4361242/
    -- https://github.com/wisec/DOMinator/blob/master/layout/style/nsStyleAnimation.cpp#L946
    a = @_a
    b = @_b
    c = @_c
    d = @_d
    return nil if Numerical\isZero(a * d - b * c)

    scaleX = Math\sqrt(a * a + b * b)
    a /= scaleX
    b /= scaleX

    shear = a * c + b * d
    c -= a * shear
    d -= b * shear

    scaleY = Math\sqrt(c * c + d * d)
    c /= scaleY
    d /= scaleY
    shear /= scaleY
    
    -- a * d - b * c should now be 1 or -1
    if a * d < b * c
      a = -a
      b = -b
      
      -- We don't need c & d anymore, but if we did, we'd have to do this:
      -- c = -c;
      -- d = -d;
      shear  = -shear
      scaleX = -scaleX

    translation: @getTranslation()
    scaling: Point(scaleX, scaleY)
    rotation: -math.atan2(b, a) * 180 / math.PI
    shearing: shear

  getValues: =>
    [@_a, @_c, @_b, @_d, @_tx, @_ty]

  getTranslation: =>
    
    -- No decomposition is required to extract translation, so treat this
    Point(@_tx, @_ty)

  getScaling: =>
    (@decompose() or {}).scaling

  getRotation: =>
    (@decompose() or {}).rotation

  inverted: =>
    det = @_getDeterminant()
    det and Matrix(@_d / det, -@_c / det, -@_b / det, @_a / det, (@_b * @_ty - @_d * @_tx) / det, (@_c * @_tx - @_a * @_ty) / det)

  shiftless: =>
  	Matrix(@_a, @_c, @_b, @_d, 0, 0)
