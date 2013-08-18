class Curve extends Base
	new: (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) ->
    count = arg.length

    if count is 0
      @_segment1 = Segment()
      @_segment2 = Segment()
    else if count is 1
      -- Note: This copies src existing segments through bean getters
      @_segment1 = Segment(arg0.segment1)
      @_segment2 = Segment(arg0.segment2)
    else if count is 2
      @_segment1 = Segment(arg0)
      @_segment2 = Segment(arg1)
    else
      point1  = nil
      handle1 = nil
      handle2 = nil
      point2  = nil

      if count is 4
        point1  = arg0
        handle1 = arg1
        handle2 = arg2
        point2  = arg3
      else if count is 8
        -- Convert getValue() array back to points and handles so we
        -- can create segments for those.
        point1  = [arg0, arg1]
        point2  = [arg6, arg7]
        handle1 = [arg2 - arg0, arg3 - arg1]
        handle2 = [arg4 - arg6, arg5 - arg7]

      @_segment1 = Segment(point1, nil, handle1)
      @_segment2 = Segment(point2, handle2, nil)

  _changed: =>
    -- Clear cached values.
    delete @_length
    delete @_bounds

  getPoint1: =>
    @_segment1._point

  setPoint1: (point) =>
    point = Point\read(arg)
    @_segment1._point\set point.x, point.y

  getPoint2: =>
    @_segment2._point

  setPoint2: (point) =>
    point = Point\read(arg)
    @_segment2._point\set point.x, point.y

  getHandle1: =>
    @_segment1._handleOut

  setHandle1: (point) =>
    point = Point\read(arg)
    @_segment1\_handleOut.set point.x, point.y

  getHandle2: =>
    @_segment2._handleIn

  setHandle2: (point) =>
    point = Point\read(arg)
    @_segment2._handleIn\set point.x, point.y

  getSegment1: =>
    @_segment1

  getSegment2: =>
    @_segment2

  getPath: =>
    @_path

  getIndex: =>
    @_segment1._index
 
  getNext: =>
    curves = @_path and @_path._curves
    curves and (curves[@_segment1._index + 1] or @_path._closed and curves[0]) or nil

  getPrevious: =>
    curves = @_path and @_path._curves
    curves and (curves[@_segment1._index - 1] or @_path._closed and curves[curves.length - 1]) or null
  
  isSelected: =>
    @getHandle1()\isSelected() and @getHandle2()\isSelected()

  setSelected: (selected) =>
    @getHandle1()\setSelected selected
    @getHandle2()\setSelected selected

  getValues: =>
    Curve\getValues @_segment1, @_segment2

  getPoints: =>
    
    -- Convert to array of absolute points
    coords = @getValues()
    points = []
    i = 0

    while i < 8
      points\push Point(coords[i], coords[i + 1])
      i += 2

    points

  getLength: -> 
    src = arg_[0]
    to  = arg_[1]
    fullLength = arg_.length is 0 or src is 0 and to is 1

    return @_length if fullLength and @_length?
    length = Curve\getLength(@getValues(), src, to)

    @_length = length if fullLength
    length

  getArea: =>
    Curve\getArea @getValues()

  getPart: (src, to) =>
    Curve(Curve\getPart(@getValues(), src, to))

  isLinear: =>
    @_segment1._handleOut\isZero() and @_segment2._handleIn\isZero()

  getIntersections: (curve) =>
    Curve\getIntersections @getValues(), curve\getValues(), this, curve, []

  reverse: =>
    Curve(@_segment2\reverse(), @_segment1\reverse())

  divide: (parameter) ->
    res = null
    
    -- Accept CurveLocation objects, and objects that act like them:
    parameter = parameter.parameter if parameter and parameter.curve is this
    if parameter > 0 and parameter < 1
      parts    = Curve\subdivide(@getValues(), parameter)
      isLinear = @isLinear()
      left     = parts[0]
      right    = parts[1]
      
      -- Write back the results:
      if !isLinear
        @_segment1._handleOut\set left[2] - left[0], left[3] - left[1]
        
        -- segment2 is the end segment. By inserting newSegment
        -- between segment1 and 2, 2 becomes the end segment.
        -- Convert absolute -> relative
        @_segment2._handleIn\set right[4] - right[6], right[5] - right[7]
      
      -- Create the new segment, convert absolute -> relative:
      x = left[6]
      y = left[7]

      segment = Segment(Point(x, y), not isLinear and Point(left[4] - x, left[5] - y), not isLinear and Point(right[2] - x, right[3] - y))
      
      -- Insert it in the segments list, if needed:
      if @_path
        -- Insert at the end if this curve is a closing curve of a
        -- closed path, since otherwise it would be inserted at 0.
        if @_segment1._index > 0 and @_segment2._index is 0
          @_path\add segment
        else
          @_path\insert @_segment2._index, segment
        
        -- The way Path--_add handles curves, this curve will always
        -- become the owner of the newly inserted segment.
        -- TODO: I expect this.getNext() to produce the correct result,
        -- but since we're inserting differently in _add (something
        -- linked with CurveLocation--divide()), this is not the case...
        res = @ -- this.getNext();
      else
        
        -- otherwise create it src the result of split
        eend       = @_segment2
        @_segment2 = segment
        res        = Curve(segment, eend)

    res

  split: (parameter) =>
    (if @_path then @_path\split(@_segment1._index, parameter) else nil)

  clone: =>
    Curve(@_segment1, @_segment2)

  @create: (path, segment1, segment2) =>
    curve           = Base\create(Curve)
    curve._path     = path
    curve._segment1 = segment1
    curve._segment2 = segment2
    curve

  @getValues: (segment1, segment2) =>
    p1 = segment1._point
    h1 = segment1._handleOut
    h2 = segment2._handleIn
    p2 = segment2._point
    [p1._x, p1._y, p1._x + h1._x, p1._y + h1._y, p2._x + h2._x, p2._y + h2._y, p2._x, p2._y]

  @evaluate: (v, offset, isParameter, ttype) =>
    t   = (if isParameter then offset else Curve\getParameterAt(v, offset, 0))
    p1x = v[0]
    p1y = v[1]
    c1x = v[2]
    c1y = v[3]
    c2x = v[4]
    c2y = v[5]
    p2x = v[6]
    p2y = v[7]
    x   = nil
    y   = nil
    
    -- Handle special case at beginning / end of curve
    if ttype is 0 and (t is 0 or t is 1)
      x = (if t is 0 then p1x else p2x)
      y = (if t is 0 then p1y else p2y)
    else
      -- Calculate the polynomial coefficients.
      cx  = 3 * (c1x - p1x)
      bx  = 3 * (c2x - c1x) - cx
      ax  = p2x - p1x - cx - bx
      cy  = 3 * (c1y - p1y)
      by_ = 3 * (c2y - c1y) - cy
      ay  = p2y - p1y - cy - by_
      switch ttype
        when 0 -- point
          -- Calculate the curve point at parameter value t
          x = ((ax * t + bx) * t + cx) * t + p1x
          y = ((ay * t + by_) * t + cy) * t + p1y
        -- tangent, 1st derivative
        when 1, 2 -- normal, 1st derivative
          -- Prevent tangents and normals of length 0:
          -- http://stackoverflow.com/questions/10506868/
          tMin = Numerical.TOLERANCE ----=
          if t < tMin and c1x is p1x and c1y is p1y or t > 1 - tMin and c2x is p2x and c2y is p2y
            x = c2x - c1x
            y = c2y - c1y
          else  
            -- Simply use the derivation of the bezier function for both
            -- the x and y coordinates:
            x = (3 * ax * t + 2 * bx) * t + cx
            y = (3 * ay * t + 2 * by_) * t + cy
        when 3 -- curvature, 2nd derivative
          x = 6 * ax * t + 2 * bx
          y = 6 * ay * t + 2 * by_
    
    -- The normal is simply the rotated tangent:
    (if type is 2 then Point(y, -x) else Point(x, y))

  @subdivide: (v, t) =>
    p1x = v[0]
    p1y = v[1]
    c1x = v[2]
    c1y = v[3]
    c2x = v[4]
    c2y = v[5]
    p2x = v[6]
    p2y = v[7]
    t   = 0.5  if t is nil
    
    -- Triangle computation, with loops unrolled.
    u = 1 - t
    
    -- Interpolate src 4 to 3 points
    p3x = u * p1x + t * c1x
    p3y = u * p1y + t * c1y
    p4x = u * c1x + t * c2x
    p4y = u * c1y + t * c2y
    p5x = u * c2x + t * p2x
    p5y = u * c2y + t * p2y
    
    -- Interpolate src 3 to 2 points
    p6x = u * p3x + t * p4x
    p6y = u * p3y + t * p4y
    p7x = u * p4x + t * p5x
    p7y = u * p4y + t * p5y
    
    -- Interpolate src 2 points to 1 point
    p8x = u * p6x + t * p7x
    p8y = u * p6y + t * p7y
    
    -- We now have all the values we need to build the subcurves:
    -- left
    [[p1x, p1y, p3x, p3y, p6x, p6y, p8x, p8y], [p8x, p8y, p7x, p7y, p5x, p5y, p2x, p2y]] -- right

      solveCubic: (v, coord, val, roots) ->
      p1 = v[coord]
      c1 = v[coord + 2]
      c2 = v[coord + 4]
      p2 = v[coord + 6]
      c = 3 * (c1 - p1)
      b = 3 * (c2 - c1) - c
      a = p2 - p1 - c - b
      Numerical.solveCubic a, b, c, p1 - val, roots

  @getParameterOf: (v, x, y) =>
    
    -- Handle beginnings and end seperately, as they are not detected
    -- sometimes.
    return 0 if math.abs(v[0] - x) < Numerical.TOLERANCE and math.abs(v[1] - y) < Numerical.TOLERANCE
    return 1 if math.abs(v[6] - x) < Numerical.TOLERANCE and math.abs(v[7] - y) < Numerical.TOLERANCE
  
    txs = []
    tys = []
    sx  = Curve\solveCubic(v, 0, x, txs)
    sy  = Curve\solveCubic(v, 1, y, tys)
    tx  = nil
    ty  = nil
    
    -- sx, sy == -1 means infinite solutions:
    -- Loop through all solutions for x and match with solutions for y,
    -- to see if we either have a matching pair, or infinite solutions
    -- for one or the other.
    cx = 0

    while sx is -1 or cx < sx
      if sx is -1 or (tx = txs[cx++]) >= 0 and tx <= 1
        cy = 0

        while sy is -1 or cy < sy
          if sy is -1 or (ty = tys[cy++]) >= 0 and ty <= 1
            -- Handle infinite solutions by assigning root of
            -- the other polynomial
            if sx is -1
              tx = ty
            else 
            	ty = tx  if sy is -1
            
            -- Use average if we're within tolerance
            return (tx + ty) * 0.5 if math.abs(tx - ty) < Numerical.TOLERANCE
        
        -- Avoid endless loops here: If sx is infinite and there was
        -- no fitting ty, there's no solution for this bezier
        break if sx is -1

    nil
  
  -- TODO: Find better name
  @getPart: (v, src, to) =>
    v = Curve\subdivide(v, src)[1] if src > 0

    -- Interpolate the  parameter at 'to' in the new curve and
    -- cut there.
    v = Curve\subdivide(v, (to - src) / (1 - src))[0]  if to < 1 -- [0] left
    v

  @isLinear: (v) =>
    v[0] is v[2] and v[1] is v[3] and v[4] is v[6] and v[5] is v[7]

  @isFlatEnough: (v, tolerance) =>
    
    -- Thanks to Kaspar Fischer and Roger Willcocks for the following:
    -- http://hcklbrrfnn.files.wordpress.com/2012/08/bez.pdf
    p1x = v[0]
    p1y = v[1]
    c1x = v[2]
    c1y = v[3]
    c2x = v[4]
    c2y = v[5]
    p2x = v[6]
    p2y = v[7]

    ux = 3 * c1x - 2 * p1x - p2x
    uy = 3 * c1y - 2 * p1y - p2y
    vx = 3 * c2x - 2 * p2x - p1x
    vy = 3 * c2y - 2 * p2y - p1y
    math.max(ux * ux, vx * vx) + math.max(uy * uy, vy * vy) < 10 * tolerance * tolerance

  @getArea: (v) =>
    p1x = v[0]
    p1y = v[1]
    c1x = v[2]
    c1y = v[3]
    c2x = v[4]
    c2y = v[5]
    p2x = v[6]
    p2y = v[7]
    
    -- http://objectmix.com/graphics/133553-area-closed-bezier-curve.html
    (3.0 * c1y * p1x - 1.5 * c1y * c2x - 1.5 * c1y * p2x - 3.0 * p1y * c1x - 1.5 * p1y * c2x - 0.5 * p1y * p2x + 1.5 * c2y * p1x + 1.5 * c2y * c1x - 3.0 * c2y * p2x + 0.5 * p2y * p1x + 1.5 * p2y * c1x + 3.0 * p2y * c2x) / 10

  @getBounds: (v) =>
    min   = v\slice(0, 2) -- Start with values of point1
    max   = min\slice() -- clone
    roots = [0, 0]

    i = 0
    while i < 2
      Curve\_addBounds v[i], v[i + 2], v[i + 4], v[i + 6], i, 0, min, max, roots
      i++

    Rectangle(min[0], min[1], max[0] - min[0], max[1] - min[1])

  @_getCrossings: (v, prev, x, y, roots) =>    
	  -- Implementation of the crossing number algorithm:
	  -- http://en.wikipedia.org/wiki/Point_in_polygon
	  -- Solve the y-axis cubic polynomial for y and count all solutions
	  -- to the right of x as crossings.
	  
	  -- Checks the y-slope between the current curve and the previous for a
	  -- change of orientation, when a solution is found at t == 0
	  changesOrientation = (tangent) =>
	    Curve\evaluate(prev, 1, true, 1).y * tangent.y > 0

	  count     = Curve\solveCubic(v, 1, y, roots)
	  crossings = 0
	  tolerance = Numerical.TOLERANCE
	  abs       = math.abs

	  if count is -1
	    -- Infinite solutions, so we have a horizontal curve.
	    -- Find parameter through getParameterOf()
	    roots[0] = Curve\getParameterOf(v, x, y)
	    count    = (if roots[0] isnt nil then 1 else 0)

	  i = 0
	  while i < count
	    t = roots[i]

	    if t > -tolerance and t < 1 - tolerance
	      pt = Curve\evaluate(v, t, true, 0)
	      if x < pt.x + tolerance
	        -- Pass 1 for Curve.evaluate() type to calculate tangent
	        tan = Curve\evaluate(v, t, true, 1)
	        
	        -- Handle all kind of edge cases when points are on
	        -- contours or rays are touching countours, to termine
	        -- wether the crossing counts or not.
	        -- See if the actual point is on the countour:
	        if abs(pt.x - x) < tolerance
	          -- Do not count the crossing if it is on the left hand
	          -- side of the shape (tangent pointing upwards), since
	          -- the ray will go out the other end, count as crossing
	          -- there, and the point is on the contour, so to be
	          -- considered inside.
	          angle = tan\getAngle()
	          
	          -- Handle special case where point is on a corner,
	          -- in which case this crossing is skipped if both
	          -- tangents have the same orientation.
	          continue if angle > -180 and angle < 0 and (t > tolerance or changesOrientation(tan))
	        else 
	          -- Skip touching stationary points:
	          
	          -- Check derivate for stationary points. If root is
	          -- close to 0 and not changing vertical orientation
	          -- from the previous curve, do not count this root,
	          -- as it's touching a corner.
	          continue  if abs(tan.y) < tolerance or t < tolerance and not changesOrientation(tan)
	        crossings++
	    i++

	  crossings

    @_addBounds: (v0, v1, v2, v3, coord, padding, min, max, roots) =>
      -- Code ported and further optimised from:
      -- http://blog.hackers-cafe.net/2009/06/how-to-calculate-bezier-curves-bounding.html
      add = (value, padding) ->
        left  = value - padding
        right = value + padding

        min[coord] = left  if left < min[coord]
        max[coord] = right if right > max[coord]
      
      -- Calculate derivative of our bezier polynomial, divided by 3.
      -- Doing so allows for simpler calculations of a, b, c and leads to the
      -- same quadratic roots.
      a     = 3 * (v1 - v2) - v0 + v3
      b     = 2 * (v0 + v2) - 4 * v1
      c     = v1 - v0
      count = Numerical\solveQuadratic(a, b, c, roots)
      
      -- Add some tolerance for good roots, as t = 0 / 1 are added
      -- seperately anyhow, and we don't want joins to be added with
      -- radiuses in getStrokeBounds()
      tMin = Numerical.TOLERANCE ----=
      tMax = 1 - tMin
      
      -- Only add strokeWidth to bounds for points which lie  within 0 < t < 1
      -- The corner cases for cap and join are handled in getStrokeBounds()
      add v3, 0
      i = 0

      while i < count
        t = roots[i]
        u = 1 - t
        
        -- Test for good roots and only add to bounds if good. 
        -- Calculate bezier polynomial at t.
        add u * u * u * v0 + 3 * u * u * t * v1 + 3 * u * t * t * v2 + t * t * t * v3, padding  if tMin < t and t < tMax
        i++

  @getParameterAt: (offset, start) =>
    Curve\getParameterAt @getValues(), offset, (if start isnt nil then start else (if offset < 0 then 1 else 0))

  @getParameterOf: (point) =>
    point = Point\read(arg)
    Curve\getParameterOf @getValues(), point.x, point.y

  @getLocationAt: (offset, isParameter) =>
    offset = @getParameterAt(offset) if !isParameter
    CurveLocation(@, offset)

  getNearestLocation: (point) =>
    -- Accommodate imprecision in comparison
    refine = (t) ->
      if t >= 0 and t <= 1
        dist = point\getDistance(Curve\evaluate(values, t, true, 0), true)
        if dist < minDist
          minDist = dist
          minT    = t
          true

    point     = Point\read(arg)
    values    = @getValues()
    count     = 100
    tolerance = Numerical.TOLERANCE
    minDist   = Infinity
    minT      = 0
    max       = 1 + tolerance
    
    i = 0
    while i <= count
      refine i / count
      i++
    
    -- Now iteratively refine solution until we reach desired precision.
    step  = 1 / (count * 2)
    step /= 2  if not refine(minT - step) and not refine(minT + step) while step > tolerance
    pt = Curve\evaluate(values, minT, true, 0)
    CurveLocation(@, minT, pt, null, null, null, point\getDistance(pt))

  @getNearestPoint: (point) =>
    @getNearestLocation(arg)\getPoint()

 @getLengthIntegrand: (v) =>
    -- Calculate the coefficients of a Bezier derivative.
    p1x = v[0]
    p1y = v[1]
    c1x = v[2]
    c1y = v[3]
    c2x = v[4]
    c2y = v[5]
    p2x = v[6]
    p2y = v[7]

    ax  = 9 * (c1x - c2x) + 3 * (p2x - p1x)
    bx  = 6 * (p1x + c2x) - 12 * c1x
    cx  = 3 * (c1x - p1x)
    ay  = 9 * (c1y - c2y) + 3 * (p2y - p1y)
    by_ = 6 * (p1y + c2y) - 12 * c1y
    cy  = 3 * (c1y - p1y)

    (t) -> 
      -- Calculate quadratic equations of derivatives for x and y
      dx = (ax * t + bx) * t + cx
      dy = (ay * t + by_) * t + cy
      math.sqrt dx * dx + dy * dy
  
	@getIterations: (a, b) =>
	  -- Guess required precision based and size of range...
	  -- TODO: There should be much better educated guesses for
	  -- this. Also, what does this depend on? Required precision?
	  math.max 2, math.min(16, math.ceil(math.abs(b - a) * 32))

  @getLength: (v, a, b) =>
    a = 0  if a is nil
    b = 1  if b is nil
    
    -- See if the curve is linear by checking p1 == c1 and p2 == c2
    if v[0] is v[2] and v[1] is v[3] and v[6] is v[4] and v[7] is v[5]
      -- Straight line
      dx = v[6] - v[0]
      dy = v[7] - v[1]
      return (b - a) * math.sqrt(dx * dx + dy * dy)

    ds = getLengthIntegrand(v)
    Numerical\integrate ds, a, b, getIterations(a, b)

  @getParameterAt: (v, offset, start) => 
    -- See if we're going forward or backward, and handle cases
    -- differently
    
    -- Use integrand to calculate both range length and part
    -- lengths in f(t) below.
    
    -- Get length of total range
    
    -- Use offset / rangeLength for an initial guess for t, to
    -- bring us closer:
    
    -- Iteratively calculate curve range lengths, and add them up,
    -- using integration precision depending on the size of the
    -- range. This is much faster and also more precise than not
    -- modifing start and calculating total length each time.
    f = (t) ->
      count   = @getIterations(start, t)
      length += (if start < t then Numerical\integrate(ds, start, t, count) else -Numerical.integrate(ds, t, start, count))
      start   = t
      length  - offset

    return start if offset is 0

    forward = offset > 0
    a = (if forward then start else 0)
    b = (if forward then 1 else start)

    offset = math.abs(offset)
    ds     = getLengthIntegrand(
    	v)
    rangeLength = Numerical\integrate(ds, a, b, @getIterations(a, b))

    return (if forward then b else a)  if offset >= rangeLength

    guess = offset / rangeLength
    length = 0

    -- Initial guess for x
    Numerical\findRoot f, ds, (if forward then a + guess else b - guess), a, b, 16, Numerical.TOLERANCE
  
  @addLocation: (locations, curve1, t1, point1, curve2, t2, point2) =>
    -- Avoid duplicates when hitting segments (closed paths too)
    first = locations[0]
    last  = locations[locations.length - 1]
    locations\push CurveLocation(curve1, t1, point1, curve2, t2, point2) if (not first or not point1.equals(first._point)) and (not last or not point1\equals(last._point))

  @addCurveIntersections: (v1, v2, curve1, curve2, locations, range1, range2, recursion) =>
    if options.fatline
      -- NOTE: range1 and range1 are only used for recusion
      recursion = (recursion or 0) + 1
      
      -- Avoid endless recursion.
      -- Perhaps we should fall back to a more expensive method after this,
      -- but so far endless recursion happens only when there is no real
      -- intersection and the infinite fatline continue to intersect with the
      -- other curve outside its bounds!
      return if recursion > 20
      
      -- Set up the parameter ranges.
      range1 = range1 or [0, 1]
      range2 = range2 or [0, 1]
      
      -- Get the clipped parts from the original curve, to avoid cumulative
      -- errors
      part1     = Curve\getPart(v1, range1[0], range1[1])
      part2     = Curve\getPart(v2, range2[0], range2[1])
      iteration = 0
      
      -- markCurve(part1, '--f0f', true);
      -- markCurve(part2, '--0ff', false);
      -- Loop until both parameter range converge. We have to handle the
      -- degenerate case seperately, where fat-line clipping can become
      -- numerically unstable when one of the curves has converged to a point
      -- and the other hasn't.
      while iteration++ < 20
        -- First we clip v2 with v1's fat-line
        range       = nil
        intersects1 = @clipFatLine(part1, part2, range = range2.slice())
        intersects2 = 0
        
        -- Stop if there are no possible intersections
        break if intersects1 is 0
        if intersects1 > 0
          -- Get the clipped parts from the original v2, to avoid
          -- cumulative errors
          range2 = range
          part2  = Curve\getPart(v2, range2[0], range2[1])
          
          -- markCurve(part2, '--0ff', false);
          -- Next we clip v1 with nuv2's fat-line
          intersects2 = @clipFatLine(part2, part1, range = range1.slice())
          
          -- Stop if there are no possible intersections
          break if intersects2 is 0
          if intersects1 > 0
            -- Get the clipped parts from the original v2, to avoid
            -- cumulative errors
            range1 = range
            part1  = Curve\getPart(v1, range1[0], range1[1])
        
        -- markCurve(part1, '--f0f', true);
        
        -- Get the clipped parts from the original v1
        -- Check if there could be multiple intersections
        if intersects1 < 0 or intersects2 < 0
          
          -- Subdivide the curve which has converged the least from the
          -- original range [0,1], which would be the curve with the
          -- largest parameter range after clipping
          if range1[1] - range1[0] > range2[1] - range2[0]
            -- subdivide v1 and recurse
            t = (range1[0] + range1[1]) / 2
            @addCurveIntersections v1, v2, curve1, curve2, locations, [range1[0], t], range2, recursion
            @addCurveIntersections v1, v2, curve1, curve2, locations, [t, range1[1]], range2, recursion
            break
          else
            -- subdivide v2 and recurse
            t = (range2[0] + range2[1]) / 2
            @addCurveIntersections v1, v2, curve1, curve2, locations, range1, [range2[0], t], recursion
            @addCurveIntersections v1, v2, curve1, curve2, locations, range1, [t, range2[1]], recursion
            break
        
        -- We need to bailout of clipping and try a numerically stable
        -- method if both of the parameter ranges have converged reasonably
        -- well (according to Numerical.TOLERANCE).
        ----=
        if math.abs(range1[1] - range1[0]) <= Numerical.TOLERANCE and math.abs(range2[1] - range2[0]) <= Numerical.TOLERANCE
          t1 = (range1[0] + range1[1]) / 2
          t2 = (range2[0] + range2[1]) / 2
          @addLocation locations, curve1, t1, Curve\evaluate(v1, t1, true, 0), curve2, t2, Curve\evaluate(v2, t2, true, 0)
          break
    else ----
      bounds1 = Curve\getBounds(v1)
      bounds2 = Curve\getBounds(v2)

      if bounds1\touches(bounds2)
        -- See if both curves are flat enough to be treated as lines, either
        -- because they have no control points at all, or are "flat enough"
        -- If the curve was flat in a previous iteration, we don't need to
        -- recalculate since it does not need further subdivision then.
        ----=
        if (Curve\isLinear(v1) or Curve\isFlatEnough(v1, Numerical.TOLERANCE)) and (Curve\isLinear(v2) or Curve\isFlatEnough(v2, Numerical.TOLERANCE))
          
          -- See if the parametric equations of the lines interesct.
          @addLineIntersection v1, v2, curve1, curve2, locations
        else 
          -- Subdivide both curves, and see if they intersect.
          -- If one of the curves is flat already, no further subdivion
          -- is required.
          v1s = Curve\subdivide(v1)
          v2s = Curve\subdivide(v2)

          i = 0
          while i < 2
            j = 0

            while j < 2
              Curve\getIntersections v1s[i], v2s[j], curve1, curve2, locations
              j++
            i++

      locations

  @addCurveLineIntersections = (v1, v2, curve1, curve2, locations) =>
    flip = Curve\isLinear(v1)
    vc   = (if flip then v2 else v1)
    vl   = (if flip then v1 else v2)
    l1x  = vl[0]
    l1y  = vl[1]
    l2x  = vl[6]
    l2y  = vl[7]
    
    -- Rotate both curve and line around l1 so that line is on x axis
    lvx = l2x - l1x
    lvy = l2y - l1y
    
    -- Angle with x axis (1, 0)
    angle = math.atan2(-lvy, lvx)
    sin   = math.sin(angle)
    cos   = math.cos(angle)
    
    -- (rl1x, rl1y) = (0, 0)
    rl2x = lvx * cos - lvy * sin
    rl2y = lvy * cos + lvx * sin
    vcr  = []

    i = 0
    while i < 8
      x = vc[i] - l1x
      y = vc[i + 1] - l1y
      vcr\push x * cos - y * sin, y * cos + x * sin
      i += 2

    roots = []
    count = Curve\solveCubic(vcr, 1, 0, roots)
    
    -- NOTE: count could be -1 for inifnite solutions, but that should only
    -- happen with lines, in which case we should not be here.
    i = 0
    while i < count
      t = roots[i]
      if t >= 0 and t <= 1
        point = Curve\evaluate(vcr, t, true, 0)
        
        -- We do have a point on the infinite line. Check if it falls on
        -- the line *segment*.
        
        -- The actual intersection point
        @addLocation locations, (if flip then curve2 else curve1), t, Curve.evaluate(vc, t, true, 0), (if flip then curve1 else curve2) if point.x >= 0 and point.x <= rl2x
      i++

  @addLineIntersection: (v1, v2, curve1, curve2, locations) =>
    point = Line\intersect(v1[0], v1[1], v1[6], v1[7], v2[0], v2[1], v2[6], v2[7])
    
    -- Passing null for parameter leads to lazy determination of parameter
    -- values in CurveLocation--getParameter() only once they are requested.
    @addLocation locations, curve1, null, point, curve2  if point

  if options.fatline
    clipFatLine: (v1, v2, range2) =>
      p0x = v1[0]
      p0y = v1[1]
      p1x = v1[2]
      p1y = v1[3]
      p2x = v1[4]
      p2y = v1[5]
      p3x = v1[6]
      p3y = v1[7]
      q0x = v2[0]
      q0y = v2[1]
      q1x = v2[2]
      q1y = v2[3]
      q2x = v2[4]
      q2y = v2[5]
      q3x = v2[6]
      q3y = v2[7]

      getSignedDistance = Line\getSignedDistance
      d1 = getSignedDistance(p0x, p0y, p3x, p3y, p1x, p1y) or 0
      d2 = getSignedDistance(p0x, p0y, p3x, p3y, p2x, p2y) or 0

      factor = (if d1 * d2 > 0 then 3 / 4 else 4 / 9)
      dmin   = factor * Math.min(0, d1, d2)
      dmax   = factor * Math.max(0, d1, d2)
      dq0    = @getSignedDistance(p0x, p0y, p3x, p3y, q0x, q0y)
      dq1    = @getSignedDistance(p0x, p0y, p3x, p3y, q1x, q1y)
      dq2    = @getSignedDistance(p0x, p0y, p3x, p3y, q2x, q2y)
      dq3    = @getSignedDistance(p0x, p0y, p3x, p3y, q3x, q3y)

      return 0 if dmin > math.max(dq0, dq1, dq2, dq3) or dmax < math.min(dq0, dq1, dq2, dq3)

      hull = @getConvexHull(dq0, dq1, dq2, dq3)
      swap = nil

      if dq3 < dq0
        swap = dmin
        dmin = dmax
        dmax = swap

      tmaxdmin = -Infinity
      tmin     = Infinity
      tmax     = -Infinity

      i = 0
      l = hull.length

      while i < l
        p1 = hull[i]
        p2 = hull[(i + 1) % l]

        if p2[1] < p1[1]
          swap = p2
          p2   = p1
          p1   = swap

        x1  = p1[0]
        y1  = p1[1]
        x2  = p2[0]
        y2  = p2[1]
        inv = (y2 - y1) / (x2 - x1)

        if dmin >= y1 and dmin <= y2
          ixdx     = x1 + (dmin - y1) / inv
          tmin     = ixdx if ixdx < tmin
          tmaxdmin = ixdx if ixdx > tmaxdmin

        if dmax >= y1 and dmax <= y2
          ixdx = x1 + (dmax - y1) / inv
          tmax = ixdx  if ixdx > tmax
          tmin = 0 if ixdx < tmin

        i++

      if tmin isnt Infinity and tmax isnt -Infinity
        min  = math.min(dmin, dmax)
        max  = math.max(dmin, dmax)
        tmax = 1 if dq3 > min and dq3 < max
        tmin = 0 if dq0 > min and dq0 < max
        tmax = 1 if tmaxdmin > tmax

        v2tmin    = range2[0]
        tdiff     = range2[1] - v2tmin
        range2[0] = v2tmin + tmin * tdiff
        range2[1] = v2tmin + tmax * tdiff

        return 1 if (tdiff - (range2[1] - range2[0])) / tdiff >= 0.2

      return -1 if Curve\getBounds(v1)\touches(Curve\getBounds(v2))
      0

    getConvexHull = (dq0, dq1, dq2, dq3) ->
      p0 = [0, dq0]
      p1 = [1 / 3, dq1]
      p2 = [2 / 3, dq2]
      p3 = [1, dq3]

      getSignedDistance = Line\getSignedDistance
      dist1 = getSignedDistance(0, dq0, 1, dq3, 1 / 3, dq1)
      dist2 = getSignedDistance(0, dq0, 1, dq3, 2 / 3, dq2)

      return [p0, p1, p3, p2]  if dist1 * dist2 < 0

      pmax  = nil
      cross = nil

      if math.abs(dist1) > math.abs(dist2)
        pmax  = p1
        cross = (dq3 - dq2 - (dq3 - dq0) / 3) * (2 * (dq3 - dq2) - dq3 + dq1) / 3
      else
        pmax  = p2
        cross = (dq1 - dq0 + (dq0 - dq3) / 3) * (-2 * (dq0 - dq1) + dq0 - dq2) / 3
     
      (if cross < 0 then [p0, pmax, p3] else [p0, p1, p2, p3])

  @getIntersections: (v1, v2, curve1, curve2, locations) =>
    linear1 = Curve\isLinear(v1)
    linear2 = Curve\isLinear(v2)
    
    -- Determine the correct intersection method based on values of
    -- linear1 & 2:
    ((if linear1 and linear2 then @addLineIntersection! else (if linear1 or linear2 then @addCurveLineIntersections() else @addCurveIntersections()))) v1, v2, curve1, curve2, locations
    locations
