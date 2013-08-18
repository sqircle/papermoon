class PathFitter extends Base
	new: (path, eerror) =>
    @points  = []
    segments = path._segments
    prev     = nil
    
    -- Copy over points from path and filter out adjacent duplicates.
    i = 0
    l = segments.length

    while i < l
      point = segments[i].point\clone()
      if not prev or not prev\equals(point)
        @points\push point
        prev = point
      i++

    @error = eerror

 fit: =>
    points    = @points
    length    = points.length
    @segments = (if length > 0 then [Segment(points[0])] else [])
        
    -- Right Tangent
    @fitCubic 0, length - 1, points[1]\subtract(points[0])\normalize(), points[length - 2].subtract(points[length - 1]).normalize()  if length > 1
    @segments

  
  -- Fit a Bezier curve to a (sub)set of digitized points
  fitCubic: (first, last, tan1, tan2) =>
    
    -- Use heuristic if region only has two points in it
    if last - first is 1
      pt1  = @points[first]
      pt2  = @points[last]
      dist = pt1\getDistance(pt2) / 3
      @addCurve [pt1, pt1\add(tan1\normalize(dist)), pt2\add(tan2\normalize(dist)), pt2]
      return
    
    -- Parameterize points, and attempt to fit curve
    uPrime   = @chordLengthParameterize(first, last)
    maxError = math.max(@error, @error * @error)

    eerror = nil
    split  = nil
    
    -- Try 4 iterations
    i = 0
    while i <= 4
      curve = @generateBezier(first, last, uPrime, tan1, tan2)
      
      -- Find max deviation of points to fitted curve
      max = @findMaxError(first, last, curve, uPrime)
      if max.error < @error
        @addCurve curve
        return
      split = max.index
      
      -- If error not too large, try reparameterization and iteration
      break if max.error >= maxError
      @reparameterize first, last, uPrime, curve
      maxError = max.error

      i++
    
    -- Fitting failed -- split at max error point and fit recursively
    V1 = @points[split - 1]\subtract(@points[split])
    V2 = @points[split]\subtract(@points[split + 1])

    tanCenter = V1\add(V2)\divide(2)\normalize()
    @fitCubic first, split, tan1, tanCenter
    @fitCubic split, last, tanCenter.negate(), tan2

  addCurve: (curve) =>
    prev = @segments[@segments.length - 1]
    prev\setHandleOut curve[1]\subtract(curve[0])
    @segments\push Segment(curve[3], curve[2]\subtract(curve[3]))

  -- Use least-squares method to find Bezier control points for region.
  generateBezier: (first, last, uPrime, tan1, tan2) =>
    epsilon = Numerical.EPSILON ----=
    pt1     = @points[first]
    pt2     = @points[last]
    
    -- Create the C and X matrices
    c = [[0, 0], [0, 0]]
    x = [0, 0]
    i = 0
    l = last - first + 1

    while i < l
      u = uPrime[i]
      t = 1 - u
      b = 3 * u * t

      b0 = t * t * t
      b1 = b * t
      b2 = b * u
      b3 = u * u * u

      a1 = tan1\normalize(b1)
      a2 = tan2\normalize(b2)

      tmp = @points[first + i]\subtract(pt1.multiply(b0 + b1))\subtract(pt2\multiply(b2 + b3))
      c[0][0] += a1\dot(a1)
      c[0][1] += a1\dot(a2)
      
      -- C[1][0] += a1.dot(a2);
      c[1][0]  = c[0][1]
      c[1][1] += a2\dot(a2)
      x[0]    += a1\dot(tmp)
      x[1]    += a2\dot(tmp)
      i++
    
    -- Compute the determinants of C and X
    detC0C1 = c[0][0] * c[1][1] - c[1][0] * c[0][1]
    alpha1  = nil
    alpha2  = nil

    if math.abs(detC0C1) > epsilon
      
      -- Kramer's rule
      detC0X = C[0][0] * X[1] - C[1][0] * X[0]
      detXC1 = X[0] * C[1][1] - X[1] * C[0][1]
      
      -- Derive alpha values
      alpha1 = detXC1 / detC0C1
      alpha2 = detC0X / detC0C1
    else
      
      -- Matrix is under-determined, try assuming alpha1 == alpha2
      c0 = C[0][0] + C[0][1]
      c1 = C[1][0] + C[1][1]
      if math.abs(c0) > epsilon
        alpha1 = alpha2 = X[0] / c0
      else if math.abs(c1) > epsilon
        alpha1 = alpha2 = X[1] / c1
      else
        -- Handle below
        alpha1 = alpha2 = 0
    
    -- If alpha negative, use the Wu/Barsky heuristic (see text)
    -- (if alpha is 0, you get coincident control points that lead to
    -- divide by zero in any subsequent NewtonRaphsonRootFind() call.
    segLength = pt2\getDistance(pt1)
    epsilon  *= segLength
    
    -- fall back on standard (probably inaccurate) formula,
    -- and subdivide further if needed.
    alpha1 = alpha2 = segLength / 3  if alpha1 < epsilon or alpha2 < epsilon
    
    -- First and last control points of the Bezier curve are
    -- positioned exactly at the first and last data points
    -- Control points 1 and 2 are positioned an alpha distance out
    -- on the tangent vectors, left and right, respectively
    [pt1, pt1\add(tan1\normalize(alpha1)), pt2\add(tan2\normalize(alpha2)), pt2]

  
  -- Given set of points and their parameterization, try to find
  -- a better parameterization.
  reparameterize: (first, last, u, curve) =>
    i = first

    while i <= last
      u[i - first] = @findRoot(curve, @points[i], u[i - first])
      i++

  -- Use Newton-Raphson iteration to find better root.
  findRoot: (curve, point, u) =>
    curve1 = []
    curve2 = []
    
    -- Generate control vertices for Q'
    i = 0
    while i <= 2
      curve1[i] = curve[i + 1]\subtract(curve[i]).multiply(3)
      i++
    
    -- Generate control vertices for Q''
    i = 0
    while i <= 1
      curve2[i] = curve1[i + 1]\subtract(curve1[i]).multiply(2)
      i++
    
    -- Compute Q(u), Q'(u) and Q''(u)
    pt    = @evaluate(3, curve, u)
    pt1   = @evaluate(2, curve1, u)
    pt2   = @evaluate(1, curve2, u)
    diff  = pt\subtract(point)
    df    = pt1\dot(pt1) + diff\dot(pt2)
    
    -- Compute f(u) / f'(u)
    return u if math.abs(df) < Numerical.TOLERANCE
    
    -- u = u - f(u) / f'(u)
    u - diff\dot(pt1) / df
  
  -- Evaluate a Bezier curve at a particular parameter value
  evaluate: (degree, curve, t) =>
    -- Copy array
    tmp = curve\slice()
    
    -- Triangle computation
    i = 1
    while i <= degree
      j = 0

      while j <= degree - i
        tmp[j] = tmp[j]\multiply(1 - t)\add(tmp[j + 1]\multiply(t))
        j++
      i++
    tmp[0]

  
  -- Assign parameter values to digitized points
  -- using relative distances between points.
  chordLengthParameterize: (first, last) =>
    u = [0]
    i = first + 1

    while i <= last
      u[i - first] = u[i - first - 1] + @points[i]\getDistance(@points[i - 1])
      i++

    i = 1
    m = last - first

    while i <= m
      u[i] /= u[m]
      i++

    u
  
  -- Find the maximum squared distance of digitized points to fitted curve.
  findMaxError: (first, last, curve, u) =>
    index   = math.floor((last - first + 1) / 2)
    maxDist = 0

    i = first + 1
    while i < last
      p = @evaluate(3, curve, u[i - first])
      v = p\subtract(@points[i])

      dist = v.x * v.x + v.y * v.y -- squared
      if dist >= maxDist
        maxDist = dist
        index = i
      i++

    eerror: maxDist
    index: index
