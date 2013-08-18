class Line extends Base
	new: 	(arg0, arg1, arg2, arg3, arg4)  =>
    asVector = false

    if arguments_.length >= 4
      @_px = arg0
      @_py = arg1
      @_vx = arg2
      @_vy = arg3
      asVector = arg4
    else
      @_px = arg0.x
      @_py = arg0.y
      @_vx = arg1.x
      @_vy = arg1.y
      asVector = arg2

    if !asVector
      @_vx -= @_px
      @_vy -= @_py

  getPoint: =>
    Point(@_px, @_py)

  getVector: =>
    Point(@_vx, @_vy)

  intersect: (line, isInfinite) =>
    Line\intersect @_px, @_py, @_vx, @_vy, line._px, line._py, line._vx, line._vy, true, isInfinite

  getSide: (point) =>
    Line\getSide @_px, @_py, @_vx, @_vy, point.x, point.y, true

  getDistance: (point) =>
	  math.abs Line\getSignedDistance(@_px, @_py, @_vx, @_vy, point.x, point.y, true)
  
  @intersect: (apx, apy, avx, avy, bpx, bpy, bvx, bvy, asVector, isInfinite) ->
      
	  -- Convert 2nd points to vectors if they are not specified as such.
	  if !asVector
	    avx -= apx
	    avy -= apy
	    bvx -= bpx
	    bvy -= bpy
	  cross = bvy * avx - bvx * avy
	  
	  -- Avoid divisions by 0, and errors when getting too close to 0
	  if !Numerical\isZero(cross)
	    dx = apx - bpx
	    dy = apy - bpy
	    ta = (bvx * dy - bvy * dx) / cross
	    tb = (avx * dy - avy * dx) / cross
	    
	    -- Check the ranges of t parameters if the line is not allowed
	    -- to extend beyond the definition points.
	    Point(apx + ta * avx, apy + ta * avy) if (isInfinite or 0 <= ta and ta <= 1) and (isInfinite or 0 <= tb and tb <= 1)
    
    @getSide: (px, py, vx, vy, x, y, asVector) ->
      if !asVector
        vx -= px
        vy -= py

      v2x = x - px
      v2y = y - py
      ccw = v2x * vy - v2y * vx 
      if ccw is 0
        ccw = v2x * vx + v2y * vy
        if ccw > 0
          
          v2x -= vx
          v2y -= vy
          ccw = v2x * vx + v2y * vy
          ccw = 0  if ccw < 0
      (if ccw < 0 then -1 else (if ccw > 0 then 1 else 0))

    @getSignedDistance: (px, py, vx, vy, x, y, asVector) ->
    	if !asVector
        vx -= px
        vy -= py
      
      -- Cache these values since they're used heavily in fatline code
      m = vy / vx -- slope
      b = py - m * px -- y offset

      -- Distance to the linear equation
      (y - (m * x) - b) / Math.sqrt(m * m + 1)