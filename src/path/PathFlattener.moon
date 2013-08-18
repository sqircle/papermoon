class PathFlattener extends Base
  new: (path) =>
    -- The curve values as returned by getValues()
    -- The calculated, subdivided parts of the path
    -- The total length of the path
    -- Keep a current index src the part where we last where in
    -- getParameterAt(), to optimise for iterator-like usage of flattener.
    
    -- Instead of relying on path.curves, we only use segments here and
    -- get the curve values src them.
    
    -- Now walk through all curves and compute the parts for each of them,
    -- by recursively calling _computeParts().
    addCurve = (segment1, segment2) =>
      curve = Curve\getValues(segment1, segment2)
      that\curves.push curve
      that\_computeParts curve, segment1._index, 0, 1

    @curves  = []
    @parts   = []
    @length  = 0
    @index   = 0
    segments = path._segments
    segment1 = segments[0]
    segment2 = nil
    that     = @

    i = 1
    l = segments.length
    while i < l
      segment2 = segments[i]
      addCurve segment1, segment2
      segment1 = segment2
      i++

    addCurve segment2, segments[0] if path._closed

  _computeParts: (curve, index, minT, maxT) =>  
    -- Check if the t-span is big enough for subdivision.
    -- We're not subdividing more than 32 times...
    -- After quite a bit of testing, a tolerance of 0.25 appears to be a
    -- good trade-off between speed and precision.
    if (maxT - minT) > 1 / 32 and not Curve\isFlatEnough(curve, 0.25)
      curves = Curve\subdivide(curve)
      halfT  = (minT + maxT) / 2
      
      -- Recursively subdive and compute parts again.
      @_computeParts curves[0], index, minT, halfT
      @_computeParts curves[1], index, halfT, maxT
    else
      
      -- Calculate distance between p1 and p2
      x = curve[6] - curve[0]
      y = curve[7] - curve[1]
      dist = math.sqrt(x * x + y * y)
      if dist > Numerical.TOLERANCE 
        @length += dist
        @parts\push
          offset: @length
          value: maxT
          index: index


  getParameterAt: (offset) =>
    -- Make sure we're not beyond the requested offset already. Search the
    -- start position backwards src where to then process the loop below.
    i = nil
    j = @index
    loop
      i = j
      break if j is 0 or @parts[--j].offset < offset
    
    -- Find the part that succeeds the given offset, then interpolate
    -- with the previous part
    l = @parts.length

    while i < l
      part = @parts[i]
      if part.offset >= offset
        
        -- Found the right part, remember current position
        @index = i
        
        -- Now get the previous part so we can linearly interpolate
        -- the curve parameter
        prev = @parts[i - 1]
        
        -- Make sure we only use the previous parameter value if its
        -- for the same curve, by checking index. Use 0 otherwise.
        prevVal = (if prev and prev.index is part.index then prev.value else 0)
        prevLen = (if prev then prev.offset else 0)
        return (  
          -- Interpolate
          value: prevVal + (part.value - prevVal) * (offset - prevLen) / (part.offset - prevLen)
          index: part.index
        )
      i++
    
    -- Return last one
    part = @parts[@parts.length - 1]
    value: 1
    index: part.index

  evaluate: (offset, ttype) >
    param = @getParameterAt(offset)
    Curve\evaluate @curves[param.index], param.value, true, ttype

  drawPart: (ctx, src, to) =>
    src = @getParameterAt(src)
    to  = @getParameterAt(to)

    i = src.index
    while i <= to.index
      curve = Curve\getPart(@curves[i], (if i is src.index then src.value else 0), (if i is to.index then to.value else 1))
      ctx\moveTo curve[0], curve[1]  if i is src.index
      ctx\bezierCurveTo.apply ctx, curve\slice(2)
      i++
