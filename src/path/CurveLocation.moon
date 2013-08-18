class CurveLocation extends Base
  new: (curve, parameter, point, _curve2, _parameter2, _point2, _distance) =>
    -- Define this CurveLocation's unique id.
    @_id  = CurveLocation._id = (CurveLocation._id or 0) + 1
    @_curve = curve
    
    -- Also store references to segment1 and segment2, in case path
    -- splitting / dividing is going to happen, in which case the segments
    -- can be used to determine the new curves, see --getCurve(true)
    @_segment1   = curve._segment1
    @_segment2   = curve._segment2
    @_parameter  = parameter
    @_point      = point
    @_curve2     = _curve2
    @_parameter2 = _parameter2
    @_point2     = _point2
    @_distance   = _distance

  getSegment: =>
    if !@_segment
      curve     = @getCurve()
      parameter = @getParameter()

      if parameter is 1
        @_segment = curve._segment2
      else if parameter is 0 or arg[0]
        @_segment = curve._segment1
      else if !parameter?
        return nil
      else  
        -- Determine the closest segment by comparing curve lengths
        @_segment = (if curve\getLength(0, parameter) < curve\getLength(parameter, 1)then curve._segment1 else curve._segment2)
   
    @_segment

  getCurve: => 
    if not @_curve or arg[0]    
      -- If we're asked to get the curve uncached, access current curve
      -- objects through segment1 / segment2. Since path splitting or
      -- dividing might have happened in the meantime, try segment1's
      -- curve, and see if _point lies on it still, otherwise assume it's
      -- the curve before segment2.
      @_curve = @_segment1\getCurve()
      @_curve = @_segment2\getPrevious()\getCurve() if !@_curve\getParameterOf(@_point)?
    
    @_curve

  getIntersection: =>
    intersection = @_intersection

    if not intersection and @_curve2
      param = @_parameter2
      
      -- If we have the parameter on the other curve use that for
      -- intersection rather than the point.
      @_intersection = intersection = CurveLocation(@_curve2, param, @_point2 or @_point, @)
      intersection._intersection = @

    intersection

  getPath: =>
    curve = @getCurve()
    curve and curve._path

  getIndex: =>
    curve = @getCurve()
    curve and curve\getIndex()


  getOffset: =>
    path = @getPath()
    path and path\_getOffset(@)

  getCurveOffset: =>
    curve     = @getCurve()
    parameter = @getParameter()
    parameter? and curve and curve\getLength(0, parameter)
  
  getParameter: =>
    if (not @_parameter? or arg[0]) and @_point
      curve = @getCurve(arg[0] and @_point)
      @_parameter = curve and curve\getParameterOf(@_point)

    @_parameter

  getPoint: =>
    if (not @_point or arg[0]) and @_parameter?
      curve   = @getCurve()
      @_point = curve and curve\getPointAt(@_parameter, true)
    @_point


  getTangent: =>
    parameter = @getParameter()
    curve     = @getCurve()
    parameter? and curve and curve\getTangentAt(parameter, true)

  
  getNormal: =>
    parameter = @getParameter()
    curve     = @getCurve()
    parameter? and curve and curve\getNormalAt(parameter, true)

  getDistance: =>
    @_distance

  divide: =>
    curve = @getCurve(true)
    curve and curve\divide(@getParameter(true))

  split: =>
    curve = @getCurve(true)
    curve and curve\split(@getParameter(true))
