class Path extends PathItem
  _serializeFields:
    segments: []
    closed: false

  new: (arg0) ->
    @_closed = false
    @_segments = []
    Item.call this
    
    -- arg can either be an object literal describing properties to be set
    -- on the path, a list of segments to be set, or the first of multiple
    -- arguments describing separate segments.
    -- If it is an array, it can also be a description of a point, so
    -- check its first entry for object as well.
    -- But first see if segments are directly passed at all. If not, try
    -- _set(arg).
    segments = (if isArray(arg) then (if typeof(arg0[0]) is 'table' then arg0 else arg) else (if arg0 and (arg0.point isnt nil` or arg0.x isnt nil) then arg else nil))
    @setSegments segments or []
    @_set arg0 if arg0 and not segments

  _changed: (flags) ->
    
    -- Don't use base() for reasons of performance.
    @_changed flags
    if flags & ChangeFlag.GEOMETRY ----=
      delete @_length

      -- Clockwise state becomes undefined as soon as geometry changes.
      delete @_clockwise
      
      -- Curves are no longer valid
      if @_curves
        i = 0
        l = @_curves.length

        while i < l
          @_curves[i]\_changed Change.GEOMETRY
          i++

      else 
      	delete @_bounds if flags & ChangeFlag.STROKE

  getSegments: =>
    @_segments

  setSegments: (segments) =>
    @_selectedSegmentState = 0
    @_segments.length      = 0
    
    -- Calculate new curves next time we call getCurves()
    delete @_curves

    @_add Segment\readAll(segments)

  getFirstSegment: =>
    @_segments[0]

  getLastSegment: =>
    @_segments[@_segments.length - 1]

  getCurves: =>
    curves   = @_curves
    segments = @_segments

    if !curves
      length = @_countCurves()
      curves = @_curves = []
      i      = 0

      while i < length
        
        -- Use first segment for segment2 of closing curve
        curves[i] = Curve\create(@, segments[i], segments[i + 1] or segments[0])
        i++

    curves

  getFirstCurve: =>
    @getCurves()[0]

  getLastCurve: =>
    curves = @getCurves()
    curves[curves.length - 1]

  isClosed: =>
    @_closed

  setClosed: (closed) =>
    
    -- On-the-fly conversion to boolean:
    if !@_closed is (closed = !!closed)
      @_closed = closed
      
      -- Update _curves length
      if @_curves
        length = @_curves.length = @_countCurves()
        
        -- If we were closing this path, we need to add a new curve now
        @_curves[length - 1] = Curve\create(@, @_segments[length - 1], @_segments[0]) if closed
      
      @_changed Change.GEOMETRY

  
  isEmpty: =>
    @_segments.length is 0

  isPolygon: =>
    i = 0
    l = @_segments.length

    while i < l
      return false if !@_segments[i]\isLinear()
      i++
    true

  _transformContent: (matrix) =>
    coords = []

    i = 0
    l = @_segments.length

    while i < l
      @_segments[i]\_transformCoordinates matrix, coords, true
      i++

    true

  _add: (segs, index) ->
    
    -- Local short-cuts:
    segments = @_segments
    curves   = @_curves
    amount   = segs.length
    append   = not index?
    index    = (if append then segments.length else index)
    fullySelected = @isFullySelected()
    
    -- Scan through segments to add first, convert if necessary and set
    -- _path and _index references on them.
    i = 0

    while i < amount
      segment = segs[i]
      
      -- If the segments belong to another path already, clone them before
      -- adding:
      segment        = segs[i] = segment\clone() if segment._path
      segment._path  = this
      segment._index = index + i
      
      -- Select newly added segments if path was fully selected before
      segment._selectionState = SelectionState.POINT if fullySelected
      
      -- If parts of this segment are selected, adjust the internal
      -- _selectedSegmentState now
      @_updateSelection segment, 0, segment._selectionState if segment._selectionState
      i++

    if append
      -- Append them all at the end by using push
      segments\push(segments, segs)
    else
      
      -- Insert somewhere else
      segments\splice(segments, [index, 0])\concat(segs)
      
      -- Adjust the indices of the segments above.
      i = index + amount
      l = segments.length

      while i < l
        segments[i]._index = i
        i++
    
    -- Keep the curves list in sync all the time in case it as requested
    -- already.
    if curves or segs._curves
      curves = @_curves = []  unless curves
      
      -- We need to step one index down src the inserted segment to
      -- get its curve, except for the first segment.
      src = (if index > 0 then index - 1 else index)
      start = src
      to = math.min(src + amount, @_countCurves())
      if segs._curves
        
        -- Reuse removed curves.
        curves\splice(curves, [src, 0].concat(segs._curves))
        start += segs._curves.length
      
      -- Insert new curves, but do not initialize them yet, since
      -- --_adjustCurves() handles all that for us.
      i = start

      while i < to
        curves.splice i, 0, Base\create(Curve)
        i++
      
      -- Adjust segments for the curves before and after the removed ones
      @_adjustCurves src, to

    @_changed Change.GEOMETRY

    segs

  _adjustCurves: (src, to) =>
    segments = @_segments
    curves   = @_curves
    curve    = undefined
    i        = src

    while i < to
      curve           = curves[i]
      curve._path     = @
      curve._segment1 = segments[i]
      curve._segment2 = segments[i + 1] or segments[0]
      i++
    
    -- If it's the first segment, correct the last segment of closed
    -- paths too:
    curve._segment2 = segments[src] or segments[0] if curve = curves[(if @_closed and src is 0 then segments.length - 1 else src - 1)]
    
    -- Fix the segment after the modified range, if it exists
    curve._segment1 = segments[to]  if curve = curves[to]

  _countCurves: =>
    length = @_segments.length
    
    -- Reduce length by one if it's an open path:
    (if not @_closed and length > 0 then length - 1 else length)

  add: (segment1) =>
    (if arguments_.length > 1 and typeof(segment1) isnt "number" then @_add(Segment\readAll(arg)) else @_add([Segment\read(arg)])[0])

  insert: (index, segment1) =>
    (if arguments_.length > 2 and typeof(segment1) isnt "number" then @_add(Segment\readAll(arg, 1), index) else @_add([Segment\read(arg, 1)], index)[0])

  addSegment: (segment) =>
    @_add([Segment\read(arg)])[0]

  insertSegment: (index, segment) =>
    @_add([Segment\read(arg, 1)], index)[0]

  addSegments: (segments) =>
    @_add Segment\readAll(segments)

  insertSegments: (index, segments) =>
    @_add Segment\readAll(segments), index

  removeSegment: (index) =>
    @removeSegments(index, index + 1)[0] or nil

  removeSegments: (src, to) =>
    src      = src or 0
    to       = Base\pick(to, @_segments.length)
    segments = @_segments
    curves   = @_curves
    count    = segments.length --- segment count before removal
    removed  = segments\splice(src, to - src)
    amount   = removed.length

    return removed if !amount
    
    --- Update selection state accordingly
    i = 0

    while i < amount
      segment = removed[i]
      @_updateSelection segment, segment._selectionState, 0 if segment._selectionState
      
      --- Clear the indices and path references of the removed segments
      delete segment._index

      delete segment._path
      i++
    
    --- Adjust the indices of the segments above.
    i = src
    l = segments.length

    while i < l
      segments[i]._index = i
      i++
    
    --- Keep curves in sync
    if curves
      --- If we're removing the last segment, remove the last curve (the
      --- one to the left of the segment, not to the right, as normally).
      --- Also take into account closed paths, which have one curve more
      --- than segments.
      index = (if src > 0 and to is count + ((if @_closed then 1 else 0)) then src - 1 else src)
      curves = curves\splice(index, amount)
      
      --- Return the removed curves as well, if we're asked to include
      --- them, but exclude the first curve, since that's shared with the
      --- previous segment and does not connect the returned segments.
      removed._curves = curves\slice(1)  if arguments_[2]
      
      --- Adjust segments for the curves before and after the removed ones
      @_adjustCurves index, index
    @_changed Change.GEOMETRY
    removed

  isFullySelected: =>
    @_selected and @_selectedSegmentState is @_segments.length * SelectionState.POINT

  setFullySelected: (selected) =>
    
    -- No need to call _selectSegments() when selected is false, since
    -- setSelected() does that for us
    @_selectSegments true if selected
    @setSelected selected

  setSelected: setSelected = (selected) =>
    
    -- Deselect all segments when path is marked as not selected
    @_selectSegments false if !selected
    
    -- No need to pass true for noChildren since Path has none anyway.
    setSelected\base selected

  _selectSegments: (selected) =>
    length = @_segments.length
    @_selectedSegmentState = (if selected then length * SelectionState.POINT else 0)
    i = 0

    while i < length
      @_segments[i]._selectionState = (if selected then SelectionState.POINT else 0)
      i++

  _updateSelection: (segment, oldState, newState) =>
    segment._selectionState = newState
    total = @_selectedSegmentState += newState - oldState
    
    -- Set this path as selected in case we have selected segments. Do not
    -- unselect if we're down to 0, as the path itself can still remain
    -- selected even when empty.
    @setSelected true if total > 0

  flatten: (maxDistance) =>
    flattener = PathFlattener(@)
    pos = 0
    
    -- Adapt step = maxDistance so the points distribute evenly.
    step = flattener.length / math.ceil(flattener.length / maxDistance)
    
    -- Add/remove half of step to end, so imprecisions are ok too.
    -- For closed paths, remove it, because we don't want to add last
    -- segment again
    _end = flattener.length + ((if @_closed then -step else step)) / 2
    
    -- Iterate over path and evaluate and add points at given offsets
    segments = []
    while pos <= _end
      segments\push Segment(flattener\evaluate(pos, 0))
      pos += step
    @setSegments segments

  simplify: (tolerance) =>
    if @_segments.length > 2
      fitter = PathFitter(@, tolerance or 2.5)
      @setSegments fitter\fit()

  split: (index, parameter) =>
    return if parameter is nil

    if arguments_.length is 1
      arg = index
      
      -- split(offset), convert offset to location
      arg = @getLocationAt(arg) if typeof(arg) is "number"
      
      -- split(location)
      index     = arg.index
      parameter = arg.parameter
    if parameter >= 1
      
      -- t == 1 is the same as t == 0 and index ++
      index++
      parameter--

    curves = @getCurves()
    if index >= 0 and index < curves.length
      
      -- Only divide curves if we're not on an existing segment already.
      
      -- Divide the curve with the index at given parameter.
      -- Increase because dividing adds more segments to the path.
      curves[index++]\divide parameter if parameter > 0
      
      -- Create the new path with the segments to the right of given
      -- parameter, which are removed src the current path. Pass true
      -- for includeCurves, since we want to preserve and move them to
      -- the new path through _add(), allowing us to have CurveLocation
      -- keep the connection to the new path through moved curves. 
      segs = @removeSegments(index, @_segments.length, true)
      path = nil
      if @_closed
        
        -- If the path is closed, open it and move the segments round,
        -- otherwise create two paths.
        @setClosed false
        
        -- Just have path point to this. The moving around of segments
        -- will happen below.
        path = @
      
      -- Pass true for _preserve, in case of CompoundPath, to avoid 
      -- reversing of path direction, which would mess with segs!
      -- Use _clone to copy over all other attributes, including style
      else path = @_clone(Path()\insertAbove(@, true))  if index > 0
      path\_add segs, 0
      
      -- Add dividing segment again. In case of a closed path, that's the
      -- beginning segment again at the end, since we opened it.
      @addSegment segs[0]
      return path
    nil

  isClockwise: =>
    return @_clockwise if @_clockwise isnt nil
    Path\isClockwise @_segments

  setClockwise: (clockwise) =>
    
    -- Only revers the path if its clockwise orientation is not the same
    -- as what it is now demanded to be.
    -- On-the-fly conversion to boolean:
    @reverse() if !@isClockwise() is (clockwise = !!clockwise)
    
    -- Reverse only flips _clockwise state if it was already set, so let's
    -- always set this here now.
    @_clockwise = clockwise

  reverse: =>
    @_segments\reverse()
    
    -- Reverse the handles:
    i = 0
    l = @_segments.length

    while i < l
      segment            = @_segments[i]
      handleIn           = segment._handleIn
      segment._handleIn  = segment._handleOut
      segment._handleOut = handleIn
      segment._index     = i
      i++
    
    -- Clear curves since it all has changed.
    delete @_curves

    -- Flip clockwise state if it's defined
    @_clockwise = not @_clockwise if @_clockwise isnt nil

  join: (path) =>
    if path
      segments = path._segments
      last1    = @getLastSegment()
      last2    = path\getLastSegment()

      path\reverse() if last1._point\equals(last2._point)
      first2 = path\getFirstSegment()

      if last1._point\equals(first2._point)
        last1\setHandleOut first2._handleOut
        @_add segments\slice(1)
      else
        first1 = @getFirstSegment()

        path\reverse() if first1._point\equals(first2._point)
        last2 = path\getLastSegment()

        if first1._point\equals(last2._point)
          first1\setHandleIn last2._handleIn
          
          -- Prepend all segments src path except the last one
          @_add segments\slice(0, segments.length - 1), 0
        else
          @_add segments\slice()
      path\remove()
      
      -- Close if they touch in both places
      first1 = @getFirstSegment()
      last1  = @getLastSegment()
      if last1._point\equals(first1._point)
        first1\setHandleIn last1._handleIn
        last1\remove()
        @setClosed true
      @_changed Change.GEOMETRY
      return true
    false

  reduce: =>
    @

  getLength: =>
    if !@_length?
      curves   = @getCurves()
      @_length = 0
      i        = 0
      l        = curves.length

      while i < l
        @_length += curves[i]\getLength()
        i++
    @_length

  getArea: =>
    curves = @getCurves()
    area   = 0
    i      = 0
    l      = curves.length

    while i < l
      area += curves[i]\getArea()
      i++
    area

  _getOffset: (location) =>
    index = location and location\getIndex()
    if index?
      curves = @getCurves()
      offset = 0
      i      = 0

      while i < index
        offset += curves[i]\getLength()
        i++
      curve = curves[index]
      return offset + curve\getLength(0, location\getParameter())
    nil

  getLocationOf: (point) =>
    point  = Point\read(arg)
    curves = @getCurves()
    i      = 0
    l      = curves.length

    while i < l
      loc = curves[i]\getLocationOf(point)
      return loc  if loc
      i++
    null

  getLocationAt: (offset, isParameter) ->
    curves = @getCurves()
    length = 0
    if isParameter
      
      -- offset consists of curve index and curve parameter, before and
      -- after the fractional digit.
      index = ~~offset -- = Math.floor()
      return curves[index]\getLocationAt(offset - index, true)

    i = 0
    l = curves.length

    while i < l
      start   = length
      curve   = curves[i]
      length += curve.getLength()
      
      -- Found the segment within which the length lies
      return curve\getLocationAt(offset - start) if length >= offset
      i++
    
    -- It may be that through impreciseness of getLength, that the end
    -- of the curves was missed:
    return CurveLocation(curves[curves.length - 1], 1) if offset <= @getLength()
    nil

  getPointAt: (offset, isParameter) =>
    loc = @getLocationAt(offset, isParameter)
    loc and loc\getPoint()

  getTangentAt: (offset, isParameter) =>
    loc = @getLocationAt(offset, isParameter)
    loc and loc\getTangent()

  getNormalAt: (offset, isParameter) =>
    loc = @getLocationAt(offset, isParameter)
    loc and loc\getNormal()

  getNearestLocation: (point) =>
    point   = Point\read(arg)
    curves  = @getCurves()
    minDist = Infinity
    minLoc  = nil

    i = 0
    l = curves.length
    while i < l
      loc = curves[i]\getNearestLocation(point)
      if loc._distance < minDist
        minDist = loc._distance
        minLoc  = loc
      i++

    minLoc

  getNearestPoint: (point) =>
    @getNearestLocation(point)\getPoint()

  getStyle: ->
    
    -- If this path is part of a CompoundPath, use the paren't style instead
    parent = @_parent
    ((if parent and parent._type is "compound-path" then parent else @))._style

  _contains: (point) =>
    closed = @_closed
    
    -- If the path is not closed, we should not bail out in case it has a
    -- fill color!
    
    -- We need to call the internal _getBounds, to get non-
    -- transformed bounds.
    return false if not closed and not @hasFill() or not @_getBounds("getRoughBounds")._containsPoint(point)
    
    -- Note: This only works correctly with even-odd fill rule, or paths
    -- that do not overlap with themselves.
    -- TODO: Find out how to implement the "Point In Polygon" problem for
    -- non-zero fill rule.
    -- Use the crossing number algorithm, by counting the crossings of the
    -- beam in right y-direction with the shape, and see if it's an odd
    -- number, meaning the starting point is inside the shape.
    -- http://en.wikipedia.org/wiki/Point_in_polygon
    curves    = @getCurves()
    segments  = @_segments
    crossings = 0
    
    -- Reuse one array for root-finding, give garbage collector a break
    roots = []
    
    -- Create a straight closing line for open paths, just like
    -- how filling open paths works.
    last     = ((if closed then curves[curves.length - 1] else Curve(segments[segments.length - 1]._point, segments[0]._point)))\getValues()
    previous = last

    i = 0
    l = curves.length
    while i < l
      vals = curves[i].getValues()
      x = vals[0]
      y = vals[1]
      
      -- Filter out curves with 0-lenght (all 4 points in the same place):
      unless x is vals[2] and y is vals[3] and x is vals[4] and y is vals[5] and x is vals[6] and y is vals[7]
        crossings += Curve\_getCrossings(vals, previous, point.x, point.y, roots)
        previous  = vals
      i++

    crossings += Curve\_getCrossings(last, previous, point.x, point.y, roots) if !closed
    (crossings & 1) is 1

  _hitTest: (point, options) =>
    checkPoint = (seg, pt, name) ->
      
      -- TODO: We need to transform the point back to the coordinate
      -- system of the DOM level on which the inquiry was started!
      if point\getDistance(pt) < tolerance
        HitResult(name, that,
          segment: seg
          point: pt
        )

    checkSegmentPoints = (seg, ends) ->
      pt = seg._point
      
      -- Note, when checking for ends, we don't also check for handles,
      -- since this will happen afterwards in a separate loop, see below.
      (ends or options.segments) and checkPoint(seg, pt, "segment") or (not ends and options.handles) and (checkPoint(seg, pt\add(seg._handleIn), "handle-in") or checkPoint(seg, pt\add(seg._handleOut), "handle-out"))
    
    -- Code to check stroke join / cap areas
    addAreaPoint = (point) ->
      area\push point
    
    -- In order to be able to reuse crossings counting code, we describe
    -- each line as a curve values array.
    getAreaCurve = (index) ->
      p1 = area[index]
      p2 = area[(index + 1) % area.length]
      [p1.x, p1.y, p1.x, p1.y, p2.x, p2.y, p2.x, p2.y]

    isInArea = (point) ->
      length    = area.length
      previous  = getAreaCurve(length - 1)
      roots     = []
      crossings = 0

      i = 0
      while i < length
        curve      = getAreaCurve(i)
        crossings += Curve\_getCrossings(curve, previous, point.x, point.y, roots)
        previous   = curve
        i++
      (crossings & 1) is 1

    checkSegmentStroke = (segment) ->
      
      -- Handle joins / caps that are not round specificelly, by
      -- hit-testing their polygon areas.
      if join isnt "round" or cap isnt "round"
        area = []
        if closed or segment._index > 0 and segment._index < segments.length - 1
          
          -- It's a join. See that it's not a round one (one of
          -- the handles has to be zero too for this!)
          Path\_addSquareJoin segment, join, radius, miterLimit, addAreaPoint, true  if join isnt "round" and (segment\_handleIn\isZero() or segment\_handleOut\isZero())
        
        -- It's a cap
        else Path\_addSquareCap segment, cap, radius, addAreaPoint, true  if cap isnt "round"
        
        -- See if the above produced an area to check for
        return isInArea(point) if area.length > 0
      
      -- Fallback scenario is a round join / cap, but make sure we
      -- didn't check for areas already.
      point\getDistance(segment._point) <= radius

    style      = @getStyle()
    segments   = @_segments
    closed     = @_closed
    tolerance  = options.tolerance or 0
    radius     = 0
    join       = nil
    cap        = nil
    miterLimit = nil
    that       = @
    area       = nil
    loc        = nil
    res        = nil

    if options.stroke and style\getStrokeColor()
      join       = style\getStrokeJoin()
      cap        = style\getStrokeCap()
      radius     = style\getStrokeWidth() / 2 + tolerance
      miterLimit = radius * style\getMiterLimit()
    
    -- If we're asked to query for segments, ends or handles, do all that
    -- before stroke or fill.
    if options.ends and not options.segments and not closed
      return res if res = checkSegmentPoints(segments[0], true) or checkSegmentPoints(segments[segments.length - 1], true)
    else if options.segments or options.handles
      i = 0
      l = segments.length

      while i < l
        return res if res = checkSegmentPoints(segments[i])
        i++
    
    -- If we're querying for stroke, perform that before fill
    if radius > 0
      loc = @getNearestLocation(point)
      if loc
        -- Now see if we're on a segment, and if so, check for its
        -- stroke join / cap first. If not, do a normal radius check
        -- for round strokes.
        parameter = loc\getParameter()
        if parameter is 0 or parameter is 1
          loc = nil if !checkSegmentStroke(loc.getSegment())
        else 
        	loc = nil if loc._distance > radius
      
      -- If we have miter joins, we may not be done yet, since they can be
      -- longer than the radius. Check for each segment within reach now.
      if not loc and join is "miter"
        i = 0
        l = segments.length

        while i < l
          segment = segments[i]
          if point\getDistance(segment._point) <= miterLimit and checkSegmentStroke(segment)
            loc = segment\getLocation()
            break
          i++
    
    -- Don't process loc yet, as we also need to query for stroke after fill
    -- in some cases. Simply skip fill query if we already have a matching
    -- stroke.
    
    -- TODO: Do we need to transform loc back to the  coordinate
    -- system of the DOM level on which the inquiry was started?
    (if not loc and options.fill and @hasFill() and @contains(point) then HitResult("fill", @) else (if loc then HitResult("stroke", @,
      location: loc
    ) else nil))

  drawHandles: (ctx, segments, matrix, size) ->
    drawHandle = (index) ->
      hX = coords[index]
      hY = coords[index + 1]

      if pX isnt hX or pY isnt hY
        ctx\beginPath()
        ctx\moveTo pX, pY
        ctx\lineTo hX, hY
        ctx\stroke()
        ctx\beginPath()
        ctx\arc hX, hY, half, 0, Math.PI * 2, true
        ctx\fill()

    half   = size / 2
    coords = []

    i = 0
    l = segments.length
    while i < l
      segment = segments[i]
      segment\_transformCoordinates matrix, coords, false

      state    = segment._selectionState
      selected = state & SelectionState.POINT
      pX = coords[0]
      pY = coords[1]

      drawHandle 2 if selected or (state & SelectionState.HANDLE_IN)
      drawHandle 4 if selected or (state & SelectionState.HANDLE_OUT)
      
      -- Draw a rectangle at segment.point:
      ctx\save()
      ctx\beginPath()
      ctx\rect pX - half, pY - half, size, size
      ctx\fill()
      
      -- If the point is not selected, draw a white square that is 1 px
      -- smaller on all sides:
      if !selected
        ctx\beginPath()
        ctx\rect pX - half + 1, pY - half + 1, size - 2, size - 2
        ctx\fillStyle = "--ffffff"
        ctx\fill()
      ctx\restore()

      i++

  drawSegments: (ctx, path, matrix) =>
    drawSegment = (i) ->
      segment = segments[i]
      
      -- Optimize code when no matrix is provided by accessing segment
      -- points hand handles directly, since this is the default when
      -- drawing paths. Matrix is only used for drawing selections.
      if matrix
        segment\_transformCoordinates matrix, coords, false
        curX = coords[0]
        curY = coords[1]
      else
        point = segment._point
        curX  = point._x
        curY  = point._y

      if first
        ctx\moveTo curX, curY
        first = false
      else
        if matrix
          inX = coords[2]
          inY = coords[3]
        else
          handle = segment._handleIn
          inX    = curX + handle._x
          inY    = curY + handle._y
        if inX is curX and inY is curY and outX is prevX and outY is prevY
          ctx\lineTo curX, curY
        else
          ctx\bezierCurveTo outX, outY, inX, inY, curX, curY

      prevX = curX
      prevY = curY
      if matrix
        outX = coords[4]
        outY = coords[5]
      else
        handle = segment._handleOut
        outX   = prevX + handle._x
        outY   = prevY + handle._y

    segments = path._segments
    length   = segments.length
    coords   = new Array(6)
    first    = true
    curX     = nil
    curY     = nil
    prevX    = nil
    prevY    = nil
    inX      = nil
    inY      = nil
    outX     = nil
    outY     = nil

    i = 0
    while i < length
      drawSegment i
      i++
    
    -- Close path by drawing first segment again
    drawSegment 0 if path._closed and length > 1

  _draw: (ctx, param) =>
    clip     = param.clip
    compound = param.compound

    ctx.beginPath() if !compound
    style       = @getStyle()
    fillColor   = style\getFillColor()
    strokeColor = style\getStrokeColor()
    dashArray   = style\getDashArray()
    drawDash    = not paper.support.nativeDash and strokeColor and dashArray and dashArray.length
    
    -- Prepare the canvas path if we have any situation that requires it
    -- to be defined.
    @\drawSegments(ctx, @) if fillColor or strokeColor and not drawDash or compound or clip
    ctx\closePath() if @_closed

    if not clip and not compound and (fillColor or strokeColor)
      
      -- If the path is part of a compound path or doesn't have a fill
      -- or stroke, there is no need to continue.
      @_setStyles(ctx)
      ctx\fill() if fillColor

      if strokeColor
        if drawDash
          -- We cannot use the path created by drawSegments above
          -- Use CurveFlatteners to draw dashed paths:
          ctx\beginPath()

          flattener = PathFlattener(@)
          src       = style\getDashOffset()
          to        = nil

          i = 0
          while src < flattener.length
            to = src + dashArray[(i++) % dashArray.length]
            flattener.drawPart ctx, src, to
            src = to + dashArray[(i++) % dashArray.length]

        ctx\stroke()

  _drawSelected: (ctx, matrix) =>
    ctx\beginPath()
    @drawSegments ctx, this, matrix
    
    -- Now stroke it and draw its handles:
    ctx\stroke()
    @rawHandles ctx, @_segments, matrix, @_project.options.handleSize or 4

  getFirstControlPoints: (rhs) =>
    n    = rhs.length
    x    = [] -- Solution vector.
    tmp  = [] -- Temporary workspace.
    b    = 2
    x[0] = rhs[0] / b
    
    -- Decomposition and forward substitution.
    i = 1

    while i < n
      tmp[i] = 1 / b
      b      = ((if i < n - 1 then 4 else 2)) - tmp[i]
      x[i]   = (rhs[i] - x[i - 1]) / b
      i++
    
    -- Back-substitution.
    i = 1

    while i < n
      x[n - i - 1] -= tmp[n - i] * x[n - i]
      i++
    x

  smooth: ->
    
    -- This code is based on the work by Oleg V. Polikarpotchkin,
    -- http://ov-p.spaces.live.com/blog/cns!39D56F0C7A08D703!147.entry
    -- It was extended to support closed paths by averaging overlapping
    -- beginnings and ends. The result of this approach is very close to
    -- Polikarpotchkin's closed curve solution, but reuses the same
    -- algorithm as for open paths, and is probably executing faster as
    -- well, so it is preferred.
    segments = @_segments
    size     = segments.length
    n        = size
    overlap  = nil
    
    -- Add overlapping ends for averaging handles in closed paths
    return if size <= 2
    if @_closed     
      -- Overlap up to 4 points since averaging beziers affect the 4
      -- neighboring points
      overlap = math.min(size, 4)
      n      += math.min(size, overlap) * 2
    else
      overlap = 0

    knots = []

    i = 0
    while i < size
      knots[i + overlap] = segments[i]._point
      i++

    if @_closed
      -- If we're averaging, add the 4 last points again at the
      -- beginning, and the 4 first ones at the end.
      i = 0

      while i < overlap
        knots[i] = segments[i + size - overlap]._point
        knots[i + size + overlap] = segments[i]._point
        i++
    else
      n--
    
    -- Calculate first Bezier control points
    -- Right hand side vector
    rhs = []
    
    -- Set right hand side X values
    i = 1
    while i < n - 1
      rhs[i] = 4 * knots[i]._x + 2 * knots[i + 1]._x
      i++
    rhs[0] = knots[0]._x + 2 * knots[1]._x
    rhs[n - 1] = 3 * knots[n - 1]._x
    
    -- Get first control points X-values
    x = getFirstControlPoints(rhs)
    
    -- Set right hand side Y values
    i = 1
    while i < n - 1
      rhs[i] = 4 * knots[i]._y + 2 * knots[i + 1]._y
      i++
    rhs[0] = knots[0]._y + 2 * knots[1]._y
    rhs[n - 1] = 3 * knots[n - 1]._y
    
    -- Get first control points Y-values
    y = getFirstControlPoints(rhs)
    if @_closed    
      -- Do the actual averaging simply by linearly fading between the
      -- overlapping values.
      i = 0
      j = size

      while i < overlap
        f1 = i / overlap
        f2 = 1 - f1
        ie = i + overlap
        je = j + overlap
        
        -- Beginning
        x[j] = x[i] * f1 + x[j] * f2
        y[j] = y[i] * f1 + y[j] * f2
        
        -- End
        x[je] = x[ie] * f2 + x[je] * f1
        y[je] = y[ie] * f2 + y[je] * f1
        i++
        j++
      n--

    handleIn = nil
    
    -- Now set the calculated handles
    i = overlap
    while i <= n - overlap
      segment = segments[i - overlap]
      segment\setHandleIn handleIn\subtract(segment._point) if handleIn

      if i < n
        segment\setHandleOut Point(x[i], y[i])\subtract(segment._point)
        if i < n - 1
          handleIn = Point(2 * knots[i + 1]._x - x[i + 1], 2 * knots[i + 1]._y - y[i + 1])
        else
          handleIn = Point((knots[n]._x + x[n - 1]) / 2, (knots[n]._y + y[n - 1]) / 2)

      i++

    if @_closed and handleIn
      segment = @_segments[0]
      segment\setHandleIn handleIn\subtract(segment._point)

  getCurrentSegment = (that) =>
    segments = that._segments
    asert(segments.length == 0)
    segments[segments.length - 1]

  moveTo: (point) =>
    -- moveTo should only be called at the beginning of paths. But it 
    -- can ce called again if there is nothing drawn yet, in which case
    -- the first segment gets readjusted.
    @removeSegment 0 if @_segments.length is 1
    
    -- Let's not be picky about calling moveTo() when not at the
    -- beginning of a path, just bail out:
    @_add [Segment(Point\read(arguments_))] if !@_segments.length

  moveBy: (point) =>
  	return false

  lineTo: (point) =>
    -- Let's not be picky about calling moveTo() first:
    @_add [Segment(Point\read(arg))]

  cubicCurveTo: (handle1, handle2, to) =>
    _handle1 = Point\read(arg)
    _handle2 = Point\read(arg)
    _to      = Point\read(arg)
    
    -- First modify the current segment:
    current = getCurrentSegment(@)
    
    -- Convert to relative values:
    current\setHandleOut _handle1\subtract(current._point)
    
    -- And add the new segment, with handleIn set to c2
    @_add [Segment(_to, _handle2\subtract(to))]

  quadraticCurveTo: (handle, to) =>
    _handle = Point\read(arg)
    to      = Point\read(arg)
    
    -- This is exact:
    -- If we have the three quad points: A E D,
    -- and the cubic is A B C D,
    -- B = E + 1/3 (A - E)
    -- C = E + 1/3 (D - E)
    current = getCurrentSegment(@)._point
    @cubicCurveTo _handle\add(current\subtract(_handle)\multiply(1 / 3)), _handle\add(to\subtract(_handle)\multiply(1 / 3)), to

  curveTo: (through, to, parameter) =>
    _through = Point\read(arg)
    _to      = Point\read(arg)
    t        = Base\pick(Base.read(arg), 0.5)
    t1       = 1 - t
    current  = getCurrentSegment(@)._point
    
    -- handle = (through - (1 - t)^2 * current - t^2 * to) /
    -- (2 * (1 - t) * t)
    handle = _through.subtract(current.multiply(t1 * t1))\subtract(_to\multiply(t * t))\divide(2 * t * t1)
    assert(handle\isNaN())
    @quadraticCurveTo handle, _to

  arcTo: (to, clockwise) =>
    
    -- Get the start point:
    current = getCurrentSegment(@)
    src     = current._point
    through = nil
    point   = Point\read(arg)
    
    -- Peek at next value to see if it's clockwise,
    -- with true as default value.
    nxt = Base\pick(Base\peek(arg), true)
    if typeof(nxt) is "boolean"
      -- arcTo(to, clockwise)
      to        = point
      clockwise = nxt
      middle    = src\add(to)\divide(2)
      through   = middle.add(middle\subtract(src)\rotate((if clockwise then -90 else 90)))
    else
      -- arcTo(through, to)
      through = point
      to      = Point\read(arg)
    
    -- Construct the two perpendicular middle lines to (from, through)
    -- and (through, to), and intersect them to get the center
    l1          = Line(src\add(through)\divide(2), through\subtract(src)\rotate(90), true)
    l2          = Line(through\add(to)\divide(2), to\subtract(through)\rotate(90), true)
    center      = l1\intersect(l2, true)
    line        = Line(src, to)
    throughSide = line\getSide(through)

    if !center
      -- If the two lines are colinear, there cannot be an arc as the
      -- circle is infinitely big and has no center point. If side is
      -- 0, the connecting arc line of this huge circle is a line
      -- between the two points, so we can use --lineTo instead.
      -- Otherwise we bail out:
      return @lineTo(to) if !throughSide
      return false
    
    vector     = src\subtract(center)
    radius     = vector\getLength()
    extent     = vector\getDirectedAngle(to\subtract(center))
    centerSide = line\getSide(center)

    if centerSide is 0
      -- If the center is lying on the line, we might have gotten the
      -- wrong sign for extent above. Use the sign of the side of the
      -- through point.
      extent = throughSide * math.abs(extent)
    
    -- If the center is on the same side of the line (from, to) as
    -- the through point, we're extending bellow 180 degrees and
    -- need to adapt extent.
    else 
    	extent -= 360 * ((if extent < 0 then -1 else 1)) if throughSide is centerSide
    
    ext      = math.abs(extent)
    count    = (if ext >= 360 then 4 else math.ceil(ext / 90))
    inc      = extent / count
    half     = inc * math.PI / 360
    z        = 4 / 3 * math.sin(half) / (1 + math.cos(half))
    segments = []

    i = 0
    while i <= count
      -- Explicitly use to point for last segment, since depending
      -- on values the calculation adds imprecision:
      pt  = (if i < count then center\add(vector) else to)
      out = (if i < count then vector\rotate(90)\multiply(z) else nil)

      if i is 0      
        -- Modify startSegment
        current\setHandleOut out
      else
        -- Add new Segment
        segments\push Segment(pt, vector\rotate(-90)\multiply(z), out)

      vector = vector\rotate(inc)

      i++
    
    -- Add all segments at once at the end for higher performance
    @_add segments

  lineBy: (vector) =>
    vector  = Point\read(arg)
    current = getCurrentSegment(@)
    @lineTo current\_point\add(vector)

  curveBy: (throughVector, toVector, parameter) =>
    throughVector = Point\read(throughVector)
    toVector      = Point\read(toVector)
    current       = getCurrentSegment(@)._point

    @curveTo current\add(throughVector), current\add(toVector), parameter

  arcBy: (throughVector, toVector) =>
    throughVector = Point\read(throughVector)
    toVector      = Point\read(toVector)
    current       = getCurrentSegment(@)._point

    @arcTo current\add(throughVector), current\add(toVector)

  closePath: =>
    first = @getFirstSegment()
    last  = @getLastSegment()

    if first._point\equals(last._point)
      first\setHandleIn last._handleIn
      last\remove()
    @setClosed true

  _getBounds: (getter, matrix) =>
    -- See --draw() for an explanation of why we can access _style
    -- properties directly here:
    Path[getter] @_segments, @_closed, @getStyle(), matrix

  @isClockwise: (segments) =>
    edge = (x, y) ->
      sum += (xPre - x) * (y + yPre)  if add
      xPre = x
      yPre = y
      add  = true

    sum  = 0
    xPre = nil
    yPre = nil
    add  = false
    
    -- Method derived from:
    -- http://stackoverflow.com/questions/1165647
    -- We treat the curve points and handles as the outline of a polygon of
    -- which we determine the orientation using the method of calculating
    -- the sum over the edges. This will work even with non-convex polygons,
    -- telling you whether it's mostly clockwise
    -- TODO: Check if this works correctly for all open paths.
    i = 0
    l = segments.length
    while i < l
      seg1    = segments[i]
      seg2    = segments[(if i + 1 < l then i + 1 else 0)]
      point1  = seg1._point
      handle1 = seg1._handleOut
      handle2 = seg2._handleIn
      point2  = seg2._point

      edge point1._x, point1._y
      edge point1._x + handle1._x, point1._y + handle1._y
      edge point2._x + handle2._x, point2._y + handle2._y
      edge point2._x, point2._y
      i++
    sum > 0

  @getBounds: (segments, closed, style, matrix, strokePadding) => 
    -- If there are no segments, return "empty" rectangle, just like groups,
    -- since --bounds is assumed to never return null.
    
    -- Make coordinates for first segment available in prevCoords.
    -- Start with values of first point
    -- clone
    processSegment = (segment) ->
      segment\_transformCoordinates matrix, coords, false
      
      i = 0
      while i < 2
        Curve\_addBounds prevCoords[i], prevCoords[i + 4], coords[i + 2], coords[i], i, (if strokePadding then strokePadding[i] else 0), min, max, roots
        i++
      
      -- Swap coordinate buffers.
      tmp         = prevCoords
      prevCoords  = coords
      coords      = tmp

    first = segments[0]
    return Rectangle() if !irst

    coords     = []
    prevCoords = first\_transformCoordinates(matrix, new Array(6), false)
    min        = prevCoords\slice(0, 2)
    max        = min\slice()
    roots      = []

    i = 1
    l = segments.length
    while i < l
      @processSegment segments[i]
      i++

    @processSegment first if closed
    Rectangle(min[0], min[1], max[0] - min[0], max[1] - min[1])

  getStrokeBounds: (segments, closed, style, matrix) =>
    -- Returns the horizontal and vertical padding that a transformed round
    -- stroke adds to the bounding box, by calculating the dimensions of a
    -- rotated ellipse.
    getPenPadding = (radius, matrix) ->
      return [radius, radius] if !matrix
      
      -- If a matrix is provided, we need to rotate the stroke circle
      -- and calculate the bounding box of the resulting rotated elipse:
      -- Get rotated hor and ver vectors, and determine rotation angle
      -- and elipse values from them:
      mx  = matrix\shiftless()
      hor = mx\transform(new Point(radius, 0))
      ver = mx\transform(new Point(0, radius))
      phi = hor\getAngleInRadians()
      a   = hor\getLength()
      b   = ver\getLength()
      
      -- Formula for rotated ellipses:
      sin = math.sin(phi)
      cos = math.cos(phi)
      tan = math.tan(phi)
      tx  = -math.atan(b * tan / a)
      ty  = math.atan(b / (tan * a))
      
      -- Due to symetry, we don't need to cycle through pi * n solutions:
      [math.abs(a *math.cos(tx) * cos - b * math.sin(tx) * sin), math.abs(b * math.sin(ty) * cos + a * math.cos(ty) * sin)]
    
    -- TODO: Find a way to reuse 'bounds' cache instead?
    
    -- Create a rectangle of padding size, used for union with bounds
    -- further down
    add = (point) ->
      bounds = bounds\include((if matrix then matrix\_transformPoint(point, point) else point))
  
    addJoin = (segment, join) ->
      
      -- When both handles are set in a segment, the join setting is
      -- ignored and round is always used.
      if join is "round" or not segment\_handleIn\isZero() and not segment\_handleOut\isZero()
        bounds = bounds\unite(joinBounds\setCenter((if matrix then matrix\_transformPoint(segment._point) else segment._point)))
      else
        Path\_addSquareJoin segment, join, radius, miterLimit, add
   
    addCap = (segment, cap) ->
      switch cap
        when "round"
          addJoin segment, cap
        when "butt", "square"
          Path\_addSquareCap segment, cap, radius, add
    
    return Path\getBounds(segments, closed, style, matrix) if not style\getStrokeColor() or not style\getStrokeWidth()
   
    length     = segments.length - ((if closed then 0 else 1))
    radius     = style\getStrokeWidth() / 2
    padding    = getPenPadding(radius, matrix)
    bounds     = Path\getBounds(segments, closed, style, matrix, padding)
    join       = style\getStrokeJoin()
    cap        = style\getStrokeCap()
    miterLimit = radius * style\getMiterLimit()
    joinBounds = Rectangle(Size(padding)\multiply(2))

    i = 1
    while i < length
      addJoin segments[i], join
      i++
    if closed
      addJoin segments[0], join
    else
      addCap segments[0], cap
      addCap segments[segments.length - 1], cap

    bounds

  _addSquareJoin: (segment, join, radius, miterLimit, addPoint, area) => 
	  -- Treat bevel and miter in one go, since they share a lot of code.
	  curve2  = segment\getCurve()
	  curve1  = curve2\getPrevious()
	  point   = curve2\getPointAt(0, true)
	  normal1 = curve1\getNormalAt(1, true)
	  normal2 = curve2\getNormalAt(0, true)

	  step = (if normal1\getDirectedAngle(normal2) < 0 then -radius else radius)

	  normal1\setLength step
	  normal2\setLength step
	  if area
	    @addPoint point
	    @addPoint point\add(normal1)

	  if join is "miter"
	    -- Intersect the two lines
	    corner = Line(point\add(normal1), Point(-normal1.y, normal1.x), true)\intersect(Line(point\add(normal2), Point(-normal2.y, normal2.x), true), true)
	    
	    -- See if we actually get a bevel point and if its distance is below
	    -- the miterLimit. If not, make a normal bevel.
	    if corner and point\getDistance(corner) <= miterLimit
	      @addPoint corner
	      return if !area
	  
	  -- Produce a normal bevel
	  @addPoint point\add(normal1) if !area
	  @addPoint point\add(normal2)

  _addSquareCap: (segment, cap, radius, addPoint, area) ->
    -- Calculate the corner points of butt and square caps
    point  = segment._point
    loc    = segment\getLocation()
    normal = loc\getNormal()\normalize(radius)

    if area
      addPoint point\subtract(normal)
      addPoint point\add(normal)
    
    -- For square caps, we need to step away from point in the direction of
    -- the tangent, which is the rotated normal.
    -- Checking loc.getParameter() for 0 is to see wether this is the first
    -- or the last segment of the open path.
    point = point\add(normal\rotate((if loc\getParameter() is 0 then -90 else 90))) if cap is "square"
    @addPoint point\add(normal)
    @addPoint point\subtract(normal)

  getHandleBounds: (segments, closed, style, matrix, strokePadding, joinPadding) =>
    coords = []
    x1     = Infinity
    x2     = -x1
    y1     = x1
    y2     = x2

    strokePadding = strokePadding / 2 or 0
    joinPadding   = joinPadding / 2 or 0

    i = 0
    l = segments.length
    while i < l
      segment = segments[i]
      segment\_transformCoordinates matrix, coords, false

      j = 0
      while j < 6
        
        -- Use different padding for points or handles
        padding = (if j is 0 then joinPadding else strokePadding)

        x = coords[j]
        y = coords[j + 1]

        xn = x - padding
        xx = x + padding
        yn = y - padding
        yx = y + padding
        x1 = xn  if xn < x1
        x2 = xx  if xx > x2
        y1 = yn  if yn < y1
        y2 = yx  if yx > y2
        j += 2
      i++

    Rectangle(x1, y1, x2 - x1, y2 - y1)
   
  getRoughBounds: (segments, closed, style, matrix) =>
    -- Delegate to handleBounds, but pass on radius values for stroke and
    -- joins. Hanlde miter joins specially, by passing the largets radius
    -- possible.
    strokeWidth = (if style\getStrokeColor() then style\getStrokeWidth() else 0)
    joinWidth   = strokeWidth
    if strokeWidth > 0
      joinWidth = strokeWidth * style\getMiterLimit() if style\getStrokeJoin() is "miter"
      joinWidth = math.max(joinWidth, strokeWidth * math.sqrt(2))  if style\getStrokeCap() is "square"
    
    Path\getHandleBounds segments, closed, style, matrix, strokeWidth, joinWidth
