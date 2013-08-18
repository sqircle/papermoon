class Segment extends Base
  new: Segment = (arg0, arg1, arg2, arg3, arg4, arg5) =>
    count       = arg.length
    createPoint = SegmentPoint\create()
    point       = nil
    handleIn    = nil
    handleOut   = nil
    
    -- TODO: Use Point.read or Point.readNamed to read these?
    if count is 0
      -- Nothing
    else if count is 1   
      -- Note: This copies from existing segments through bean getters
      if arg0.point
        point     = arg0.point
        handleIn  = arg0.handleIn
        handleOut = arg0.handleOut
      else
        point = arg0
    else if count < 6
      if count is 2 and arg1.x is `nil`
        point = [arg0, arg1]
      else
        point = arg0
        
        -- Doesn't matter if these arguments exist, SegmentPointcreate
        -- produces creates points with (0, 0) otherwise
        handleIn  = arg1
        handleOut = arg2
    else if count is 6
      point     = [arg0, arg1]
      handleIn  = [arg2, arg3]
      handleOut = [arg4, arg5]

    @createPoint @, "_point", point
    @createPoint @, "_handleIn", handleIn
    @createPoint @, "_handleOut", handleOut

  _serialize: (options) =>
    -- If the Segment is linear, only serialize point, otherwise handles too
    Base\serialize (if @isLinear() then @_point else [@_point, @_handleIn, @_handleOut]), options, true

  _changed: (point) =>
    return if !@_path
    
    -- Delegate changes to affected curves if they exist. Check _curves
    -- first to make sure we're not creating it by calling this.getCurve().
    curve = @_path._curves and @getCurve()

    other = nil
    if curve
      curve\_changed()
      
      -- Get the other affected curve, which is the previous one for
      -- _point or _handleIn changing when this segment is _segment1 of
      -- the curve, for all other cases it's the next (e.g. _handleOut
      -- when this segment is _segment2)
      other\_changed() if other = (curve[(if point is @_point or point is @_handleIn and curve._segment1 is this then "getPrevious" else "getNext")]())
    
    @_path\_changed(Change.GEOMETRY)

  getPoint: =>
    @_point

  setPoint: (point) =>
    point = Point\read(arg)
    
    -- Do not replace the internal object but update it instead, so
    -- references to it are kept alive.
    @_point\set point.x, point.y

  getHandleIn: =>
    @_handleIn

  setHandleIn: (point) =>
    point = Point\read(arg)
    
    -- See --setPoint:
    @_handleIn\set point.x, point.y

  getHandleOut: =>
    @_handleOut

  setHandleOut: (point) =>
    point = Point\read(arg)
    
    -- See
    @_handleOut\set point.x, point.y
  
  isLinear: =>
    @_handleIn\isZero() and @_handleOut\isZero()

  setLinear: =>
    @_handleIn\set 0, 0
    @_handleOut\set 0, 0

  _isSelected: (point) =>
    state = @_selectionState

    (if point is @_point then !!(state & SelectionState.POINT) else (if point is @_handleIn then !!(state & SelectionState.HANDLE_IN) else (if point is @_handleOut then !!(state & SelectionState.HANDLE_OUT) else false)))

  _setSelected: (point, selected) =>
    path     = @_path
    selected = !!selected -- convert to boolean
    state    = @_selectionState or 0
    
    -- For performance reasons use array indices to access the various
    -- selection states: 0 = point, 1 = handleIn, 2 = handleOut

    selection = [!!(state & SelectionState.POINT), !!(state & SelectionState.HANDLE_IN), !!(state & SelectionState.HANDLE_OUT)] ----=
    if point is @_point
      if selected    
        -- We're selecting point, deselect the handles
        selection[1] = selection[2] = false
      else
        previous = @getPrevious()
        nnext    = @getNext()
        
        -- When deselecting a point, the handles get selected instead
        -- depending on the selection state of their neighbors.
        selection[1] = previous and (previous._point\isSelected() or previous._handleOut\isSelected())
        selection[2] = nnext and (nnext._point.isSelected() or nnext._handleIn.isSelected())

      selection[0] = selected
    else
      index = (if point is @_handleIn then 1 else 2)

      if !(selection[index] is selected)
        -- When selecting handles, the point get deselected.
        selection[0] = false  if selected
        selection[index] = selected

    @_selectionState = ((if selection[0] then SelectionState.POINT else 0)) | ((if selection[1] then SelectionState.HANDLE_IN else 0)) | ((if selection[2] then SelectionState.HANDLE_OUT else 0)) ----=
    
    -- If the selection state of the segment has changed, we need to let
    -- it's path know and possibly add or remove it from
    -- project._selectedItems
    if path and state isnt @_selectionState
      path\_updateSelection @, state, @_selectionState
      
      -- Let path know that we changed something and the view should be
      -- redrawn
      path\_changed Change.ATTRIBUTE

  isSelected: =>
    @_isSelected @_point

  setSelected: (selected) =>
    @_setSelected @_point, selected

  getIndex: =>
    (if @_index isnt nil then @_index else nil)

  getPath: =>
    @_path or nil

  getCurve: =>
    path  = @_path
    index = @_index

    if path
      -- The last segment of an open path belongs to the last curve
      index -- if not path._closed and index is path._segments.length - 1
      return path\getCurves()[index] or nil
    nil

  getLocation: =>
    curve = @getCurve()
    
    -- Determine whether the parameter for this segment is 0 or 1 based on
    -- whether there is a next curve or not, as --getNext() takes closed into
    -- account and all.
    (if curve then CurveLocation(curve, (if curve\getNext() then 0 else 1)) else nil)

  getNext: =>
    segments = @_path and @_path._segments
    segments and (segments[@_index + 1] or @_path._closed and segments[0]) or nil

  getPrevious: =>
    segments = @_path and @_path._segments
    segments and (segments[@_index - 1] or @_path._closed and segments[segments.length - 1]) or nil
  
  reverse: =>
    Segment(@_point, @_handleOut, @_handleIn)

  remove: =>
    (if @_path then !!@_path\removeSegment(@_index) else false)

  clone: =>
    Segment(@_point, @_handleIn, @_handleOut)

  equals: (segment) =>
    segment is this or segment and @_point\equals(segment._point) and @_handleIn\equals(segment._handleIn) and @_handleOut\equals(segment._handleOut) or false

  _transformCoordinates: (matrix, coords, change) => 
    -- Use matrix.transform version() that takes arrays of multiple
    -- points for largely improved performance, as no calls to
    -- Point.read() and Point constructors are necessary.
    point = @_point
    
    -- If change is true, only transform handles if they are set, as
    -- _transformCoordinates is called only to change the segment, no
    -- to receive the coords.
    -- This saves some computation time. If change is false, always
    -- use the real handles, as we just want to receive a filled
    -- coords array for getBounds().
    handleIn  = (if not change or not @_handleIn\isZero() then @_handleIn else nil)
    handleOut = (if not change or not @_handleOut\isZero() then @_handleOut else nil)

    x         = point._x
    y         = point._y
    i         = 2
    coords[0] = x
    coords[1] = y
    
    -- We need to convert handles to absolute coordinates in order
    -- to transform them.
    if handleIn
      coords[i++] = handleIn._x + x
      coords[i++] = handleIn._y + y

    if handleOut
      coords[i++] = handleOut._x + x
      coords[i++] = handleOut._y + y
    
    -- If no matrix was previded, this was just called to get the coords and
    -- we are done now.
    if matrix
      matrix\_transformCoordinates coords, 0, coords, 0, i / 2

      x = coords[0]
      y = coords[1]

      if change
        -- If change is true, we need to set the new values back
        point._x = x
        point._y = y
        i        = 2

        if handleIn
          handleIn._x = coords[i++] - x
          handleIn._y = coords[i++] - y

        if handleOut
          handleOut._x = coords[i++] - x
          handleOut._y = coords[i++] - y
      else     
        -- We want to receive the results in coords, so make sure
        -- handleIn and out are defined too, even if they're 0
        if !handleIn
          coords[i++] = x
          coords[i++] = y

        if !handleOut
          coords[i++] = x
          coords[i++] = y
    return coords
