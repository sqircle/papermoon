class CompoundPath extends PathItem
  _serializeFields:
    children: []

  new: (arg0) ->
    super()
    
    -- CompoundPath has children and supports named children.
    @_children      = []
    @_namedChildren = {}
    @addChildren (if isArray(arg0) then arg0 else arg) if arg0 and not @_set(arg0)
  
  insertChildren: insertChildren = (index, items, _preserve) =>
    
    -- Pass on 'path' for _type, to make sure that only paths are added as
    -- children.
    items = insertChildren.base.call(this, index, items, _preserve, "path")
    
    -- All children except for the bottom one (first one in list) are set
    -- to anti-clockwise orientation, so that they appear as holes, but
    -- only if their orientation was not already specified before
    -- (= _clockwise is defined).
    i = 0
    l = not _preserve and items and items.length

    while i < l
      item = items[i]
      item\setClockwise item._index is 0 if item._clockwise is nil
      i++

    items

  reduce: =>
    if @_children.length is 1
      child = @_children[0]
      child\insertAbove @
      @remove()
      return child
    @

  reverse: =>
    children = @_children
    i        = 0
    l        = children.length

    while i < l
      children[i]\reverse()
      i++

  smooth: =>
    i = 0
    l = @_children.length

    while i < l
      @_children[i]\smooth()
      i++

  isClockwise: =>
    child = @getFirstChild()
    child and child\isClockwise()

  setClockwise: (clockwise) =>
    @reverse() if !@isClockwise() is !!clockwise

  getFirstSegment: =>
    first = @getFirstChild()
    first and first\getFirstSegment()

  getLastSegment: =>
    last = @getLastChild()
    last and last\getLastSegment()

  getCurves: =>
    children = @_children
    curves   = []

    i = 0
    l = children.length
    while i < l
      curves = curves\concat(children[i]\getCurves())
      i++
    curves

  getFirstCurve: =>
    first = @getFirstChild()
    first and first\getFirstCurve()

  getLastCurve: =>
    last = @getLastChild()
    last and last\getFirstCurve()

  getArea: =>
    children = @_children
    area     = 0

    i = 0
    l = children.length

    while i < l
      area += children[i]\getArea()
      i++
    area

  getPathData: =>
    children = @_children
    paths    = []

    i = 0
    l = children.length

    while i < l
      paths.push children[i]\getPathData(arg[0])
      i++

    paths.join " "

  _contains: (point) =>
    -- Compound paths are a little complex: In order to determine wether a
    -- point is inside a path or not due to the even-odd rule, we need to
    -- check all the children and count how many intersect. If it's an odd
    -- number, the point is inside the path. Once we know it's inside the
    -- path, _hitTest also needs access to the first intersecting element, 
    -- for the HitResult, so we collect and return a list here.
    children = []

    i = 0
    l = @_children.length
    while i < l
      child = @_children[i]
      children\push child  if child\contains(point)
      i++
    (children.length & 1) is 1 and children

  _hitTest: 
	  _hitTest = (point, options) ->
	    res = _hitTest.base.call(this, point, Base.merge(options,
	      fill: false
	    ))
	    if not res and options.fill and @hasFill()
	      res = @_contains(point)
	      res = (if res then new HitResult("fill", res[0]) else null)
	    res

  _draw: (ctx, param) =>
    children = @_children
    style    = @_style
    
    -- Return early if the compound path doesn't have any children:
    return if children.length is 0
    ctx\beginPath()

    param = param\extend(compound: true)

    i = 0
    l = children.length
    while i < l
      children[i]\draw ctx, param
      i++

    if !param.clip
      @_setStyles ctx
      ctx\fill()   if style\getFillColor()
      ctx\stroke() if style\getStrokeColor()

  getCurrentPath = (that) ->
    assert(that._children.length)
    that._children[that._children.length - 1]

  fields:
    -- Note: Documentation for these methods is found in PathItem, as they
    -- are considered abstract methods of PathItem and need to be defined in
    -- all implementing classes.
    moveTo: (point) ->
      path = Path()
      @addChild(path)
      path\moveTo(arg)

    moveBy: (point) ->
      @moveTo getCurrentPath(@)\getLastSegment()._point\add(Point\read(arg))

    closePath: ->
      @getCurrentPath(@)\closePath()
