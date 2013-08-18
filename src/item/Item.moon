class Item extends Base
  _applyMatrix: true
  _boundsSelected: false
  _locked: false
	_visible: true
	_blendMode: "normal"
	_opacity: 1
	_guide: false

  _serializeFields:
    name: null
    matrix: Matrix()
    locked: false
    visible: true
    blendMode: "normal"
    opacity: 1
    guide: false
    clipMask: false
    data: {}

  new: (point) =>
    -- Define this Item's unique id.
    @_id = Item._id = (Item._id or 0) + 1
    
    -- If _project is already set, the item was already moved into the DOM
    -- hierarchy. Used by Layer, where it's added to project.layers instead
    if !@_project
      project = paper.project
      layer = project.activeLayer
      if layer
        layer\addChild(@)
      else
        @_setProject project
    @_style  = Style(@_project._currentStyle, @)
    @_matrix = Matrix()
    @_matrix.translate point if point

  _events: ->
  	-- Flags defining which native events are required by which Paper events
    -- as required for counting amount of necessary natives events.
    -- The mapping is native -> virtual
    mouseFlags =
      mousedown:
        mousedown: 1
        mousedrag: 1
        click: 1
        doubleclick: 1

      mouseup:
        mouseup: 1
        mousedrag: 1
        click: 1
        doubleclick: 1

      mousemove:
        mousedrag: 1
        mousemove: 1
        mouseenter: 1
        mouseleave: 1
    
    -- Entry for all mouse events in the _events list
    mouseEvent =
      install: (type) ->
        -- If the view requires counting of installed mouse events,
        -- increase the counters now according to mouseFlags
        counters = @_project.view._eventCounters
        if counters
          for key of mouseFlags
            counters[key] = (counters[key] or 0) + (mouseFlags[key][type] or 0)

      uninstall: (type) ->
        -- If the view requires counting of installed mouse events,
        -- decrease the counters now according to mouseFlags
        counters = @_project.view._eventCounters
        if counters
          for key of mouseFlags
            counters[key] -= mouseFlags[key][type] or 0


  _changed: (flags) =>
    if flags & ChangeFlag.GEOMETRY
      
      -- Clear cached bounds and position whenever geometry changes
      delete @_bounds

      delete @_position
    
    -- Clear cached bounds of all items that this item contributes to.
    -- We call this on the parent, since the information is cached on
    -- the parent, see getBounds().
    @_parent._clearBoundsCache() if @_parent and (flags & (ChangeFlag.GEOMETRY | ChangeFlag.STROKE))
    
    -- Clear cached bounds of all items that this item contributes to.
    -- We don't call this on the parent, since we're already the parent
    -- of the child that modified the hierarchy (that's where these
    -- HIERARCHY notifications go)
    @_clearBoundsCache() if flags & ChangeFlag.HIERARCHY

    @_project._needsRedraw = true if flags & ChangeFlag.APPEARANCE
    
    -- If this item is a symbol's definition, notify it of the change too
    @_parentSymbol._changed flags  if @_parentSymbol
    
    -- Have project keep track of changed items, so they can be iterated.
    -- This can be used for example to update the SVG tree. Needs to be
    -- activated in Project
    if @_project._changes
      entry = @_project._changesById[@_id]
      if entry
        entry.flags |= flags
      else
        entry =
          item: this
          flags: flags

        @_project._changesById[@_id] = entry
        @_project._changes.push entry

  set: (props) =>
    @_set props if props

  getId: =>
    @_id

  getType: =>
    @_type

  getName: =>
    @_name

  setName: (name, unique) =>
    
    -- Note: Don't check if the name has changed and bail out if it has not,
    -- because setName is used internally also to update internal structures
    -- when an item is moved from one parent to another.
    
    -- If the item already had a name, remove the reference to it from the
    -- parent's children object:
    @_removeFromNamed() if @_name
    if name and @_parent
      children      = @_parent._children
      namedChildren = @_parent._namedChildren
      orig          = name
      i             = 1
      
      -- If unique is true, make sure we're not overriding other names
      name = orig + " " + (i++)  while unique and children[name]
      (namedChildren[name] = namedChildren[name] or []).push @
      children[name] = @
    @_name = name or nil
    @_changed ChangeFlag.ATTRIBUTE


  getStyle: =>
    @_style

  setStyle: (style) =>
    
    -- Don't access _style directly so Path#getStyle() can be overriden for
    -- CompoundPaths.
    @getStyle().set style

  hasFill: =>
    !!@getStyle().getFillColor()

  hasStroke: =>
    !!@getStyle().getStrokeColor()

  isSelected: =>
    if @_children
      i = 0
      l = @_children.length

      while i < l
        return true if @_children[i].isSelected()
        i++
    @_selected

  setSelected: (selected) =>
    
    -- Don't recursively call #setSelected() if it was called with
    -- noChildren set to true, see #setFullySelected().
    if @_children and not arg[1]
      i = 0
      l = @_children.length

      while i < l
        @_children[i].setSelected selected
        i++

    if !(selected = !!selected) is @_selected
      @_selected = selected
      @_project._updateSelection @
      @_changed Change.ATTRIBUTE

  _selected: false
  isFullySelected: =>
    if @_children and @_selected
      i = 0
      l = @_children.length

      while i < l
        return false if !@_children[i].isFullySelected()
        i++
      return true
    
    -- If there are no children, this is the same as #selected
    @_selected

  setFullySelected: (selected) =>
    if @_children
      i = 0
      l = @_children.length

      while i < l
        @_children[i].setFullySelected selected
        i++
    
    -- Pass true for hidden noChildren argument
    @setSelected selected, true

  isClipMask: =>
    @_clipMask

  setClipMask: (clipMask) =>
    
    -- On-the-fly conversion to boolean:
    if !@_clipMask is (clipMask = !!clipMask)
      @_clipMask = clipMask
      if clipMask
        @setFillColor null
        @setStrokeColor null
      @_changed Change.ATTRIBUTE
      
      -- Tell the parent the clipping mask has changed
      @_parent._changed ChangeFlag.CLIPPING if @_parent 

  _clipMask: false

  getData: =>
    @_data = {} if !@_data
    @_data

  setData: (data) =>
    @_data = data

  getPosition: -> 
    
    -- Cache position value.
    -- Pass true for dontLink in getCenter(), so receive back a normal point
    pos = @_position or (@_position = @getBounds().getCenter(true))
    
    -- Do not cache LinkedPoints directly, since we would not be able to
    -- use them to calculate the difference in #setPosition, as when it is
    -- modified, it would hold new values already and only then cause the
    -- calling of #setPosition.
    (if arg[0] then pos else LinkedPoint\create(@, "setPosition", pos.x, pos.y))

  setPosition: (point) =>
    
    -- Calculate the distance to the current position, by which to
    -- translate the item. Pass true for dontLink, as we do not need a
    -- LinkedPoint to simply calculate this distance.
    @translate Point\read(arg).subtract(@getPosition(true))

  getMatrix: =>
    @_matrix

  setMatrix: (matrix) =>
    
    -- Use Matrix#initialize to easily copy over values.
    @_matrix.initialize matrix
    @_changed Change.GEOMETRY

  _getCachedBounds: (getter, matrix, cacheItem) =>
    
    -- See if we can cache these bounds. We only cache the bounds
    -- transformed with the internally stored _matrix, (the default if no
    -- matrix is passed).
    cache = (not matrix or matrix\equals(@_matrix)) and getter
    
    -- Set up a boundsCache structure that keeps track of items that keep
    -- cached bounds that depend on this item. We store this in our parent,
    -- for multiple reasons:
    -- The parent receives HIERARCHY change notifications for when its
    -- children are added or removed and can thus clear the cache, and we
    -- save a lot of memory, e.g. when grouping 100 items and asking the
    -- group for its bounds. If stored on the children, we would have 100
    -- times the same structure.
    -- Note: This needs to happen before returning cached values, since even
    -- then, _boundsCache needs to be kept up-to-date.
    if cacheItem and @_parent
      
      -- Set-up the parent's boundsCache structure if it does not
      -- exist yet and add the cacheItem to it.
      id = cacheItem._id
      ref = @_parent._boundsCache = @_parent._boundsCache or
        
        -- Use both a hashtable for ids and an array for the list,
        -- so we can keep track of items that were added already
        ids: {}
        list: []

      if !ref.ids[id]
        ref.list.push cacheItem
        ref.ids[id] = cacheItem
    return @_bounds[cache].clone() if cache and @_bounds and @_bounds[cache]
    
    -- If the result of concatinating the passed matrix with our internal
    -- one is an identity transformation, set it to null for faster
    -- processing
    identity = @_matrix.isIdentity()
    matrix = (if not matrix or matrix.isIdentity() then (if identity then null else @_matrix) else (if identity then matrix else matrix.clone().concatenate(@_matrix)))
    
    -- If we're caching bounds on this item, pass it on as cacheItem, so the
    -- children can setup the _boundsCache structures for it.
    bounds = @_getBounds(getter, matrix, (if cache then this else cacheItem))
    
    -- If we can cache the result, update the _bounds cache structure
    -- before returning
    if cache
      @_bounds        = {} if !@_bounds
      @_bounds[cache] = bounds.clone()
    bounds

  _clearBoundsCache: =>
    if @_boundsCache
      i    = 0
      list = @_boundsCache.list
      l    = list.length

      while i < l
        item = list[i]
        delete item._bounds

        -- We need to recursively call _clearBoundsCache, because if the
        -- cache for this item's children is not valid anymore, that
        -- propagates up the DOM tree.
        item._clearBoundsCache() if item isnt @ and item._boundsCache
        i++
      delete @_boundsCache

  _getBounds: (getter, matrix, cacheItem) =>
    -- Note: We cannot cache these results here, since we do not get
    -- _changed() notifications here for changing geometry in children.
    -- But cacheName is used in sub-classes such as PlacedSymbol and Raster.
    children = @_children
    
    -- TODO: What to return if nothing is defined, e.g. empty Groups?
    -- Scriptographer behaves weirdly then too.
    return Rectangle() if not children or children.length is 0
   
    x1 = Infinity
    x2 = -x1
    y1 = x1
    y2 = x2

    i = 0
    l = children.length

    while i < l
      child = children[i]
      if child._visible and not child.isEmpty()
        rect = child._getCachedBounds(getter, matrix, cacheItem)
        x1 = Math.min(rect.x, x1)
        y1 = Math.min(rect.y, y1)
        x2 = Math.max(rect.x + rect.width, x2)
        y2 = Math.max(rect.y + rect.height, y2)

      i++

    (if isFinite(x1) then Rectangle(x1, y1, x2 - x1, y2 - y1) else Rectangle())

  setBounds: (rect) =>
    rect   = Rectangle\read(arg)
    bounds = @getBounds()
    matrix = Matrix()
    center = rect\getCenter()
    
    -- Read this from bottom to top:
    -- Translate to new center:
    matrix\translate center
    
    -- Scale to new Size, if size changes and avoid divisions by 0:
    matrix\scale (if bounds.width isnt 0 then rect.width / bounds.width else 1), (if bounds.height isnt 0 then rect.height / bounds.height else 1)  if rect.width isnt bounds.width or rect.height isnt bounds.height
    
    -- Translate to bounds center:
    center = bounds\getCenter()
    matrix\translate -center.x, -center.y
    
    -- Now execute the transformation
    @transform matrix

  getProject: =>
    @_project

  _setProject: (project) =>
    if !@_project is project
      @_project = project
      if @_children
        i = 0
        l = @_children.length

        while i < l
          @_children[i]._setProject project
          i++

  getLayer: =>
    parent = @
    return parent if parent.__class Layer while parent = parent._parent
    nil

  getParent: =>
    @_parent

  setParent: (item) =>
    item.addChild @

  getChildren: =>
    @_children

  setChildren: (items) =>
    @removeChildren()
    @addChildren items

  getFirstChild: =>
    @_children and @_children[0] or nil

  getLastChild: ->
    @_children and @_children[@_children.length - 1] or nil

  getNextSibling: =>
    @_parent and @_parent._children[@_index + 1] or null

  getPreviousSibling: =>
    @_parent and @_parent._children[@_index - 1] or null

  getIndex: =>
    @_index

  isInserted: =>
    (if @_parent then @_parent.isInserted() else false)

  clone: ->
    @_clone Item!

	_clone: (copy) =>
		-- Copy over style
    copy.setStyle @_style
    
    -- If this item has children, clone and append each of them:
    if @_children
      
      -- Clone all children and add them to the copy. tell #addChild we're
      -- cloning, as needed by CompoundPath#insertChild().
      i = 0
      l = @_children.length

      while i < l
        copy.addChild @_children[i].clone(), true
        i++
    
    -- Only copy over these fields if they are actually defined in 'this'
    -- TODO: Consider moving this to Base once it's useful in more than one
    -- place
    keys = ["_locked", "_visible", "_blendMode", "_opacity", "_clipMask", "_guide"]
    i    = 0
    l    = keys.length

    while i < l
      key = keys[i]
      copy[key] = this[key]  if @hasOwnProperty(key)
      i++
    
    -- Use Matrix#initialize to easily copy over values.
    copy._matrix.initialize @_matrix
    
    -- Copy over the selection state, use setSelected so the item
    -- is also added to Project#selectedItems if it is selected.
    copy.setSelected @_selected
    
    -- Clone the name too, but make sure we're not overriding the original
    -- in the same parent, by passing true for the unique parameter.
    copy.setName @_name, true if @_name
    copy

  copyTo: (itemOrProject) =>
    copy = @clone()
    if itemOrProject.layers
      itemOrProject.activeLayer.addChild copy
    else
      itemOrProject.addChild copy
    copy

  rasterize: (resolution)  =>
    bounds = @getStrokeBounds()
    scale  = (resolution or 72) / 72
    canvas = CanvasProvider\getCanvas(bounds\getSize()\multiply(scale))
  
    ctx    = canvas.getContext("2d")
    matrix = Matrix()\scale(scale)\translate(-bounds.x, -bounds.y)
    ctx\save()
    matrix\applyToContext ctx
    
    -- See Project#draw() for an explanation of Base.merge()
    @draw ctx, Base\merge(transforms: [matrix])
    raster = Raster(canvas)
    raster\setBounds bounds
    ctx\restore()
    
    raster

  contains: (point) ->
    
    # See CompoundPath#_contains() for the reason for !!
    !!@_contains(@_matrix._inverseTransform(Point.read(arguments_)))

  _contains: (point) =>
    if @_children
      i = @_children.length - 1

      while i >= 0
        return true if @_children[i].contains(point)
        i--
      return false
    
    -- We only implement it here for items with rectangular content,
    -- for anything else we need to override #contains()
    -- TODO: There currently is no caching for the results of direct calls
    -- to this._getBounds('getBounds') (without the application of the
    -- internal matrix). Performance improvements could be achieved if
    -- these were cached too. See #_getCachedBounds().
    point\isInside @_getBounds("getBounds")

  hitTest: (point, options) =>
    checkBounds = (type, part) ->
      pt = bounds["get" + part]()
      
      -- TODO: We need to transform the point back to the coordinate
      -- system of the DOM level on which the inquiry was started!
      if point.getDistance(pt) < options.tolerance
        HitResult(type, that,
          name: Base.hyphenate(part)
          point: pt
        )
    return nil if @_locked

    point   = Point\read(arg)
    options = HitResult\getOptions(Base\read(arg))
    
    -- Check if the point is withing roughBounds + tolerance, but only if
    -- this item does not have children, since we'd have to travel up the
    -- chain already to determine the rough bounds.
    return nil if not @_children and not @getRoughBounds().expand(options.tolerance)._containsPoint(point)
    
    -- Transform point to local coordinates but use untransformed point
    -- for bounds check above.
    point = @_matrix._inverseTransform(point)
    
    -- Ignore top level layers:
    if (options.center or options.bounds) and not (@.__class == Layer and not @_parent)
      
      -- Don't get the transformed bounds, check against transformed
      -- points instead
      bounds = @getBounds()
      that = @
      
      -- TODO: Move these into a private scope
      points = ["TopLeft", "TopRight", "BottomLeft", "BottomRight", "LeftCenter", "TopCenter", "RightCenter", 
					      "BottomCenter"]
      res    = nil
      return res if options.center and (res = checkBounds("center", "Center"))
      if options.bounds
        i = 0

        while i < 8
          return res  if res = checkBounds("bounds", points[i])
          i++
    
    -- TODO: Support option.type even for things like CompoundPath where
    -- children are matched but the parent is returned.
    
    -- Filter for guides or selected items if that's required
    (if @_children or not (options.guides and not @_guide or options.selected and not @_selected) then @_hitTest(point, options) else null)
 
  _hitTest: (point, options) =>
    if @_children
      
      -- Loop backwards, so items that get drawn last are tested first
      i = @_children.length - 1

      while i >= 0
        res = @_children[i].hitTest(point, options)
        return res  if res
        i--
    else 
    	HitResult("fill", @) if @hasFill() and @_contains(point)

  addChild: (item, _preserve) =>
    @insertChild nil, item, _preserve

  insertChild: (index, item, _preserve) =>
    res = @insertChildren(index, [item], _preserve)
    res and res[0]

  addChildren: (items, _preserve) =>
    @insertChildren @_children.length, items, _preserve

  insertChildren: (index, items, _preserve, _type) =>
    
    -- CompoundPath#insertChildren() requires _preserve and _type:
    -- _preserve avoids changing of the children's path orientation
    -- _type enforces the inserted type.
    children = @_children
    if children and items and items.length > 0
      
      -- We need to clone items because it might be
      -- an Item#children array. Also, we're removing elements if they
      -- don't match _type. Use Array.prototype.slice becaus items can be
      -- an arguments object.
      items = items.copy()
      
      -- Remove the items from their parents first, since they might be
      -- inserted into their own parents, affecting indices.
      -- Use the loop also to filter out wrong _type.
      i = items.length - 1

      while i >= 0
        item = items[i]
        if _type and item._type isnt _type
          items.splice i, 1
        else
          item._remove true
        i--

      Base\splice children, items, index, 0
      i = 0
      l = items.length

      while i < l
        item = items[i]
        item._parent = @
        item._setProject @_project
        
        -- Setting the name again makes sure all name lookup structures
        -- are kept in sync.
        item.setName item._name if item._name
        i++
      @_changed Change.HIERARCHY
    else
      items = nil
    items

  insertAbove: (item, _preserve) =>
    index = item._index
    index++ if item._parent is @_parent and index < @_index
    item._parent.insertChild index, @, _preserve

  insertBelow: (item, _preserve) =>
    index = item._index
    index = index - 1  if item._parent is @_parent and index > @_index
    item._parent.insertChild index, @, _preserve

  sendToBack: =>
    @_parent.insertChild 0, this

  bringToFront: =>
    @_parent.addChild @

  _removeFromNamed: =>
    children      = @_parent._children
    namedChildren = @_parent._namedChildren
    name          = @_name
    namedArray    = namedChildren[name]
    index         = (if namedArray then namedArray.indexOf(this) else -1)
    return if index is -1
    
    -- Remove the named reference
    delete children[name] if children[name] is this
    
    -- Remove this entry
    namedArray.splice index, 1
    
    -- If there are any items left in the named array, set
    -- the last of them to be this.parent.children[this.name]
    if namedArray.length
      children[name] = namedArray[namedArray.length - 1]
    else
      -- Otherwise delete the empty array
      delete namedChildren[name]

  _remove: (notify) =>
    if @_parent
      @_removeFromNamed()  if @_name
      Base\splice @_parent._children, null, @_index, 1 if @_index?
      
      -- Notify parent of changed hierarchy
      @_parent._changed Change.HIERARCHY if notify
      @_parent = nil
      return true
    false

  remove: =>
    @_remove true

  removeChildren: (src, to) =>
    return nil if !@_children
    src = src or 0
    to = Base\pick(to, @_children.length)
    
    -- Use Base.splice(), wich adjusts #_index for the items above, and
    -- deletes it for the removed items. Calling #_remove() afterwards is
    -- fine, since it only calls Base.splice() if #_index is set.
    removed = Base\splice(@_children, null, src, to - src)
    i       = removed.length - 1

    while i >= 0
      removed[i]._remove false
      i--
    @_changed Change.HIERARCHY if removed.length > 0 
    removed

  reverseChildren: =>
    if @_children
      @_children.reverse()
      
      -- Adjust inidces
      i = 0
      l = @_children.length

      while i < l
        @_children[i]._index = i
        i++
      @_changed Change.HIERARCHY

  isEditable: =>
    item = @
    while item
      return false if not item._visible or item._locked
      item = item._parent
    true

  _getOrder: (item) =>
    
    -- Private method that produces a list of anchestors, starting with the
    -- root and ending with the actual element as the last entry.
    getList = (item) =>
      list = []
      loop
        list.unshift item
        break unless item = item._parent
      list

    list1 = getList(this)
    list2 = getList(item)

    i = 0
    l = math.min(list1.length, list2.length)

    while i < l
      -- Found the position in the parents list where the two start
      -- to differ. Look at who's above who.
      return (if list1[i]._index < list2[i]._index then 1 else -1)  unless list1[i] is list2[i]
      i++
    0

  hasChildren: =>
    @_children and @_children.length > 0

  isAbove: (item) =>
    @_getOrder(item) is -1

  isBelow: (item) =>
    @_getOrder(item) is 1

  isParent: (item) =>
    @_parent is item

  isChild: (item) =>
    item and item._parent is @

  isDescendant: (item) =>
    parent = @
    return true if parent is item while parent = parent._parent
    false

  isAncestor: (item) =>
    (if item then item\isDescendant(@) else false)

  isGroupedWith: (item) =>
    parent = @_parent
    while parent
      
      -- Find group parents. Check for parent._parent, since don't want
      -- top level layers, because they also inherit from Group
      return true if parent._parent and /^(group|layer|compound-path)$/.test(parent._type) and item\isDescendant(parent)
      
      -- Keep walking up otherwise
      parent = parent._parent
    false

  scale: (hor, ver, center) => 
    
    -- See Matrix#scale for explanation of this:
    if arg.length < 2 or typeof(ver) is "table"
      center = ver
      ver    = hor

    @transform Matrix()\scale(hor, ver, center or @getPosition(true))

  translate: (delta) =>
    mx = Matrix()
    @transform mx\translate\apply(mx, arg)

  rotate: (angle, center) =>
    @transform Matrix()\rotate(angle, center or @getPosition(true))

  shear: (hor, ver, center) =>
    
    -- See Matrix#scale for explanation of this:
    if arg.length < 2 or typeof(ver) is "table"
      center = ver
      ver    = hor

    @transform Matrix()\shear(hor, ver, center or @getPosition(true))
  
  transform: (matrix) =>
    
    -- Calling _changed will clear _bounds and _position, but depending
    -- on matrix we can calculate and set them again.
    bounds   = @_bounds
    position = @_position
    
    -- Simply preconcatenate the internal matrix with the passed one:
    @_matrix\preConcatenate matrix
    
    -- Call applyMatrix if we need to directly apply the accumulated
    -- transformations to the item's content.
    @applyMatrix false if @_applyMatrix or arg[1]
    
    -- We always need to call _changed since we're caching bounds on all
    -- items, including Group.
    @_changed Change.GEOMETRY
    
    -- Detect matrices that contain only translations and scaling
    -- and transform the cached _bounds and _position without having to
    -- fully recalculate each time.
    if bounds and matrix\getRotation() % 90 is 0
      
      -- Transform the old bound by looping through all the cached bounds
      -- in _bounds and transform each.
      for key of bounds
        rect = bounds[key]
        matrix\_transformBounds rect, rect
      
      -- If we have cached bounds, update _position again as its 
      -- center. We need to take into account _boundsGetter here too, in 
      -- case another getter is assigned to it, e.g. 'getStrokeBounds'.
      getter     = @_boundsGetter
      rect       = bounds[getter and getter\getBounds! or getter or "getBounds"]
      @_position = rect\getCenter(true)  if rect
      @_bounds   = bounds
    
    --Transform position as well.
    else 
    	@_position = matrix\_transformPoint(position, position)  if position
    
    -- Allow chaining here, since transform() is related to Matrix functions
    @

  _transformContent: (matrix, applyMatrix) =>
    children = @_children
    if children and children.length > 0
      i = 0
      l = children.length

      while i < l
        children[i\transform matrix, applyMatrix
        i++
      true

  applyMatrix: (_dontNotify) =>
    
    -- Call #_transformContent() with the internal _matrix and pass true for
    -- applyMatrix. Application is not possible on Raster, PointText,
    -- PlacedSymbol, since the matrix is where the actual location /
    -- transformation state is stored.
    -- Pass on the transformation to the content, and apply it there too,
    -- by passing true for the 2nd hidden parameter.
    matrix = @_matrix
    if @_transformContent(matrix, true)
      
      -- When the matrix could be applied, we also need to transform
      -- color styles with matrices (only gradients so far):
      style = @_style
      
      -- pass true for dontMerge so we don't recursively transform
      -- styles on groups' children.
      fillColor   = style\getFillColor(true)
      strokeColor = style\getStrokeColor(true)
      fillColor\transform matrix   if fillColor
      strokeColor\transform matrix if strokeColor
      
      -- Reset the internal matrix to the identity transformation if it
      -- was possible to apply it.
      matrix\reset()

    @_changed Change.GEOMETRY if !_dontNotify

  fitBounds: (rectangle, fill) ->
    
    -- TODO: Think about passing options with various ways of defining
    -- fitting.
    rectangle = Rectangle\read(arg)
    bounds    = @getBounds()
    itemRatio = bounds.height / bounds.width
    rectRatio = rectangle.height / rectangle.width

    scale     = (if ((if fill then itemRatio > rectRatio else itemRatio < rectRatio)) then rectangle.width / bounds.width else rectangle.height / bounds.height)
    newBounds = Rectangle(new Point(), Size(bounds.width * scale, bounds.height * scale))
    newBounds\setCenter rectangle\getCenter()
    @setBounds newBounds

  _setStyles: (ctx) =>
    
    -- We can access internal properties since we're only using this on
    -- items without children, where styles would be merged.
    style        = @_style
    width        = style\getStrokeWidth()
    join         = style\getStrokeJoin()
    cap          = style\getStrokeCap()
    limit        = style\getMiterLimit()
    fillColor    = style\getFillColor()
    strokeColor  = style\getStrokeColor()
    dashArray    = style\getDashArray()
    dashOffset   = style\getDashOffset()

    ctx.lineWidth = width  if width?
    ctx.lineJoin = join  if join
    ctx.lineCap = cap  if cap
    ctx.miterLimit = limit  if limit
    ctx.fillStyle = fillColor.toCanvasStyle(ctx)  if fillColor
    if strokeColor
      ctx.strokeStyle = strokeColor.toCanvasStyle(ctx)
      if paper.support.nativeDash and dashArray and dashArray.length
        if "setLineDash" of ctx
          ctx.setLineDash dashArray
          ctx.lineDashOffset = dashOffset
        else
          ctx.mozDash       = dashArray
          ctx.mozDashOffset = dashOffset
    
    -- If the item only defines a strokeColor or a fillColor, draw it
    -- directly with the globalAlpha set, otherwise we will do it later when
    -- we composite the temporary canvas.
    ctx.globalAlpha = @_opacity  if not fillColor or not strokeColor

	draw: (ctx, param) ->
    return  if not @_visible or @_opacity is 0
    
    # Each time the project gets drawn, it's _drawCount is increased.
    # Keep the _drawCount of drawn items in sync, so we have an easy
    # way to filter out selected items that are not being drawn, e.g.
    # because they are currently not part of the DOM.
    @_drawCount = @_project._drawCount
    
    # Keep calculating the current global matrix, by keeping a history
    # and pushing / popping as we go along.
    transforms = param.transforms
    parentMatrix = transforms[transforms.length - 1]
    globalMatrix = parentMatrix.clone().concatenate(@_matrix)
    transforms.push @_globalMatrix = globalMatrix
    
    # If the item has a blendMode or is defining an opacity, draw it on
    # a temporary canvas first and composite the canvas afterwards.
    # Paths with an opacity < 1 that both define a fillColor
    # and strokeColor also need to be drawn on a temporary canvas
    # first, since otherwise their stroke is drawn half transparent
    # over their fill.
    # Exclude Raster items since they never draw a stroke and handle
    # opacity by themselves (they also don't call _setStyles)
    blending = @_blendMode isnt "normal"
    parentCtx = undefined
    itemOffset = undefined
    prevOffset = undefined
    if blending or @_opacity < 1 and @_type isnt "raster" and (@_type isnt "path" or @hasFill() and @hasStroke())
      
      # Apply the paren't global matrix to the calculation of correct
      # bounds.
      bounds = @getStrokeBounds(parentMatrix)
      return  if not bounds.width or not bounds.height
      
      # Store previous offset and save the parent context, so we can
      # draw onto it later
      prevOffset = param.offset
      
      # Floor the offset and ceil the size, so we don't cut off any
      # antialiased pixels when drawing onto the temporary canvas.
      itemOffset = param.offset = bounds.getTopLeft().floor()
      
      # Set ctx to the context of the temporary canvas,
      # so we draw onto it, instead of the parentCtx
      parentCtx = ctx
      ctx = CanvasProvider.getContext(bounds.getSize().ceil().add(new Size(1, 1)))
    ctx.save()
    
    # Translate the context so the topLeft of the item is at (0, 0)
    # on the temporary canvas.
    ctx.translate -itemOffset.x, -itemOffset.y  if parentCtx
    
    # Apply globalMatrix when blitting into temporary canvas.
    ((if parentCtx then globalMatrix else @_matrix)).applyToContext ctx
    
    # If we're blending and a clipItem is defined for the current rendering
    # loop, we need to draw the clip item again into the separate canvas.
    param.clipItem.draw ctx, param.extend(clip: true)  if blending and param.clipItem
    @_draw ctx, param
    ctx.restore()
    transforms.pop()
    ctx.clip()  if param.clip
    
    # If a temporary canvas was created before, composite it onto the
    # parent canvas:
    if parentCtx
      
      # Restore previous offset.
      param.offset = prevOffset
      
      # If the item has a blendMode, use BlendMode#process to
      # composite its canvas on the parentCanvas.
      if blending
        
        # The pixel offset of the temporary canvas to the parent
        # canvas.
        BlendMode.process @_blendMode, ctx, parentCtx, @_opacity, itemOffset.subtract(prevOffset)
      else
        
        # Otherwise just set the globalAlpha before drawing the
        # temporary canvas on the parent canvas.
        parentCtx.save()
        
        # Reset transformations, since we're blitting and pixel
        # scale and with a given offset.
        parentCtx.setTransform 1, 0, 0, 1, 0, 0
        parentCtx.globalAlpha = @_opacity
        parentCtx.drawImage ctx.canvas, itemOffset.x, itemOffset.y
        parentCtx.restore()
      
      # Return the temporary context, so it can be reused
      CanvasProvider.release ctx

  removeOn: (obj) ->
    for name of obj
      if obj[name]
        key = "mouse" + name
        project = @_project
        sets = project._removeSets = project._removeSets or {}
        sets[key] = sets[key] or {}
        sets[key][@_id] = this
    this
