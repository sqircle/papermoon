class Group extends Item
  _serializeFields:
    children: []

  new: (arg0) =>
    super\new(arg)
    
    -- Allow Group to have children and named children
    @_children      = []
    @_namedChildren = {}
    @addChildren (if isArray(arg0) then arg0 else arg) if arg0 and not @_set(arg0)

  _changed: _changed = (flags) =>
    _changed\base(flags)
        
    --- Apply matrix now that we have content.
    @applyMatrix() if flags & ChangeFlag.HIERARCHY and not @_matrix.isIdentity()

    -- Clear cached clip item whenever hierarchy changes
    delete @_clipItem if flags & (ChangeFlag.HIERARCHY | ChangeFlag.CLIPPING)

  _getClipItem: =>
    -- Allow us to set _clipItem to null when none is found and still return
    -- it as a defined value without searching again
    return @_clipItem if @_clipItem isnt nil
    i = 0
    l = @_children.length

    while i < l
      child = @_children[i]
      return @_clipItem = child if child._clipMask
      i++
    
    -- Make sure we're setting _clipItem to null so it won't be searched for
    -- nex time.
    @_clipItem = nil

  isClipped: =>
    !!@_getClipItem()

  setClipped: (clipped) =>
    child = @getFirstChild()
    child\setClipMask clipped if child

  _draw: (ctx, param) =>
    clipItem = param.clipItem = @_getClipItem()
    clipItem\draw ctx, param\extend(clip: true) if clipItem
    i = 0
    l = @_children.length

    while i < l
      item = @_children[i]
      item\draw ctx, param if item isnt clipItem
      i++

    param.clipItem = nil