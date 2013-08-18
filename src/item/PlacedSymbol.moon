class PlacedSymbol extends Item
  _applyMatrix: false
  
  -- PlacedSymbol uses strokeBounds for bounds
  _boundsGetter:
    getBounds: "getStrokeBounds"

  _boundsSelected: true
  _serializeFields:
    symbol: null

	new: (arg0, arg1) =>
    -- Support two forms of item initialization: Passing one object literal
    -- describing all the different properties to be set, or a symbol (arg0)
    -- and a point where it should be placed (arg1).
    super(arg1 isnt nil and Point\read(arg, 1))
    
    -- If we can handle setting properties through object literal, we're all
    -- set. Otherwise we need to set symbol.
    @setSymbol (if arg0.__class is Symbol then arg0 else Symbol(arg0)) if arg0 and not @_set(arg0)
  
  getSymbol: =>
    @_symbol

  setSymbol: (symbol) =>
    
    -- Remove from previous symbol's instances
    delete @_symbol._instances[@_id]  if @_symbol
    @_symbol = symbol
    
    -- Add to the new one's
    symbol._instances[@_id] = @

  clone: =>
    @_clone PlacedSymbol(@symbol)

  isEmpty: =>
    @_symbol._definition.isEmpty()

   _getBounds: (getter, matrix) =>
    
    -- Redirect the call to the symbol definition to calculate the bounds
    -- TODO: Implement bounds caching through passing on of cacheItem, so
    -- that Symbol--_changed() notification become unnecessary!
    @symbol._definition._getCachedBounds getter, matrix

  _hitTest: (point, options, matrix) =>
    result = @_symbol._definition._hitTest(point, options, matrix)
    
    -- TODO: When the symbol's definition is a path, should hitResult
    -- contain information like HitResult--curve?
    result.item = @ if result
    result

  _draw: (ctx, param) =>
    @symbol._definition.draw ctx, param
