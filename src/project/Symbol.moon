class Symbol extends Base
  new: (item, dontCenter) =>
    -- Define this Symbols's unique id.
    @_id     = Symbol._id = (Symbol._id or 0) + 1
    @project = paper.project
    @project.symbols\push this
    @setDefinition item, dontCenter  if item
    
    -- Hash to keep track of placed instances
    @_instances = {}

  _serialize: (options, dictionary) =>
    dictionary\add @, ->
      Base\serialize [@constructor.name, @_definition], options, false, dictionary

  _changed: (flags) =>
    -- Notify all PlacedSymbols of the change in our definition, so they
    -- can clear cached bounds.
    Base\each @_instances, (item) ->
      item._changed flags

  getDefinition: =>
    @_definition

  setDefinition: (item) => --, dontCenter   
    -- Make sure we're not steatling another symbol's definition
    item = item\clone() if item._parentSymbol
    
    -- Remove previous definition's reference to this symbol
    delete @_definition._parentSymbol if @_definition
    @_definition = item
    
    -- Remove item from DOM, as it's embedded in Symbol now.
    item\remove()
    item\setSelected false
    
    -- Move position to 0, 0, so it's centered when placed.
    item\setPosition(Point()) if arg[1]
    item._parentSymbol = @
    @_changed Change.GEOMETRY

  place: (position) =>
    PlacedSymbol(@, position)

  clone: =>
    Symbol(@_definition\clone())
