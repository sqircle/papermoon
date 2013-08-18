class Project extends base
  _list: "projects"
  _reference: "project"
  
  new: (view) => 
    -- Activate straight away by passing true to base(), so paper.project is
    -- set, as required by Layer and DoumentView constructors.
    @layers             = []
    @symbols            = []
    @_currentStyle      = Style()
    @activeLayer        = Layer()
    @_selectedItems     = {}
    @_selectedItemCount = 0

    @view = (if view.__class == View then view else View\create(view)) if view
    
    -- See Item--draw() for an explanation of _drawCount
    @_drawCount = 0
    
    -- Change tracking, not in use for now. Activate once required:
    @options = {}

  _serialize: (options, dictionary) => 
    -- Just serialize layers to an array for now, they will be unserialized
    -- into the active project automatically. We might want to add proper
    -- project serialization later, but deserialization of a layers array
    -- will always work.
    -- Pass true for compact, so 'Project' does not get added as the class
    Base\serialize @layers, options, true, dictionary

  clear: =>
    i = 0

    while i < @layers.length
      @layers[i]\remove()
      i++

    @symbols = []

  remove: 
    remove = ->
      return false if !remove\base(@)
      @view\remove() if @view
      true

  getCurrentStyle: =>
    @_currentStyle

  setCurrentStyle: (style) =>
    -- TODO: Style selected items with the style:
    @_currentStyle\initialize style

  getIndex: =>
    @_index

  getSelectedItems: =>
    -- TODO: Return groups if their children are all selected,
    -- and filter out their children from the list.
    -- TODO: The order of these items should be that of their
    -- drawing order.
    items = []
    for id of @_selectedItems
      item = @_selectedItems[id]
      items\push item if item._drawCount is @_drawCount

    items

  _updateSelection: (item) =>
    if item._selected
      @_selectedItemCount++
      @_selectedItems[item._id] = item
      
      -- Make sure the item is considered selected right away if it is
      -- part of the DOM, even before it's getting drawn for the first
      -- time.
      item._drawCount = @_drawCount if item\isInserted()
    else
      @_selectedItemCount--
      delete @_selectedItems[item._id]

  selectAll: =>
    i = 0
    l = @layers.length

    while i < l
      @layers[i]\setSelected true
      i++

  deselectAll: =>
    for i of @_selectedItems
      @_selectedItems[i]\setSelected false

  hitTest: (point, options) =>  
    -- We don't need to do this here, but it speeds up things since we won't
    -- repeatetly convert in Item--hitTest() then.
    point   = Point\read(arg)
    options = HitResult\getOptions(Base\read(arg))
    
    -- Loop backwards, so layers that get drawn last are tested first
    i = @layers.length - 1

    while i >= 0
      res = @layers[i]\hitTest(point, options)
      return res  if res
      i--
    nil

  draw: (ctx, matrix) =>
    @_drawCount++

    ctx\save()
    matrix\applyToContext ctx
    
    -- Use Base.merge() so we can use param.extend() to easily override
    -- values
    param = Base\merge(
      offset: Point(0, 0)
      
      -- A stack of concatenated matrices, to keep track of the current
      -- global matrix, since Canvas is not able tell us (yet).
      transforms: [matrix]
    )

    i = 0
    l = @layers.length

    while i < l
      @layers[i]\draw ctx, param
      i++

    ctx\restore()
    
    -- Draw the selection of the selected items in the project:
    if @_selectedItemCount > 0
      ctx\save()
      ctx\strokeWidth = 1

      for id of @_selectedItems
        item = @_selectedItems[id]
        if item._drawCount is @_drawCount and (item._drawSelected or item._boundsSelected)
          
          -- Allow definition of selected color on a per item and per
          -- layer level, with a fallback to --009dec
          color = item\getSelectedColor() or item\getLayer()\getSelectedColor()

          ctx.strokeStyle = ctx.fillStyle = (if color then color\toCanvasStyle(ctx) else "--009dec")
          mx = item._globalMatrix

          item\_drawSelected(ctx, mx) if item._drawSelected
          if item._boundsSelected
            -- We need to call the internal _getBounds, to get non-
            -- transformed bounds.
            -- TODO: Implement caching for these too!
            coords = mx\_transformCorners(item\_getBounds("getBounds"))
            
            -- Now draw a rectangle that connects the transformed
            -- bounds corners, and draw the corners.
            ctx\beginPath()

            i = 0
            while i < 8
              ctx[(if i is 0 then "moveTo" else "lineTo")] coords[i], coords[++i]
              i++

            ctx\closePath()
            ctx\stroke()

            i = 0
            while i < 8
              ctx\beginPath()
              ctx\rect coords[i] - 2, coords[++i] - 2, 4, 4
              ctx\fill()
              i++

      ctx\restore()
