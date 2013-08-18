class Style extends Base
  -- windingRule / resolution / fillOverprint / strokeOverprint are currently
  -- not supported.
  defaults:
    
    -- path styles
    fillColor:     nil
    strokeColor:   nil
    selectedColor: nil
    strokeWidth:   1
    strokeCap:     "butt"
    strokeJoin:    "miter"
    miterLimit:    10
    dashOffset:    0
    dashArray:     []
    
    -- character styles
    font:     "sans-serif"
    fontSize: 12
    leading:  nil
    
    -- paragraph styles
    justification: "left"

  flags:
    strokeWidth:   Change.STROKE
    strokeCap:     Change.STROKE
    strokeJoin:    Change.STROKE
    miterLimit:    Change.STROKE
    font:          Change.GEOMETRY
    fontSize:      Change.GEOMETRY
    leading:       Change.GEOMETRY
    justification: Change.GEOMETRY

  item: {}

  fields:
    _defaults: defaults
    
    -- Override default fillColor for text items
    _textDefaults: Base\merge(defaults,
      fillColor: Color() -- black
    )

  new: Style = (style, _item) =>
    
    -- We keep values in a separate object that we can iterate over.
    @_values   = {}
    @_item     = _item
    @_defaults = @_textDefaults if _item.__class == TextItem

    @set(style) if style

  set: (style) =>
    
    -- If the passed style object is also a Style, clone its clonable
    -- fields rather than simply copying them.
    isStyle = style.__class Style
    
    -- Use the other stlyle's _values object for iteration
    values = (if isStyle then style._values else style)

    if values
      for key of values
        if key of @_defaults
          value = values[key]
          
          -- Delegate to setter, so Group styles work too.
          @[key] = (if value and isStyle and value\clone() then value\clone() else value)

  getLeading: 
    getLeading = ->
      
      -- Override leading to return fontSize * 1.2 by default.
      leading = getLeading\base(@)
      (if leading? then leading else @getFontSize() * 1.2)

  getFontStyle: =>
    size = @getFontSize()
    ((if /[a-z]/i\test(size) then size + " " else size + "px ")) + @getFont()
