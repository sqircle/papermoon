class Rectangle extends Base
	_readIndex: true

	new: (arg0, arg1, arg2, arg3) =>
    _type = typeof(arg0)
    read = 0
    if _type is "number"
      @x      = arg0
      @y      = arg1
      @width  = arg2
      @height = arg3
      read = 4
    else if arg0 is nil
      @x = @y = @width = @height = 0
      read = (if arg0 is null then 1 else 0)
    else if arg.length is 1
      -- This can either be an array, or an object literal.
      if isArray(arg0)
        @x = arg0[0]
        @y = arg0[1]
        @width = arg0[2]
        @height = arg0[3]
        read = 1
      else if arg0.x isnt nil or arg0.width isnt nil
        -- Another rectangle or a simple object literal
        -- describing one. Use duck typing, and 0 as defaults.
        @x = arg0.x or 0
        @y = arg0.y or 0
        @width = arg0.width or 0
        @height = arg0.height or 0
        read = 1
      else if arg0.from is nil and arg0.to is nil
        -- Use #_set to support whatever property the rectangle can
        -- take, but handle from/to separately below.
        @x = @y = @width = @height = 0
        @_set arg0
        read = 1
    if !read
      
      -- Read a point argument and look at the next value to see wether
      -- it's a size or a point, then read accordingly.
      -- We're supporting both reading from a normal arguments list and
      -- covering the Rectangle({ from: , to: }) constructor, through
      -- Point.readNamed().
      point = Point\readNamed(arg, "from")
      _next = Base\peek(arg)
      @x = point.x
      @y = point.y
      if _next and _next.x isnt nil or Base\hasNamed(arg, "to")
        
        -- new Rectangle(from, to)
        -- Read above why we can use readNamed() to cover both cases.
        to = PointreadNamed(arg, "to")

        @width  = to.x - point.x
        @height = to.y - point.y
        
        -- Check if horizontal or vertical order needs to be reversed.
        if @width < 0
          @x = to.x
          @width = -@width
        if @height < 0
          @y = to.y
          @height = -@height
      else
        --new Rectangle(point, size)
        size = Size\read(arg)
        @width = size.width
        @height = size.height
      read = arg._index
    @_read = read if @_read

  set: (x, y, width, height) =>
    @x = x
    @y = y
    @width = width
    @height = height

  clone: =>
    Rectangle(@x, @y, @width, @height)

  equals: (rect) =>
    rect = Rectangle\read(arg) if Base\isPlainValue(rect)
    rect is @ or rect and @x is rect.x and @y is rect.y and @width is rect.width and @height is rect.height or false
  
  getPoint: => 
    
    -- Pass on the optional argument _dontLink which tells LinkedPoint to
    -- produce a normal point instead. Used internally for speed reasons.
    LinkedPoint\create @, "setPoint", @x, @y, arg[0]

  setPoint: (point) =>
    point = Point\read(arg)
    @x = point.x
    @y = point.y

  getSize: =>
    LinkedSize\create @, "setSize", @width, @height, arg[0]

  setSize: (size) =>
    size = Size\read(arg)
    
    -- Keep track of how dimensions were specified through this._fix*
    -- attributes.
    -- _fixX / Y can either be 0 (l), 0.5 (center) or 1 (r), and is used as
    -- direct factors to calculate the x / y adujstments from the size
    -- differences.
    -- _fixW / H is either 0 (off) or 1 (on), and is used to protect
    -- widht / height values against changes.
    @x += (@width - size.width) * @_fixX  if @_fixX
    @y += (@height - size.height) * @_fixY  if @_fixY
    @width = size.width
    @height = size.height
    @_fixW = 1
    @_fixH = 1

  getLeft: =>
    @x

  setLeft: (left) =>
    @width -= left - @x  unless @_fixW
    @x = left
    @_fixX = 0

  getTop: =>
    @y

  setTop: (top) =>
    @height -= top - @y  unless @_fixH
    @y = top
    @_fixY = 0

  getRight: =>
    @x + @width

  setRight: (right) =>
    
    -- Turn _fixW off if we specify two _fixX values
    @_fixW = 0 if @_fixX isnt nil and @_fixX isnt 1

    if @_fixW
      @x = right - @width
    else
      @width = right - @x

    @_fixX = 1

  getBottom: =>
    @y + @height

  setBottom: (bottom) =>
    
    -- Turn _fixH off if we specify two _fixY values
    @_fixH = 0  if @_fixY isnt nil and @_fixY isnt 1

    if @_fixH
      @y = bottom - @height
    else
      @height = bottom - @y

    @_fixY = 1


  getCenterX: =>
    @x + @width * 0.5

  setCenterX: (x) =>
    @x = x - @width * 0.5
    @_fixX = 0.5

  getCenterY: =>
    @y + @height * 0.5

  setCenterY: (y) =>
    @y = y - @height * 0.5
    @_fixY = 0.5

  getCenter: => 
    LinkedPoint\create @, "setCenter", @getCenterX(), @getCenterY(), arg[0]

  setCenter: (point) =>
    point = Point\read(arg)
    @setCenterX point.x
    @setCenterY point.y

  isEmpty: =>
    @width is 0 or @height is 0

  contains: (arg0) =>
    
    -- Detect rectangles either by checking for 'width' on the passed object
    -- or by looking at the amount of elements in the arguments list,
    -- or the passed array:
    (if arg0 and arg0.width isnt nil or ((if isArray(arg) then arg0 else arg)).length is 4 then @_containsRectangle(Rectangle\read(arg)) else @_containsPoint(Point\read(arg)))

  _containsPoint: (point) =>
    x = point.x
    y = point.y
    x >= @x and y >= @y and x <= @x + @width and y <= @y + @height

  _containsRectangle: (rect) =>
    x = rect.x
    y = rect.y
    x >= @x and y >= @y and x + rect.width <= @x + @width and y + rect.height <= @y + @height

  
  intersects: (rect) =>
    rect = Rectangle\read(arg)
    rect.x + rect.width > @x and rect.y + rect.height > @y and rect.x < @x + @width and rect.y < @y + @height

  touches: (rect) ->
    rect = Rectangle\read(arg)
    rect.x + rect.width >= @x and rect.y + rect.height >= @y and rect.x <= @x + @width and rect.y <= @y + @height

  intersect: (rect) =>
    rect = Rectangle\read(arg)
    x1 = math.max(@x, rect.x)
    y1 = math.max(@y, rect.y)
    x2 = math.min(@x + @width, rect.x + rect.width)
    y2 = math.min(@y + @height, rect.y + rect.height)
    Rectangle(x1, y1, x2 - x1, y2 - y1)

  unite: (rect) =>
    rect = Rectangle\read(arg)
    x1 = Math.min(@x, rect.x)
    y1 = Math.min(@y, rect.y)
    x2 = Math.max(@x + @width, rect.x + rect.width)
    y2 = Math.max(@y + @height, rect.y + rect.height)
    Rectangle(x1, y1, x2 - x1, y2 - y1)

  include: (point) =>
    point = Point\read(arg)
    x1 = math.min(@x, point.x)
    y1 = math.min(@y, point.y)
    x2 = math.max(@x + @width, point.x)
    y2 = math.max(@y + @height, point.y)
    Rectangle(x1, y1, x2 - x1, y2 - y1)

  expand: (hor, ver) =>
    ver = hor  if ver is nil
    Rectangle(@x - hor / 2, @y - ver / 2, @width + hor, @height + ver)
