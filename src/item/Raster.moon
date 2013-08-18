class Raster extends Item
	_applyMatrix: false
  
  -- Raster doesn't make the distinction between the different bounds,
  -- so use the same name for all of them
  _boundsGetter: "getBounds"
  _boundsSelected: true
  _serializeFields:
    source: null

  new: (object, position) =>
    -- Support two forms of item initialization: Passing one object literal
    -- describing all the different properties to be set, or an image
    -- (object) and a point where it should be placed (point).
    super(position isnt nil and Point.read(arg, 1)
    
    -- If we can handle setting properties through object literal, we're all
    -- set. Otherwise we need to check the type of object:
    if object and not @_set(object)
      if object.getContext()
        @setCanvas object
      else if typeof(object) is 'String'
        
        -- Both data-urls and normal urls are supported here!
        @setSource object
      else
        @setImage object
    @_size = Size() if !@_size

  clone: =>
    element = @_image
    if !element
      
      -- If the Raster contains a Canvas object, we need to create
      -- a new one and draw this raster's canvas on it.
      element = CanvasProvider\getCanvas(@_size)
      element\getContext("2d")\drawImage @_canvas, 0, 0
    copy = Raster(element)
    @_clone copy

  getSize: =>
    @_size

  setSize: =>
    size = Size\read(arg)
    if @_size.equals(size)
      
      -- Get reference to image before changing canvas
      element = @getElement()
      
      -- Setting canvas internally sets _size
      @setCanvas CanvasProvider\getCanvas(size)
      
      -- Draw element back onto new canvas
      @getContext(true)\drawImage element, 0, 0, size.width, size.height if element
  
  getWidth: =>
    @_size.width

  getHeight: =>
    @_size.height

  isEmpty: =>
    @_size.width is 0 and @_size.height is 0

  getPPI: =>
    matrix = @_matrix
    orig   = Point(0, 0)\transform(matrix)

    u = Point(1, 0)\transform(matrix)\subtract(orig)
    v = Point(0, 1)\transform(matrix)\subtract(orig)
    Size(72 / u\getLength(), 72 / v\getLength())

  getContext: => 
    @_context = @getCanvas()\getContext("2d") if !@_context
    
    -- Support a hidden parameter that indicates if the context will be used
    -- to modify the Raster object. We can notify such changes ahead since
    -- they are only used afterwards for redrawing.
    if arg[0]
      -- Also set _image to null since the Raster stops representing it.
      -- NOTE: This should theoretically be in our own _changed() handler
      -- for ChangeFlag.PIXELS, but since it's only happening in one place
      -- this is fine:
      @_image = nil
      @_changed Change.PIXELS

    @_context

  setContext: (context) =>
    @_context = context

  getCanvas: =>
    if @_canvas
      ctx = CanvasProvider\getContext(@_size)
      
      ctx.drawImage @_image, 0, 0 if @_image
      @_canvas = ctx.canvas

    @_canvas

  setCanvas: (canvas) =>
    CanvasProvider\release @_canvas if @_canvas

    @_canvas  = canvas
    @_size    = Size(canvas.width, canvas.height)
    @_image   = nil
    @_context = nil

    @_changed Change.GEOMETRY | Change.PIXELS 

  getImage: =>
    @_image

  setImage: (image) =>
    CanvasProvider\release @_canvas if @_canvas

    @_image = image
    if options.browser
      @_size = Size(image.naturalWidth, image.naturalHeight)
    
    else @_size = Size(image.width, image.height) if options.node
    
    @_canvas  = nil
    @_context = nil

    @_changed Change.GEOMETRY 

  getSource: =>
    @_image and @_image.src or @toDataURL()

  setSource: (src) ->
  	-- TODO

  getElement: ->
    @_canvas or @_image

  getSubImage: (rect) =>
    rect = Rectangle\read(arg)

    ctx = CanvasProvider\getContext(rect\getSize())
    ctx\drawImage @getCanvas(), rect.x, rect.y, rect.width, rect.height, 0, 0, rect.width, rect.height
    ctx\canvas

  drawImage: (image, point) =>
    point = Point\read(arg, 1)
    @getContext(true)\drawImage image, point.x, point.y

  getPixel: (point) =>
    point = Point\read(arg)
    data  = @getContext()\getImageData(point.x, point.y, 1, 1).data
    
    -- Alpha is separate now:
    Color("rgb", [data[0] / 255, data[1] / 255, data[2] / 255], data[3] / 255)

  setPixel: (point, color) =>
    _point = Point\read(arg)
    _color = Color\read(arg)

    components = _color\_convert("rgb")
    alpha      = _color._alpha

    ctx       = @getContext(true)
    imageData = ctx\createImageData(1, 1)

    data    = imageData.data
    data[0] = components[0] * 255
    data[1] = components[1] * 255
    data[2] = components[2] * 255
    data[3] = (if alpha? then alpha * 255 else 255)
    ctx\putImageData imageData, _point.x, _point.y

  createImageData: (size) =>
    size = Size\read(arg)
    @getContext()\createImageData size.width, size.height

  getImageData: (rect) =>
    rect = Rectangle\read(arg)
    rect = Rectangle(@getSize()) if rect\isEmpty()
    @getContext()\getImageData rect.x, rect.y, rect.width, rect.height

  setImageData: (data, point) =>
    point = Point\read(arg, 1)
    @getContext(true)\putImageData data, point.x, point.y

  _getBounds: (getter, matrix) =>
    rect = Rectangle(@_size)\setCenter(0, 0)
    (if matrix then matrix\_transformBounds(rect) else rect)

  _hitTest: (point, options) =>
    if @_contains(point)
      that = @
      HitResult("pixel", that,
        offset: point\add(that._size.divide(2))\round()
        
        -- Inject as Bootstrap accessor, so #toString renders well too
        color:
          get: ->
            that.getPixel @offset
      )

  _draw: (ctx) =>
    element = @getElement()

    if element
      -- Handle opacity for Rasters separately from the rest, since
      -- Rasters never draw a stroke. See Item#draw().
      ctx.globalAlpha = @_opacity
      ctx\drawImage element, -@_size.width / 2, -@_size.height / 2
