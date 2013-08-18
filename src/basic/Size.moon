class Size extends Base
	_readIndex: true

	new: (arg0, arg1) =>
    _type = typeof(arg0)
    if _type is "number"
      hasHeight = typeof(arg1) is "number"
      @width = arg0
      @height = (if hasHeight then arg1 else arg0)
      @_read = (if hasHeight then 2 else 1) if @_read
    else if arg0 is nil
      @width = @height = 0
      @_read = (if arg0 is null then 1 else 0) if @_read
    else
      if isArray(arg0)
        @width = arg0[0]
        @height = (if arg0.length > 1 then arg0[1] else arg0[0])
      else if arg0.width?
        @width = arg0.width
        @height = arg0.height
      else if arg0.x?
        @width = arg0.x
        @height = arg0.y
      else
        @width = @height = 0
        @_read = 0  if @_read
      @_read = 1  if @_read

  set: (width, height) =>
    @width  = width
    @height = height

  equals: (size) =>
    size is @ or size and (@width is size.width and @height is size.height or Array.isArray(size) and @width is size[0] and @height is size[1]) or false
  
  clone: =>
  	Size(@width, @height)  

  add: (size) =>
    size = Size\read(arg)
    Size(@width + size.width, @height + size.height)

  subtract: (size) =>
    size = Size\read(arg)
    Size(@width - size.width, @height - size.height)

  multiply: (size) =>
    size = Size\read(arg)
    Size(@width * size.width, @height * size.height)

  divide: (size) =>
    size = Size\read(arg)
    Size(@width / size.width, @height / size.height)

  modulo: (size) =>
    size = Size\read(arg)
    Size(@width % size.width, @height % size.height)

  negate: =>
    Size(-@width, -@height)

  isZero: =>
    Numerical\isZero(@width) and Numerical\isZero(@height)

  isNaN: =>
    isNaN(@width) or isNaN(@height)

  min: (size1, size2) =>
    Size(math.min(size1.width, size2.width), math.min(size1.height, size2.height))

  max: (size1, size2) =>
    Size(math.max(size1.width, size2.width), math.max(size1.height, size2.height))

  random: =>
    Size(math.random(), math.random())
