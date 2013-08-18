class Color extends Base
  -- Parsers of values for setters, by type and property
  nameToRGB: (name) =>
    cached = colorCache[name]
    if !cached
      -- Use a canvas to draw to with the given name and then retrieve rgb
      -- values from. Build a cache for all the used colors.
      if !colorCtx
        colorCtx = CanvasProvider\getContext(1, 1)
        colorCtx\globalCompositeOperation = "copy"
      
      -- Set the current fillStyle to transparent, so that it will be
      -- transparent instead of the previously set color in case the new
      -- color can not be interpreted.
      colorCtx.fillStyle = "rgba(0,0,0,0)"
      
      -- Set the fillStyle of the context to the passed name and fill the
      -- canvas with it, then retrieve the data for the drawn pixel:
      colorCtx.fillStyle = name
      colorCtx\fillRect 0, 0, 1, 1

      data   = colorCtx\getImageData(0, 0, 1, 1).data
      cached = colorCache[name] = [data[0] / 255, data[1] / 255, data[2] / 255]

    cached\slice()

  hexToRGB: (string) =>
    -- TODO fix this
    hex = string.match(/^--?(\w{1,2})(\w{1,2})(\w{1,2})$/)

    if hex.length >= 4
      components = [0, 0, 0]

      i = 0
      while i < 3
        value = hex[i + 1]
        components[i] = (if value.length is 1 then value + value else value) / 255
        i++
      components

  types:
    gray: ["gray"]
    rgb: ["red", "green", "blue"]
    hsb: ["hue", "saturation", "brightness"]
    hsl: ["hue", "saturation", "lightness"]
    gradient: ["gradient", "origin", "destination", "highlight"]

  parsers: {}
  colorCache: {}
  colorCtx: nil
  
  -- For hsb-rgb conversion, used to lookup the right parameters in the
  -- values array.
  -- 0
  -- 1
  -- 2
  -- 3
  -- 4
  hsbIndices: [[0, 3, 1], [2, 0, 1], [1, 0, 3], [1, 2, 0], [3, 1, 0], [0, 1, 2]]
  
  -- Calling convention for converters:
  -- The components are passed as an arguments list, and returned as an array.
  -- alpha is left out, because the conversion does not change it.
  converters:
    "rgb-hsb": (r, g, b) ->
      max   = math.max(r, g, b)
      min   = math.min(r, g, b)
      delta = max - min

      h = (if delta is 0 then 0 else ((if max is r then (g - b) / delta + ((if g < b then 6 else 0)) else (if max is g then (b - r) / delta + 2 else (r - g) / delta + 4))) * 60) -- max == b
      [h, (if max is 0 then 0 else delta / max), max]

    "hsb-rgb": (h, s, b) ->
      h = (h / 60) % 6 -- Scale to 0..6
      i = math.floor(h) -- 0..5
      f = h - i
      i = hsbIndices[i]

      -- b, index 0
      -- p, index 1
      -- q, index 2

      v = [b, b * (1 - s), b * (1 - s * f), b * (1 - s * (1 - f))] -- t, index 3
      [v[i[0]], v[i[1]], v[i[2]]]

    
    -- HSL code is based on:
    -- http://mjijackson.com/2008/02/rgb-to-hsl-and-rgb-to-hsv-color-model-conversion-algorithms-in-javascript
    "rgb-hsl": (r, g, b) ->
      max        = math.max(r, g, b)
      min        = math.min(r, g, b)
      delta      = max - min
      achromatic = delta is 0

      h = (if achromatic then 0 else ((if max is r then (g - b) / delta + ((if g < b then 6 else 0)) else (if max is g then (b - r) / delta + 2 else (r - g) / delta + 4))) * 60) -- max == b
      l = (max + min) / 2
      s = (if achromatic then 0 else (if l < 0.5 then delta / (max + min) else delta / (2 - max - min)))

      [h, s, l]

    "hsl-rgb": (h, s, l) ->
      h /= 360
      return [l, l, l] if s is 0

      t3s = [h + 1 / 3, h, h - 1 / 3]
      t2  = (if l < 0.5 then l * (1 + s) else l + s - l * s)
      t1  = 2 * l - t2

      c = []
      i = 0
      while i < 3
        t3  = t3s[i]
        t3 += 1 if t3 < 0
        t3 -= 1 if t3 > 1

        c[i] = (if 6 * t3 < 1 then t1 + (t2 - t1) * 6 * t3 else (if 2 * t3 < 1 then t2 else (if 3 * t3 < 2 then t1 + (t2 - t1) * ((2 / 3) - t3) * 6 else t1)))
        i++
      c

    "rgb-gray": (r, g, b) ->    
      -- Using the standard NTSC conversion formula that is used for
      -- calculating the effective luminance of an RGB color:
      -- http://www.mathworks.com/support/solutions/en/data/1-1ASCU/index.html?solution=1-1ASCU
      [r * 0.2989 + g * 0.587 + b * 0.114]

    "gray-rgb": (g) ->
      [g, g, g]

    "gray-hsb": (g) ->
      [0, 0, g]

    "gray-hsl": (g) ->
      -- TODO: Is lightness really the same as brightness for gray?
      [0, 0, g]
    
  -- Tell Base.read that the Point constructor supports reading with index
  _readIndex: true

  new: (arg0) ->    
    -- We are storing color internally as an array of components
    slice      = []
    args       = arg
    read       = 0
    ttype      = nil
    components = nil
    alpha      = nil
    values     = nil
    
    -- If first argument is an array, replace arguments with it.
    if isArray(arg0)
      args = arg0
      arg0  = args[0]
    
    -- First see if it's a type string argument, and if so, set it and
    -- shift it out of the arguments list.
    argType = arg? and typeof(arg0)
    if argType is "string" and arg0 of types
      ttype = arg0
      arg0  = args[1]

      if isArray(arg) 
        -- Internal constructor that is called with the following
        -- arguments, without parsing: (type, componets, alpha)
        components = arg0
        alpha      = args[2]
      else
        -- For deserialization, shift out and process normally.
        read = 1 if @_read -- Will be increased below

        -- Shift type out of the arguments, and process normally.
        args    = slice.shift(1)
        argType = typeof(arg0)
    if !components
      -- Determine if there is a values array
      
      -- Do not use Array.isArray() to also support arguments
      values = (if argType is "number" then args else (if argType is "object" and arg.length? then arg else nil))
      
      -- The various branches below produces a values array if the
      -- values still need parsing, and a components array if they are
      -- already parsed.
      if values 
        -- type = values.length >= 4
        -- 		? 'cmyk'
        -- 		: values.length >= 3
        ttype  = (if values.length >= 3 then "rgb" else "gray") if !ttype
        length = types[type].length
        alpha  = values[length]
        read  += (if values is arguments_ then length + ((if alpha? then 1 else 0)) else 1)  if @_read
        values = slice if values.length > length
      else if argType is "string"
        -- TODO Fix Regex
        --- components = (if arg.match(/^--[0-9a-f]{3,6}$/i) then hexToRGB(arg) else nameToRGB(arg))
        ttype = "rgb"
      else if argType is "table"
        if arg0.__class is Color
          ttype      = arg0._type
          components = arg0._components\slice()
          alpha      = arg0._alpha

          if ttype is "gradient"
            -- Clone all points, since they belong to the other
            -- color already.
            i = 1
            l = components.length

            while i < l
              point         = components[i]
              components[i] = point\clone() if point
              i++
        else if arg.__class is Gradient
          ttype  = "gradient"
          values = args
        else
          -- Determine type by presence of object property names
         ttype = (if "hue" of arg0 then (if "lightness" of arg0 then "hsl" else "hsb") else (if "gradient" of arg0 or "stops" of arg0 or "radial" of arg0 then "gradient" else (if "gray" of arg0 then "gray" else "rgb")))
          
          -- Convert to array and parse in one loop, for efficiency
          properties   = types[ttype]
          parse        = parsers[ttype]
          @_components = components = []

          i = 0
          l = properties.length

          while i < l
            value = arg0[properties[i]]
            -- Allow implicit definition of gradients through
            -- stops / radial properties. Conversion happens
            -- here on the fly:
            if not value? and i is 0 and type is "gradient" and "stops" of arg0
              value =
                stops: arg0.stops
                radial: arg0.radial

            value         = parse[i]
            components[i] = value if value?
            i++

          alpha = arg0.alpha

      read = 1 if @_read and type
    
    -- Default fallbacks: rgb, black
    @_type = ttype or "rgb"
    
    -- Define this gradient Color's unique id.
    @_id = Color._id = (Color._id or 0) + 1 if ttype is "gradient"
    if !components   
      -- Produce a components array now, and parse values. Even if no
      -- values are defined, parsers are still called to produce
      -- defaults.
      @_components = components = []
      parse        = parsers[@_type]

      i = 0
      l = parse.length
      while i < l
        value         = parse[i]
        components[i] = value if value?
        i++

    @_components = components
    @_alpha      = alpha
    @_read       = read if @_read

  _serialize: (options, dictionary) =>
    components = @getComponents()
    -- We can ommit the type for gray and rgb:
    Base\serialize (if /^(gray|rgb)$/\test(@_type) then components else [@_type]\concat(components)), options, true, dictionary

  _changed: =>
    @_canvasStyle = nil
    @_owner\_changed Change.STYLE if @_owner

  clone: =>
    Color(@_type, @_components\slice(), @_alpha)

  _convert: (ttype) =>
    converter = nil
    -- Convert to and from rgb if no direct converter exists
    (if @_type is ttype then @_components.slice() else (if (converter = converters[@_type + "-" + ttype]) then converter.apply(this, @_components) else converters["rgb-" + ttype].apply(this, converters[@_type + "-rgb"].apply(this, @_components))))

  convert: (ttype) =>
    Color(ttype, @_convert(ttype), @_alpha)

  getType: =>
    @_type

  setType: (ttype) =>
    @_components = @_convert(ttype)
    @_type       = ttype

  getComponents: =>
    components = @_components\slice()
    components\push @_alpha  if @_alpha?
    components

  getAlpha: =>
    (if @_alpha? then @_alpha else 1)

  setAlpha: (alpha) =>
    @_alpha = (if not alpha? then nil else math.min(math.max(alpha, 0), 1))
    @_changed()

  hasAlpha: =>
    @_alpha?

  equals: (color) =>
    color = Color.read(arg) if Base\isPlainValue(color)
    color is this or color and @_type is color._type and @_alpha is color._alpha and Base\equals(@_components, color._components) or false

  transform: (matrix) =>
    if @_type is "gradient"
      components = @_components

      i = 1
      l = components.length
      while i < l
        point = components[i]
        matrix\_transformPoint point, point, true
        i++

      @_changed()
