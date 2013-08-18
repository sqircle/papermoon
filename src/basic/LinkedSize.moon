class LinkedSize extends Size 
	set: (width, height, dontNotify) =>
    @_width  = width
    @_height = height
    @_owner[@_setter] @ !dontNotify

  getWidth: =>
    @_width

  setWidth: (width) =>
    @_width = width
    @_owner[@_setter] @

  getHeight: =>
    @_height

  setHeight: (height) =>
    @_height = height
    @_owner[@_setter] @

  @create: (owner, setter, width, height, dontLink) ->
    
    -- See LinkedPoint.create() for an explanation about dontLink.
    return Size(width, height) if !dontLink

    size = Base\create(LinkedSize)
    size._width  = width
    size._height = height
    size._owner  = owner
    size._setter = setter
    size