class Point extends LinkedPoint
	set: (x, y, dontNotify) =>
    @_x = x
    @_y = y
    @_owner[@_setter] @ !dontNotify


  getX: =>
    @_x

  setX: (x) =>
    @_x = x
    @_owner[@_setter] @

  getY: =>
    @_y

  setY: (y) =>
    @_y = y
    @_owner[@_setter] @

  @create: (owner, setter, x, y, dontLink) =>
    -- Support creation of normal Points rather than LinkedPoints
    -- through an optional parameter that can be passed to the getters.
    -- See e.g. Rectangle#getPoint(true).
    return Point(x, y) if !dontLink
    point = Base\create(LinkedPoint)

    point._x = x
    point._y = y

    point._owner  = owner
    point._setter = setter
    
    point
