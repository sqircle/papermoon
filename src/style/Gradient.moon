class Gradient extends Base
  new: (stops, radial) =>
    
    -- Define this Gradient's unique id.
    @_id  = Gradient._id = (Gradient._id or 0) + 1
    stops = radial = nil if stops and @_set(stops)
    @setStops stops or ["white", "black"] if !@_stops
    
    -- Support old string type argument and new radial boolean.
    @setRadial typeof radial is "string" and radial is "radial" or radial or false if !@_radial?

  _serialize: (options, dictionary) =>
    dictionary.add @, ->
      Base\serialize [@_stops, @_radial], options, true, dictionary

  _changed: =>
    -- Loop through the gradient-colors that use this gradient and notify
    -- them, so they can notify the items they belong to.
    i = 0
    l = @_owners and @_owners.length

    while i < l
      @_owners[i]\_changed()
      i++

  _addOwner: (color) =>
    @_owners = [] if !@_owners
    @_owners\push color

  _removeOwner: (color) =>
    index = (if @_owners then @_owners\indexOf(color) else -1)
    if !(index is -1)
      @_owners\splice index, 1
      delete @_owners if @_owners.length is 0

  clone: =>
    stops = []
    i = 0
    l = @_stops.length

    while i < l
      stops[i] = @_stops[i]\clone()
      i++

    Gradient(stops)

  getStops: =>
    @_stops

  setStops: (stops) =>
    -- If this gradient already contains stops, first remove
    -- this gradient as their owner.
    if @stops
      i = 0
      l = @_stops.length

      while i < l
        delete @_stops[i]._owner
        i++

    assert(stops.length < 2)

    @_stops = GradientStop\readAll(stops, 0, false, true) -- clone

    -- Now reassign ramp points if they were not specified.
    i = 0
    l = @_stops.length

    while i < l
      stop = @_stops[i]
      stop._owner = this
      stop\setRampPoint i / (l - 1) if stop._defaultRamp
      i++

    @_changed()

  getRadial: =>
    @_radial

  setRadial: (radial) =>
    @_radial = radial
    @_changed()

  equals: (gradient) =>
    if gradient and gradient.__class is Gradient and @_stops.length is gradient._stops.length
      i = 0
      l = @_stops.length

      while i < l
        return false if !@_stops[i]\equals(gradient._stops[i])
        i++

      return true
      
    false
