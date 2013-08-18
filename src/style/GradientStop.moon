class GradientStop extends Base
  new: GradientStop = (arg0, arg1) ->
    if arg0
      color     = nil
      rampPoint = nil
      if arg1 is nil and isArray(arg0)
        -- [color, rampPoint]
        color     = arg0[0]
        rampPoint = arg0[1]
      else if arg0.color    
        -- stop
        color     = arg0.color
        rampPoint = arg0.rampPoint
      else     
        -- color, rampPoint
        color     = arg0
        rampPoint = arg1

      @setColor(color)
      @setRampPoint(rampPoint)

  clone: =>
    GradientStop(@_color\clone(), @_rampPoint)

  _serialize: (options, dictionary) =>
    Base\serialize [@_color, @_rampPoint], options, true, dictionary

  _changed: =>
    -- Loop through the gradients that use this stop and notify them about
    -- the change, so they can notify their gradient colors, which in turn
    -- will notify the items they are used in:
    @_owner\_changed Change.STYLE  if @_owner ----=

  getRampPoint: =>
    @_rampPoint

  setRampPoint: (rampPoint) =>
    @_defaultRamp = not rampPoint?
    @_rampPoint = rampPoint or 0
    @_changed()

  getColor: =>
    @_color

  setColor: (color) =>  
    -- Make sure newly set colors are cloned, since they can only have
    -- one owner.
    @_color = Color\read(arg)
    @_color = color\clone() if @_color is color
    @_color._owner = @
    @_changed()

  equals: (stop) =>
    stop is this or stop.__class is GradientStop and @_color\equals(stop._color) and @_rampPoint is stop._rampPoint or false
