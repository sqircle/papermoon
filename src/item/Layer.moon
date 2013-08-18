class Layer extends Group
	new: (items) ->
    @_project = paper\project
    
    -- Push it onto project.layers and set index:
    @_index = @_project\layers\push(@) - 1
    super\(arg)
    @activate()

  _remove: _remove = (notify) =>
    return _remove.base.call(this, notify)  if @_parent
    if @_index?
      @_project.activeLayer = @getNextSibling() or @getPreviousSibling()  if @_project.activeLayer is this
      Base.splice @_project.layers, null, @_index, 1
      
      # Tell project we need a redraw. This is similar to _changed()
      # mechanism.
      @_project._needsRedraw = true
      return true
    false

  getNextSibling: 
	  getNextSibling = ->
	    (if @_parent then super\getNextSibling() else @_project.layers[@_index + 1] or nil)

  getPreviousSibling: 
	  getPreviousSibling = ->
	    (if @_parent then super\getPreviousSibling() else @_project.layers[@_index - 1] or nil)

  isInserted: 
	  isInserted = ->
	    (if @_parent then super\isInserted else @_index?)
