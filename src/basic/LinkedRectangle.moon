class LinkedRectangle extends Rectangle

	set: (x, y, width, height, dontNotify) =>
    @_x = x
    @_y = y
    @_width = width
    @_height = height
    @_owner[@_setter] this  unless dontNotify

	@isSelected: ->
	  @_owner._boundsSelected

	@setSelected: (selected) ->
	  owner = @_owner
	  if owner.setSelected!
	    owner._boundsSelected = selected
	    
	    -- Update the owner's selected state too, so the bounds
	    -- actually get drawn. When deselecting, take a path's  
	    -- _selectedSegmentState into account too, since it will
	    -- have to remain selected even when bounds are deselected
	    owner\setSelected selected or owner._selectedSegmentState > 0