class HitResult extends Base
	new: (type, item, values) =>
    @type = type
    @item = item
    
    -- Inject passed values, so we can be flexible about the HitResult
    -- properties.
    -- This allows the definition of getters too, e.g. for 'pixel'.
    if values
      values.enumerable = true
      @inject values

  getOptions: (options) =>  
    -- Use _merged property to not repeatetly call merge in recursion.
    (if options and options._merged then options else Base\merge(
      
      -- Type of item, for instanceof check: PathItem, TexItem, etc
      type: null
      
      -- Tolerance
      tolerance: paper.project.options.hitTolerance or 2
      
      -- Hit the fill of items
      fill: not options
      
      -- Hit the curves of path items, taking into account the stroke
      -- width.
      stroke: not options
      
      -- Hit the part of segments that curves pass through, excluding
      -- its segments (Segment--point)
      segments: not options
      
      -- Hit the parts of segments that define the curvature
      handles: false
      
      -- Only first or last segment hits on path (mutually exclusive
      -- with segments: true)
      ends: false
      
      -- Hit test the center of the bounds
      center: false
      
      -- Hit test the corners and side-centers of the boudning box
      bounds: false
      
      --  Hit items that are marked as guides
      guides: false
      
      -- Only hit selected objects
      selected: false
      
      -- Mark as merged, so next time Base.merge isn't called
      _merged: true
    , options))
