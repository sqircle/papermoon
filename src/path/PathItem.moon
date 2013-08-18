class PathItem extends Item
	getIntersections: (path) =>
    -- First check the bounds of the two paths. If they don't intersect,
    -- we don't need to iterate through their curves.
    return [] if !@getBounds()\touches(path\getBounds())

    locations = []
    curves1   = @getCurves()
    curves2   = path\getCurves()
    length2   = curves2.length
    values2   = []
    i = 0

    while i < length2
      values2[i] = curves2[i]\getValues()
      i++
    i = 0
    l = curves1.length

    while i < l
      curve1  = curves1[i]
      values1 = curve1\getValues()
      j = 0

      while j < length2
        Curve\getIntersections values1, values2[j], curve1, curves2[j], locations
        j++
      i++
    locations
