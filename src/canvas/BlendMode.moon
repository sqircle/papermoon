class BlendMode 
  new: (blendMode, srcContext, dstContext, alpha, offset) ->

    srcCanvas = srcContext.canvas
    dstData   = dstContext\getImageData(offset.x, offset.y, srcCanvas.width, srcCanvas.height)
    dst       = dstData.data
    src       = srcContext\getImageData(0, 0, srcCanvas.width, srcCanvas.height).data

    -- source
    -- backdrop
    -- destination
    
    -- Conversion methods for HSL modes, as described by
    -- http://www.aiim.org/documents/standards/pdf/blend_modes.pdf
    -- The setters modify the variables dr, dg, db directly.

    process = modes[blendMode]
    return if !process

    i = 0
    l = dst.length
    while i < l
      sr = src[i]
      br = dst[i]
      sg = src[i + 1]
      bg = dst[i + 1]
      sb = src[i + 2]
      bb = dst[i + 2]
      sa = src[i + 3]
      ba = dst[i + 3]


      a1 = sa * alpha / 255
      a2 = 1 - a1

      dst[i]     = a1 * dr + a2 * br
      dst[i + 1] = a1 * dg + a2 * bg
      dst[i + 2] = a1 * db + a2 * bb
      dst[i + 3] = sa * alpha + a2 * ba
      
      i += 4

    dstContext\putImageData dstData, offset.x, offset.y

  getLum = (r, g, b) ->
    0.2989 * r + 0.587 * g + 0.114 * b

  setLum = (r, g, b, l) ->
    d  = l - @getLum(r, g, b)
    dr = r + d
    dg = g + d
    db = b + d

    l  = @getLum(dr, dg, db)
    mn = @min(dr, dg, db)
    mx = @max(dr, dg, db)

    if mn < 0
      lmn = l - mn

      dr = l + (dr - l) * l / lmn
      dg = l + (dg - l) * l / lmn
      db = l + (db - l) * l / lmn

    if mx > 255
      ln  = 255 - l
      mxl = mx - l

      dr = l + (dr - l) * ln / mxl
      dg = l + (dg - l) * ln / mxl
      db = l + (db - l) * ln / mxl

  getSat = (r, g, b) ->
    max(r, g, b) - min(r, g, b)

  setSat = (r, g, b, s) ->

    col = [r, g, b]
    mx = max(r, g, b) -- max
    mn = min(r, g, b) -- min
    md = nil

    -- mid
    -- Determine indices for min and max in col:
    mn = (if mn is r then 0 else (if mn is g then 1 else 2))
    mx = (if mx is r then 0 else (if mx is g then 1 else 2))
    
    -- Determine the index in col that is not used yet by min and max,
    -- and assign it to mid:
    md = (if min(mn, mx) is 0 then (if max(mn, mx) is 1 then 2 else 1) else 0)
    
    -- Now perform the actual algorithm
    if col[mx] > col[mn]
      col[md] = (col[md] - col[mn]) * s / (col[mx] - col[mn])
      col[mx] = s
    else
      col[md] = col[mx] = 0
    col[mn] = 0
    
    -- Finally write out the values
    dr = col[0]
    dg = col[1]
    db = col[2]

  min; math.min
  max; math.max
  abs; math.abs
  sr: nil
  sg: nil
  sb: nil
  sa: nil
  br: nil
  bg: nil
  bb: nil
  ba: nil
  dr: nil
  dg: nil
  db: nil

  modes:
    multiply: ->
      dr = br * sr / 255
      dg = bg * sg / 255
      db = bb * sb / 255

    screen: ->
      dr = 255 - (255 - br) * (255 - sr) / 255
      dg = 255 - (255 - bg) * (255 - sg) / 255
      db = 255 - (255 - bb) * (255 - sb) / 255

    overlay: ->
      dr = (if br < 128 then 2 * br * sr / 255 else 255 - 2 * (255 - br) * (255 - sr) / 255)
      dg = (if bg < 128 then 2 * bg * sg / 255 else 255 - 2 * (255 - bg) * (255 - sg) / 255)
      db = (if bb < 128 then 2 * bb * sb / 255 else 255 - 2 * (255 - bb) * (255 - sb) / 255)

    "soft-light": ->
      t  = sr * br / 255
      dr = t + br * (255 - (255 - br) * (255 - sr) / 255 - t) / 255
      t  = sg * bg / 255
      dg = t + bg * (255 - (255 - bg) * (255 - sg) / 255 - t) / 255
      t  = sb * bb / 255
      db = t + bb * (255 - (255 - bb) * (255 - sb) / 255 - t) / 255

    "hard-light": ->  
      -- = Reverse of overlay
      dr = (if sr < 128 then 2 * sr * br / 255 else 255 - 2 * (255 - sr) * (255 - br) / 255)
      dg = (if sg < 128 then 2 * sg * bg / 255 else 255 - 2 * (255 - sg) * (255 - bg) / 255)
      db = (if sb < 128 then 2 * sb * bb / 255 else 255 - 2 * (255 - sb) * (255 - bb) / 255)

    "color-dodge": ->
      dr = (if sr is 255 then sr else min(255, br * 255 / (255 - sr)))
      dg = (if sg is 255 then sg else min(255, bg * 255 / (255 - sg)))
      db = (if sb is 255 then sb else min(255, bb * 255 / (255 - sb)))

    "color-burn": ->
      dr = (if sr is 0 then 0 else max(255 - ((255 - br) * 255) / sr, 0))
      dg = (if sg is 0 then 0 else max(255 - ((255 - bg) * 255) / sg, 0))
      db = (if sb is 0 then 0 else max(255 - ((255 - bb) * 255) / sb, 0))

    darken: ->
      dr = (if br < sr then br else sr)
      dg = (if bg < sg then bg else sg)
      db = (if bb < sb then bb else sb)

    lighten: ->
      dr = (if br > sr then br else sr)
      dg = (if bg > sg then bg else sg)
      db = (if bb > sb then bb else sb)

    difference: ->
      dr = br - sr
      dr = -dr  if dr < 0
      dg = bg - sg
      dg = -dg  if dg < 0
      db = bb - sb
      db = -db  if db < 0

    exclusion: ->
      dr = br + sr * (255 - br - br) / 255
      dg = bg + sg * (255 - bg - bg) / 255
      db = bb + sb * (255 - bb - bb) / 255

    -- HSL Modes:
    hue: ->
      setSat sr, sg, sb, getSat(br, bg, bb)
      setLum dr, dg, db, getLum(br, bg, bb)

    saturation: ->
      setSat br, bg, bb, getSat(sr, sg, sb)
      setLum dr, dg, db, getLum(br, bg, bb)

    luminosity: ->
      setLum br, bg, bb, getLum(sr, sg, sb)

    color: ->
      setLum sr, sg, sb, getLum(br, bg, bb)
   
    -- TODO: Not in Illustrator:
    add: ->
      dr = min(br + sr, 255)
      dg = min(bg + sg, 255)
      db = min(bb + sb, 255)

    subtract: ->
      dr = max(br - sr, 0)
      dg = max(bg - sg, 0)
      db = max(bb - sb, 0)

    average: ->
      dr = (br + sr) / 2
      dg = (bg + sg) / 2
      db = (bb + sb) / 2

    negation: ->
      dr = 255 - abs(255 - sr - br)
      dg = 255 - abs(255 - sg - bg)
      db = 255 - abs(255 - sb - bb)
