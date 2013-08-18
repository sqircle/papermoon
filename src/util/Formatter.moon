class Formatter extends Base
  new: (precision) =>
    @precision  = precision or 5
    @multiplier = math.pow(10, @precision)

  number: (val) =>
    -- It would be nice to use Number--toFixed() instead, but it pads with 0,
    -- unecessarily consuming space.
    math.round(val * @multiplier) / @multiplier

  point: (val, separator) =>
    @number(val.x) + (separator or ",") + @number(val.y)

  size: (val, separator) =>
    @number(val.width) + (separator or ",") + @number(val.height)

  rectangle: (val, separator) =>
    @point(val, separator) + (separator or ",") + @size(val, separator)
