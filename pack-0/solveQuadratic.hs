solveQuadratic :: Float -> Float -> Float -> (Float, Float)
solveQuadratic a b c = if d < 0 then error "no solutions" else (x, y)
    where d = b * b - 4 * a * c
          x = (-b + sqrt d) / (2 * a)
          y = (-b - sqrt d) / (2 * a)
