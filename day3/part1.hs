--input = 265149
input = 265149
t = fnod input
l = get_length t

coord l = ((l / 2), -(l / 2))

get_distance_to_zero (x,y) = abs x + abs y

find = get_distance_to_zero $ solve input (coord l) 'L' l t l

solve n c d s v l
  | n == v = c
  | s == 0 = solve n (update_coords (get_next_direction d) c) (get_next_direction d) l (v - 1) l
  | otherwise = solve n (update_coords d c) d (s - 1) (v - 1) l

update_coords dir (x, y)
  | dir == 'L' = ((x-1), y)
  | dir == 'R' = ((x+1), y)
  | dir == 'D' = (x, (y-1))
  | dir == 'U' = (x, (y+1))
  | otherwise = (x, y)

get_next_direction :: Char -> Char
get_next_direction dir
  | dir == 'L' = 'U'
  | dir == 'U' = 'R'
  | dir == 'R' = 'D'
  | otherwise = '?' -- Whats an empty char?

-- findNextOddPerfectSquare
fnod n = if n `mod` 2 /= 0 && is_square n then n else fnod (n + 1)

get_length n = (sqrt $ (fromIntegral n::Double)) - 1

is_square n = sq * sq == n
  where sq = floor $ sqrt $ (fromIntegral n::Double)
