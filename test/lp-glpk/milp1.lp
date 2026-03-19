maximize
 + 11 b01 + 14 b02 + 16 b03
 + 9 b04 + 18 b05 + 12 b06
 + 10 b07 + 15 b08 + 19 b09
 + 13 b10 + 17 b11 + 11 b12
 + 14 b13 + 12 b14 + 20 b15
 + 8 b16 + 21 b17 + 16 b18
 + 10 b19 + 13 b20 + 24 b21
 + 9 b22 + 18 b23 + 15 b24
subject to
 link01u: + g01 - b01 - b02 - b03 <= 0
 link01l: + g01 - b01 - b02 - b03 >= 0
 link02u: + g02 - b04 - b05 - b06 <= 0
 link02l: + g02 - b04 - b05 - b06 >= 0
 link03u: + g03 - b07 - b08 - b09 <= 0
 link03l: + g03 - b07 - b08 - b09 >= 0
 link04u: + g04 - b10 - b11 - b12 <= 0
 link04l: + g04 - b10 - b11 - b12 >= 0
 link05u: + g05 - b13 - b14 - b15 <= 0
 link05l: + g05 - b13 - b14 - b15 >= 0
 link06u: + g06 - b16 - b17 - b18 <= 0
 link06l: + g06 - b16 - b17 - b18 >= 0
 link07u: + g07 - b19 - b20 - b21 <= 0
 link07l: + g07 - b19 - b20 - b21 >= 0
 link08u: + g08 - b22 - b23 - b24 <= 0
 link08l: + g08 - b22 - b23 - b24 >= 0
 g01_min: + g01 >= 1
 g02_min: + g02 >= 1
 g03_min: + g03 >= 1
 g04_min: + g04 >= 1
 g05_min: + g05 >= 1
 g06_min: + g06 >= 1
 g07_min: + g07 >= 1
 g08_min: + g08 >= 1
 g01_max: + g01 <= 2
 g02_max: + g02 <= 2
 g03_max: + g03 <= 2
 g04_max: + g04 <= 2
 g05_max: + g05 <= 2
 g06_max: + g06 <= 2
 g07_max: + g07 <= 2
 g08_max: + g08 <= 2
 capacity:
  + 4 b01 + 5 b02 + 6 b03
  + 3 b04 + 7 b05 + 5 b06
  + 4 b07 + 6 b08 + 8 b09
  + 5 b10 + 7 b11 + 4 b12
  + 6 b13 + 5 b14 + 7 b15
  + 3 b16 + 8 b17 + 6 b18
  + 4 b19 + 5 b20 + 9 b21
  + 3 b22 + 7 b23 + 6 b24 <= 52
 count_min: + g01 + g02 + g03 + g04 + g05 + g06 + g07 + g08 >= 10
 count_max: + g01 + g02 + g03 + g04 + g05 + g06 + g07 + g08 <= 12
 top_tier: + b03 + b05 + b09 + b11 + b15 + b17 + b21 + b23 <= 5
bounds
 0 <= g01 <= 3
 0 <= g02 <= 3
 0 <= g03 <= 3
 0 <= g04 <= 3
 0 <= g05 <= 3
 0 <= g06 <= 3
 0 <= g07 <= 3
 0 <= g08 <= 3
binary
 b01 b02 b03 b04 b05 b06 b07 b08 b09 b10 b11 b12
 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24
general
 g01 g02 g03 g04 g05 g06 g07 g08
end
