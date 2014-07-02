--[[x,x,x],[x,x,x],[x,x,x]]

import System.Random
getRandom a = do {
  x <- randomRIO(0,5);
  return x;
}