def shit = c ->
  let a = if (c == 8) c + 3 else 8
  in {
    a = a + 1
    a
  }

def fuck = a -> a + 5

def wtd = () -> let twoB = 3 in twoB

def hahah = () -> let a = 3 in { a }

def ha1 = c -> let a = c in {
  a = a + 1
  a
}

def ha2 = c -> let a = if (c == 8) c + 3 else 8 in a

def ha3 = c -> if (c == 9) c + 3 else 9

def main = (omg, holy) ->
  shit(omg(holy + 1))

