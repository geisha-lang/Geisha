def fuck = (a, b) -> {
  if (a)
    a = a + 1
  else
    a = 3
  a + b
}

def omg = (a, b) -> {
  if (a) a + 1 else b
}

def shit = c ->
  let a = if (c) c + 3 else 8
  in {
    a = a + 1
    a
  }
